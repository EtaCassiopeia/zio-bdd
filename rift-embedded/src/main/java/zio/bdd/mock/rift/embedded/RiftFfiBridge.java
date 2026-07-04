package zio.bdd.mock.rift.embedded;

import java.lang.foreign.Arena;
import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.SymbolLookup;
import java.lang.foreign.ValueLayout;
import java.lang.invoke.MethodHandle;
import java.nio.file.Path;

/**
 * Project Panama (FFM) bindings to the {@code librift_ffi} C-ABI: an opaque handle + JSON in / JSON
 * out over the Rift engine, so the embedded provider can drive Rift in-process with no Docker.
 * Compiled against the '''stable''' Foreign Function &amp; Memory API (JEP 454, final in JDK 22), so this
 * module requires JDK 22+ at runtime (plus {@code --enable-native-access}) — no {@code --enable-preview},
 * and the class is not version-locked to a single JDK. Authored in Java because the downcall
 * {@link MethodHandle#invoke} is signature-polymorphic (handled natively by javac per call site),
 * avoiding the cross-language pitfalls of calling these from Scala.
 *
 * <p>Requires the C-ABI v2 (rift#343, first shipped in {@code librift_ffi} v0.9.0): the data plane
 * ({@code rift_start/stop/create_imposter/replace_stubs/recorded/free}) plus the in-process admin
 * plane ({@code rift_serve_admin}), per-imposter delete ({@code rift_delete_imposter}), build
 * identity ({@code rift_build_info}), and thread-local error text ({@code rift_last_error}). All
 * symbols are bound at {@link #start}, so a pre-v2 library fails fast there with a missing-symbol
 * error rather than degrading silently — zio-bdd pins the v2 natives and does not support older Rift.
 *
 * <p>Boundary discipline mirrors the crate: memory is created and freed on the same side (returned
 * {@code *mut c_char} buffers are handed straight back to {@code rift_free}; {@code rift_build_info}
 * is the one static string that must NOT be freed); the handle owns a Tokio runtime on its own
 * threads, so the blocking downcalls are wrapped in {@code ZIO.attemptBlocking} by the Scala caller.
 * One bridge is safe to share across threads — the engine is {@code Sync} and every input string is
 * marshalled in a per-call confined arena.
 *
 * <p>Errors are returned as the crate's sentinels (port {@code 0}, rc {@code -1}, null pointer),
 * never exceptions; the library also records a thread-local reason ({@code rift_last_error}) that a
 * failed call folds into its {@link RuntimeException} so the caller's {@code MockError} carries the
 * engine-side message instead of "see engine tracing". A genuine FFM/link failure surfaces as a
 * {@link RuntimeException} the caller turns into a {@code MockError}.
 */
public final class RiftFfiBridge implements AutoCloseable {

  private static final Linker LINKER = Linker.nativeLinker();

  private final Arena arena;
  private final MemorySegment handle;
  private final MethodHandle createImposter;
  private final MethodHandle replaceStubs;
  private final MethodHandle recorded;
  private final MethodHandle free;
  private final MethodHandle stop;

  // v2 (rift#343): the in-process admin plane, per-imposter delete, build identity, thread-local error.
  private final MethodHandle serveAdmin;
  private final MethodHandle deleteImposter;
  private final MethodHandle buildInfo;
  private final MethodHandle lastError;

  private RiftFfiBridge(Arena arena, SymbolLookup lookup, MemorySegment handle) {
    this.arena = arena;
    this.handle = handle;
    this.createImposter = downcall(lookup, "rift_create_imposter",
        FunctionDescriptor.of(ValueLayout.JAVA_SHORT, ValueLayout.ADDRESS, ValueLayout.ADDRESS));
    this.replaceStubs = downcall(lookup, "rift_replace_stubs",
        FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.JAVA_SHORT, ValueLayout.ADDRESS));
    this.recorded = downcall(lookup, "rift_recorded",
        FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_SHORT));
    this.free = downcall(lookup, "rift_free", FunctionDescriptor.ofVoid(ValueLayout.ADDRESS));
    this.stop = downcall(lookup, "rift_stop", FunctionDescriptor.ofVoid(ValueLayout.ADDRESS));
    // v2 symbols are mandatory: binding them here makes a pre-v2 library fail fast at start with a
    // missing-symbol error (rift_build_info being the canonical v2 marker).
    this.buildInfo = downcall(lookup, "rift_build_info", FunctionDescriptor.of(ValueLayout.ADDRESS));
    this.serveAdmin = downcall(lookup, "rift_serve_admin",
        FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS));
    this.deleteImposter = downcall(lookup, "rift_delete_imposter",
        FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.JAVA_SHORT));
    this.lastError = downcall(lookup, "rift_last_error", FunctionDescriptor.of(ValueLayout.ADDRESS));
  }

  /**
   * Load the native library, link the symbols, and start the engine ({@code rift_start}). Throws
   * if the library cannot be loaded/linked or the engine's runtime could not be created.
   */
  public static RiftFfiBridge start(String libPath) throws Throwable {
    Arena arena = Arena.ofShared();
    try {
      SymbolLookup lookup = SymbolLookup.libraryLookup(Path.of(libPath), arena);
      MethodHandle startH = downcall(lookup, "rift_start", FunctionDescriptor.of(ValueLayout.ADDRESS));
      MemorySegment handle = (MemorySegment) startH.invoke();
      if (handle.address() == 0L) {
        throw new IllegalStateException("rift_start returned null (engine runtime could not be created)");
      }
      return new RiftFfiBridge(arena, lookup, handle);
    } catch (Throwable t) {
      arena.close();
      throw t;
    }
  }

  private static MethodHandle downcall(SymbolLookup lookup, String name, FunctionDescriptor descriptor) {
    MemorySegment symbol = lookup.find(name)
        .orElseThrow(() -> new IllegalStateException("librift_ffi missing symbol: " + name));
    return LINKER.downcallHandle(symbol, descriptor);
  }

  /** Create an imposter from a JSON config. Returns its bound port, or {@code 0} on any error. */
  public int createImposter(String configJson) {
    try (Arena call = Arena.ofConfined()) {
      short port = (short) createImposter.invoke(handle, call.allocateFrom(configJson));
      return port & 0xFFFF;
    } catch (Throwable t) {
      throw failure("rift_create_imposter", t);
    }
  }

  /** Replace all stubs on {@code port} from a JSON array. Returns {@code 0} on success, {@code -1} on error. */
  public int replaceStubs(int port, String stubsJson) {
    try (Arena call = Arena.ofConfined()) {
      return (int) replaceStubs.invoke(handle, (short) port, call.allocateFrom(stubsJson));
    } catch (Throwable t) {
      throw failure("rift_replace_stubs", t);
    }
  }

  /**
   * The recorded requests for {@code port} as a JSON array string, or {@code null} on any error
   * (unknown port, encode failure). The native buffer is freed via {@code rift_free} before return.
   */
  public String recorded(int port) {
    try {
      MemorySegment result = (MemorySegment) recorded.invoke(handle, (short) port);
      return readAndFree(result);
    } catch (Throwable t) {
      throw failure("rift_recorded", t);
    }
  }

  /**
   * Start the in-process admin API from an options JSON ({@code {"host":..,"port":..}}); returns the
   * admin-info JSON ({@code {"adminPort":N,"adminUrl":"http://..","metricsPort":..}}). Throws with the
   * engine's {@code rift_last_error} message on a null sentinel (bad JSON, bind failure, or a plane
   * already serving — one plane per handle).
   */
  public String serveAdmin(String optionsJson) {
    MemorySegment result;
    try (Arena call = Arena.ofConfined()) {
      // The returned buffer is engine-owned, independent of `call`, so reading it after the arena
      // closes is safe — only the input string lives in `call`.
      result = (MemorySegment) serveAdmin.invoke(handle, call.allocateFrom(optionsJson));
    } catch (Throwable t) {
      throw failure("rift_serve_admin", t);
    }
    String json;
    try {
      json = readAndFree(result);
    } catch (Throwable t) {
      throw failure("rift_serve_admin", t);
    }
    if (json == null) {
      throw new IllegalStateException("rift_serve_admin returned null" + lastErrorSuffix());
    }
    return json;
  }

  /** Delete one imposter, freeing its port. Returns {@code 0} on success, {@code -1} on error. */
  public int deleteImposter(int port) {
    try {
      return (int) deleteImposter.invoke(handle, (short) port);
    } catch (Throwable t) {
      throw failure("rift_delete_imposter", t);
    }
  }

  /**
   * The engine's build identity JSON ({@code {"version":..,"commit":..,"builtAt":..,"features":[..]}}).
   * The pointer is a STATIC string owned by the library — it is read but never freed.
   */
  public String buildInfo() {
    try {
      MemorySegment result = (MemorySegment) buildInfo.invoke();
      if (result.address() == 0L) {
        return null;
      }
      return result.reinterpret(Long.MAX_VALUE).getString(0);
    } catch (Throwable t) {
      throw failure("rift_build_info", t);
    }
  }

  /**
   * Stop the engine ({@code rift_stop}) and release the native library. Not idempotent — call
   * exactly once; the scoped layer guarantees a single release. In v2 mode {@code rift_stop} also
   * shuts the admin/metrics listeners down before the manager.
   */
  @Override
  public void close() {
    Throwable primary = null;
    try {
      stop.invoke(handle);
    } catch (Throwable t) {
      primary = new RuntimeException("rift_stop downcall failed", t);
      throw (RuntimeException) primary;
    } finally {
      // Always unload the library; a close failure must not mask a rift_stop failure.
      try {
        arena.close();
      } catch (Throwable t) {
        if (primary != null) primary.addSuppressed(t);
        else throw t;
      }
    }
  }

  // Read a returned *mut c_char (or null → Java null), freeing it via rift_free either way — never
  // letting a free failure mask the read failure. Shared by recorded / serve_admin.
  private String readAndFree(MemorySegment result) throws Throwable {
    if (result.address() == 0L) {
      return null;
    }
    Throwable primary = null;
    try {
      return result.reinterpret(Long.MAX_VALUE).getString(0);
    } catch (Throwable t) {
      primary = t;
      throw t;
    } finally {
      try {
        free.invoke(result);
      } catch (Throwable t) {
        if (primary != null) primary.addSuppressed(t);
        else throw t;
      }
    }
  }

  /**
   * The engine's thread-local last-error text ({@code rift_last_error}) recorded by a failed
   * {@code rift_*} call on '''this''' thread, or {@code null} if none. Callers reading it after a
   * returned sentinel (port {@code 0}, rc {@code -1}, null) MUST do so on the same thread as the
   * failing call — the crate confines the error to that thread and clears it on read. Best-effort:
   * never throws.
   */
  public String lastError() {
    return lastErrorText();
  }

  // The thread-local last-error text from the engine (null when none). Reading it clears it on the
  // crate side, and the buffer is freed via rift_free.
  private String lastErrorText() {
    try {
      MemorySegment result = (MemorySegment) lastError.invoke();
      return readAndFree(result);
    } catch (Throwable t) {
      return null; // best-effort diagnostics — never let error-reading mask the original failure
    }
  }

  private String lastErrorSuffix() {
    String msg = lastErrorText();
    return (msg == null || msg.isEmpty()) ? "" : " (" + msg + ")";
  }

  private RuntimeException failure(String op, Throwable t) {
    return new RuntimeException(op + " downcall failed" + lastErrorSuffix(), t);
  }
}
