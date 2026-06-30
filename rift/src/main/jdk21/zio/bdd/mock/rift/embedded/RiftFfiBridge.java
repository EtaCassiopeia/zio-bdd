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
 * Project Panama (FFM) bindings to the {@code librift_ffi} C-ABI (rift#204): an opaque handle +
 * JSON in / JSON out over the Rift engine, so {@code MockControl.embedded} can drive Rift
 * in-process with no Docker. Authored in Java because FFM is a preview API in JDK 21 and the
 * downcall {@link MethodHandle#invoke} is signature-polymorphic — both are handled natively by
 * javac (the {@code --enable-preview} class bit and the per-call-site descriptor), avoiding the
 * cross-language pitfalls of calling these from Scala.
 *
 * <p>Boundary discipline mirrors the crate: memory is created and freed on the same side
 * ({@link #recorded} hands the {@code *mut c_char} straight back to {@code rift_free}); the handle
 * owns a Tokio runtime on its own threads, so the blocking downcalls are wrapped in
 * {@code ZIO.attemptBlocking} by the Scala caller. One bridge is safe to share across threads —
 * the engine is {@code Sync} and every input string is marshalled in a per-call confined arena.
 *
 * <p>Errors are returned as the crate's sentinels (port {@code 0}, rc {@code -1}, null pointer),
 * never exceptions; a genuine FFM/link failure surfaces as a {@link RuntimeException} the caller
 * turns into a {@code MockError}.
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

  // rift_delete_all is intentionally not bound: it is a global reset that would tear down sibling
  // imposters, so the adapter destroys a single space via rift_replace_stubs([]) instead.
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
      short port = (short) createImposter.invoke(handle, call.allocateUtf8String(configJson));
      return port & 0xFFFF;
    } catch (Throwable t) {
      throw new RuntimeException("rift_create_imposter downcall failed", t);
    }
  }

  /** Replace all stubs on {@code port} from a JSON array. Returns {@code 0} on success, {@code -1} on error. */
  public int replaceStubs(int port, String stubsJson) {
    try (Arena call = Arena.ofConfined()) {
      return (int) replaceStubs.invoke(handle, (short) port, call.allocateUtf8String(stubsJson));
    } catch (Throwable t) {
      throw new RuntimeException("rift_replace_stubs downcall failed", t);
    }
  }

  /**
   * The recorded requests for {@code port} as a JSON array string, or {@code null} on any error
   * (unknown port, encode failure). The native buffer is freed via {@code rift_free} before return.
   */
  public String recorded(int port) {
    try {
      MemorySegment result = (MemorySegment) recorded.invoke(handle, (short) port);
      if (result.address() == 0L) {
        return null;
      }
      Throwable primary = null;
      try {
        return result.reinterpret(Long.MAX_VALUE).getUtf8String(0);
      } catch (Throwable t) {
        primary = t;
        throw t;
      } finally {
        // Free the buffer either way, but never let a free failure mask the read failure.
        try {
          free.invoke(result);
        } catch (Throwable t) {
          if (primary != null) primary.addSuppressed(t);
          else throw t;
        }
      }
    } catch (Throwable t) {
      throw new RuntimeException("rift_recorded downcall failed", t);
    }
  }

  /**
   * Stop the engine ({@code rift_stop}) and release the native library. Not idempotent — call
   * exactly once; the scoped layer guarantees a single release.
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
}
