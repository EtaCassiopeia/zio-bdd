#!/usr/bin/env bash
#
# Fail if a published POM exposes a first-party artifact it shouldn't at a consumer-visible scope.
#
# Guards issue #331: `zio-bdd-mock-conformance` (#329) publishes with its compile scope deliberately
# reduced to the SPI (`zio-bdd-mock`) — the bundled adapters (rift, wiremock, rift-embedded, natives)
# are Test-scope only, so a third-party consumer inherits no backend. Nothing else guards this: MIMA
# is a no-op for the module (`mimaPreviousArtifacts := Set.empty`) and `sbt test` never inspects a POM,
# so a future `.dependsOn(<adapter>)` that forgets `% Test` would silently re-leak an adapter into the
# published POM. This script is wired into the JDK-22 CI job (embedded-stable) and the release publish
# job — the only builds where `stableFfm` adds the embedded deps, so the leak class is actually present.
#
# Rule: every <dependency> whose <groupId> equals <group-id> and whose EFFECTIVE scope is inherited by
# a downstream consumer (`compile` or `runtime`) must be the SPI itself — <artifactId> exactly
# <allowed-prefix>, modulo the Scala-version suffix (`zio-bdd-mock_3`). Any other first-party
# compile/runtime dep is a leak, and a POM that shows no SPI dep at all is treated as malformed.
#
# Parsed with xml.etree, not grep/sed: <scope> is optional in Maven and its ABSENCE means `compile`
# (Maven's default), so a scope-less dependency — exactly what a dropped `% Test` produces — is the
# leak we most need to catch, and a text match on `<scope>test</scope>` would sail right past it.
#
# Usage: assert-pom-compile-scope.sh <pom-file> <group-id> <allowed-artifactId-prefix>
#   e.g. assert-pom-compile-scope.sh conformance/target/.../zio-bdd-mock-conformance_3-1.4.3.pom \
#          io.github.etacassiopeia zio-bdd-mock
#
# `set -u` is load-bearing: a missing positional arg aborts the run rather than silently checking
# against an empty group-id / prefix (which would pass everything).
set -euo pipefail

pom_file=${1:?usage: assert-pom-compile-scope.sh <pom-file> <group-id> <allowed-artifactId-prefix>}
group_id=${2:?usage: assert-pom-compile-scope.sh <pom-file> <group-id> <allowed-artifactId-prefix>}
allowed_prefix=${3:?usage: assert-pom-compile-scope.sh <pom-file> <group-id> <allowed-artifactId-prefix>}

if [ ! -f "$pom_file" ]; then
  echo "::error::POM file not found: $pom_file" >&2
  exit 2
fi

POM_FILE="$pom_file" GROUP_ID="$group_id" ALLOWED_PREFIX="$allowed_prefix" python3 - <<'PY'
import os, sys, xml.etree.ElementTree as ET

pom, group, prefix = os.environ["POM_FILE"], os.environ["GROUP_ID"], os.environ["ALLOWED_PREFIX"]

try:
    root = ET.parse(pom).getroot()
except ET.ParseError as e:
    print(f"::error::{pom} is not valid XML: {e}", file=sys.stderr)
    sys.exit(2)

# The POM's default namespace is unpredictable across generators; match on the local tag name.
def local(tag):
    return tag.rsplit("}", 1)[-1]

def child_text(el, name):
    for c in el:
        if local(c.tag) == name:
            return (c.text or "").strip()
    return None

# Scopes a downstream consumer transitively inherits. `test`/`provided` never reach a consumer's
# classpath; `compile` AND `runtime` both do, so both must be free of bundled adapters. An absent
# <scope> is Maven's default `compile` — exactly what a dropped `% Test` produces.
INHERITED = ("compile", "runtime")

def is_spi(aid):
    # The only allowed artifact is the SPI itself, `<prefix>`, modulo its Scala-version suffix
    # (`zio-bdd-mock_3`). A prefix-*creep* adapter such as `zio-bdd-mock-embedded_3` shares the
    # leading text but not this shape, so match `<prefix>` or `<prefix>_...` exactly — never a
    # looser `startswith(prefix)`, since prefix creep is the very failure class this guard exists for.
    return aid is not None and (aid == prefix or aid.startswith(prefix + "_"))

leaks = []
saw_spi = False
for dep in root.iter():
    if local(dep.tag) != "dependency":
        continue
    if child_text(dep, "groupId") != group:
        continue
    scope = child_text(dep, "scope") or "compile"
    if scope not in INHERITED:
        continue
    aid = child_text(dep, "artifactId")
    if is_spi(aid):
        saw_spi = True
    else:
        # A first-party inherited-scope dep with no artifactId is fail-open in a fail-closed guard;
        # flag it rather than skip it.
        leaks.append(aid or "<missing artifactId>")

if leaks:
    joined = ", ".join(sorted(leaks))
    print(
        f"::error::{pom}: consumer-visible scope leak — group '{group}' must expose only '{prefix}' "
        f"(the SPI) at compile/runtime scope, but found: {joined}. "
        f"Add `% Test` to the offending dependsOn/libraryDependency.",
        file=sys.stderr,
    )
    sys.exit(1)

if not saw_spi:
    # A well-formed but SPI-less POM proves nothing — a wrong-module or truncated makePom. Fail loud
    # rather than print OK on a POM that never actually demonstrated the invariant.
    print(
        f"::error::{pom}: found no '{prefix}' dependency at compile/runtime scope — this is not the "
        f"expected conformance artifact (wrong module, or a broken makePom?).",
        file=sys.stderr,
    )
    sys.exit(2)

print(f"OK: {os.path.basename(pom)} exposes only '{prefix}' (the SPI) at compile/runtime scope for '{group}'")
PY
