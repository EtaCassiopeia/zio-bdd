#!/usr/bin/env bash
# Gate for the MockControl docs (#188): objective accuracy checks over docs/. Fails (non-zero) on any
# missing page, missing nav link, broken internal link, or stale/incorrect version/capability fact.
set -uo pipefail
cd "$(dirname "$0")/.." || exit 2
fail=0
err() { echo "FAIL: $*"; fail=1; }

NEW_PAGES=(mocking mock-dsl mock-adapters mock-gherkin mock-advanced mock-cookbook)

# 1. All six new pages exist
for p in "${NEW_PAGES[@]}"; do
  [[ -f "docs/$p.md" ]] || err "missing page docs/$p.md"
done

# 2. index.md links every new page
for p in "${NEW_PAGES[@]}"; do
  grep -q "$p.md" docs/index.md || err "docs/index.md does not link $p.md"
done

# 3. No broken internal .md links across docs/ (every [..](x.md) target must exist)
while IFS= read -r line; do
  f="${line%%:*}"; tgt="${line##*:}"
  base="${tgt%%#*}"                      # strip anchors
  [[ -z "$base" || "$base" == http* ]] && continue
  [[ -f "docs/$base" ]] || err "broken internal link in $f -> $base"
done < <(grep -rhoE "\]\(([a-zA-Z0-9._-]+\.md)(#[a-zA-Z0-9_-]+)?\)" docs/ 2>/dev/null \
          | sed -E 's/\]\(//; s/\)//' | awk -F: '{print FILENAME":"$0}' FILENAME="docs" | sort -u \
          || true)

# 4. Version accuracy in the mock pages. Derive the expected version from the install-snippet
#    coordinates themselves (io.github.etacassiopeia %% "zio-bdd-*" % "x.y.z") rather than pinning a
#    literal that silently rots on every release: the old `1.2.0` pin stuck through the entire 1.4.x
#    line and failed on master (#313). scripts/bump-doc-versions.sh rewrites those coordinates on
#    release, so this gate stays green as long as the snippets are present, mutually consistent, and
#    a real published (non-SNAPSHOT, non-pre-1.2) version.
MOCK_MD=$(printf 'docs/%s.md ' "${NEW_PAGES[@]}")
mock_versions=$(grep -rhE 'io\.github\.etacassiopeia' $MOCK_MD \
  | grep -oE '%[[:space:]]*"[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z.]+)?"' \
  | grep -oE '[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z.]+)?' | sort -u)
if [[ -z "$mock_versions" ]]; then
  err "no zio-bdd install-snippet version found in the mock pages"
elif [[ $(printf '%s\n' "$mock_versions" | grep -c .) -ne 1 ]]; then
  err "inconsistent install-snippet versions in the mock pages: $(echo $mock_versions)"
elif printf '%s' "$mock_versions" | grep -qE 'SNAPSHOT|^1\.(0|1)\.0$'; then
  err "stale or SNAPSHOT install-snippet version in a mock page: $mock_versions"
fi

# 5. Rift container image: current default is v0.9.0; no stale v0.8.0 presented as the default
if grep -RnE 'rift-proxy:v0\.8\.0' docs/mock-adapters.md; then err "stale Rift image v0.8.0 in mock-adapters.md"; fi

# 6. Capability matrix: embedded is capability-complete (all six); no stale "no capabilities"/"four"
if grep -RniE 'embedded[^.]{0,40}(no capabilit|four capabilit|only four)' $MOCK_MD; then err "stale 'embedded has no/four capabilities' claim"; fi
grep -Rq 'StateInspection' docs/mock-adapters.md || err "capability matrix missing StateInspection in mock-adapters.md"

# 7. Embedded runtime story (#285: single `zio-bdd-rift` artifact, no standalone embedded/natives
#    jars): the doc must name the single artifact plus the runtime rift-java-embedded/rift-java-natives
#    ServiceLoader dependencies and --enable-native-access, and must NOT still advertise the deleted
#    two-artifact/JDK-21-split world.
A=docs/mock-adapters.md
grep -q 'zio-bdd-rift' "$A" || err "mock-adapters.md missing the zio-bdd-rift artifact coordinate"
grep -q 'rift-java-embedded' "$A" || err "mock-adapters.md missing the rift-java-embedded runtime dependency"
grep -q 'rift-java-natives' "$A" || err "mock-adapters.md missing the rift-java-natives runtime dependency"
grep -q 'enable-native-access' "$A" || err "mock-adapters.md missing --enable-native-access"
grep -q 'zio-bdd-rift-embedded-jdk21' "$A" && err "mock-adapters.md still references the deleted zio-bdd-rift-embedded-jdk21 artifact"
grep -q 'zio-bdd-rift-embedded-natives' "$A" && err "mock-adapters.md still references the deleted zio-bdd-rift-embedded-natives artifact"

# 8. Used By section seeded with Rift (README or docs landing)
grep -RqiE 'used by' README.md docs/index.md && grep -RqiE '\[?rift\]?\(https://github.com/EtaCassiopeia/rift' README.md docs/index.md \
  || err "no 'Used By' section listing Rift (README.md or docs/index.md)"

if [[ $fail -eq 0 ]]; then echo "OK: mock docs gate passed"; fi
exit $fail
