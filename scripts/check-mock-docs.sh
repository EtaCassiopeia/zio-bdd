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

# 4. Version accuracy in the new mock pages
MOCK_MD=$(printf 'docs/%s.md ' "${NEW_PAGES[@]}")
grep -Rq '1.2.0' $MOCK_MD || err "no 1.2.0 version reference in the mock pages"
if grep -RnE '"zio-bdd[a-z-]*" +% +"1\.(0|1)\.0"' $MOCK_MD; then err "stale 1.0.0/1.1.0 dependency version in a mock page"; fi
if grep -Rn '1.2.0-SNAPSHOT' $MOCK_MD; then err "SNAPSHOT version leaked into a mock page"; fi

# 5. Rift container image: current default is v0.9.0; no stale v0.8.0 presented as the default
if grep -RnE 'rift-proxy:v0\.8\.0' docs/mock-adapters.md; then err "stale Rift image v0.8.0 in mock-adapters.md"; fi

# 6. Capability matrix: embedded is capability-complete (all six); no stale "no capabilities"/"four"
if grep -RniE 'embedded[^.]{0,40}(no capabilit|four capabilit|only four)' $MOCK_MD; then err "stale 'embedded has no/four capabilities' claim"; fi
grep -Rq 'StateInspection' docs/mock-adapters.md || err "capability matrix missing StateInspection in mock-adapters.md"

# 7. Embedded JDK story: both artifacts + both JDKs + LuaJIT + native-access, in mock-adapters.md
A=docs/mock-adapters.md
grep -q 'zio-bdd-rift-embedded-jdk21' "$A" || err "mock-adapters.md missing the JDK-21 embedded artifact"
grep -q 'zio-bdd-rift-embedded-natives' "$A" || err "mock-adapters.md missing the embedded-natives artifact"
grep -qE 'JDK ?21' "$A" && grep -qE 'JDK ?22' "$A" || err "mock-adapters.md missing the JDK 21/22 split"
grep -q 'enable-native-access' "$A" || err "mock-adapters.md missing --enable-native-access"
grep -qiE 'luajit' "$A" || err "mock-adapters.md missing the LuaJIT prerequisite"

# 8. Used By section seeded with Rift (README or docs landing)
grep -RqiE 'used by' README.md docs/index.md && grep -RqiE '\[?rift\]?\(https://github.com/EtaCassiopeia/rift' README.md docs/index.md \
  || err "no 'Used By' section listing Rift (README.md or docs/index.md)"

if [[ $fail -eq 0 ]]; then echo "OK: mock docs gate passed"; fi
exit $fail
