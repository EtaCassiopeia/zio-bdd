#!/usr/bin/env bash
# Bump the zio-bdd install-snippet version across README + docs to <version>.
#
# Only lines that declare an `io.github.etacassiopeia` coordinate are touched, so a
# free-form version mention in prose is never disturbed. Idempotent: running it with
# the version the docs already carry produces no change.
#
# Usage: scripts/bump-doc-versions.sh <version>   (e.g. 1.5.0, or 1.5.0-RC1)
set -euo pipefail

version="${1:?usage: scripts/bump-doc-versions.sh <version> (e.g. 1.5.0)}"
if ! printf '%s' "$version" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+([-.][0-9A-Za-z.]+)?$'; then
  echo "error: '$version' is not a semantic version" >&2
  exit 2
fi

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
files=(README.md docs/quickstart.md docs/mock-adapters.md docs/mock-cookbook.md docs/migrating.md)

changed=0
for rel in "${files[@]}"; do
  f="$root/$rel"
  [ -f "$f" ] || continue
  # On any line carrying an io.github.etacassiopeia coordinate, bump the `% "x.y.z"` literal.
  # `/ge` builds the replacement as an expression, so the version needs no regex-escaping.
  VERSION="$version" perl -i -pe \
    'if (/io\.github\.etacassiopeia/) { s/(%\s*)"[0-9]+\.[0-9]+\.[0-9]+(?:[-.][0-9A-Za-z.]+)?"/$1 . q{"} . $ENV{VERSION} . q{"}/ge }' \
    "$f"
  if ! git -C "$root" diff --quiet -- "$rel" 2>/dev/null; then
    echo "  bumped $rel"
    changed=1
  fi
done

if [ "$changed" -eq 0 ]; then
  echo "docs already at $version — no changes"
fi
