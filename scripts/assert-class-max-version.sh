#!/usr/bin/env bash
#
# Fail if a .class file's bytecode major version exceeds a ceiling.
#
# Guards issue #247: the JDK-21 embedded FFM bridge (`RiftFfiBridge`) must stay class-file major
# version <= 65 so it loads on a JDK-21 runtime. v1.3.1 shipped it as class 66 (Java 22) with nothing
# preventing the regression; this script is wired into the CI preview job and the release publish job.
#
# Reads the major version straight from the class-file header: bytes 0-3 are the 0xCAFEBABE magic,
# bytes 4-5 the minor version, bytes 6-7 the (big-endian) major version. A preview class (e.g.
# class 65 built with --enable-preview) has minor 0xFFFF but the major is still 65, so this is the
# right field to compare. No JDK toolchain required.
#
# Usage: assert-class-max-version.sh <class-file> <max-major>
#
# `set -u` is load-bearing: on a truncated/non-.class file `od` yields too few tokens, so the
# positional args below are unset and the arithmetic aborts rather than silently computing major=0.
set -euo pipefail

class_file=${1:?usage: assert-class-max-version.sh <class-file> <max-major>}
max_major=${2:?usage: assert-class-max-version.sh <class-file> <max-major>}

if [ ! -f "$class_file" ]; then
  echo "::error::class file not found: $class_file" >&2
  exit 2
fi

# Verify the 0xCAFEBABE magic before trusting the version bytes, so a non-class file (a partial
# compile, a wrong path) can't slip through as a bogus "major version 0".
magic=$(od -An -tx1 -N4 "$class_file" | tr -d ' \n')
if [ "$magic" != "cafebabe" ]; then
  echo "::error::$class_file is not a Java class file (magic=$magic)" >&2
  exit 2
fi

# Word-split the two bytes on any whitespace (od's spacing differs between GNU and BSD).
set -- $(od -An -tu1 -j6 -N2 "$class_file")
major=$(( $1 * 256 + $2 ))

if [ "$major" -gt "$max_major" ]; then
  echo "::error::$class_file is class-file major version $major (> $max_major) — won't load on the target JDK" >&2
  exit 1
fi

echo "OK: $(basename "$class_file") is class-file major version $major (<= $max_major)"
