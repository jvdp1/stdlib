#!/usr/bin/env bash

set -ex

# Get fypp preprocessor
fypp="${FYPP:-$(which fypp)}"

# Arguments for the fypp preprocessor
fyflags="${FYFLAGS:--DMAXRANK=4}"

# Number of parallel jobs for preprocessing
if [ $(uname) = "Darwin" ]; then
  njob="$(sysctl -n hw.ncpu)"
else
  njob="$(nproc)"
fi

# Files to remove from collection
prune=(
  "test/test_always_fail.f90"
  "test/test_always_skip.f90"
  "test/hash_functions/test_hash_functions.f90"
  "src/common.f90"
  "src/f18estop.f90"
)

major=$(cut -d. -f1 VERSION)
minor=$(cut -d. -f2 VERSION)
patch=$(cut -d. -f3 VERSION)
fyflags="${fyflags} -DPROJECT_VERSION_MAJOR=${major} -DPROJECT_VERSION_MINOR=${minor} -DPROJECT_VERSION_PATCH=${patch}"

# Preprocess stdlib sources
find src -maxdepth 1 -iname "*.fypp" \
  | cut -f1 -d. | xargs -P "$njob" -I{} "$fypp" "{}.fypp" "{}.f90" $fyflags

# Collect stdlib source files

rm "${prune[@]}"

