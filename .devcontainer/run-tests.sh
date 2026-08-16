#!/usr/bin/env bash
#
# Run the tinyplot test suite inside a Linux container.
#
# Snapshot tests compare against SVGs rendered on Linux with Liberation fonts,
# so they only pass in a Linux environment. This script provides a terminal
# alternative to the VS Code devcontainer: same base image, no editor required.
#
# On Apple Silicon, native (arm64) runs produce a couple of snapshot diffs
# because the reference SVGs were rendered on amd64 (CI). Use PLATFORM=linux/amd64
# for an arm64-faithful, CI-matching run (slower, emulated). See `make testall-ci`.
#
# Usage:
#   .devcontainer/run-tests.sh                       # whole suite
#   .devcontainer/run-tests.sh inst/tinytest/test-legend.R   # one file
#   .devcontainer/run-tests.sh -i                    # interactive R session
#   .devcontainer/run-tests.sh -s                    # shell in the container
#   .devcontainer/run-tests.sh -b                    # force image rebuild
#
# Env vars:
#   RUNTIME   container CLI to use (default: first of docker/podman/finch/nerdctl)
#   PLATFORM  e.g. linux/amd64 to match CI's architecture (default: native)

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DOCKERFILE="$REPO/.devcontainer/Dockerfile"

# Tag per platform, so that native and emulated images coexist instead of
# clobbering each other's tag and forcing a rebuild on every switch.
if [[ -n "${PLATFORM:-}" ]]; then
  IMAGE="tinyplot-test:${PLATFORM##*/}"
else
  IMAGE="tinyplot-test:local"
fi

# Pick a container runtime. Finch, nerdctl and podman are all CLI-compatible
# with docker for the handful of subcommands used here.
if [[ -z "${RUNTIME:-}" ]]; then
  for c in docker podman finch nerdctl; do
    if command -v "$c" >/dev/null 2>&1; then RUNTIME="$c"; break; fi
  done
fi
if [[ -z "${RUNTIME:-}" ]]; then
  echo "error: no container runtime found (tried docker, podman, finch, nerdctl)." >&2
  echo "Set RUNTIME=<cli> to override." >&2
  exit 1
fi

# Note: macOS ships bash 3.2, where expanding an empty array under `set -u`
# is an "unbound variable" error, so every optional-args array below is guarded
# with `${arr[@]+"${arr[@]}"}` rather than a bare `"${arr[@]}"`.
PLATFORM_ARG=()
[[ -n "${PLATFORM:-}" ]] && PLATFORM_ARG=(--platform "$PLATFORM")

REBUILD=0
MODE="test"
while getopts ":bis" opt; do
  case $opt in
    b) REBUILD=1 ;;
    i) MODE="R" ;;
    s) MODE="shell" ;;
    \?) echo "error: unknown option -$OPTARG" >&2; exit 1 ;;
  esac
done
shift $((OPTIND - 1))

# Build if the image is missing, if -b was passed, or if the Dockerfile is
# newer than the image we last built from it.
STAMP="$REPO/.devcontainer/.build-stamp-${IMAGE##*:}"
needs_build=$REBUILD
if ! "$RUNTIME" image inspect "$IMAGE" >/dev/null 2>&1; then
  needs_build=1
elif [[ "$DOCKERFILE" -nt "$STAMP" ]]; then
  needs_build=1
fi

if [[ $needs_build -eq 1 ]]; then
  echo ">> building $IMAGE via $RUNTIME"
  "$RUNTIME" build ${PLATFORM_ARG[@]+"${PLATFORM_ARG[@]}"} \
    -t "$IMAGE" -f "$DOCKERFILE" "$REPO"
  touch "$STAMP"
fi

# On Linux, bind-mounted files are written back as root unless we map the
# caller's uid/gid. Docker Desktop, Finch and podman already handle this on
# macOS, so only do it where it is needed.
USER_ARG=()
if [[ "$(uname -s)" == "Linux" && "$RUNTIME" != "podman" ]]; then
  USER_ARG=(--user "$(id -u):$(id -g)")
fi

RUN=("$RUNTIME" run --rm
     ${PLATFORM_ARG[@]+"${PLATFORM_ARG[@]}"}
     ${USER_ARG[@]+"${USER_ARG[@]}"}
     -v "$REPO":/work -w /work -e NOT_CRAN=true)

case "$MODE" in
  shell) exec "${RUN[@]}" -it "$IMAGE" bash ;;
  R)     exec "${RUN[@]}" -it "$IMAGE" R -q ;;
esac

if [[ $# -gt 0 ]]; then
  # Run only the named test file(s).
  for f in "$@"; do
    echo ">> $f"
    "${RUN[@]}" "$IMAGE" Rscript -e \
      "pkgload::load_all(quiet=TRUE);tinytest::run_test_file('$f')"
  done
else
  exec "${RUN[@]}" "$IMAGE" Rscript -e \
    "pkgload::load_all(quiet=TRUE);tinytest::run_test_dir('inst/tinytest')"
fi
