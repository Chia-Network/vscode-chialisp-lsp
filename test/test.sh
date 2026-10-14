#!/bin/bash

set -euo pipefail

rm -rf chialisp.json
rm -rf ./test/*.vsix
cp -r chialisp-*.vsix ./test
cd test
npm install

# Build the image before starting the browser test.
docker build -t code-server-test .

test_root="$PWD"
diagnostics="$test_root/diagnostics"
rm -rf "$diagnostics"
mkdir -p "$diagnostics"
workspace=""
server_pid=""
cleanup() {
    docker rm -f code-server-test >/dev/null 2>&1 || true
    if [ -n "$server_pid" ]; then
        wait "$server_pid" 2>/dev/null || true
        server_pid=""
    fi
    if [ -n "$workspace" ]; then
        rm -rf "$workspace"
        workspace=""
    fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

echo "Headless integration test (single attempt)"
cleanup
workspace="$(mktemp -d "${TMPDIR:-/tmp}/chialisp-headless.XXXXXX")"
# Keep test modifications isolated from the source fixtures.
tar --exclude='./node_modules' --exclude='./diagnostics' -cf - . | tar -xf - -C "$workspace"
ln -s "$test_root/node_modules" "$workspace/node_modules"
attempt_diagnostics="$diagnostics/attempt-1"
mkdir -p "$attempt_diagnostics"
HEADLESS_TEST_IMAGE_READY=1 TEST_WORKSPACE="$workspace" sh ./run-server.sh >"$attempt_diagnostics/server.log" 2>&1 &
server_pid=$!
/bin/bash ./wait-for-it.sh -t 90 -h localhost -p 8080

set +e
(cd "$workspace" && HEADLESS_TEST_DIAGNOSTICS="$attempt_diagnostics" ./node_modules/.bin/jest) 2>&1 | tee "$attempt_diagnostics/jest.log"
status=${PIPESTATUS[0]}
set -e
echo "$status" >"$attempt_diagnostics/exit-status.txt"
cleanup
exit "$status"
