#!/bin/sh

set -e

# Keep direct invocations usable; test.sh has already built the image once.
docker rm -f code-server-test >/dev/null 2>&1 || true
if [ "${HEADLESS_TEST_IMAGE_READY:-0}" != "1" ]; then
    docker build -t code-server-test .
fi
workspace="${TEST_WORKSPACE:-$PWD}"
exec docker run --name code-server-test -p 127.0.0.1:8080:8080 \
  -v "$workspace/config:/home/coder/.config" \
  -v "$workspace:/home/coder/project" \
  -u "$(id -u):$(id -g)" \
  -e "DOCKER_USER=$USER" \
  code-server-test
