#!/bin/sh

docker kill code-server-test
docker rm code-server-test
docker build -t code-server-test .
docker run -it --name code-server-test -p 127.0.0.1:8080:8080 \
  -v "$PWD/config:/home/coder/.config" \
  -v "$PWD:/home/coder/project" \
  -u "$(id -u):$(id -g)" \
  -e "DOCKER_USER=$USER" \
  code-server-test
