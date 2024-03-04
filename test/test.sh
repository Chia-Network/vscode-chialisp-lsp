#!/bin/bash

set -e

rm -rf chialisp.json
rm -rf ./test/*.vsix
cp -r chialisp-*.vsix ./test
cd test
# Automatically bump chromedriver to keep the downloads
# synced.  After a few months, old chromedriver binaries
# cease being available.
npm install --save chromedriver@latest
npm install
sh ./run-server.sh &

/bin/bash ./wait-for-it.sh -t 90 -h localhost -p 8080

STATUS=1
if ./node_modules/.bin/jest ; then
    STATUS=0
else
    STATUS=1
fi

docker kill code-server-test
docker rm code-server-test

exit ${STATUS}
