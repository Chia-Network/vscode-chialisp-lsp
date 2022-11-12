cp -r chialisp-*.vsix ./test
cd test
npm install
sh ./run-server.sh &

/bin/bash ./wait-for-it.sh -h localhost -p 8080

STATUS=1
if ./node_modules/.bin/jest ; then
    STATUS=0
else
    STATUS=1
fi

docker kill code-server-test
docker rm code-server-test

exit ${STATUS}
