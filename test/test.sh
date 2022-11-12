cp -r chialisp-*.vsix ./test
cd test
npm install
sh ./run-server.sh &

/bin/bash ./wait-for-it.sh -h localhost -p 8080

./node_modules/.bin/jest

docker kill code-server-test
docker rm code-server-test
