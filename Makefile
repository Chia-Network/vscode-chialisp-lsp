all:
	cd wasm && wasm-pack build --release --target nodejs && cp -r pkg/* ../runner/build/ && cp -r pkg/* ../debug/build/
	(cd runner && npm install && npm run build)
	(cd debug && npm install && npm run build)
