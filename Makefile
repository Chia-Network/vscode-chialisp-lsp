all:
	cd wasm && wasm-pack build --release --target nodejs && cp -r pkg/* ../runner/build/
	cd runner && npm install && npm run build
