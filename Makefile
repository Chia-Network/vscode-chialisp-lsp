all:
	cd wasm && wasm-pack build --release --target nodejs
	cd runner && npm install && npm run build
