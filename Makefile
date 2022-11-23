all:
	cd wasm && wasm-pack build --target nodejs
	cd runner && npm run build
