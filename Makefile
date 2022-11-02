all:
	# Pull and build clvm_tools_rs
	git clone https://github.com/Chia-Network/clvm_tools_rs
	cd clvm_tools_rs/wasm && git checkout 20220805-language-server && wasm-pack build --target=nodejs && cp pkg/* ../../runner/build
	# Build the runner
	cd runner && npm install && npm link clvm_tools_wasm && npm run build
	# Remove the subdir post-build
	rm -rf clvm_tools_rs
