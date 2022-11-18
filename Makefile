all:
	# Pull and build clvm_tools_rs
	git clone https://github.com/Chia-Network/clvm_tools_rs
	cd clvm_tools_rs/wasm && git checkout 20221117-add-code-action-for-unknown-include && wasm-pack build --target=nodejs && cp pkg/* ../../runner/build
	# Build the runner
	cd runner && npm install && npm run build
	# Remove the subdir post-build
	rm -rf clvm_tools_rs
