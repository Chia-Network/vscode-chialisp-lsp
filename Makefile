all:
	# Pull and build clvm_tools_rs
	git clone https://github.com/Chia-Network/clvm_tools_rs
	cd clvm_tools_rs && git checkout 20220805-language-server && make && cd ../runner && npm link ../clvm_tools_rs/pkg
	# Build the runner
	rm -rf node_modules/clvm_tools_rs
	cd runner && npm install && npm link clvm_tools_rs && npm run build
	# Remove the subdir post-build
	rm -rf clvm_tools_rs
