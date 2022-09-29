all:
	cd .. && cargo build --target wasm32-unknown-unknown && wasm-bindgen --nodejs --out-dir ./lsp-host/build ./target/wasm32-unknown-unknown/debug/clvm_tools_rs.wasm
