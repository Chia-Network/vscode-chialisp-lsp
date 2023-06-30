VER=0.1.50

if [ ! -d clvm_tools_rs-${VER} ] ; then
	curl "https://download.chia.net/simple-dev/clvm-tools-rs/clvm_tools_rs-${VER}.tar.gz" | tar xzvf -
fi
