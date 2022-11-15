default: $(shell echo priv/{arm64,x86_64}-apple-macos11.0/elixirkit_nif.so)

priv/%/elixirkit_nif.so: src/elixirkit_nif.m
	mkdir -p `dirname $@`
	clang  -shared -undefined dynamic_lookup -target $* -o $@ $< \
	-I$(shell elixir -e 'IO.puts "#{:code.root_dir()}/usr/include"')
