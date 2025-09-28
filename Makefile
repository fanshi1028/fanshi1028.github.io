
.PHONY= tailwind build optim

all: tailwind build optim

tailwind:
	tailwindcss -m -i static/input.css -o public/output.css

build:
	wasm32-wasi-cabal --project-file=wasm.cabal.project build fanshi1028-site:exe:fanshi1028-site
	cp -r static/index.js js/
	$(eval my_wasm=$(shell wasm32-wasi-cabal --project-file=wasm.cabal.project list-bin fanshi1028-site | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output js/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/fanshi1028-site.wasm -o public/fanshi1028-site.wasm
	wasm-tools strip -o public/fanshi1028-site.wasm public/fanshi1028-site.wasm

