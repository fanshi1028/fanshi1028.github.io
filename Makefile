
.PHONY= tailwind update build optim

all: tailwind build optim

tailwind:
	tailwindcss -m -i static/input.css -o static/output.css
	rm static/input.css

update:
	wasm32-wasi-cabal --project-file=wasm.cabal.project update

build:
	wasm32-wasi-cabal --project-file=wasm.cabal.project build fanshi1028-site:exe:fanshi1028-site
	cp -r static js
	$(eval my_wasm=$(shell wasm32-wasi-cabal --project-file=wasm.cabal.project list-bin fanshi1028-site | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output js/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/fanshi1028-site.wasm -o public/fanshi1028-site.wasm
	wasm-tools strip -o public/fanshi1028-site.wasm public/fanshi1028-site.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle public

