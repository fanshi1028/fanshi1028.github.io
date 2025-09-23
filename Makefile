
.PHONY= tailwind update build optim

all: tailwind update build optim

tailwind:
	tailwindcss -m -i static/input.css -o static/output.css

update:
	wasm32-wasi-cabal --project-file=wasm.cabal.project update

build:
	wasm32-wasi-cabal --project-file=wasm.cabal.project build fanshi1028-site:exe:fanshi1028-site
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal --project-file=wasm.cabal.project list-bin fanshi1028-site | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/fanshi1028-site.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle public

