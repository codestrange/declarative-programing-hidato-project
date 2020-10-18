.DEFAULT_GOAL=help

runi: hidato.hs structures/structures.hs ## Ejecutar el programa en el interprete
	ghci hidato.hs structures/structures.hs

compile: hidato.hs structures/structures.hs ## Compilar el programa
	ghc -o bin/program hidato.hs structures/structures.hs

run: compile bin/program ## Compilar y ejucutar el programa
	./bin/program

help: ## Muestra esta ayuda
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
