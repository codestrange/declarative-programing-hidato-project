.DEFAULT_GOAL=help

runi: app/Main.hs src/Structures.hs src/Algorithms.hs ## Ejecutar el programa en el interprete
	ghci app/Main.hs src/Structures.hs src/Algorithms.hs

compile: app/Main.hs src/Structures.hs src/Algorithms.hs ## Compilar el programa
	ghc -o bin/program app/Main.hs src/Structures.hs src/Algorithms.hs

run: compile bin/program ## Compilar y ejucutar el programa
	./bin/program

help: ## Muestra esta ayuda
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
