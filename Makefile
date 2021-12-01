
package = "bot"

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

haskell_files = $(shell find . -name '*.hs')

build:
	$(stack) build $(package)

test:
	$(stack) test $(package)

run:
	$(stack) build --fast && $(stack) exec -- $(package)

hlint:
	hlint src && hlint app && hlint test

format:
	fourmolu -i $(haskell_files)

.PHONY : build test hlint format run