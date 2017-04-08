.DEFAULT_GOAL := run
.PHONY: intstall run test

install:
	yarn
	elm package install

run:
	./node_modules/.bin/elm-reactor

test:
	./node_modules/.bin/elm-test
