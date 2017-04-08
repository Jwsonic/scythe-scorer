.DEFAULT_GOAL := run
.PHONY: intstall run test build deploy

install:
	yarn
	elm package install

run:
	./node_modules/.bin/elm-reactor

test:
	./node_modules/.bin/elm-test

build: intstall
	./node_modules/.bin/elm-make src/Scythe.elm --output dist/scythe.js

deploy: build
	./node_modules/.bin/gh-pages-deploy
