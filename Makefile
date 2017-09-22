.DEFAULT_GOAL := run
.PHONY: intstall run test build deploy

install:
	yarn
	elm package install

run:
	./node_modules/.bin/elm-reactor -a 0.0.0.0

test:
	./node_modules/.bin/elm-test

analyse:
	./node_modules/.bin/elm-analyse

analyse-server:
	./node_modules/.bin/elm-analyse -s -p 3001

build: intstall
	./node_modules/.bin/elm-make src/Scythe.elm --output dist/scythe.js

deploy: build
	./node_modules/.bin/gh-pages-deploy
