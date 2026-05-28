.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


node_modules: package.json package-lock.json
	if test ! -d $@; then npm install; fi


.PHONY: format
format:
	elmfmt -i src


.PHONY: start
start: node_modules elm.json elm-land.json
	npx elm-land server


.PHONY: test
test:
	npx elm-review


dist: node_modules elm.json elm-land.json $(shell find src) $(shell find static)
	npx elm-land build
	touch dist  # Update timestamp


.PHONY: deploy
deploy: dist
	netlify deploy --prod --dir=dist --site=airsequel-sheet-music
