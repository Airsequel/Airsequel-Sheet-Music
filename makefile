.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


node_modules: package.json package-lock.json
	if test ! -d $@; then npm install; fi


.PHONY: start
start: node_modules elm.json elm-land.json
	npx elm-land server


dist: node_modules elm.json elm-land.json $(shell find src) $(shell find static)
	npx elm-land build
	touch dist  # Update timestamp


.PHONY: deploy
deploy: dist
	@echo "Open https://app.netlify.com/sites/airsequel-sheet-music/deploys"
	@echo "and drag & drop the ./dist directory."
