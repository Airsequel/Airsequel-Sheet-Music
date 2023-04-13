.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: start
start:
	npx elm-land server


dist: elm.json elm-land.json src static
	npx elm-land build


.PHONY: deploy
deploy: dist
	@echo "Open https://app.netlify.com/sites/airsequel-sheet-music/deploys"
	@echo "and drag & drop the ./dist directory."
