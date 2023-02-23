dist:
	npx elm-land build


.PHONY: deploy
deploy: dist
	npx --yes surge $< airsequel-sheet-music.surge.sh
