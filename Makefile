install: elm
	stack clean
	stack install

dev: elm
	stack clean
	stack install --flag poc-yesod-elm:dev

elm:
	elm make src/frontend/Main.elm --output=src/static/app.js

.PHONY: install dev elm
