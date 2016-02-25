install:
	elm make src/frontend/Main.elm --output=src/static/app.js
	stack clean
	stack install

.PHONY: install
