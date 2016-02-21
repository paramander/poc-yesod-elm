install:
	elm make src/frontend/Main.elm --output=src/static/app.js
	rm -rf .stack-work/dist/
	stack install

.PHONY: install
