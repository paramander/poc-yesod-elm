install: elm
	stack clean
	stack install

dev: elm
	stack clean
	stack install --flag poc-yesod-elm:dev

elm:
	elm make src/frontend/Public.elm --output=src/static/app.js
	elm make src/frontend/Admin.elm --output=src/static/admin.js

.PHONY: install dev elm
