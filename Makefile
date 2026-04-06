.PHONY: build test run frontend-install frontend-build frontend-dev dev elm-build elm-run

build:
	stack build

test:
	stack test

run:
	stack exec lambdachan-exe

frontend-install:
	cd frontend && npm install

frontend-build: frontend-install
	cd frontend && npm run build

frontend-dev:
	cd frontend && npm run dev

# Run Haskell API server and Vite dev server concurrently.
# Visit http://localhost:5173 during development.
dev:
	stack exec lambdachan-exe & cd frontend && npm run dev

# Build Elm to a plain JS bundle without npm/Vite.
# Outputs to frontend/dist/ which the Haskell server serves.
# Visit http://localhost:8080 after running elm-run.
elm-build:
	mkdir -p frontend/dist
	cd frontend && elm make src/Main.elm --output=dist/main.js
	cp frontend/src/style.css frontend/dist/style.css
	cp frontend/dist-index.html frontend/dist/index.html

elm-run: elm-build
	stack exec lambdachan-exe
