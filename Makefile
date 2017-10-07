.PHONY: benchmark build clean doc docker-build docker-build-pg docker-up docker-up-pg dump-th ghci haddock haddock-server hlint install psql run tags test watch watch-test
all: build

# Run the benchmark.
benchmark:
	stack bench

# Build the executables.
build:
	stack build

# Clean up the built packages.
clean:
	stack clean

# Create the documentation.
doc: build
	stack exec -- kitty-cat-map-doc

# Build all the images defined in docker/docker-compose.yml.
docker-build:
	docker-compose --file docker/docker-compose.yml build

# Build only the pg image defined in docker/docker-compose.yml.
docker-build-pg:
	docker-compose --file docker/docker-compose.yml build pg

# Use docker to build the API documentation.
docker-doc:
	docker-compose --file docker/docker-compose.yml run backend /root/.local/bin/kitty-cat-map-doc

# Use docker-compose to launch all the images from docker/docker-compose.yml.
docker-up:
	docker-compose --file docker/docker-compose.yml up

# Use docker-compose to launch only the pg image from docker/docker-compose.yml.
docker-up-pg:
	docker-compose --file docker/docker-compose.yml up pg

# Dump template haskell splices.
dump-th:
	-stack build --ghc-options="-ddump-splices"
	@echo
	@echo "Splice files:"
	@echo
	@find "$$(stack path --dist-dir)" -name "*.dump-splices" | sort

# Run ghci (REPL).  See the note in the `.ghci` file for why the
# `NoImplicitPrelude` flag is needed.
ghci:
	stack ghci --ghci-options "-XNoImplicitPrelude" .

# Generate the documentation.
haddock:
	stack build --haddock

# This runs a small python websever on port 8001 serving up haddocks for
# packages you have installed.
#
# In order to run this, you need to have run `make build-haddock`.
haddock-server:
	cd "$$(stack path --local-doc-root)" && python -m http.server 8001

# Run hlint.
hlint:
	hlint src/

# Install the executables to ~/.local/bin/.
install:
	stack install

# Run psql and connect to the kitty-cat-map database.
psql:
	PGPASSWORD=foobar psql --username kitty-cat-map --dbname kitty-cat-map --host 127.0.0.1 --port 5532

# Run the server.
run: build
	stack exec -- kitty-cat-map-server

# Create a TAGS file for use in emacs or vim.
tags:
	hasktags --ignore-close-implementation --etags .

# Run the tests.
test:
	stack test

# Build while watching for changes.  Rebuild when change is detected.
watch:
	stack build --file-watch --fast .

# Run tests while watching for changes, rebuild and rerun tests when change is
# detected.
watch-test:
	stack test --file-watch --fast .
