.PHONY: benchmark build clean docker-build docker-up dump-th ghci haddock haddock-server hlint install psql run tags test watch watch-test
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

# Build all the images defined in docker/docker-compose.yml.
docker-build:
	docker-compose --file docker/docker-compose.yml build

# Use docker-compose to launch all the images from docker/docker-compose.yml.
docker-up:
	docker-compose --file docker/docker-compose.yml up

# Dump template haskell splices.
dump-th:
	-stack build --ghc-options="-ddump-splices"
	@echo
	@echo "Splice files:"
	@echo
	@find "$$(stack path --dist-dir)" -name "*.dump-splices" | sort

# Run ghci (REPL).
ghci:
	stack ghci

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
	PGPASSWORD=foobar psql -U kitty-cat-map -d kitty-cat-map -h 127.0.0.1

# Run the server.
run: build
	stack exec -- kitty-cat-map-exe

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
