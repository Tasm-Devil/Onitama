all: setup build

# Development build (debug mode)
build: client-build server-build

# Production build (optimized)
release: setup client-release server-build

client-build:
	(cd client ; make debug)

client-release:
	(cd client ; make release)

setup:
	stack setup
	stack test --only-dependencies

server-build:
	stack build

server-start: build
	stack exec server

server-start-config: build
	stack exec -- server --config 'onitama-server.yaml'

test:
	stack test
	(cd client ; make run-tests)
	(cd client ; make debug)

clean:
	rm -rf .stack-work
	(cd client ; make clean)

.PHONY: all build release client-build client-release setup server-build server-start test clean