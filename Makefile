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
	stack exec -- server --dev

server-start-config: build
	stack exec -- server --config 'onitama-server.yaml'

# Server test suite (rules, pass rule, minimax, HTTP API against the AI)
test: test-server

test-server:
	stack test

# Start a server with a prepared game in which White (you) has no legal move
# and must pass. Open http://localhost:8080/1
# The server works on a temporary copy, so every run starts from the same position.
demo-pass: build
	@db=$$(mktemp --suffix=.json) && cp server/test/fixtures/must-pass.json $$db && \
	echo "Demo: open http://localhost:8080/1" && \
	stack exec -- server --dev --no-cleanup --database $$db

# Let two AI depths play against each other, e.g.
#   make ai-match ARGS="--white-depth 3 --black-depth 5 --games 20 --cards mixed"
#   stack exec -- ai-match --help
ai-match: server-build
	stack exec -- ai-match $(ARGS)

clean:
	rm -rf .stack-work
	(cd client ; make clean)

# The server is compiled inside the image (see Dockerfile), only the client is built here.
docker: client-release
	docker build -t onitama .
	docker save onitama:latest > onitama.tar

docker-start: docker
	docker run -p 8080:8080 onitama:latest

deploy: docker
	rsync --partial --progress onitama.tar deneaux.de:~
	ssh deneaux.de 'docker load < ~/onitama.tar && rm ~/onitama.tar'

.PHONY: all build release client-build client-release setup server-build server-start test test-server ai-match demo-pass clean docker docker-start deploy