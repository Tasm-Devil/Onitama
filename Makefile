all: setup build

build: client-build server-build

client-build:
	(cd client ; make)

setup:
	stack setup
	stack test --only-dependencies

server-build:
	stack build

server-start: build 
	stack exec server

test:
	stack test
	(cd client ; make run-tests)
	(cd client ; make)

clean:
	rm -r .stack-work
	(cd client ; make clean)