#!/bin/sh

make release
docker build -t onitama:latest .
#docker run -p 8080:8080 onitama:latest 
docker save onitama:latest > onitama.tar

## Some more usefull docker commands:

# docker images
# docker rmi onitama:latest
# docker system prune -a

## This is an experimental Docker build with haskell compiler and more. It's a huge file! Read more: https://hub.docker.com/_/haskell/
# docker build -f Dockerfile.full-build -t onitama-haskell .
