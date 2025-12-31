#!/bin/sh

make build
docker build -t onitama-server .
# docker build -f Dockerfile.full-build -t onitama-server .


docker run -it -p 8080:8080 --rm onitama-server:latest 

# docker images
# docker rmi onitama-server:latest
# docker system prune -a