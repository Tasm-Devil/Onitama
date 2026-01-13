#!/bin/sh

make all
docker build -t onitama:latest .
# docker build -f Dockerfile.full-build -t onitama-haskell .
docker run -p 8080:8080 onitama:latest 

# docker images
# docker rmi onitama:latest
# docker system prune -a
# docker save onitama:latest > onitama.tar