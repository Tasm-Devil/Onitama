FROM debian:bookworm-slim

WORKDIR /app

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 \
    netbase && \
    rm -rf /var/lib/apt/lists/*

COPY .stack-work/install/*/*/*/bin/server /app/server
COPY gamedb.json ./
COPY assets ./assets

EXPOSE 8080
CMD ["/app/server"]