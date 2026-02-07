FROM debian:bookworm-slim

WORKDIR /app

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 \
    netbase && \
    rm -rf /var/lib/apt/lists/*

# Copy server binary
COPY .stack-work/install/*/*/*/bin/server /app/server

# Copy assets (run 'make release' before building to get optimized elm.js)
COPY assets/elm.js ./assets/elm.js
COPY assets/sse.js ./assets/sse.js
COPY assets/favicon.svg ./assets/favicon.svg
COPY assets/index.html ./assets/index.html
COPY assets/localStorage.js ./assets/localStorage.js
COPY assets/sound.js ./assets/sound.js
COPY assets/style.css ./assets/style.css
COPY assets/mp3/ ./assets/mp3/

# Note: Onitama_Logo.svg and elm.min.js are excluded from Docker image

EXPOSE 8080
CMD ["/app/server"]