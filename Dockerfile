# ── Stage 1: build the server inside the container ──
# The binary must be linked against a glibc that is not newer than the one in
# the runtime image. Building on the host (e.g. Fedora with glibc 2.43) and
# copying the binary into Debian fails with "GLIBC_2.43 not found".
# haskell:9.6.7 is Debian 11 (glibc 2.31); the runtime image below has 2.36.
FROM haskell:9.6.7 AS build

WORKDIR /src

# Dependencies first: this layer is cached until package.yaml or stack.yaml change.
# README.md is listed in package.yaml, but copying the real file here would
# invalidate the cache on every README edit, so a placeholder is used instead.
COPY package.yaml stack.yaml stack.yaml.lock ./
RUN touch README.md

# Stack's root (/root/.stack) lives in a BuildKit cache that survives between
# builds, so compiled dependencies are reused even when this layer reruns.
# The cache hides the image's own Stack config, hence the explicit GHC flags.
RUN --mount=type=cache,target=/root/.stack \
    stack --system-ghc --no-install-ghc build --only-dependencies onitama:exe:server

# Application
COPY server ./server
RUN --mount=type=cache,target=/root/.stack \
    stack --system-ghc --no-install-ghc --local-bin-path /out install onitama:exe:server

# ── Stage 2: runtime image ──
FROM debian:bookworm-slim

WORKDIR /app

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 \
    netbase && \
    rm -rf /var/lib/apt/lists/*

# Copy server binary from the build stage
COPY --from=build /out/server /app/server

# Copy assets (run 'make client-release' before building to get optimized elm.js)
COPY assets/elm.js ./assets/elm.js
COPY assets/sse.js ./assets/sse.js
COPY assets/sound.js ./assets/sound.js
COPY assets/favicon.svg ./assets/favicon.svg
COPY assets/index.html ./assets/index.html
COPY assets/style.css ./assets/style.css
COPY assets/mp3/ ./assets/mp3/

# Note: Onitama_Logo.svg and elm.min.js are excluded from Docker image

EXPOSE 8080
CMD ["/app/server"]
