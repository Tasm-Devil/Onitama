FROM haskell:9.4.8

COPY . /app

WORKDIR /app

RUN make all

EXPOSE 8080
CMD make server-start