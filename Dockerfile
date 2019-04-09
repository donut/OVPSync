
ARG BUILD_IMAGE
FROM $BUILD_IMAGE as build

COPY . /app

WORKDIR /app
RUN esy install
RUN esy build

FROM alpine:3.9

# Needed for @opam/tls
RUN apk add --no-cache gmp-dev
# Need for @opam/caqti-driver-mariadb
RUN apk add --no-cache mariadb-dev

RUN mkdir /app
COPY --from=build /app/_esy/default/build/default /app

COPY entrypoint.sh /app/docker-entrypoint.sh
RUN chmod +x /app/docker-entrypoint.sh

WORKDIR /app
ENTRYPOINT ["/bin/sh", "/app/docker-entrypoint.sh"]
