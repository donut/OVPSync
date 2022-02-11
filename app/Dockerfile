ARG UBUNTU_VERSION=22.04


FROM ocaml/opam:ubuntu-${UBUNTU_VERSION}-ocaml-4.07 as development

# Default user from inherited image is opam
USER root

# Timezone
# See https://stackoverflow.com/a/44333806/134014
ARG TZ
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get update \
  && DEBIAN_FRONTEND=noninteractive apt-get install --assume-yes tzdata \
  && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install --assume-yes \
  # Niceties
  fish vim \
  # Needed for @opam/conf-gmp@opam:1
  libgmp-dev \
  # Needed for @opam/caqti-driver-mariadb \
  pkg-config  libmariadb-dev libssl-dev \
  # Clean up
  && rm -rf /var/lib/apt/lists/*

# Project directory
# PROJECT_PREFIX can be used so that the path matches the path on the host
# system. This can facilitate IDE plugins that rely on .merlin files.
ARG PROJECT_PREFIX
RUN mkdir -p ${PROJECT_PREFIX}
RUN chown -R opam:opam ${PROJECT_PREFIX}

USER opam



#### Build for production ###
FROM development as app-build

RUN mkdir -p ${PROJECT_PREFIX}/app
WORKDIR ${PROJECT_PREFIX}/app

COPY Makefile ./

## Install and build dependencies in steps separate from `make main.exe` (which
## would do all these steps anyway) so that Docker will store these separate 
## steps in the build cache and avoid repeating them every time code changes 
## in the project.

# Dependencies
COPY switch-export.opam ./
RUN make dependencies

# Source
COPY dune-project ./
COPY bin bin/
COPY lib lib/

# Fix permissions
USER root
RUN chown -R opam:opam .
USER opam

# Generated source files
RUN make clean-ml-of-atd
RUN make ml-of-atd

# Build
RUN make main.exe
RUN pwd && ls -lah .



#### Production ###
FROM ubuntu:${UBUNTU_VERSION} as production

## Timezone
## See https://stackoverflow.com/a/44333806/134014
ARG TZ
ENV TZ=$TZ
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get update \
  && DEBIAN_FRONTEND=noninteractive apt-get install --assume-yes tzdata \
  && rm -rf /var/lib/apt/lists/*

# Libraries required for running the app.
RUN apt-get update && apt-get install --assume-yes \
  # Needed for @opam/caqti-driver-mariadb
  libmariadb-dev \
  # Needed for @opam/cohttp-lwt-unix
  # See https://github.com/mirage/ocaml-cohttp/issues/675
  # Might be fixed in modern versions of cohttp.
  netbase \
  # Needed for /app/entrypoint.sh
  netcat \
  # Clean up
  && rm -rf /var/lib/apt/lists/*

# Copy executable from build stage.
ARG PROJECT_PREFIX
RUN mkdir -p ${PROJECT_PREFIX}
COPY --from=app-build ${PROJECT_PREFIX}/app/_build/default ${PROJECT_PREFIX}

# Let's go!
WORKDIR ${PROJECT_PREFIX}
ENTRYPOINT ["./entrypoint.sh"]
CMD ["./_env/config.json"]
