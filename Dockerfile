### Build esy ###
# Adapted from https://github.com/andreypopp/esy-docker/blob/eae0eb686c15576cdf2c9d05309a2585b0a3e95e/esy-docker.mk

FROM ubuntu:bionic as esy-bin

ENV TERM=dumb \
  LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib

WORKDIR /

# Packages needed to build OCaml libraries. Not really sure how many of these
# are actually necessary, but testing them via elimination takes too long for
# now. Just note that some of these may be unnecessary or only required by 
# specific libraries.
RUN apt-get update && apt-get install --assume-yes \
  build-essential \
  ca-certificates wget \
  curl unzip git \
  gcc g++ musl-dev make automake autoconf perl m4 libtool \
  pkg-config libssl-dev \
  && rm -rf /var/lib/apt/lists/*

# Yarn/Node
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
  && echo "deb https://dl.yarnpkg.com/debian/ stable main" \
  | tee /etc/apt/sources.list.d/yarn.list \
  && apt-get update  \
  && apt-get install --assume-yes yarn \
  && rm -rf /var/lib/apt/lists/*

# Install esy to /esy
RUN mkdir /esy
WORKDIR /esy

ENV NPM_CONFIG_PREFIX=/esy
RUN yarn global add esy@0.5.8

ENV PATH=/esy/bin:$PATH


### Development environment ###
FROM esy-bin as development

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
  # Needed for @opam/caqti-driver-mariadb
  libmariadb-dev \
  # Needed for @esy-ocaml/libffi@3.2.10
  texinfo \
  # Clean up
  && rm -rf /var/lib/apt/lists/*

RUN mkdir /app
WORKDIR /app

ENTRYPOINT ["tail", "-f", "/dev/null"]


### Build for production ###
FROM development as app-build

WORKDIR /app

COPY docker/app/Makefile ./

# Install and build dependencies in steps separate from `make main.exe` which
# would do this anyway so that Docker will store these separate steps in the
# build cache and avoid repeating them every time code changes in the project.
COPY esy.lock esy.lock/
COPY dune-project package.json ./
RUN make install-dependencies
RUN make build-dependencies

COPY bin bin/
COPY lib lib/

RUN make clean-ml-of-atd
RUN make main.exe


### Production ###
FROM ubuntu:bionic as production

# Timezone
# See https://stackoverflow.com/a/44333806/134014
ARG TZ
ENV TZ=$TZ
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get update \
  && DEBIAN_FRONTEND=noninteractive apt-get install --assume-yes tzdata \
  && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install --assume-yes \
  # Needed for @opam/caqti-driver-mariadb
  libmariadb-dev \
  # Needed for @esy-ocaml/libffi@3.2.10
  texinfo \
  # Needed for /app/entrypoint.sh
  netcat \
  # Clean up
  && rm -rf /var/lib/apt/lists/*

RUN mkdir /app
COPY --from=app-build /app/_esy/default/build/default /app

WORKDIR /app
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["/app/_env/config.json"]
