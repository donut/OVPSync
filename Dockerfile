### Build esy ###
# Adapted from https://github.com/andreypopp/esy-docker/blob/eae0eb686c15576cdf2c9d05309a2585b0a3e95e/esy-docker.mk

FROM ubuntu:bionic as esy-bin

ENV TERM=dumb \
  LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib

WORKDIR /

# Packages needed to build OCaml libraries. Not really sure how many of these
# are actually necessary, but testing them via elimination takes too long for
# now. Just now that some of these may be unecessary or only required by 
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
RUN yarn global add esy@0.5.6

ENV PATH=/esy/bin:$PATH


### Development environment ###
FROM esy-bin as development

# See https://serverfault.com/q/683605/54523
ARG TZ
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

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

# See https://serverfault.com/q/683605/54523
ARG TZ
ENV TZ=$TZ
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

WORKDIR /app

COPY bin bin/
COPY lib lib/
COPY esy.lock esy.lock/
COPY docker/app/Makefile dune-project package.json ./

RUN make main.exe


### Production ###
FROM ubuntu:bionic as production

# See https://serverfault.com/q/683605/54523
ARG TZ
ENV TZ=$TZ
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

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
