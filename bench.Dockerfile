FROM debian
RUN apt-get update -qq && apt-get install -qq -yy make curl gcc g++ patch bzip2 git unzip
RUN adduser --disabled-password --gecos '' --shell /bin/bash opam
USER opam
COPY --chown=opam:opam . /src
WORKDIR /src
RUN make cold
USER root
RUN install ./opam /usr/local/bin/
USER opam
RUN opam init -n --disable-sandboxing git+https://github.com/ocaml/opam-repository#26770281fa1ea8b13aab979c1dfbd326e9ab512c
