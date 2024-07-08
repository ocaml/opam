FROM debian:bullseye
RUN apt-get update -qq && apt-get install -qq -yy make curl gcc g++ patch bzip2 git unzip
RUN adduser --disabled-password --gecos '' --shell /bin/bash opam
ENV OPAMREPOSHA 26770281fa1ea8b13aab979c1dfbd326e9ab512c
RUN git clone https://github.com/ocaml/opam-repository --depth 1 /rep/opam-repository
RUN git -C /rep/opam-repository fetch origin $OPAMREPOSHA
RUN git -C /rep/opam-repository checkout $OPAMREPOSHA
USER opam
COPY --chown=opam:opam . /src
WORKDIR /src
RUN make cold
USER root
RUN install ./opam /usr/local/bin/
USER opam
RUN opam init --bare -n --disable-sandboxing /rep/opam-repository
RUN opam switch create --fake default 4.14.0
RUN opam list --all -s --all-versions > /home/opam/all-packages
RUN find /rep/opam-repository -name opam -type f > /home/opam/all-opam-files
