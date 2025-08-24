FROM debian:12
RUN apt-get update -qq && apt-get install -qq -yy make curl gcc g++ patch bzip2 git unzip
RUN adduser --disabled-password --gecos '' --shell /bin/bash opam
RUN git clone https://github.com/ocaml/opam-repository /rep/opam-repository
RUN cp -r /rep/opam-repository /rep/opam-repository-before-phase1 && cd /rep/opam-repository-before-phase1 && git checkout 2025-01-before-archiving-phase1 && rm -rf .git
RUN cp -r /rep/opam-repository /rep/opam-repository-after-phase3 && cd /rep/opam-repository-after-phase3 && git checkout 2cd8e78594a8943245b8061dbdabd39e7ca492fb && rm -rf .git
RUN git -C /rep/opam-repository checkout 26770281fa1ea8b13aab979c1dfbd326e9ab512c
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
RUN cat /home/opam/all-opam-files | xargs -d '\n' cat > /home/opam/all-opam-content
