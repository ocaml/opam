FROM debian:12
RUN apt-get update -qq && apt-get install -qq -yy make curl gcc g++ patch bzip2 git unzip rsync
RUN adduser --disabled-password --gecos '' --shell /bin/bash opam
ENV OPAMREPOSHA 26770281fa1ea8b13aab979c1dfbd326e9ab512c
# 50 commits before OPAMREPOSHAPHASE1
ENV OPAMREPOSHAPHASE1DELTA 1db3104e98a25ff2b5f859189c9408dc760260e7
ENV OPAMREPOSHAPHASE1 2d85539cd05071ec4f91d6c4ee35d2adc44aa283
RUN git clone https://github.com/ocaml/opam-repository /rep/opam-repository
RUN git -C /rep/opam-repository checkout $OPAMREPOSHA
RUN git clone /rep/opam-repository /rep/opam-repository-small-diff
RUN git -C /rep/opam-repository-small-diff checkout $OPAMREPOSHAPHASE1DELTA
RUN git clone --bare --no-hardlinks /rep/opam-repository /rep/opam-repository.git
RUN git clone --bare --no-hardlinks /rep/opam-repository-small-diff /rep/opam-repository-small-diff.git
RUN chown -R opam:opam /rep
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
