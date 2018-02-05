FROM multiarch/debian-debootstrap:%TARGET_TAG%
# May need configuration on the host:
#    docker run --rm --privileged multiarch/qemu-user-static:register --reset
LABEL Description="opam release builds" Vendor="OCamlPro" Version="1.0"

RUN apt-get update && apt-get install bzip2 g++ make patch wget libltdl-dev --yes && apt-get clean --yes
RUN useradd -U --create-home opam

ADD https://caml.inria.fr/pub/distrib/ocaml-4.04/ocaml-4.04.2.tar.gz /root/

WORKDIR /root
RUN tar xzf ocaml-4.04.2.tar.gz
WORKDIR ocaml-4.04.2
RUN ./configure %CONF% -prefix /usr/local
RUN make world opt.opt
RUN make install
RUN rm -rf /root/ocaml-4.04.2 /root/ocaml-4.04.2.tar.gz

ENV PATH /usr/local/bin:/usr/bin:/bin
USER opam
VOLUME /src
WORKDIR /home/opam/
CMD tar xzf /src/opam-full-${VERSION}.tar.gz && \
    cd opam-full-${VERSION} && \
    echo "(${LINKING})" > src/client/linking.sexp && \
    ./configure --with-mccs && \
    make lib-ext opam && \
    strip opam && \
    cp opam /src/opam-${VERSION}-${TARGET}
