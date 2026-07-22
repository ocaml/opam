#!/bin/sh
# Helpers to regenerate 'configure' with the good version of autoconf
# make sure to have committed your changes in 'configure.ac' before launching it
# Does not work on worktrees
set -euo pipefail

dockerfile=$(mktemp)
cat > "$dockerfile" << EOF
FROM ubuntu:26.04
RUN apt-get update && apt-get install -yy make autoconf git
COPY --chown=0:0 .git /mnt/.git
WORKDIR /mnt
RUN git reset --hard HEAD
RUN make configure
CMD cat configure
EOF

docker build -f "$dockerfile" -t cat-opam-configure .
docker run --rm cat-opam-configure > configure
