#!/bin/sh

set -eu

#for target in alpine archlinux centos debian fedora gentoo opensuse oraclelinux ubuntu; do
target=$1
dir=.github/actions/$target

mkdir -p $dir

### Generate the action
cat >$dir/action.yml << EOF
name: 'depexts-$target'
description: 'Test external dependencies handling for $target'
runs:
  using: 'docker'
  image: 'Dockerfile'
EOF

### Generate the Dockerfile
mainlibs="m4 git rsync patch tar unzip bzip2 make wget"
ocaml="ocaml ocaml-compiler-libs"

case "$target" in
  alpine)
    cat >$dir/Dockerfile << EOF
FROM alpine
RUN apk add $mainlibs $ocaml
RUN apk add g++
EOF
    ;;
  archlinux)
# no automake
    cat >$dir/Dockerfile << EOF
FROM archlinux
RUN pacman -Syu --noconfirm $mainlibs $ocaml gcc diffutils
EOF
    ;;
 centos)
    cat >$dir/Dockerfile << EOF
FROM centos:7
RUN yum install -y $mainlibs $ocaml
RUN yum install -y gcc-c++
EOF
    ;;
  debian)
  cat >$dir/Dockerfile << EOF
FROM debian
RUN apt update
RUN apt install -y $mainlibs $ocaml
RUN apt install -y g++
EOF
    ;;
  fedora)
  cat >$dir/Dockerfile << EOF
FROM fedora
RUN dnf install -y $mainlibs $ocaml diffutils
RUN dnf install -y gcc-c++
EOF
    ;;
  gentoo)
  mainlibs=${mainlibs/git/dev-vcs\/git}
  mainlibs=${mainlibs/tar/app-arch\/tar}
  cat >$dir/Dockerfile << EOF
# name the portage image
FROM gentoo/portage as portage
# image is based on stage3
FROM gentoo/stage3
# copy the entire portage volume in
COPY --from=portage /var/db/repos/gentoo /var/db/repos/gentoo
RUN emerge -qv $mainlibs
EOF
    ;;
  opensuse)
  # glpk-dev is installed manually because os-family doesn't handle tumbleweed
    cat >$dir/Dockerfile << EOF
FROM opensuse/leap:15.3
RUN zypper --non-interactive install $mainlibs $ocaml diffutils gzip glpk-devel
RUN zypper --non-interactive install gcc-c++
EOF
    ;;
  oraclelinux)
    cat >$dir/Dockerfile << EOF
FROM oraclelinux:8
RUN yum install -y $mainlibs
RUN yum install -y gcc-c++
EOF
  ;;
  ubuntu)
  cat >$dir/Dockerfile << EOF
FROM ubuntu:20.04
RUN apt update
RUN apt install -y $mainlibs $ocaml
RUN apt install -y g++
EOF
    ;;
esac

# Copy 2.1 opam binary from cache
cp binary/opam $dir/opam

cat >>$dir/Dockerfile << EOF
RUN test -d /opam || mkdir /opam
ENV OPAMROOTISOK 1
ENV OPAMROOT /opam/root
ENV OPAMYES 1
ENV OPAMCONFIRMLEVEL unsafe-yes
ENV OPAMPRECISETRACKING 1
COPY opam /usr/bin/opam
COPY entrypoint.sh /opam/entrypoint.sh
ENTRYPOINT ["/opam/entrypoint.sh"]
EOF


### Generate the entrypoint
cat >$dir/entrypoint.sh << EOF
#!/bin/sh
set -eux

git config --global --add safe.directory /github/workspace
# For systems that don't have an up to date compiler, to avoid ocaml-secondary
echo 'default-invariant: [ "ocaml" {>= "4.09.0"} ]' > /opam/opamrc
opam init --no-setup --disable-sandboxing --bare --config /opam/opamrc git+$OPAM_REPO#$OPAM_REPO_SHA
echo 'archive-mirrors: "https://opam.ocaml.org/cache"' >> \$OPAMROOT/config
opam switch create this-opam ocaml

# Workdir is /github/workpaces
cd /github/workspace
EOF

# workaround for opensuse, mccs & glpk
if [ $target = "opensuse" ]; then
  cat >>$dir/entrypoint.sh << EOF
OPAMEDITOR="sed -i 's|^build.*$|& [\\"mv\\" \\"src/glpk/dune-shared\\" \\"src/glpk/dune\\"]|'" opam pin edit mccs -yn
#opam show --raw mccs
EOF
fi

cat >>$dir/entrypoint.sh << EOF
opam install . --deps
eval \$(opam env)
./configure
make
./opam config report
./opam switch create confs --empty
EOF

test_depext () {
  for pkg in $@ ; do
    echo "./opam install $pkg || true" >> $dir/entrypoint.sh
  done
}

test_depext conf-gmp conf-which conf-autoconf

# disable automake for centos, as os-family returns rhel
if [ $target != "centos" ] && [ $target != "opensuse" ]; then
  test_depext conf-automake
fi

# additionnal
test_depext dpkg # gentoo
test_depext lib-sundials-dev # os version check

chmod +x $dir/entrypoint.sh

#done
