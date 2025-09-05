#!/bin/bash

set -ue

#for target in alpine archlinux centos debian fedora gentoo opensuse oraclelinux nix ubuntu; do
target=$1
dir=.github/actions/$target

mkdir -p "$dir"

### Generate the action
cat > "$dir/action.yml" << EOF
name: 'depexts-$target'
description: 'Test external dependencies handling for $target'
runs:
  using: 'docker'
  image: 'Dockerfile'
EOF

### Generate the Dockerfile
mainlibs="m4 git rsync tar unzip bzip2 make wget"
ocaml="ocaml ocaml-compiler-libs"

OCAML_CONSTRAINT=''

case "$target" in
  alpine)
    cat > "$dir/Dockerfile" << EOF
FROM alpine
RUN apk add $mainlibs $ocaml
RUN apk add g++
EOF
    ;;
  altlinux)
    cat > "$dir/Dockerfile" << EOF
FROM alt
RUN apt-get update
RUN apt-get install -y $mainlibs $ocaml
RUN apt-get install -y gcc-c++
EOF
    ;;
  archlinux)
# no automake
    cat > "$dir/Dockerfile" << EOF
FROM archlinux
RUN pacman -Syu --noconfirm $mainlibs $ocaml gcc diffutils
EOF
    ;;
 centos)
   # CentOS 7 doesn't support OCaml 5 (GCC is too old)
   OCAML_CONSTRAINT=' & < "5.0"'
    cat > "$dir/Dockerfile" << EOF
FROM almalinux:9.4
RUN dnf install 'dnf-command(config-manager)' -y
RUN dnf config-manager --set-enabled crb
RUN yum install -y $mainlibs $ocaml
RUN yum install -y gcc-c++ diffutils
RUN sed -i 's/ID="almalinux"/ID="centos"/' /etc/os-release
EOF
    ;;
  debian)
  cat > "$dir/Dockerfile" << EOF
FROM debian
RUN apt update
RUN apt install -y $mainlibs $ocaml
RUN apt install -y g++
EOF
    ;;
  fedora)
  cat > "$dir/Dockerfile" << EOF
FROM fedora:43
RUN dnf install -y $mainlibs $ocaml diffutils
RUN dnf install -y gcc-c++
EOF
    ;;
  gentoo)
  mainlibs=${mainlibs/git/dev-vcs\/git}
  mainlibs=${mainlibs/tar/app-arch\/tar}
  mainlibs=${mainlibs/bzip2/app-arch\/bzip2}
  cat > "$dir/Dockerfile" << EOF
# name the portage image
FROM gentoo/portage as portage
# image is based on stage3
FROM gentoo/stage3
# copy the entire portage volume in
COPY --from=portage /var/db/repos/gentoo /var/db/repos/gentoo
RUN getuto
RUN echo 'FEATURES="getbinpkg"' >> /etc/portage/make.conf
RUN emerge -qv $mainlibs
EOF
    ;;
  opensuse)
  # glpk-dev is installed manually because os-family doesn't handle tumbleweed
    cat > "$dir/Dockerfile" << EOF
FROM opensuse/leap:15.3
RUN zypper --non-interactive install $mainlibs $ocaml diffutils gzip glpk-devel
RUN zypper --non-interactive install gcc-c++
EOF
    ;;
  oraclelinux)
    cat > "$dir/Dockerfile" << EOF
FROM oraclelinux:8
RUN yum install -y $mainlibs
RUN yum install -y gcc-c++
EOF
  ;;
  nix)
    mainlibs=${mainlibs/m4/gnum4}
    mainlibs=${mainlibs/make/gnumake}
    mainlibs=${mainlibs/tar/}
    mainlibs=${mainlibs/git/}
    mainlibs=$(echo "$mainlibs" | sed -E 's/([[:alnum:]]+)/nixpkgs.\1/g')
    additionallibs="gcc diffutils getconf gnused gawk"
    additionallibs=$(echo "$additionallibs" | sed -E 's/([[:alnum:]]+)/nixpkgs.\1/g')
    # We don't use $ocaml as nix compiler lib is not main ocaml compiler
    cat > "$dir/Dockerfile" << EOF
FROM nixos/nix
RUN nix-channel --update
RUN nix-env -iA $mainlibs $additionallibs nixpkgs.ocaml
EOF
    ;;
  ubuntu)
  cat > "$dir/Dockerfile" << EOF
FROM ubuntu:20.04
RUN apt update
RUN apt install -y $mainlibs $ocaml
RUN apt install -y g++
EOF
    ;;
esac

OCAML_INVARIANT="\"ocaml\" {>= \"4.09.0\"$OCAML_CONSTRAINT}"

# Copy released opam binary from cache
cp binary/opam "$dir/opam"

LOCAL_REPO=/opam/repo
CONF_BRANCH=confs

cat >> "$dir/Dockerfile" << EOF
RUN test -d /opam || mkdir /opam
ENV OPAMROOTISOK=1
ENV OPAMROOT=/opam/root
ENV OPAMYES=1
ENV OPAMCONFIRMLEVEL=unsafe-yes
ENV OPAMPRECISETRACKING=1
COPY opam /usr/bin/opam
RUN echo 'default-invariant: [ $OCAML_INVARIANT ]' > /opam/opamrc
# Retrieve opam repo
RUN git clone $OPAM_REPO --single-branch --branch master $LOCAL_REPO
RUN git config --global user.email "gha@op.am"
RUN git config --global user.name "OPAM GHA"
RUN git -C $LOCAL_REPO reset --hard $OPAM_REPO_SHA
RUN git -C $LOCAL_REPO reset --soft \$(git -C $LOCAL_REPO rev-list --all | tail -1)
RUN git -C $LOCAL_REPO commit -qm "all packages"
# Build a branch that contains only confs packages
RUN git -C $LOCAL_REPO checkout -b $CONF_BRANCH
RUN git -C $LOCAL_REPO rm -q \$(git -C $LOCAL_REPO ls-files packages | grep -v "^packages/conf-")
RUN git -C $LOCAL_REPO commit -qm "keep only confs"
# Setup opam
RUN /usr/bin/opam init --no-setup --disable-sandboxing --bare --config /opam/opamrc git+file://$LOCAL_REPO#master
RUN echo 'archive-mirrors: "https://opam.ocaml.org/cache"' >> \$OPAMROOT/config
RUN /usr/bin/opam switch create this-opam --formula='$OCAML_INVARIANT'
RUN /usr/bin/opam install opam-core opam-state opam-solver opam-repository opam-format opam-client --deps
RUN /usr/bin/opam clean -as --logs
COPY entrypoint.sh /opam/entrypoint.sh
ENTRYPOINT ["/opam/entrypoint.sh"]
EOF


### Generate the entrypoint
cat > "$dir/entrypoint.sh" << EOF
#!/bin/sh
set -eux

git config --global --add safe.directory /github/workspace

## CI WORKING DIR
# Workdir is /github/workpaces
cd /github/workspace

## LOCAL TESTING WORKING DIR
# with docker run -v local/path/opam:/opam/local-git:ro
#git clone /opam/local-git --single-branch --branch branch-name --depth 1 local-opam
# with a distant branch
#git clone https://github.com/ocaml/opam --single-branch --branch branch-name --depth 1 local-opam
#cd local-opam

/usr/bin/opam install . --deps
eval \$(/usr/bin/opam env)
./configure
make

EOF

if [ "$target" = nix ]; then
  cat >> "$dir/entrypoint.sh" << EOF
./opam var --global os-family=nixos
./opam var --global os-distribution=nixos

EOF
fi


cat >> "$dir/entrypoint.sh" << EOF
./opam config report
./opam switch create confs --empty --repo rconf=git+file://$LOCAL_REPO#$CONF_BRANCH
EOF

# Test depexts

DEPEXTS2TEST=""
test_depext () {
  DEPEXTS2TEST="$DEPEXTS2TEST $*"
}

test_depext conf-gmp.5 conf-which.1

if [ "$target" != gentoo ]; then
  test_depext conf-autoconf.0.2
fi

# disable automake for centos, as os-family returns rhel
if [ "$target" != centos ] && [ "$target" != gentoo ] && [ "$target" != opensuse ]; then
  test_depext conf-automake.1
fi

# additional
if [ "$target" != oraclelinux ]; then
  test_depext conf-dpkg.1 # gentoo
fi

# package with os-version check

if [ "$target" = debian ] || [ "$target" = ubuntu ]; then
  test_depext conf-sundials.2
  # conf-libgccjit.1 conf-rdkit.1
fi

if [ "$target" = alpine ]; then
 test_depext conf-clang-format.1
 # conf-pandoc.0.1
fi

#if [ "$target" = altlinux ]; then
#  test_depext xxx
#fi

if [ "$target" = fedora ]; then
 test_depext conf-emacs.1
fi

if [ "$target" = oraclelinux ] || [ "$target" = centos ]; then
  test_depext conf-pkg-config.3
fi

# oraclelinux: conf-libev.4-12 conf-npm.1
# centos: conf-perl.2

if [ -z "$DEPEXTS2TEST" ]; then
  echo "ERROR: You should at least define one depext to test"
  exit 3
fi

cat >> "$dir/entrypoint.sh" << EOF
ERRORS=""
test_depexts () {
  for pkg in \$@ ; do
    ./opam install \$pkg || ERRORS="\$ERRORS \$pkg"
  done
}

test_depexts $DEPEXTS2TEST

if [ -z "\$ERRORS" ]; then
  exit 0
else
  echo "ERROR on packages\$ERRORS"
  exit 1
fi
EOF

# Test depexts update
cat >> "$dir/entrypoint.sh" << EOF
./opam update --depexts || ERRORS="\$ERRORS opam-update-depexts"
EOF

chmod +x "$dir/entrypoint.sh"

#done
