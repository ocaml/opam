## Steps to follow for each release

* Update version (and copyright year) in `configure.ac`, `shell/opam_installer.sh`
* Run `make configure` to regenerate `./configure`
* Run `make tests`
* Run `opam-rt` (with and without aspcud)
* Run `make doc` to re-generate the API documetation
* Check that 'make with-ocamlbuild install-with-ocamlbuild' works
* Check that 'make libinstall-with-ocamlbuild' works (try to compile `opam-rt` and `opam2web` with it)

--

* update the CHANGELOG
* tag the release
* create a release on github based on your tag (https://github.com/ocaml/opam/releases/new)

--

* Generate an inclusive source tarball (and the binary for your current arch while you're at it):
```
./shell/release.sh full-archive binary publish -n git-name:git-token
```
* Check that it's been properly uploaded on https://github.com/ocaml/opam/releases
* Ask people on other archs (and with write access to opam) to run
```
wget https://raw.github.com/ocaml/opam/master/shell/release.sh && \
bash -ue ./release.sh -t $VERSION
```

--

* Add some news about the release on opam2web
* Update the installation instructions in [the Wiki](http://opam.ocaml.org/doc/Quick_Install.html)
* Update the opam-lib, opamfu, opam2web opam packages
* Announce ! (platform-list, caml-list)
