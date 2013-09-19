## Step to follow for each release

* `export VERSION=... HOST=...`
* Update version (and copyright year) in `configure.ac`
* Run `make configure` to regenerate `./configure`
* Run `make tests`
* Run `opam-rt` (with and without aspcud)
* Run `make doc` to re-generate the API documetation
* Check that 'make with-ocamlbuild install-with-ocamlbuild' works
* Check that 'make libinstall-with-ocamlbuild' work (try to compile `opam-rt` and `opam2web` with it)
* Run `make release` to create the full archive and upload it
* Run `ssh $HOST cd pub && ln -f opam-full-$VERSION.tar.gz opam-full-latest.tar.gz
* On every arch
  * Run `sh shell/make_update_binary.sh $VERSION` to generate the binary
  * Run `scp opam-* $HOST:pub/` to upload the binary
* Update `VERSION` in `$HOST:pub/opam_installer.sh`
* Upload `opam-lib` version in OPAM
* Fix `opam2web` constraints in OPAM and upload a new version
* Send an email on the platform list (and on the caml-list)
