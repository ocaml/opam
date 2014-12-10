> The following are beyond the scope of the [FAQ](FAQ.html), but have been found
> useful for specific use-cases or for advanced users.

### Simulate actions from the current switch state (for debugging)

- `opam upgrade --show-actions` (stop at the action summary dialog)
- `opam upgrade --dry-run` (display only)
- if you really want to try out the results:
    * `opam switch export testing-state.export`
    * `opam switch tmp-testing --alias-of system`
    * `opam switch import testing-state.export --fake`
    * try actions with `--fake` (registers them in OPAM, but doesn't actually
      run the build/install commands)
    * revert to normal: `opam switch <previous>; opam switch remove tmp-testing`
- Experiment with the solver:
    * `opam <request> --cudf=cudf-file`
    * or `opam config cudf-universe >cudf-file-1.cudf`
    * run e.g. aspcud with `aspcud cudf-file-1.cudf /dev/stdout CRITERIA`
    * `admin-scripts/cudf-debug.ml cudf-file-1.cudf` may help with conflicts


### Install in all switches

Not supported natively at the moment, but it's being considered. Quick hack:
```
for switch in $(opam switch list -s -i); do
  opam install --switch $switch PACKAGE
done
```
You may want to add `--yes` if you're confident.


### Update OPAM environment within emacs

You may use the following snippet to define an `opam-env` function:

```lisp
(defun opam-env ()
  (interactive nil)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))
```

You may want to run this at emacs startup if it doesn't inherit the proper shell
environment.


### Easily provide a set of packages for a group of users to install

The easiest way is to create a package with your prerequisites as `depends` and
have them pin that. A quick way to host the file is to use a
[Gist](https://gist.github.com). Create one with minimal contents and listing
your packages as dependencies -- the file name **has** to be `opam`:

```
opam-version: "1.2"
name: "ocaml101"
version: "0.1"
maintainer: "Louis Gesbert <louis.gesbert@ocamlpro.com>"
depends: [ "menhir" { = "20140422" }
           "merlin" { >= "2" }
           "ocp-indent"
           "ocp-index" ]
```

Save that and get the `HTTPS clone URL`. All that is needed then is to run:

```shell
$ opam pin add ocaml101 <HTTPS clone URL>
```

Furthermore, `opam update` will then pick up any modification you made to the gist.
