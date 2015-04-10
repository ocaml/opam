# Provides field proposal

This is a proposal to add a `provides:` field to the OPAM package description format.

The feature is already supported by the Cudf format, which should ease the most
difficult parts of the implementation. The purpose of this field is to make
depending on a choice of packages providing the same feature easier.

The `opam` file format is changed as such:
```
<opam> := ...
  ?provides: [ <package>+ ]
```

If package `a` is providing `b {constraint}`, this is to be understood as

> installation of `a` implies that any version of `b` satisfying `constraint`
> should be considered installed for all purposes regarding dependency
> resolution.

In particular:

- any package depending on `b` with a constraint that has a non-empty
  intersection with `constraint` can be installed ;
- any package with such an optional dependency would need to be compiled after
  `a`, and rebuilt on changes to `a` ;
- conversely, any package conflicting with `b` with a constraint compatible with
  `constraint` can't be installed together with `a`.

It may be simpler to start by implementing `provides` only for definite
versions.


## Added functionality

The feature provided can already be encoded without an extra field: given a list
of packages that `provide` a given name, a package by this name can be added
with a dependency towards either of those. However, on a repository maintainance
point of view, having to list all the alternatives adds much more burden.

Besides, it's not possible, using pinning or an additional repository, to
provide a replacement for a base-repo package without redefining it explicitly:
that's sometimes very useful to extend the OCaml versions where some package is
available, for example.


## Virtual and replacement packages

`provides` entries share the namespace of usual packages, and may therefore
create _virtual_ packages, i.e. packages that only exist as provided by other
packages. In the other case around, packages may both have a concrete definition
and appear as `provides`, in which case we would speak of _replacement_
packages.

In both cases, great care should be taken in the user interface. For example:
- what to do when the user requires the installation of a virtual package ? (In
  `apt-get`, this is an error.)
- should we print an advertisement when installing a package that has possible
  replacements ?
- when querying info on a package, possible alternatives should be shown.
- should virtual packages be listed in the normal package listing ?
- other commands referring to a given package (e.g. `pin`) may become ambiguous,
  so they should probably just ignore `provides`, and display a warning for
  virtual packages.

The case of replacement packages is a bit more tricky, because it may easily get
confusing if the dependencies aren't explicitly traced. The format of the
package index will have to be extended to allow for virtual packages, which may
not have a definite version.


## Use-cases

* camlp4 should be made a virtual package, provided by different implementations
  for different compiler versions. The current handling using package versions
  causes expectations on the upgrade of those, spurious warnings of not
  up-to-date packages, and obfuscates real upgrades.
* Allow aliases or renaming of packages (see #1879).
* Allow to fork existing package and provide a replacement in the repository
  (for example cryptokit-sha512, see #314).
* Built-in stuff in the compiler would be made simpler with `provides` lines
  instead of the concrete (but empty) `base-` packages. With compilers in
  packages, that would fit well in the compiler package's description.
* Adds flexibility in changing the granularity of packages: packagers could more
  easily go back and forth between splitting in small units or packaging a
  bundle.


## Constraint intersection

While OPAM usually solves version constraints based on the set of actual
versions, this needs to be symbolic, i.e. non-empty intersection of the sets of
_possible_ versions. For example, the intersection of `a {>= 3}` and `a {<= 3}`
is non-empty even if there was no known `a.3` version before.

This will need some care in the opam to Cudf version conversion, which is
currently based on existing versions.


## Interactions with the `features` field

While `provides` occupies the namespace of packages, and is used in dependency
resolution, `features` occupies that of variables, and is intended for use only
at build time (we should forbid its use in the `available` field, which is
resolved before dependencies). However, both indicate things that are made
available by the package, so there is a high risk of user confusion.

I think both are important features that we want, and there is no way to merge
them, but this is to be taken into account in the interface design and
documentation ; `features` might be renamed (`traits` ?).
