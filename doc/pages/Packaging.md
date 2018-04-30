# Creating and publishing opam packages

An opam package is defined by a `<pkgname>.opam`, or simply `opam` file,
containing its metadata. This short guide will get you through writing this
definition, testing it, and publishing to the
[opam-repository](https://github.com/ocaml/opam-repository).

## Creating a package definition file

For complete documentation of the format, see [the manual]((Manual.html#Packagedefinitions).

If your project does not yet have a package definition, get to the root of its
source, and then either
- run `opam pin .` to create and edit a template, and test your definition right
away,
- or create a `<pkgname>.opam` file and edit it by hand.

The file follows a simple `field: <value>` format:

```
opam-version: "2.0"
name: "project"
version: "0.1"
synopsis: "One-line description"
description: """
Longer description
"""
maintainer: "Name <email>"
authors: "Name <email>"
license: ""
homepage: ""
bug-reports: ""
dev-repo: ""
depends: [ "ocaml" "ocamlfind" ]
build: [
  ["./configure" "--prefix=%{prefix}%"]
  [make]
]
install: [make "install"]
```

The `depends:`, `build:` and `install:` are the most important fields here. If
your project uses [`dune`](https://github.com/ocaml/dune), skip `install:` and
use:

```
build: ["dune" "build" "-p" name]
```

See [below](#The-file-format-in-more-detail) for more on the format.

## Testing

Make sure you have committed everything if using a version-control system
(_e.g._ `git add *.opam && git commit`), and just run

```
opam install .
```

from the root of your project. This will attempt to install the newly-defined
package so you can check it goes as expected.

## Publishing

Publishing is done using Github's pull-request mechanism, which allows automated
checks to be run, and discussion to happen with the maintainers before your
contribution is merged. You will need a [Github](https://github.com/) account.

Submitting is most easily done using the `opam-publish` plugin. Run `opam
publish --help` for more options.

### If the project is hosted on Github

First tag the project. Assuming this is version 0.1:
```
git tag -a 0.1
git push 0.1
```
Alternatively, you can create a release using the web UI
(https://github.com/USER/REPO/releases/new).

Then just run `opam publish` from your source directory and follow the steps.

### If not

Assuming your release is available as an archive at
`https://foo.bar/project-0.1.tar.gz`, run:

```
opam publish https://foo.bar/project-0.1.tar.gz .
```

from your source directory. The final `.` argument indicates to search for
package definitions in the current directory rather than in the archive.

> `opam publish` can be re-run any number of times to update an existing
> submission, or propose changes to an already released package.

### Without opam-publish

First, you will need to add a section in the following format to the package
definition, to specify where to retrieve the source of the package:
```
url {
  src: "https://address/of/project.1.0.tar.gz"
  checksum: "md5=3ffed1987a040024076c08f4a7af9b21"
}
```

Then get to https://github.com/ocaml/opam-repository and select `Fork` on the
top-right. Clone the resulting repository, add your package definition, and
push back, as such:

```
git clone git@github.com:USER/opam-repository --branch 2.0.0
cd opam-repository
cp OPAM_FILE packages/NAME/NAME.VERSION/opam
git add packages
git commit
git push origin HEAD:add-pkg-NAME
```

Then, back to the web UI, Github should propose to file a pull-request for your
newly pushed branch. If not, select the `new pull request` button on the left.
Make sure to file your pull-request against the `2.0.0` base branch, since
package definitions in 1.2 format are not yet accepted on `master`.


## The file format in more detail

### The basics

The `opam-version` and `maintainer` fields are mandatory; you should
remove the others rather than leave them empty.
* `synopsis` should be a one-line description of what your package does, used in
  listings. It is recommended to also add a `description` field for a longer
  explanation (hint: you may delimit long strings with triple-quotation mark
  delimiters `"""` to avoid escaping issues).
* You'll probably be the `maintainer` for now, so give a way to contact you in
  case your package needs maintenance.
* Most interesting is the `build` field, that tells opam how to compile the
  project. Each element of the list is a single command in square brackets,
  containing arguments either as a string (`"./configure"`) or a variable name
  (`make`, defined by default to point at the chosen "make" command -- think
  `$(MAKE)` in Makefiles). `%{prefix}%` is another syntax to replace variables
  within strings. `opam config list` will give you a list of available
  variables. `build` instructions shouldn't need to write outside of the
  package's source directory.
* `install` is similar to `build`, but tells opam how to install. The example
  above should indeed be `install: [ [make "install"] ]`, but the extra square
  brackets are optional when there is a single element. This field can be
  skipped if your package generates a
  [`<pkgname>.install`](Manual.html#lt-pkgname-gt-install) file, like is the
  case when using `dune`.
* `depends` should be a list of existing opam package names that your package
  relies on to build and run. You'll be guaranteed those are there when you
  execute the `build` instructions, and won't be changed or removed while your
  package is installed. If contributing to the default repository at
  https://opam.ocaml.org, it is quite unlikely that you don't need at least
  `"ocaml"` there.

> Note: when running local shell scripts during _e.g._ `build:`, it is
> preferable to use `build: ["sh" "-exc" "./SCRIPT"]` than call the script
> directly.

A few other fields are available, but that should be enough to get started. Like
`install`, most fields may contain lists in square brackets rather than a single
element: `maintainer`, `author`, `homepage`, `bug-reports`, `license` and
`depends`. You may add a `remove` field, but since opam 2.0, removal of
installed files is done automatically, so that should only be needed if your
`install` modified existing files.

### Advanced usage

This is just a very short introduction, don't be afraid to consult
[the reference](Manual.html#opam) for details and more:

* [**Version constraints**](Manual.html#PackageFormulas): an optional version
  constraint can be added after any package name in `depends`: simply write
  `"package" {>= "3.2"}`. Warning, versions are strings too, don't forget the
  quotes.
* [**Formulas**](Manual.html#PackageFormulas): depends are by default a
  conjunction (all of them are required), but you can use the logical "and" `&`
  and "or" `|` operators, and even group with parentheses. The same is true for
  version constraints: `("pkg1" & "pkg2") | "pkg3" {>= "3.2" & != "3.7"}`.
* [**Build depends**](Manual.html#Filteredpackageformulas): you may add, among
  others, the key `build` in the version constraints, _e.g._
  `"package" {build & >= "3.2"}`, to indicate that there is no run-time
  dependency to this package: it is required but won't trigger rebuilds of your
  package when changed.
* [**OS constraints**](Manual.html#opamfield-available): The `available` field
  is a formula that determines your package's availability based on the
  operating system or other
  [global opam variables](Manual.html#Global-variables). For example:

    ```
    available: [ os != "macos" ]
    ```
* [**Conflicts**](Manual.html#opamfield-conflicts): some packages just can't
  coexist. The `conflicts` field is a list of packages, with optional version
  constraints. See also [`conflict-class`](Manual.html#opamfield-conflict-class)
  for _families_ of incompatible packages.
* [**Optional dependencies**](Manual.html#opamfield-depopts): they change the
  way your package builds, but are not mandatory. The `depopts` field is a
  package formula like `depends`. simple list of package names. If you require
  specific versions, add a `conflicts` field with the ones that won't work.
* [**Variables**](Manual.html#Variables): you can get a list of predefined
  variables that you can use in your opam rules with `opam config list`.
* [**Filters**](Manual.html#Filters): dependencies, commands and single command
  arguments may need to be omitted depending on the environment. This uses the
  same optional argument syntax as above, postfix curly braces, with boolean
  conditions:

    ```
    ["./configure" "--with-foo" {ocaml-version > "3.12"} "--prefix=%{prefix}%"]
    [make "byte"] { !ocaml-native }
    [make "native"] { ocaml-native }
    ```
