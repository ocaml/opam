## Tl;dr

Create a local package from the current directory

```
$ opam pin add <name> .
```

Get a local copy of an existing package and install from there

```
$ opam source <package> --pin
```

Get it back to normal

```
$ opam pin remove <package>
```

Publish to the opam repository:
* Use the [opam-publish](https://github.com/OCamlPro/opam-publish#opam-publish)
  tool (`opam install opam-publish`)
* Or by hand:
    - Fork [https://github.com/ocaml/opam-repository](https://github.com/ocaml/opam-repository)
    - Add `packages/<pkgname>/<pkgname>.<version>/opam` with your metadata
    - File a [pull request](https://github.com/ocaml/opam-repository/compare/)


# Creating opam packages

An opam package is basically a bunch of data about a software project:
* A name, version and description
* Some dependencies and constraints on other packages, compiler versions, etc.
* Build, install and remove instructions
* Some additional information (bugtracker, homepage, license, doc...)

This document will go through a simple way to get it in the right format,
whether you are packaging your own project or someone else's. It's not a
[complete guide](Manual.html#Packagedefinitions) to the opam file format.


## Creating a local package

We'll be assuming that you have a working opam installation. If not, please
first read the [install guide](Install.html).


### Get the source

Let's start from the root directory of your project source, typically obtained
from a version-controlled repository or from a tarball.

```
$ wget https://.../project.tar.gz && tar xvzf project.tar.gz
# Or
$ git clone git://.../project.git
...
$ cd project
```

### Opam pin

opam provides a feature that allows you to register packages locally,
without the need for them to exist in a repository. We call this __pinning__,
because it is an extension of the very common feature that allows to __pin__ a
package to a specific version number in other package managers.

So let's create a package __pinned__ to the current directory. We just need to
choose a name and issue the command:

```
$ opam pin add <project> . -n
```
(`-n` tells opam to not try and install just yet, we'll get to it later)


### The "opam" file

At this stage, opam will be looking for metadata for the package `<project>`, on
its repository and in the source directory. Not finding any, it will open an
editor with a pre-filled template for your package's `opam` file. It's best to
keep the project's `README` file open at this stage, as it should contain the
information we're interested in, only in a less normalised format.

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
build: [
  ["./configure" "--prefix=%{prefix}%"]
  [make]
]
install: [make "install"]
dev-repo: ""
depends: [ "ocaml" "ocamlfind" ]
```

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
  within strings. `opam config list` will give you the list of available
  variables. `build` instructions shouldn't need to write outside of the
  package's source directory.
* `install` is similar to `build`, but tells opam how to install. This is indeed
  `install: [ [make "install"] ]`, but the extra square brackets are optional
  when there is a single element, just add them if you need more than one
  command.
* `depends` should be a list of existing opam package names that your package
  relies on to build and run. You'll be guaranteed those are there when you
  execute the `build` instructions, and won't be changed or removed while your
  package is installed. If contributing to the default repository at
  https://opam.ocaml.org, it is quite unlikely that you don't need at least
  `"ocaml"` there.

A few other fields are available, but that should be enough to get started. Like
`install`, most fields may contain lists in square brackets rather than a single
element: `maintainer`, `author`, `homepage`, `bug-reports`, `license` and
`depends`. You may add a `remove` field, but since opam 2.0, removal of
installed files is done automatically, so that should only be needed if your
`install` modified existing files.

For the list of available fields and specification of the format, see the
[opam manual](Manual.html#opam).

One you save and quit, opam will run a series of checks (similar to `opam
lint`), and let you edit again in case of errors.


## Installing

The best test is to let opam attempt to install your just created package. As
for any package, you do it by hand with:

```
$ opam install <project> --verbose
```

At this point, opam will get a snapshot of the project, resolve its dependencies
and propose a course of actions for the install. Let it go and see if your
project installs successfully; it's a good idea to use `--verbose` as it will
make opam print the output of the external commands, in particular the ones in
the `build` instructions.

You can now check that everything is installed as expected.

If you need to change anything, simply do

```
$ opam pin edit <project>
```

to get back to editing the `opam` file. Manually editing the `opam` file in
your source tree, then running `opam upgrade <project>` also works.

So far, so good! You may have missed dependencies in case they were already
installed on your system, but that will be checked automatically by the
continuous integration system when you attempt to publish your package to the
opam repository, so don't worry.


## Distributing your package

The easiest way to get your package into the opam repository is to use
[opam-publish](https://github.com/ocaml/opam-publish), a tool that automates
most of the steps to get a full package ready, reviewed and published. The
official OCaml repository is hosted on Github and uses its pull-request
mechanism for requests, reviews and merges.

Your package needs to be hosted somewhere to be made widely available. While the
opam repository at https://opam.ocaml.org can cache package archives, it is not
intended as a primary hosting. If your project is hosted on Github, a tag is
enough to make an archive available.

### With opam-publish

```
$ opam publish prepare [URL]
```

This will automatically install `opam-publish` if needed, then run it. `[URL]`
is the URL from which the archive containing the sources of your package is
available — if your source is hosted on Github and the last pushed git tag
corresponds to your release, it can be omitted altogether!

This command will provide you with a `<package>.<version>/` directory. Check the
files in that directory, it's the full contents of what will be included in the
opam repository.

When you are ready, go on with

```
$ opam publish submit [DIR]
```

Where `[DIR]` can be omitted in the same circumstances as before, and is
otherwise the name of the generated `<package>.<version>/` directory.

This will submit a pull-request in your name to Github, proposing the addition
of your package definition to the main OCaml repository. You will be redirected
to the page where you can view the status of your pull request, get reviews and
discuss.

If any modifications are needed, you just need to re-run the command to update
the existing pull-request.

### By hand

#### 1. Creating the metadata

Copy your `opam` file and add to it a section with the following format:

```
url {
  src: "https://address/of/project.1.0.tar.gz"
  checksum: "md5=3ffed1987a040024076c08f4a7af9b21"
}
```

The checksum is a simple md5 of the archive, which you can obtain with _e.g._:

```
$ curl -L "https://address/of/project.1.0.tar.gz" | md5sum
```

You will then need to create a patch to the repository:

#### 2. Requesting addition to opam-repository

Publishing is handled through Github, using the pull-request mechanism. If
you're not familiar with it, it is a fancy way to:
* Make a patch to the opam repository
* Propose this patch for review and integration. This will also trigger tests
  that your package installs successfully from scratch.

1.  Go to https://github.com/ocaml/opam-repository and hit the `Fork` button on
    the top right corner (you may be asked to login or register)

2.  Get the `clone URL` on the right, and, from the shell, run `git clone <url>`
    to get your local copy

3.  Add the `opam` file with the `url {}` section into a
    `packages/<project>/<project>.<version>/` directory in the repository, and
    make that a git commit:

    ```
    $ cd opam-repository/packages
    $ mkdir -p <project>/<project>.<version>
    $ cp <opam-file> <project>/<project>.<version>/
    $ git add <project>
    $ git commit -m "Adding new package <project>.<version>"
    ```

4.  Sending that back to github is just a matter of running `git push`

5.  Back to the web interface, refresh, hit the `Pull request` button, check your
    changes and confirm;

6.  Wait for feedback!

### Once you are done

Don't forget to `opam pin remove <project>` once your project is on the
repository, if you don't want to continue using your local version. Remember
that as long as the package is pinned, opam will use the metadata found in its
source if any, but otherwise only what is in the opam repository matters. Use
`opam pin list` to list all currently pinned packages.

## Some tricks

* You may skip the first step and pin to a remote version-controlled
  repository directly, using for example

    ```
    $ opam pin add <project> git://github.com/me/project.git
    ```

* opam will propose to save your opam file back to your source, but if you want
  to take a peek at the internal version it's at
  `~/.opam/<switch>/.opam-switch/overlay/<project>/`. You may also check it with
  `opam show --raw <project>`.
* Since 1.2.1, opam will also use files `<package>.opam`, or metadata found in a
  subdirectory `opam` or `<package>.opam` in your source tree. This can be
  useful if different opam packages are built from the same source.
* You can set opam to use your local clone of the repository with

    ```
    $ opam repository add my-dev-repo path/to/opam-repository
    ```

    Don't forget to `opam pin remove <project>`, and test your changes to the
    repo directly. Don't forget to `opam update my-dev-repo` after you made
    changes to it.

* Pins can also be used to try out experimental changes to a project with
  minimal effort: you can pin to a git repository and even to a specific branch,
  tag or hash by adding `#BRANCH` to the target. So say you want to try out
  Joe's GitHub pull-request present on his branch `new-feature` on his fork of
  `project`, just do

    ```
    $ opam pin project git://github.com/Joe/project.git#new-feature
    ```

    and opam will use that to get the source (and possibly updated metadata) of
    the package; this works with any branch of any git (or even Mercurial, or
    Darcs) repo, it's not github specific.
* We've been focusing on git above, but opam can handle darcs and mercurial
  repositories too, using `darcs://` and `hg://`.

## More on opam files

The opam files can express much more than what was shown above. Without getting
into too much detail, here are some of the most useful features:

* [**Version constraints**](Manual.html#PackageFormulas): an optional version
  constraint can be added after any package name in `depends`: simply write
  `"package" {>= "3.2"}`. Warning, versions are strings too, don't forget the
  quotes.
* [**Formulas**](Manual.html#PackageFormulas): depends are by default a
  conjunction (all of them are required), but you can use the logical "and" `&`
  and "or" `|` operators, and even group with parentheses. The same is true for
  version constraints: `("pkg1" & "pkg2") | "pkg3" {>= "3.2" & != "3.7"}`.
* [**Build depends**](Manual.html#Filteredpackageformulas): you may add the key
  `build` in the version constraints, e.g. `"package" {build & >= "3.2"}`, to
  indicate that there is no run-time dependency to this package: it is required
  but won't trigger rebuilds of your package when changed.
* [**OS constraints**](Manual.html#opamfield-available): The `available` field
  is a formula that determines your package's availability based on the
  operating system or other global opam variables. For example:

    ```
    available: [ os != "darwin" ]
    ```
* [**Conflicts**](Manual.html#opamfield-conflicts): some packages just can't
  coexist. The `conflicts` field is a list of packages, with optional version
  constraints. See also `conflict-class` for _families_ of incompatible
  packages.
* [**Optional dependencies**](Manual.html#opamfield-depopts): they change the
  way your package builds, but are not mandatory. The `depopts` field is a
  package formula like `depends`. simple list of package names. If you require
  specific versions, add a `conflicts` field with the ones that won't work.
* [**Variables**](Manual.html#Variables): you can get a list of predefined
  variables that you can use in your opam rules with `opam config list`.
* [**Filters**](Manual.html#Filters): full commands, or single command
  arguments, may need to be omitted depending on the environment. This uses the
  same optional argument syntax as above, postfix curly braces, with boolean
  conditions:

    ```
    ["./configure" "--with-foo" {ocaml-version > "3.12"} "--prefix=%{prefix}%"]
    [make "byte"] { !ocaml-native }
    [make "native"] { ocaml-native }
    ```

For more, see the [opam Manual](Manual.html)
