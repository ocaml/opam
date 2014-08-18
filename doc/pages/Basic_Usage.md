# Learn to use OPAM in 2 minutes

This short tutorial covers the very basic use cases to get you started with
OPAM. A more lengthy introduction can be found in the
[Advanced Usage](Advanced_Usage.html) guide.

## Initialising OPAM

```
opam init
```

This will create the `~/.opam` directory, within which packages will be
installed and where OPAM will store its data.

## Browsing available packages

The following commands will enable you to obtain information on available
packages:

```
opam list -a            # List all available packages
opam search QUERY       # List packages with QUERY in their name or description
opam show PACKAGE       # Display information about PACKAGE
```

You may prefer to [browse them online](https://opam.ocaml.org/packages). If you
find a package there but not on your computer, either it has been recently added
and you should simply run `opam update`, or it's not available on your system or
OCaml version -- `opam install PACKAGE` should give you the reason.

## Installing a package

The two commands you will probably use the most with OPAM are:

```
opam update             # Update the packages database
opam install PACKAGE    # Download, build and install the latest version of PACKAGE
```

## Upgrading your installed packages

You may want to regularly issue these commands to keep your packages up-to-date:

```
opam update             # Update the packages database
opam upgrade            # Re-install packages that were updated since last upgrade
```

## Do more with OPAM

If you need more details and options, OPAM is self-documented through

```
opam --help
```

To learn how to use more advanced features of OPAM (package pinning, multiple
repositories, multiple compilers...), move on to the [Advanced
Usage](Advanced_Usage.html) guide, or the [Packaging tutorial](Packaging.html).
