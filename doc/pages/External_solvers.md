# External Solvers

Resolving package installations in the presence of dependencies and conflicts is
known to be an
[NP-complete problem](https://hal.archives-ouvertes.fr/file/index/docid/149566/filename/ase.pdf).
Thankfully, a [big effort](http://www.mancoosi.org/) has already been put into
solving it efficiently:

The `opam` package manager is an instance of the approach described in the article "[A modular package manager architecture](http://dl.acm.org/citation.cfm?id=2401012)", which was one of the outcomes of the [Mancoosi](http://www.mancoosi.org) research project. This architecture relies on dependency solvers for package managers, that communicate with the package manager front-end via the [CUDF format](http://www.mancoosi.org/cudf/).

## Installation and compatibility

As of 2.0.0, opam comes with a CUDF solver built-in by default, so unless you
have specifically compiled without it, you shouldn't have to be worried about
installing an external solver. However, these are still supported, and can be
useful in some specific cases. An external solver can be chosen over the
built-in one using the `--solver` command-line argument, the
`$OPAMEXTERNALSOLVER` environment variable, or the `solver:` field in the
`~/.opam/config` file. If no solver was built in or selected, opam will detect
the availability of `aspcud`, `packup` or `mccs` commands on your system and use
one automatically.

The following CUDF solvers have been tested:

- [aspcud](http://www.cs.uni-potsdam.de/wv/aspcud/) (recommended solution until opam 1.2.2)
- [packup](http://sat.inesc-id.pt/~mikolas/sw/packup/)
- [mccs](http://www.i3s.unice.fr/~cpjm/misc/mccs.html) (a modified version of which is now being used as the built-in solver)
- [p2Cudf](https://wiki.eclipse.org/Equinox/p2/CUDFResolver), which can be
  downloaded
  [here](http://eclipse.org/equinox/p2/p2CUDF/org.eclipse.equinox.p2.cudf-1.14.jar)
  and used with the configuration string `java -jar <jarfile-location> -obj
  %{criteria}% %{input}% %{output}%`.

These have been developed by a variety of research teams during the
[MISC competitions](http://www.mancoosi.org/misc/) run yearly from 2010 to 2012.

# Specifying user Preferences for the External Solvers

A fundamental distinguishing feature of the `opam` package manager is the fact that it is designed to reuse state-of-the-art dependency solving technology that gives the users the possibility to express their preferences regarding the operations to be performed during an installation, instead of being bound to an hard-coded strategy.
This section provides basic documentation on this feature, and its usage.

## What are user preferences for installations, and why are they important?
When you request the installation of some packages, say p1...pn, `opam` has a lot to do: it needs to look at all the packages already installed on your machine, find all packages available from the repositories, consider your request, and then come up with a set of actions to be performed to satisfy your request.

Unfortunately, there are a lot of assumptions hidden in your mind when you tell `opam` that you want p1...pn installed: should it choose the latest version of the p1...pn? That seems a sensible thing to do, but sometimes installing a recent version of a package p may lead to downgrading or removing another package q, which is something you might not want. What should `opam` do in this case? Remove q to get the latest p, or keep q and get the most recent p that is compatible with it?
Well, the answer is: it depends! It depends on what _you_ really want, and different users may have different points of view.

User preferences, supported by `CUDF`-compatible solvers, are the means you can use to make the assumptions in your mind explicit and known to the solver used by `opam`, so that the actions performed on your machine correspond to your personalised needs.

## How do I express my preferences?

Preferences are expressed using a simple language built by prefixing a little set of combinators with the `-` (minus) or `+` (plus) operators. The most useful combinators are the following ones:

* `new`  : the number of new packages
* `changed` : the number of packages modified
* `removed` : the number of packages removed
* `notuptodate` : the number of packages that are not at their latest version

For example, the preference `-removed` tells the solver that among all possible ways of satisfying your request, it should choose one that minimises the number of packages removed.

These combinators can be combined in a comma separated sequence, that is treated in lexicographic order by the solver.

### Default preferences for an upgrade
For example, the preference `-removed,-notuptodate,-changed` tells the solver that after ensuring that removals are minimised, it should look for a solution that minimises also the number of packages which are not at their latest version, and then reduce the changes to a minimum.

This is close to the default preference setting used by `opam` when you perform an update or an upgrade, and in practice it tries to bring _all_ your packages to the latest version available, as far as this does not implies removing too many packages. It can be set using the environment variable `OPAMUPGRADECRITERIA`, or the [`solver-upgrade-criteria:`](Manual.html#configfield-solver-upgrade-criteria) configuration field.

### Default preferences for an install
When you request to install a (set of) package(s), in general you do not expect to see all your existing packages updated, and this is why in this case it is preferable to use a different value `-removed,-changed,-notuptodate` that tries to minimise changes to the system.  It can be set using the environment variable `OPAMCRITERIA`, or the [`solver-criteria:`](Manual.html#configfield-solver-criteria) configuration field.

### Specifying preferences for opam

`opam` allows one to specify criteria on the command line, using the `--criteria` option, that will apply only to the current command.
For example, if you are a very conservative user, you might try issuing the following command:
```
opam install --criteria="-removed,-changed" ...
```

This can also be used for some tricks: if for example you want to repair your set of installed packages, you can use the `opam upgrade` command without specifying a preference for newer versions in the criteria (although you may prefer to run `opam upgrade --fixup` in this case):
```
opam upgrade --criteria="-changed"
```

## Yes, there are different versions of the user preference language

The different editions of the MISC competition led to improving the preferences language, by allowing the user progressively more flexibility. Recent solvers give access to a more sophisticated set of preferences, described in [the 2012 MISC competition rules](http://www.mancoosi.org/misc-2012/criteria/). 
For example, using `aspcud >=1.8.0`, you could use

 `-count(removed),-count(down),-sum(solution,installedsize),-notuptodate(solution),-count(changed)`

to instruct a solver to minimise downgrades, and mininise the installed size, among other criteria.

The default criteria used by opam use a custom CUDF property `version-lag` that
gives a monotonic measure of the "age" of packages, by counting the number of
newer revisions of the package. They can be seen using the `opam config report`
command:

```
# install-criteria  -removed,-count[version-lag,request],-count[version-lag,changed],-changed
# upgrade-criteria  -removed,-count[version-lag,solution],-new
```

Notice that these criteria are written for the built-in solver which, being
derived from [`mccs`](https://github.com/AltGr/ocaml-mccs), uses a slightly
different syntax for the criteria: the `-sum(subset,property)` criterion should
be written `-count[property,subset]` instead. We also make use of the `request`
subset here, which applies only to the packages that were part of the user
request, and was introduced in aspcud 1.9.0 and is not part of the official mccs
release.
