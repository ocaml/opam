# Specifying user Preferences for the External Solvers

A fundamental distinguishing feature of the `opam` package manager is the fact that it is designed to reuse state-of-the-art dependency solving technology that gives the users the possibility to express their preferences regarding the operations to be performed during an installation, instead of being bound to an hard-coded strategy.
This section provides basic documentation on this feature, and its usage.

## What are user preferences for installations, and why are them important?
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

For example, the preference  `-removed`  tells the solver that among all possible ways of satisfying your request, it should choose one that minimises the number of packages removed.

These combinators can be combined in a comma separated sequence, that is treated in lexicographic order by the solver.

### Default preferences for an upgrade
For example, the preference  `-removed,-notuptodate,-changed`  tells the solver that after ensuring that removals are minimised, it should look for a solution that minimises also the number of packages wich are not at their latest version, and then reduce the changes to a minimum.

This is the default preference setting used by `opam` when you perform an update or an upgrade, and in practice it tries to bring _all_ your packages to the latest version available, as far as this does not implies removing too many packages. It can be set using the environment variable `OPAMUPGRADECRITERIA`

### Default preferences for an install
When you request to install a (set of) package(s), in general you do not expect to see all your existing packages updated, and this is why in this case `opam` uses a different default value `-removed,-changed,-notuptodate` that tries to minimise changes to the system.  It can be set using the environment variable `OPAMCRITERIA`

### Specifying preferences for opam

Recent versions of `opam` allow to specify your criteria on the command line, using the `--criteria` option, that will apply only to the current command.
For example if you are a very conservative user, you might try issueing the following command:
```
opam install --criteria="-removed,-changed" ...
```

This can also be used for some tricks: if for example you want to repair your set of installed packages, you can use the `opam upgrade` command without specifying a preference for newer versions in the criteria:
```
opam upgrade --criteria="-changed"
```

You can also use the `OPAMCRITERIA` and `OPAMUPGRADECRITERIA` environment variables to specify your preferences (for example, adding your preferred settings to a shell profile). If both variables are set, upgrades are controlled by `OPAMUPGRADECRITERIA`, while `OPAMCRITERIA` applies to all other commands. 
If only `OPAMCRITERIA` is set, it applies to all commands. If only `OPAMUPGRADECRITERIA` is set, it applies to upgrade commands only, while all other commands are controlled by the `opam` internal default preferences.

## Yes, there are different versions of the user preference language

The `opam` package manager is an instance of the approach described in the article "[A modular package manager architecture](http://dl.acm.org/citation.cfm?id=2401012)", which was one of the outcomes of the [Mancoosi](http://www.mancoosi.org) research project. This architecture relies on external dependency solvers for package managers, that communicate with the package manager front-end via the [CUDF format](http://www.mancoosi.org/cudf/).
We have now several CUDF-compatible solvers, developed by a variety of research teams during the [MISC competitions](http://www.mancoosi.org/misc/) run yearly from 2010 to 2012:

* [aspcud](http://www.cs.uni-potsdam.de/wv/aspcud/)
* [Mccs](http://www.i3s.unice.fr/~cpjm/misc/mccs.html)
* [Packup](http://sat.inesc-id.pt/~mikolas/sw/packup/)
* [P2Cudf](https://wiki.eclipse.org/Equinox/p2/CUDFResolver)

Each of these competitions led to improving the preferences language, by allowing the user progressively more flexibility.

As of today, the preferences language described in the previous section, which corresponds to the one used in the 2010 competition, should be supported by all external solvers, but if you happen to use as external solver one of the entrants of the 2012 competition, like recent versions of `aspcud`, then you have access to a more sophisticated set of preferences, described in [the 2012 MISC competition rules](http://www.mancoosi.org/misc-2012/criteria/). 
For example, you could use

 `-count(removed), -count(down),-sum(solution,installedsize),-notuptodate(solution),-count(changed)`

to instruct a solver to minimise downgrades, and mininise the installed size, among other criteria.

The `aspcud` solver supports this extended language starting from its version 1.8.0, which unfortunately is not the version shipped by default with Ubuntu precise or Debian Wheezy.

### News in aspcud 1.9.x

Starting from version 1.9.0,  `aspcud`  adds support for three extra selectors, that are particularly useful to perform local upgrades. Here they are:

* `installrequest` is the set of packages in the solution that satisfy the requirements mentioned in the install: part of a CUDF request
* `upgraderequest` is the set of packages in the solution that satisfy the requirements mentioned in the upgrade: part of a CUDF request
* `request` is the union of the above two

Using this extended set of package selector, it is now finally possible to specify user preferences that describe optimisations to be applied only to the packages explicitly mentioned in the request. For example, `-notuptodate(request),-count(changed)` would find a solution that tries to bring all packages mentioned in the request to their latest version, while leaving all the rest as untouched as possible.

And if we have added to each package a `priority` value, we could also play with preferences like `+sum(upgraderequest,priority),-count(changed)` to get the packages mentioned in the upgrade request to the version with the highest possible priority, while leaving all the rest as untouched as possible.

## Preferences only work with the external solvers

For portability reasons, `opam` also embarks an ad-hoc solver module that is built by wrapping a set of heuristics around the code of the SAT-solver which is used in the [Dose Library](http://dose.gforge.inria.fr/public_html/) for detecting broken packages. This solver module has no support for user preferences, and is not able to manage correctly large package repositories: it is highly recommended that you install an external CUDF solver (`aspcud` is the one best supported today).

## Using external solvers in the Cloud

Thanks to support from [Irill](http://www.irill.org/), it is now possible to use an external solver for `opam` on any platform, over the network. See the [CUDF solver farm](http://cudf-solvers.irill.org/) for instructions.
The latest version of the solver is on the farm, so you can use the full preferences language with it.
