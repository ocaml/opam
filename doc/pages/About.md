# opam

## A Little Bit of History

### opam

Thomas Gazagnaire (OCamlPro, at the time) wrote the [first specification draft of opam](https://github.com/ocaml/opam/blob/30598a59c98554057ce2beda80f0d31474b94150/specs/roadmap.pdf?raw=true) at the end of Jan 2012. The specification was reviewed by Fabrice Le Fessant (OCamlPro/INRIA), Yaron Minsky of Jane Street (who funded the project), and Anil Madhavapeddy (University of Cambridge) who needed a source-based package manager to manage libraries emerging from the MirageOS project.

At about the same time, Frederic Tuong, an intern from Inria started to implement [the first version of opam](https://github.com/ocaml/opam/commits/master?page=112)
(called `ocp-get` at the time) at the end of Feb 2012. The DORM research grant funded this project in collaboration with OCamlPro and IRILL. Frederic also started to create the
[first opam packages](https://github.com/ocaml/opam-repository/commits/master?page=200) 
one month later. Frederic and Thomas worked
closely together in the following months to [demonstrate opam](https://www.youtube.com/watch?v=ivLqeRZJTGs) at the
[OCaml Workshop 2012](http://oud.ocaml.org/2012/), where (almost) everyone was already using it!
Frederic started a PhD in Oct 2012 and left the opam team to focus on his studies. Roberto Di
Cosmo and Pietro Abate, from IRILL, began helping Thomas at the end of 2012 to properly integrate their [Mancoosi](http://www.mancoosi.org/) tools (such as [CUDF](http://www.mancoosi.org/cudf/) and `dose`) so that opam could benefit from modern constraint-solving tools and be able to automatically use the
`aspcud` external solver, if available.

At the end of 2012, Vincent Bernardoff and Guillem Rieu (from OCamlPro) worked for a few months on improving the documentation and ease of use of opam for newcomers. They created [opam2web](https://github.com/ocaml/opam2web), the tool used to generate https://opam.ocaml.org.

The [first public beta of opam](http://www.ocamlpro.com/blog/2013/01/17/opam-beta.html)
 was released in Jan 2013, and a few
months later (in March 2013), [the first official release landed](http://www.ocamlpro.com/blog/2013/03/14/opam-1.0.0.html).
A few days later, Louis Gesbert (who joined OCamlPro in Dec 2012)
pushed [his first commit](https://github.com/ocaml/opam/commit/c56cf5e1e244cee9f707da8b682996bbc5dd31ff)
to the codebase. In Nov 2013,
[opam 1.1.0](https://opam.ocaml.org/blog/opam-1-1-0-released/) was released and Louis became the
technical lead. A months later, [opam 1.1.1](https://opam.ocaml.org/blog/opam-1-1-1-released/) with numerous bug fixes.

### `opam-repository`

Meanwhile, in June 2012, MirageOS started to use opam (as it was using a
custom 3.12.1 compiler). Very quickly, building on Frederic's work, Anil and Thomas shared the task
of adding new packages and monitored the pull requests on
`opam-repository`. The initial policy was to accept as many packages as
possible, which means that things were often broken. So they started
to use background [bulks builds](https://github.com/avsm/opam-bulk-logs)
 to improve the overall repository
quality. In July 2012, Jane-Street's Core libraries made [its
apparition](https://github.com/ocaml/opam-repository/commit/bad688d0f49f6c750525b0047b336eb8606e419d)
 in the repository. To improve the quality of new
packages, Travis CI was [integrated](https://github.com/ocaml/opam-repository/commit/2671cb1e968e084c13989762ea43fc1a5b4703d7) in Sept 2013 to the pull request
process. From Aug to Nov 2013, all the contributors of `opam-repository`
were contacted to relicense their contribution to CC0, which enabled the
[move of the repository](https://github.com/ocaml/opam-repository/issues/955)
to the OCaml organisation. The [opam
weather service](http://ows.irill.org/),
created by Iril and OCamlPro, was announced in
Apr 2014 and exposed quality metrics to the repository quality.

*Notes*: Some significant bumps in `opam-repository` were adoption by projects: 
- Start of the biweekly pulls from Jane Street on Core (the biggest one)
- The Ocsigen and XAPI remotes
- MirageOS releases.

## Getting Support

Opam has been created and is maintained by [OCamlPro](http://www.ocamlpro.com/). Bug reports and feature requests for the opam tool should be reported on [opam's issue-tracker](https://github.com/ocaml/opam/issues). Packaging issues or requests for a new package can be reported on the [official repository's issue-tracker](https://github.com/ocaml/opam-repository/issues).

General queries for both the tool and the packages could be addressed on the [OCaml-platform mailing-list](http://lists.ocaml.org/listinfo/platform). Insights and evolution of opam internals can discussed on the [opam-devel mailing-list](http://lists.ocaml.org/listinfo/opam-devel).

Standard commercial terms and support on opam, as well as training and consulting services, are provided by [OCamlPro](http://www.ocamlpro.com/).
