Working version changelog, used as a base for the changelog and the release
note.
Prefixes used to help generate release notes, changes, and blog posts:
* ✘ Possibly scripts breaking changes
* ◈ New option/command/subcommand
* [BUG] for bug fixes
* [NEW] for new features (not a command itself)
* [API] api updates 🕮
If there is changes in the API (new non optional argument, function renamed or
moved, etc.), please update the _API updates_ part (it helps opam library
users)

## Version

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

## Remove

## Switch

## Config

## Pin

## List

## Show

## Var/Option

## Update / Upgrade

## Tree

## Exec

## Source

## Lint

## Repository

## Lock

## Clean

## Env

## Opamfile

## External dependencies

## Format upgrade

## Sandbox

## VCS

## Build

## Infrastructure

## Release scripts

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell

## Internal

## Internal: Windows

## Test

## Reftests
### Tests
  * Add some additional test to tree, for `--dev` && `--no-switch` [#5687 @rjbou]
  * switch-set: add test that checks unsetting `OPAMSWITCH` when it was set by `opam env --set-switch` on an already set `OPAMSWITCH` variable in environment [#5742 rjbou]

### Engine
  * Set `SHELL` to `/bin/sh` in Windows to ensure `opam env` commands are consistent [#5723 @dra27]
  * Substitution for `BASEDIR` and `OPAMTMP` now recognise the directory with either forward-slashes, back-slashes, or converted to Cygwin notation (i.e. C:\cygwin64\tmp\..., C:/cygwin64/tmp/..., or /tmp/...) [#5723 @dra27]

## Github Actions

## Doc
  * Improve the installation instructions on FreeBSD [#5775 lukstafi]

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
  * `OpamSystem.apply_cygpath`: runs `cygpath` over the argument [#5723 @dra27 - function itself added in #3348]
