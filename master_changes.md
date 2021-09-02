Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Version
  *

## Global CLI
  *

## Plugins
  *

## Init
  *

## Config report
  *

## Install
  *

## Remove
  *

## Switch
  *

## Pin
  *

## List
  *

## Show
  *

## Var
  *

## Option
  *

## Lint
  *

## Lock
  *

## Opamfile
  *

## External dependencies
  *
  * Set `DEBIAN_FRONTEND=noninteractive` for unsafe-yes confirmation level [#4735 @dra27 - partially fix #4731] [2.1.0~rc2 #4739]
  * Fix depext alpine tagged repositories handling [#4763 @rjbou] [2.1.0~rc2 #4758]
  * Homebrew: Add support for casks and full-names [#4801 @kit-ty-kate]
  * Disable the detection of available packages on RHEL-based distributions.
    This fixes an issue on RHEL-based distributions where yum list used to detect available
    and installed packages would wait for user input without showing any output and/or fail
    in some cases [#4791 @kit-ty-kate - fixes #4790]
  * Archlinux: handle virtual package detection [#4831 @rjbou - partial fix #4759]
  * Fallback on dnf if yum does not exist on RHEL-based systems [#4825 @kit-ty-kate]

## Format upgrade
  * Fix format upgrade when there is missing local switches in the config file [#4763 @rjbou - fix #4713] [2.1.0~rc2 #4715]
  * Fix not recorded local switch handling, with format upgrade [#4763 @rjbou] [2.1.0~rc2 #4715]
  * Set opam root version to 2.1 [#4763 @rjbou] [2.1.0~rc2 #4715]
  * Fix 2.1~alpha2 to 2.1 format upgrade with reinit [#4763 @rjbou - fix #4748] [2.1.0~rc2 #4750]
  * Fix bypass-check handling on reinit [#4750 @rjbou] [#4763 @rjbou] [2.1.0~rc2 #4750 #4756]

## Sandbox
  *

## Repository management
  *

## VCS
  *

## Build
  *

## Infrastructure
  *

## Admin
  *

## Opam installer
  *

## State
  *

# Opam file format
  *

## Solver
  *

## Client
  *

## Internal
  * Generalise `mk_tristate_opt` to `mk_state_opt` [#4575 @rjbou]
  * Fix `mk_state_opt` and rename to `mk_enum_opt` [#4626 @rjbou]
  * Add `mk_enum_opt_all` for state flags that appears more than once [#4582 @rjbou]
  * Fix `opam exec` on native Windows when calling cygwin executables [#4588 @AltGr]
  * Fix temporary file with a too long name causing errors on Windows [#4590 @AltGr]
  * CLI: Add flag deprecation and replacement helper [#4595 @rjbou]
  * Win32 Console: fix VT100 support [#3897 #4710 @dra27]
  * Tidied the opam files [#4620 @dra27]
  * Externalise cli versioning tools from `OpamArg` into `OpamArgTools` [#4606 @rjbou]
  * Each library defines its own environment variables, that fills the config record [#4606 @rjbou]
  * Harden cygpath wrapper [#4625 @dra27]
  * Reset the plugin symlinks when the root is upgraded [#4641 @dra27 - partial fix for #4619]
  * Formalise opam dev version detection with `OpamVersion.is_dev_version` [#4665 @dra27]
  * Add `OpamStd.String.is_prefix_of` [#4694 @rjbou @dra27]
  * Fix `OpamStd.Format.pretty_list`: `last` argument dropped if list contains more than 2 elements [#4694 @rjbou]
  *

## Test
  *

## Shell
  *

## Doc
  *

## Security fixes
  *
