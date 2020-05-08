Working version changelog, used as a base for the changelog and the release
note.

## Init
  * Remove m4 from the list of recommended tools [#4184 @kit-ty-kate]

## Switch
  * Fix Not_found with `opam switch create . --deps` [#4151 @AltGr]


## Config
  * Add switch depext-bypass as modifiable field [#4191 @rjbou]

## Depext
  * Update switch depext-bypass with --assume-depext and --no-depexts [#4191 @rjbou - fix #4177]
  * Fix performance issue of depext under Docker/debian [#4165 @AltGr]
  * Refactor package status [#4152 @rjbou]
  * Add Macport support [#4152 @rjbou]
