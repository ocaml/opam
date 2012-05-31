# OCamlPro Good Practices

Here are a list of "Good Practices" when developing code for OCamlPro

## Directories

A project should contain the following sub-directories

src/libs/ : libraries used by the project (each library in a different
            directory)
src/tools/ : tools created or used by the project (each tool in a
             different library)
src/vendor/ : libraries used by the project, but not defined by the project
             (each library in a separate directory)
doc : documentation on the project

## Ocp-build files

- ".ocp" files should be stored in the same directory as their sources :
  - they should never contain paths with / (not portable)
  - if all the sources of a project are in one directory, then the .ocp
      file describing this project should be in that directory;
  - different projects should be put in different directories when possible;
    common files should define a library

## Source file naming conventions

- Files should be prefixed with the name of the project (for project 'xxxx',
     there should be 'xxxArgs.ml', 'xxxTypes', 'xxxMain.ml', etc.)
- When possible, the following suffixes should be used :
   - xxxMisc.ml : functions that are generic enough to be useful for another
                  project
   - xxxTypes.ml : all the types defined by the project
   - xxxGlobals.ml : global values of the program
   - xxxArgs.ml : the variables that store the arguments values
   - xxxMain.ml : definition of arguments, parsing and call

Note that, outside a library, all types in a program 'xxx' should be
defined in one file xxxTypes. A type should be defined in a different
module ONLY if either it is only used in that module, or the module
should be moved outside the program in a library, useful for other
programs.

## Code style

- Lines should be no longer than 80 characters

## GIT

- When multiple users are working on the same project, they should
work in different branches (their name/login should appear in the
branch name, with possibly a word explaining the purpose of the
branch, like "fabrice-master", "fabrice-windows", etc.), and
synchronise when merging their branch in a common branch (like
"master" or "next").

- Renaming branches is forbidden. Branches should always be merged, so that
users can easily upgrade their repositories.


