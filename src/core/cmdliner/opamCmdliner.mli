(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Declarative definition of command line interfaces.

    Consult the {{!page-tutorial}tutorial}, details about the supported
    {{!page-cli}command line syntax} and {{!page-examples}examples} of
    use.

    Open the module to use it, it defines only three modules in your
    scope. *)

(** Man page specification.

    Man page generation is automatically handled by [Cmdliner],
    consult the {{!page-tool_man.manual}details}.

    The {!Manpage.block} type is used to define a man page's
    content. It's a good idea to follow the
    {{!Manpage.standard_sections}standard} manual page structure.

   {b References.}
   {ul
   {- [man-pages(7)], {{:http://man7.org/linux/man-pages/man7/man-pages.7.html}
      {e Conventions for writing Linux man pages}}.}} *)
module Manpage : sig

  (** {1:man Man pages} *)

  type block =
    [ `S of string | `P of string | `Pre of string | `I of string * string
    | `Noblank | `Blocks of block list ]
  (** The type for a block of man page text.

      {ul
      {- [`S s] introduces a new section [s], see the
         {{!standard_sections}standard section names}.}
      {- [`P t] is a new paragraph with text [t].}
      {- [`Pre t] is a new preformatted paragraph with text [t].}
      {- [`I (l,t)] is an indented paragraph with label
         [l] and text [t].}
      {- [`Noblank] suppresses the blank line introduced between two blocks.}
      {- [`Blocks bs] splices the blocks [bs].}}

      Except in [`Pre], whitespace and newlines are not significant
      and are all collapsed to a single space. All block strings
      support the {{!page-tool_man.doclang}documentation markup language}.*)

  val escape : string -> string
  (** [escape s] escapes [s] so that it doesn't get interpreted by the
      {{!page-tool_man.doclang}documentation markup language}. *)

  type xref =
    [ `Main | `Cmd of string | `Tool of string | `Page of string * int ]
  (** The type for man page cross-references.
      {ul
      {- [`Main] refers to the man page of the program itself.}
      {- [`Cmd cmd] refers to the man page of the program's [cmd]
         command (which must exist).}
      {- [`Tool bin] refers to the command line tool named [bin].}
      {- [`Page (name, sec)] refers to the man page [name(sec)].}} *)

  (** {1:standard_sections Standard section names and content}

      The following are standard man page section names, roughly ordered
      in the order they conventionally appear. See also
      {{:http://man7.org/linux/man-pages/man7/man-pages.7.html}[man man-pages]}
      for more elaborations about what sections should contain. *)

  val s_description : string
  (** The [DESCRIPTION] section. This should be a description of what
      the tool does and provide a little bit of usage and
      documentation guidance. *)

  val s_commands : string
  (** The [COMMANDS] section. By default subcommands get listed here. *)

  val s_arguments : string
  (** The [ARGUMENTS] section. By default positional arguments get
      listed here. *)

  val s_options : string
  (** The [OPTIONS] section. By default optional arguments get
      listed here. *)

  val s_common_options : string
  (** The [COMMON OPTIONS] section. By default help and version options get
      listed here. For programs with multiple commands, optional arguments
      common to all commands can be added here. *)

  val s_exit_status : string
  (** The [EXIT STATUS] section. By default term status exit codes
      get listed here. *)

  val s_environment : string
  (** The [ENVIRONMENT] section. By default environment variables get
      listed here. *)

  val s_files : string
  (** The [FILES] section. *)

  val s_bugs : string
  (** The [BUGS] section. *)

  val s_examples : string
  (** The [EXAMPLES] section. *)

  val s_authors : string
  (** The [AUTHORS] section. *)

  (** {1:output Output}

    The {!print} function can be useful if the client wants to define
    other man pages (e.g. to implement a help command). *)

  type format = [ `Auto | `Pager | `Plain | `Groff ]
  (** The type for man page output specification.
      {ul
      {- [`Auto], formats like [`Pager] or [`Plain] whenever the [TERM]
         environment variable is [dumb] or unset.}
      {- [`Pager], tries to write to a discovered pager, if that fails
         uses the [`Plain] format.}
      {- [`Plain], formats to plain text.}
      {- [`Groff], formats to groff commands.}} *)
end

(** Terms.

    A term is evaluated by a program to produce a {{!Term.result}result},
    which can be turned into an {{!Term.exits}exit status}. A term made of terms
    referring to {{!Arg}command line arguments} implicitly defines a
    command line syntax. *)
module Term : sig

  (** {1:terms Terms} *)

  type +'a t
  (** The type for terms evaluating to values of type 'a. *)

  val const : 'a -> 'a t
  (** [const v] is a term that evaluates to [v]. *)

  val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
  (** [f $ v] is a term that evaluates to the result of applying
      the evaluation of [v] to the one of [f]. *)

  (** {1 Interacting with Cmdliner's evaluation} *)

  val choice_names : string list t
  (** [choice_names] is a term that evaluates to the names of the commands
      that are children of the main command. *)

  type 'a ret =
  [ `Help of Manpage.format * string option
  | `Error of (bool * string)
  | `Ok of 'a ]
  (** The type for command return values. See {!val-ret}. *)

  val ret : 'a ret t -> 'a t
  (** [ret v] is a term whose evaluation depends on the case
      to which [v] evaluates. With :
      {ul
      {- [`Ok v], it evaluates to [v].}
      {- [`Error (usage, e)], the evaluation fails and [Cmdliner] prints
         the error [e] and the term's usage if [usage] is [true].}
      {- [`Help (format, name)], the evaluation fails and [Cmdliner] prints
         a manpage in format [format]. If [name] is [None] this is the
         the main command's manpage. If [name] is [Some c] this is
         the man page of the sub command [c] of the main command.}}

      {b Note.} While not deprecated you are encouraged not use this API. *)

  (** {1:deprecated Deprecated Term evaluation interface}

      This interface is deprecated in favor of {!Cmdliner.Cmd}. Follow
      the compiler deprecation warning hints to transition. *)

  (** {2:tinfo Term information}

      Term information defines the name and man page of a term.
      For simple evaluation this is the name of the program and its
      man page. For multiple term evaluation, this is
      the name of a command and its man page. *)

  [@@@alert "-deprecated"] (* Need to be able to mention them ! *)

  type exit_info
  [@@ocaml.deprecated "Use Cmd.Exit.info instead."]
  (** The type for exit status information. *)

  type env_info
  [@@ocaml.deprecated "Use Cmd.Env.info instead."]
  (** The type for environment variable information. *)

  type info
  [@@ocaml.deprecated "Use Cmd.info instead."]
  (** The type for term information. *)

  val info :
    ?man_xrefs:Manpage.xref list -> ?man:Manpage.block list ->
    ?envs:env_info list -> ?exits:exit_info list -> ?sdocs:string ->
    ?docs:string -> ?doc:string -> ?version:string -> string -> info
  [@@ocaml.deprecated "Use Cmd.info instead."]
  (** [info sdocs man docs doc version name] is a term information
      such that:
      {ul
      {- [name] is the name of the program or the command.}
      {- [version] is the version string of the program, ignored
         for commands.}
      {- [doc] is a one line description of the program or command used
         for the [NAME] section of the term's man page. For commands this
         description is also used in the list of commands of the main
         term's man page.}
      {- [docs], only for commands, the title of the section of the main
         term's man page where it should be listed (defaults to
         {!Manpage.s_commands}).}
      {- [sdocs] defines the title of the section in which the
         standard [--help] and [--version] arguments are listed
         (defaults to {!Manpage.s_options}).}
      {- [exits] is a list of exit statuses that the term evaluation
         may produce.}
      {- [envs] is a list of environment variables that influence
         the term's evaluation.}
      {- [man] is the text of the man page for the term.}
      {- [man_xrefs] are cross-references to other manual pages. These
         are used to generate a {!Manpage.s_see_also} section.}}
      [doc], [man], [envs] support the {{!page-tool_man.doclang}documentation
      markup language} in which the following variables are recognized:
      {ul
      {- [$(tname)] the term's name.}
      {- [$(mname)] the main term's name.}} *)

 (** {2:evaluation Evaluation} *)

  type 'a result =
    [ `Ok of 'a | `Error of [`Parse | `Term | `Exn ] | `Version | `Help ]
  (** The type for evaluation results.
      {ul
      {- [`Ok v], the term evaluated successfully and [v] is the result.}
      {- [`Version], the version string of the main term was printed
       on the help formatter.}
      {- [`Help], man page about the term was printed on the help formatter.}
      {- [`Error `Parse], a command line parse error occurred and was
         reported on the error formatter.}
      {- [`Error `Term], a term evaluation error occurred and was reported
         on the error formatter (see {!Term.val-ret}').}
      {- [`Error `Exn], an exception [e] was caught and reported
         on the error formatter (see the [~catch] parameter of {!eval}).}} *)

  val eval :
    ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
    ?env:(string -> string option) -> ?argv:string array -> ('a t * info) ->
    'a result
  [@@ocaml.deprecated "Use Cmd.v and one of Cmd.eval* instead."]
  (** [eval help err catch argv (t,i)]  is the evaluation result
      of [t] with command line arguments [argv] (defaults to {!Sys.argv}).

      If [catch] is [true] (default) uncaught exceptions
      are intercepted and their stack trace is written to the [err]
      formatter.

      [help] is the formatter used to print help or version messages
      (defaults to {!Format.std_formatter}). [err] is the formatter
      used to print error messages (defaults to {!Format.err_formatter}).

      [env] is used for environment variable lookup, the default
      uses {!Sys.getenv}. *)

  (** {2:exits Turning evaluation results into exit codes}

      {b Note.} If you are using the following functions to handle
      the evaluation result of a term you should add {!default_exits} to
      the term's information {{!val-info}[~exits]} argument.

      {b WARNING.} You should avoid status codes strictly greater than 125
      as those may be used by
      {{:https://www.gnu.org/software/bash/manual/html_node/Exit-Status.html}
       some} shells. *)
end

(** Commands.

    Command line syntaxes are implicitely defined by {!Term}s. A command
    value binds a syntax and its documentation to a command name.

    A command can group a list of sub commands (and recursively). In this
    case your tool defines a tree of commands, each with its own command
    line syntax. The root of that tree is called the {e main command};
    it represents your tool and its name. *)
module Cmd : sig

  (** {1:info Command information}

      Command information defines the name and documentation of a command. *)

  (** Exit codes and their information. *)
  module Exit : sig

    (** {1:codes Exit codes} *)

    type code = int
    (** The type for exit codes.

        {b Warning.} You should avoid status codes strictly greater than 125
        as those may be used by
        {{:https://www.gnu.org/software/bash/manual/html_node/Exit-Status.html}
        some} shells. *)

    (** {1:info Exit code information} *)

    type info
    (** The type for exit code information. *)
  end

  (** Environment variable and their information. *)
  module Env : sig

    (** {1:info Environment variable information} *)

    [@@@alert "-deprecated"]
    type info = Term.env_info (* because of Arg. *)
    (** The type for environment variable information. *)
    [@@@alert "+deprecated"]
  end

  type info
  (** The type for information about commands. *)

  val info :
    ?deprecated:string -> ?man_xrefs:Manpage.xref list ->
    ?man:Manpage.block list -> ?envs:Env.info list -> ?exits:Exit.info list ->
    ?sdocs:string -> ?docs:string -> ?doc:string -> ?version:string ->
    string -> info
  (** [info ?sdocs ?man ?docs ?doc ?version name] is a term information
      such that:
      {ul
      {- [name] is the name of the command.}
      {- [version] is the version string of the command line tool, this
         is only relevant for the main command and ignored otherwise.}
      {- [deprecated], if specified the command is deprecated and the
         string is a message output on standard error when the command
         is used.}
      {- [doc] is a one line description of the command used
         for the [NAME] section of the command's man page and in command
         group listings.}
      {- [docs], for commands that are part of a group, the title of the
         section of the parent's command man page where it should be listed
         (defaults to {!Manpage.s_commands}).}
      {- [sdocs] defines the title of the section in which the
         standard [--help] and [--version] arguments are listed
         (defaults to {!Manpage.s_common_options}).}
      {- [exits] is a list of exit statuses that the command evaluation
         may produce, defaults to {!Exit.defaults}.}
      {- [envs] is a list of environment variables that influence
         the command's evaluation.}
      {- [man] is the text of the man page for the command.}
      {- [man_xrefs] are cross-references to other manual pages. These
         are used to generate a {!Manpage.s_see_also} section.}}

      [doc], [man], [envs] support the {{!page-tool_man.doclang}documentation
      markup language} in which the following variables are recognized:
      {ul
      {- [$(tname)] the (term's) command's name.}
      {- [$(mname)] the main command name.}
      {- [$(iname)] the command invocation from main command to the
         command name.}}
  *)

  (** {1:cmds Commands} *)

  type 'a t
  (** The type for commands whose evaluation result in a value of
      type ['a]. *)

  val v : info -> 'a Term.t -> 'a t
  (** [v i t] is a command with information [i] and command line syntax
      parsed by [t]. *)

  val group : ?default:'a Term.t -> info -> 'a t list -> 'a t
  (** [group i ?default cmds] is a command with information [i] that
      groups sub commands [cmds]. [default] is the command line syntax
      to parse if no sub command is specified on the command line. If
      [default] is [None] (default), the tool errors when no sub
      command is specified. *)

  val name : 'a t -> string
  (** [name c] is the name of [c]. *)

  (** {1:eval Evaluation}

      These functions are meant to be composed with {!Stdlib.exit}.
      The following exit codes may be returned by all these functions:
      {ul
      {- {!Exit.cli_error} if a parse error occurs.}
      {- {!Exit.internal_error} if the [~catch] argument is [true] (default)
         and an uncaught exception is raised.}
      {- The value of [~term_err] (defaults to {!Exit.cli_error}) if
         a term error occurs.}}

      These exit codes are described in {!Exit.defaults} which is the
      default value of the [?exits] argument of function {!val-info}. *)

  (** {2:eval_low Low level evaluation}

      This interface gives more information on command evaluation results
      and lets you choose how to map evaluation results to exit codes. *)

  type 'a eval_ok =
  [ `Ok of 'a (** The term of the command evaluated to this value. *)
  | `Version (** The version of the main cmd was requested. *)
  | `Help (** Help was requested. *) ]
  (** The type for successful evaluation results. *)

  type eval_error =
  [ `Parse (** A parse error occurred. *)
  | `Term (** A term evaluation error occurred. *)
  | `Exn (** An uncaught exception occurred. *) ]
  (** The type for erroring evaluation results. *)

  val eval_value :
    ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
    ?env:(string -> string option) -> ?argv:string array -> 'a t ->
    ('a eval_ok, eval_error) result
  (** [eval ~help ~err ~catch ~env ~argv cmd] is the evaluation result
      of [cmd] with:
      {ul
      {- [argv] the command line arguments to parse (defaults to {!Sys.argv})}
      {- [env] the function used for environment variable lookup (defaults
         to {!Sys.getenv}).}
      {- [catch] if [true] (default) uncaught exceptions
         are intercepted and their stack trace is written to the [err]
         formatter}
      {- [help] is the formatter used to print help or version messages
         (defaults to {!Format.std_formatter})}
      {- [err] is the formatter used to print error messages
         (defaults to {!Format.err_formatter}).}} *)

end

(** Terms for command line arguments.

    This module provides functions to define terms that evaluate
    to the arguments provided on the command line.

    Basic constraints, like the argument type or repeatability, are
    specified by defining a value of type {!Arg.t}. Further constraints can
    be specified during the {{!Arg.argterms}conversion} to a term. *)
module Arg : sig

(** {1:argconv Argument converters}

    An argument converter transforms a string argument of the command
    line to an OCaml value. {{!converters}Predefined converters}
    are provided for many types of the standard library. *)

  type 'a parser = string -> [ `Ok of 'a | `Error of string ]
  [@@ocaml.deprecated "Use Arg.conv or Arg.conv' instead."]
  (** The type for argument parsers.

      {b Deprecated.} Use parser signatures of {!val-conv} or {!val-conv'}. *)

  type 'a printer = Format.formatter -> 'a -> unit
  (** The type for converted argument printers. *)

  [@@@alert "-deprecated"] (* Need to be able to mention them ! *)
  type 'a conv = 'a parser * 'a printer
  (** The type for argument converters.

      {b Warning.} Do not use directly, use {!val-conv} or {!val-conv'}.
      This type will become abstract in the next major version of cmdliner. *)
  [@@@alert "+deprecated"] (* Need to be able to mention them ! *)

  val some : ?none:string -> 'a conv -> 'a option conv
  (** [some ?none c] is like [some'] but [none] is described as a
      string that will be rendered in bold. *)

(** {1:arginfo Arguments and their information}

    Argument information defines the man page information of an
    argument and, for optional arguments, its names. An environment
    variable can also be specified to read the argument value from
    if the argument is absent from the command line and the variable
    is defined. *)

  type 'a t
  (** The type for arguments holding data of type ['a]. *)

  type info
  (** The type for information about command line arguments. *)

  val info :
    ?deprecated:string -> ?absent:string -> ?docs:string -> ?docv:string ->
    ?doc:string -> ?env:Cmd.Env.info -> string list -> info
  (** [info docs docv doc env names] defines information for
      an argument.
      {ul
      {- [names] defines the names under which an optional argument
         can be referred to. Strings of length [1] (["c"]) define
         short option names (["-c"]), longer strings (["count"])
         define long option names (["--count"]). [names] must be empty
         for positional arguments.}
      {- [env] defines the name of an environment variable which is
         looked up for defining the argument if it is absent from the
         command line. See {{!page-cli.envlookup}environment variables} for
         details.}
      {- [doc] is the man page information of the argument.
         The {{!page-tool_man.doclang}documentation language} can be used and
         the following variables are recognized:
         {ul
         {- ["$(docv)"] the value of [docv] (see below).}
         {- ["$(opt)"], one of the options of [names], preference
            is given to a long one.}
         {- ["$(env)"], the environment var specified by [env] (if any).}}
         {{!doc_helpers}These functions} can help with formatting argument
         values.}
      {- [docv] is for positional and non-flag optional arguments.
         It is a variable name used in the man page to stand for their value.}
      {- [docs] is the title of the man page section in which the argument
         will be listed. For optional arguments this defaults
         to {!Manpage.s_options}. For positional arguments this defaults
         to {!Manpage.s_arguments}. However a positional argument is only
         listed if it has both a [doc] and [docv] specified.}
      {- [deprecated], if specified the argument is deprecated and the
         string is a message output on standard error when the argument
         is used.}
      {- [absent], if specified a documentation string that indicates
         what happens when the argument is absent. The document language
         can be used like in [doc]. This overrides the automatic default
         value rendering that is performed by the combinators.}} *)

  val ( & ) : ('a -> 'b) -> 'a -> 'b
  (** [f & v] is [f v], a right associative composition operator for
      specifying argument terms. *)

(** {1:optargs Optional arguments}

    The information of an optional argument must have at least
    one name or [Invalid_argument] is raised. *)

  val flag : info -> bool t
  (** [flag i] is a [bool] argument defined by an optional flag
      that may appear {e at most} once on the command line under one of
      the names specified by [i]. The argument holds [true] if the
      flag is present on the command line and [false] otherwise. *)

  val flag_all : info -> bool list t
  (** [flag_all] is like {!flag} except the flag may appear more than
      once. The argument holds a list that contains one [true] value per
      occurrence of the flag. It holds the empty list if the flag
      is absent from the command line. *)

  val vflag : 'a -> ('a * info) list -> 'a t
  (** [vflag v \[v]{_0}[,i]{_0}[;…\]] is an ['a] argument defined
      by an optional flag that may appear {e at most} once on
      the command line under one of the names specified in the [i]{_k}
      values. The argument holds [v] if the flag is absent from the
      command line and the value [v]{_k} if the name under which it appears
      is in [i]{_k}.

      {b Note.} Environment variable lookup is unsupported for
      for these arguments. *)

  val vflag_all : 'a list -> ('a * info) list -> 'a list t
  (** [vflag_all v l] is like {!vflag} except the flag may appear more
      than once. The argument holds the list [v] if the flag is absent
      from the command line. Otherwise it holds a list that contains one
      corresponding value per occurrence of the flag, in the order found on
      the command line.

      {b Note.} Environment variable lookup is unsupported for
      for these arguments. *)

  val opt : ?vopt:'a -> 'a conv -> 'a -> info -> 'a t
  (** [opt vopt c v i] is an ['a] argument defined by the value of
      an optional argument that may appear {e at most} once on the command
      line under one of the names specified by [i]. The argument holds
      [v] if the option is absent from the command line. Otherwise
      it has the value of the option as converted by [c].

      If [vopt] is provided the value of the optional argument is itself
      optional, taking the value [vopt] if unspecified on the command line. *)

  val opt_all : ?vopt:'a -> 'a conv -> 'a list -> info -> 'a list t
  (** [opt_all vopt c v i] is like {!opt} except the optional argument may
      appear more than once. The argument holds a list that contains one value
      per occurrence of the flag in the order found on the command line.
      It holds the list [v] if the flag is absent from the command line. *)

  (** {1:posargs Positional arguments}

      The information of a positional argument must have no name
      or [Invalid_argument] is raised. Positional arguments indexing
      is zero-based.

      {b Warning.} The following combinators allow to specify and
      extract a given positional argument with more than one term.
      This should not be done as it will likely confuse end users and
      documentation generation. These over-specifications may be
      prevented by raising [Invalid_argument] in the future. But for now
      it is the client's duty to make sure this doesn't happen. *)

  val pos : ?rev:bool -> int -> 'a conv -> 'a -> info -> 'a t
  (** [pos rev n c v i] is an ['a] argument defined by the [n]th
      positional argument of the command line as converted by [c].
      If the positional argument is absent from the command line
      the argument is [v].

      If [rev] is [true] (defaults to [false]), the computed
      position is [max-n] where [max] is the position of
      the last positional argument present on the command line. *)

  val pos_all : 'a conv -> 'a list -> info -> 'a list t
  (** [pos_all c v i] is an ['a list] argument that holds
      all the positional arguments of the command line as converted
      by [c] or [v] if there are none. *)

  val pos_right :
    ?rev:bool -> int -> 'a conv -> 'a list -> info -> 'a list t
  (** [pos_right] is like {!pos_left} except it holds all the positional
      arguments found on the right of the specified positional argument. *)

  (** {1:argterms Arguments as terms} *)

  val value : 'a t -> 'a Term.t
  (** [value a] is a term that evaluates to [a]'s value. *)

  val required : 'a option t -> 'a Term.t
  (** [required a] is a term that fails if [a]'s value is [None] and
      evaluates to the value of [Some] otherwise. Use this for required
      positional arguments (it can also be used for defining required
      optional arguments, but from a user interface perspective this
      shouldn't be done, it is a contradiction in terms). *)

  val non_empty : 'a list t -> 'a list Term.t
  (** [non_empty a] is term that fails if [a]'s list is empty and
      evaluates to [a]'s list otherwise. Use this for non empty lists
      of positional arguments. *)

  (** {1:predef Predefined arguments} *)

  val man_format : Manpage.format Term.t
  (** [man_format] is a term that defines a [--man-format] option and
      evaluates to a value that can be used with {!Manpage.print}. *)

  (** {1:converters Predefined converters} *)

  val int : int conv
  (** [int] converts values with {!int_of_string}. *)

  val string : string conv
  (** [string] converts values with the identity function. *)

  val enum : (string * 'a) list -> 'a conv
  (** [enum l p] converts values such that unambiguous prefixes of string names
      in [l] map to the corresponding value of type ['a].

      {b Warning.} The type ['a] must be comparable with {!Stdlib.compare}.

      @raise Invalid_argument if [l] is empty. *)

  val list : ?sep:char -> 'a conv -> 'a list conv
  (** [list sep c] splits the argument at each [sep] (defaults to [','])
      character and converts each substrings with [c]. *)

  val pair : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
  (** [pair sep c0 c1] splits the argument at the {e first} [sep] character
      (defaults to [',']) and respectively converts the substrings with
      [c0] and [c1]. *)

  (** {1:doc_helpers Documentation formatting helpers} *)

  val doc_alts_enum : ?quoted:bool -> (string * 'a) list -> string
  (** [doc_alts_enum quoted alts] is [doc_alts quoted (List.map fst alts)]. *)

  (** {1:deprecated Deprecated} *)

  [@@@alert "-deprecated"]

end
