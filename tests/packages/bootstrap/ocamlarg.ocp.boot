comp = [ "-g" ]
link = [ "-g" ]

begin library "ocaml-arg"
   files = [
     "src/subCommand.ml"
     "src/formatExt.ml"
     "src/argExt.ml"
   ]
end
