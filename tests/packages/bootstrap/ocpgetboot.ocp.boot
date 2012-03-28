comp = [ "-g" ]
link = [ "-g" ]

begin library "pcre"
   sort = true
   files = [
     "lib/pcre.ml"
   ]
   requires = [ "ocamlre" "extlib" ]
end
