comp = [ "-g" ]
link = [ "-g" ]

begin library "re"
   sort = true
   files = [
     "lib/re_str.ml"
     "lib/re_posix.ml"
     "lib/re_perl.ml"
     "lib/re_glob.ml"
     "lib/re_emacs.ml"
     "lib/re.ml"
     "lib/cset.ml"
     "lib/automata.ml"
   ]
end
