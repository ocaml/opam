comp = [ "-g" ]
link = [ "-g" ]

begin library "cudf"
   sort = true
   files = [
     "main_cudf_parse_822.ml"
     "main_cudf_check.ml"
     "cudf_types_pp.ml"
     "cudf_types.ml"
     "cudf_type_parser.mly"
     "cudf_type_lexer.mll"
     "cudf_printer.ml"
     "cudf_parser.ml"
     "cudf_conf.ml"
     "cudf_checker.ml"
     "cudf_c.ml"
     "cudf_822_parser.mly"
     "cudf_822_lexer.mll"
     "cudf.ml"
   ]
   requires = [
     "extlib"
   ]
end
