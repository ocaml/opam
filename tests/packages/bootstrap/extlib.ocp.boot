comp = [ "-g" ]
link = [ "-g" ]

begin library "extlib"
   sort = true
   files = [
     "unzip.ml"
     "uTF8.ml"
     "uChar.ml"
     "std.ml"
     "refList.ml"
     "pMap.ml"
     "option.ml"
     "optParse.ml"
     "install.ml"
     "IO.ml"
     "global.ml"
     "extString.ml"
     "extList.ml"
     "extLib.ml"
     "extHashtbl.ml"
     "extArray.ml"
     "enum.ml"
     "dynArray.ml"
     "dllist.ml"
     "bitSet.ml"
     "base64.ml"
     ]
end
