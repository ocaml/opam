let regexp pat =
  Re_perl.compile_pat pat

let extract ~rex s =
  Re.get_all (Re.exec rex s)
