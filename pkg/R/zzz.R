.onAttach = function(...) {

  # Set toolkit
  options(guiToolkit="RGtk2",width=160)

  # A starting dataset
  tabname = "sleepstudy"
  pkg = "lme4"
  eval(parse(text=paste("data(",tabname,",package=",pkg,")",sep=""),envir=.GlobalEnv))

  # build the GUI
  R2STATS()
}

