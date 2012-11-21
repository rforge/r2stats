.onAttach = function(...) {

  # Set toolkit
  options(guiToolkit="RGtk2",width=160)

  # A first dataset
  data(sleepstudy,package="lme4")

  # build the GUI
  R2STATS()
}

