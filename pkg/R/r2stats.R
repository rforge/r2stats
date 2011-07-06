#--------------------------------------------------------------------------------------------------
#
#                      R2STATS: A Graphical User Interface for GLM and GLMM in R
#                       Yvonnick Noel, University of Brittany, Rennes 2, France
#                                            2006-2010
#
#--------------------------------------------------------------------------------------------------
#                                           MAIN SCRIPT
#--------------------------------------------------------------------------------------------------

# A starting dataset
data(sleepstudy)
require(RGtk2Extras)

# Main
R2STATS = function() {

  # Create R2STATS main window
  r2stats$create()
  
  # Some settings
  r2stats$setVersion(0.65)

  # Show R2STATS interface
  r2stats$show()
}
