\name{r2stats}
\alias{r2stats}
\title{
%%   ~~ data name/kind ... ~~
  Internal object for storing the R2STATS GUI.
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
  r2stats is a proto object containing all data and methods for the R2STATS GUI.
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
  It is very unlikelily that access to this object is needed, but just in case: The R representation of any model is in the .$Rmodel field.
}
\examples{
# Once the GUI has been launched and your-model-name has been fitted,
# this gives you the internal R model object:
\dontrun{r2stats$models$your.model.name$Rmodel}
}
\author{
Yvonnick Noel, University of Brittany, Rennes, France.
}
\keyword{GUI}
