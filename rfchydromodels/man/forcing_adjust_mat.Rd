% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sac-snow-uh.R
\name{forcing_adjust_mat}
\alias{forcing_adjust_mat}
\title{Adjust monthly climo based on 4 parameters}
\usage{
forcing_adjust_mat(
  climo,
  pars,
  ll = climo * ifelse(climo > 0, 0.9, 1.1),
  ul = climo * ifelse(climo > 0, 1.1, 0.9),
  return_climo = FALSE
)
}
\arguments{
\item{climo}{blah}

\item{pars}{blah}

\item{ll}{blah}

\item{ul}{blah}

\item{return_climo}{blah}
}
\value{
stuff
}
\description{
description
}
\examples{
climo <- rep(2, 12)
pars <- c(.5, 0, 10, 0)
forcing_adjust_mat(climo, pars)
}
