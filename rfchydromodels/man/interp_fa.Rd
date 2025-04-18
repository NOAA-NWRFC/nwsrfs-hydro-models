% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sac-snow-uh.R
\name{interp_fa}
\alias{interp_fa}
\title{Interpolate forcing adjustment factors}
\usage{
interp_fa(factors, month, day, hour)
}
\arguments{
\item{factors}{12 element vector of monthly adjustment factors}

\item{month}{a vector of month values for each time step}

\item{day}{a vector of day values for each time step}

\item{hour}{a vector of hour values for each time step}
}
\value{
A vector matching the length of month, containing interpolated adjustment factors
}
\description{
This function interpolates 12 forcing adjustment factors (1 per month)
by placing them at the 15th of the month then interpolating between the
previous and next months values for every time step in between.
}
\examples{
d1 <- as.POSIXct("2001-01-01 00:00:00", tz = "UTC")
d2 <- as.POSIXct("2001-12-31 18:00:00", tz = "UTC")
dates <- seq.POSIXt(d1, d2, by = "6 hours")

month <- as.integer(format(dates, "\%m"))
day <- as.integer(format(dates, "\%d"))
hour <- as.integer(format(dates, "\%H"))
factors <- c(.5, 2, 1, 1.5, 2, .5, 1, 2.5, 3, -1.5, 0, 1)

ifa <- interp_fa(factors, month, day, hour)

plot(dates, ifa, t = "l")
points(as.POSIXct(paste0("2001-", 1:12, "-15"), tz = "UTC"), factors, col = "red")
}
