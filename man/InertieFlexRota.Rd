% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InertieFlexRota.R
\name{InertieFlexRota}
\alias{InertieFlexRota}
\title{Inertia and surface}
\usage{
InertieFlexRota(b, h, agDeg, sct, N = 100)
}
\arguments{
\item{b}{Dimesion of the base}

\item{h}{Dimension of the height}

\item{agDeg}{section orientation angle (torsion, in degrees)}

\item{sct}{Section type (see details)}

\item{N}{Number of discretizations (default to 100)}
}
\value{
A list of three:
\itemize{
\item IgFlex : bending inertia
\item IgTor : torsion inertia
\item Sr : cross-section surface
}
}
\description{
Computes the inertia of bending and torsion, and the cross-section area
}
\details{
For the section type, possible values are:
\itemize{
\item sct = 1 : triangle (bottom-oriented)
\item sct = 2 : rectangle
\item sct = 3 : triangle (top-oriented)
\item sct = 4 : ellipsis
\item sct = 5 : circle
}
}
