\name{gpd}
\alias{gpd}
\alias{dgpd}
\alias{pgpd}
\alias{qgpd}
\alias{rgpd}
\title{The Generalized Pareto Distribution}
\description{Density function, distribution function, quantile function and
random generation for the generalized Pareto distribution (GPD) with shape parameter \eqn{\gamma} and 
scale parameter \eqn{\sigma}.}
\usage{
dgpd(x, gam, sigma = 1) 
pgpd(q, gam, sigma = 1) 
qgpd(p, gam, sigma = 1)
rgpd(n, gam, sigma = 1)}

\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations.}
  \item{gam}{Shape parameter, real number.}
  \item{sigma}{Scale parameter, positive real number.}
}
\details{
     The generalized Pareto distribution function (Pickands, 1975) with
     shape parameter \eqn{\gamma} and scale parameter \eqn{\sigma} is
     
     \deqn{W_{\gamma,\sigma}(x) = 1 - {(1+\gamma x / \sigma)}_+^{-1/\gamma}.}

     If \eqn{\gamma = 0}, the distribution function is defined by continuity. The density is denoted by 
     \eqn{w_{\gamma, \sigma}}.
}
\value{
     \code{\link{dgpd}} gives the values of the density function, \code{\link{pgpd}} those of the distribution
     function, and \code{\link{qgpd}} those of the quantile function of the GPD at \eqn{x, q,} and \eqn{p}, 
     respectively. \code{\link{rgpd}} generates \eqn{n} random numbers, returned as an ordered vector.
}
\references{
Pickands, J. (1975). Statistical inference using extreme order statistics. 
\emph{Annals of Statistics}, \bold{3}, 119-131.
}
\author{
Kaspar Rufibach, \email{kaspar.rufibach@stanford.edu}, \cr \url{http://www.stanford.edu/~kasparr} 

Samuel Mueller, \email{mueller@maths.uwa.edu.au}, \cr \url{http://www.maths.uwa.edu.au/Members/mueller}}

\seealso{Similar functions are provided in the R-packages \pkg{evir} and \pkg{evd}.}

\keyword{distribution}
\keyword{htest}
\keyword{nonparametric}
