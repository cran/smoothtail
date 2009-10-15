\name{falk}
\alias{falk}
\title{Compute original and smoothed version of Falk's estimator}
\description{
Given an ordered sample of either exceedances or upper order statistics which is to be modeled using a GPD, this 
function provides Falk's estimator of the shape parameter \eqn{\gamma \in [-1,0]}. Precisely,

\deqn{\hat \gamma_{\rm{Falk}} = \hat \gamma_{\rm{Falk}}(k, n) = \frac{1}{k-1} \sum_{j=2}^k \log \Bigl(\frac{X_{(n)}-H^{-1}((n-j+1)/n)}{X_{(n)}-H^{-1}((n-k)/n)} \Bigr), \; \; k=3, \ldots ,n-1}

for $H$ either the empirical or the distribution function based on the log--concave density estimator.
Note that for any \eqn{k}, \eqn{\hat \gamma_{\rm{Falk}} : R^n \to (-\infty, 0)}. If 
\eqn{\hat \gamma_{\rm{Falk}} \not \in [-1,0)}, then it is likely that the log-concavity assumption is violated.  
}
\usage{falk(x)}
\arguments{\item{x}{Sample of strictly increasing observations.}}
\value{n x 3 matrix with columns: indices \eqn{k}, Falk's estimator using the smoothing method, and
the ordinary Falk's estimator based on the order statistics.
}
\references{
Mueller, S. and Rufibach K. (2009).
Smooth tail index estimation.
\emph{J. Stat. Comput. Simul.}, \bold{79}, 1155--1167.

Falk, M. (1995).
Some best parameter estimates for distributions with finite endpoint.
\emph{Statistics}, \bold{27}, 115--125.
}
\author{
Kaspar Rufibach (maintainer), \email{kaspar.rufibach@ifspm.uzh.ch} 

Samuel Mueller, \email{s.mueller@maths.usyd.edu.au}, \cr \url{http://www.maths.usyd.edu.au/ut/people?who=S_Mueller}

Kaspar Rufibach acknowledges support by the Swiss National Science Foundation SNF, \url{http://www.snf.ch}
}
\seealso{Other approaches to estimate \eqn{\gamma} based on the fact that the density is log--concave, thus
\eqn{\gamma \in [-1,0]}, are available as the functions \code{\link{pickands}}, \code{\link{falkMVUE}}, \code{\link{generalizedPick}}.}
\examples{
# generate ordered random sample from GPD
set.seed(1977)
n <- 20
gam <- -0.75
x <- rgpd(n, gam)

# compute tail index estimators
falk(x)
}

\keyword{distribution}
\keyword{htest}
\keyword{nonparametric}

