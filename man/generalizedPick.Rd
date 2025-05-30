\name{generalizedPick}
\alias{generalizedPick}
\title{Compute generalized Pickand's estimator}
\description{
Given an ordered sample of either exceedances or upper order statistics which is to be modeled using a GPD with 
distribution function \eqn{F}, this function provides Segers' estimator of the shape parameter \eqn{\gamma}, 
see Segers (2005). Precisely, for \eqn{k = \{1, \ldots, n-1\}}, the estimator can be written as

\deqn{\hat \gamma^k_{\rm{Segers}}(H) = \sum_{j=1}^k \Bigl(\lambda(j/k) - \lambda((j-1)/k)\Bigr) \log \Bigl(H^{-1}((n-\lfloor cj \rfloor)/n)-H^{-1}((n-j)/n) \Bigr)}

for \eqn{H} either the empirical or the distribution function based on the log--concave density estimator
and \eqn{\lambda} the mixing measure given in Segers (2005), Theorem 4.1, (i).
Note that for any \eqn{k}, \eqn{\hat \gamma^k_{\rm{Segers}} : R^n \to (-\infty, \infty)}. 
If \eqn{\hat \gamma_{\rm{Segers}} \not \in [-1,0)}, then it is likely that the log-concavity assumption is violated.  
}
\usage{generalizedPick(est, c, gam0, ks = NA)}
\arguments{
    \item{est}{Log-concave density estimate based on the sample as output by \code{logConDens} (a \code{dlc} object).}
    \item{c}{Number in \eqn{(0,1)}, determining the spacings that are used.}
    \item{gam0}{Number in \eqn{R \setminus 0.5}, specifying the mixing measure.}
    \item{ks}{Indices \eqn{k} at which Falk's estimate should be computed. If set to \code{NA} defaults to \eqn{4, \ldots, n}.}
    }
\value{n x 3 matrix with columns: indices \eqn{k}, Segers' estimator using the smoothing method, and
the ordinary Segers' estimator based on the order statistics.}
\references{
Mueller, S. and Rufibach K. (2009).
Smooth tail index estimation.
\emph{J. Stat. Comput. Simul.}, \bold{79}, 1155--1167.

Segers, J. (2005).
Generalized Pickands estimators for the extreme value index.
\emph{J. Statist. Plann. Inference}, \bold{128}, 381--396.
}
\author{
Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch} 

Samuel Mueller, \email{samuel.muller@mq.edu.au}

Kaspar Rufibach acknowledges support by the Swiss National Science Foundation SNF, \url{http://www.snf.ch}
}
\seealso{Other approaches to estimate \eqn{\gamma} based on the fact that the density is log--concave, thus 
\eqn{\gamma \in [-1,0]}, are available as the functions \code{\link{pickands}}, \code{\link{falk}}, \code{\link{falkMVUE}}.}
\examples{
# generate ordered random sample from GPD
set.seed(1977)
n <- 20
gam <- -0.75
x <- rgpd(n, gam)

## generate dlc object
est <- logConDens(x, smoothed = FALSE, print = FALSE, gam = NULL, xs = NULL)

# compute tail index estimators
generalizedPick(est, c = 0.75, gam0 = -1/3)
}

\keyword{distribution}
\keyword{htest}
\keyword{nonparametric}


