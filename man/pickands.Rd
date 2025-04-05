\name{pickands}
\alias{pickands}
\title{Compute original and smoothed version of Pickands' estimator}
\description{
Given an ordered sample of either exceedances or upper order statistics which is to be modeled using a GPD, this 
function provides Pickands' estimator of the shape parameter \eqn{\gamma \in [-1,0]}. 
Precisely, for \eqn{k=4, \ldots, n}

\deqn{\hat \gamma^k_{\rm{Pick}} = \frac{1}{\log 2} \log \Bigl(\frac{H^{-1}((n-r_k(H)+1)/n)-H^{-1}((n-2r_k(H) +1)/n)}{H^{-1}((n-2r_k(H) +1)/n)-H^{-1}((n-4r_k(H)+1)/n)} \Bigr)} 

for $H$ either the empirical or the distribution function \eqn{\hat F_n} based on the log--concave density 
estimator and

\deqn{r_k(H) = \lfloor k/4 \rfloor}

if \eqn{H} is the empirical distribution function and

\deqn{r_k(H) = k / 4}

if \eqn{H = \hat F_n}.}
\usage{pickands(est, ks = NA)}
\arguments{
    \item{est}{Log-concave density estimate based on the sample as output by \code{logConDens} (a \code{dlc} object).}
    \item{ks}{Indices \eqn{k} at which Falk's estimate should be computed. If set to \code{NA} defaults to \eqn{4, \ldots, n}.}
    }
\value{n x 3 matrix with columns: indices \eqn{k}, Pickands' estimator using the log-concave density estimate, and
the ordinary Pickands' estimator based on the order statistics.
}
\references{
Mueller, S. and Rufibach K. (2009).
Smooth tail index estimation.
\emph{J. Stat. Comput. Simul.}, \bold{79}, 1155--1167.

Pickands, J. (1975).
Statistical inference using extreme order statistics.
\emph{Annals of Statistics} \bold{3}, 119--131.
}
\author{
Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch} 

Samuel Mueller, \email{samuel.muller@mq.edu.au}

Kaspar Rufibach acknowledges support by the Swiss National Science Foundation SNF, \url{http://www.snf.ch}
}
\seealso{Other approaches to estimate \eqn{\gamma} based on the fact that the density is log--concave, thus 
\eqn{\gamma \in [-1,0]}, are available as the functions \code{\link{falk}}, \code{\link{falkMVUE}}, \code{\link{generalizedPick}}.}
\examples{
# generate ordered random sample from GPD
set.seed(1977)
n <- 20
gam <- -0.75
x <- rgpd(n, gam)

## generate dlc object
est <- logConDens(x, smoothed = FALSE, print = FALSE, gam = NULL, xs = NULL)

# compute tail index estimators
pickands(est)
}

\keyword{distribution}
\keyword{htest}
\keyword{nonparametric}
