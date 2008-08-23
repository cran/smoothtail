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
\usage{pickands(x)}
\arguments{\item{x}{Sample of strictly increasing observations.}}
\value{n x 3 matrix with columns: indices \eqn{k}, Pickands' estimator using the smoothing method, and
the ordinary Pickands' estimator based on the order statistics.
}
\references{
Mueller, S. and Rufibach K. (2006).
Smooth tail index estimation.
\emph{J. Stat. Comput. Simul.}, to appear.

Pickands, J. (1975).
Statistical inference using extreme order statistics.
\emph{Annals of Statistics} \bold{3}, 119--131.
}
\author{
Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com} 

Samuel Mueller, \email{mueller@maths.uwa.edu.au}, \cr \url{http://www.maths.usyd.edu.au/ut/people?who=S_Mueller}

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

# compute tail index estimators
pickands(x)
}

\keyword{distribution}
\keyword{htest}
\keyword{nonparametric}
