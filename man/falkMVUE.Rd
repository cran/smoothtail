\name{falkMVUE}
\alias{falkMVUE}
\title{Compute original and smoothed version of Falk's estimator for a known endpoint}
\description{
Given an ordered sample of either exceedances or upper order statistics which is to be modeled using a GPD with 
distribution function \eqn{F}, this function provides Falk's estimator of the shape parameter \eqn{\gamma \in [-1,0]} 
if the endpoint 

\deqn{\omega(F) = \sup\{x \, : \, F(x) < 1\}} 

of \eqn{F} is known. Precisely,

\deqn{\hat \gamma_{\rm{MVUE}} = \hat \gamma_{\rm{MVUE}}(k,n) = \frac{1}{k} \sum_{j=1}^k \log \Bigl(\frac{\omega(F)-H^{-1}((n-j+1)/n)}{\omega(F)-H^{-1}((n-k)/n)}\Bigr), \; \; k=2,\ldots,n-1} 

for $H$ either the empirical or the distribution function based on the log--concave density estimator.
Note that for any \eqn{k}, \eqn{\hat \gamma_{\rm{MVUE}} : R^n \to (-\infty, 0)}. If \eqn{\hat \gamma_{\rm{MVUE}} 
\not \in [-1,0)}, then it is likely that the log-concavity assumption is violated.  
}
\usage{falkMVUE(x, omega)}
\arguments{
\item{x}{Sample of strictly increasing observations.}
\item{omega}{Known endpoint. Make sure that \eqn{\omega \ge X_{(n)}}.}}
\value{n x 3 matrix with columns: indices \eqn{k}, Falk's MVUE estimator using the smoothing method, and
the ordinary Falk MVUE estimator based on the order statistics.
}
\references{
Mueller, S. and Rufibach K. (2006).
Smooth tail index estimation.
Preprint.

Falk, M. (1994).
Extreme quantile estimation in \eqn{\delta}-neighborhoods of generalized Pareto distributions.
\emph{Statistics and Probability Letters}, \bold{20}, 9--21.

Falk, M. (1995).
Some best parameter estimates for distributions with finite endpoint.
\emph{Statistics}, \bold{27}, 115--125.
}
\author{
Kaspar Rufibach (maintainer), \email{kaspar.rufibach@stanford.edu}, \cr \url{http://www.stanford.edu/~kasparr} 

Samuel Mueller, \email{mueller@maths.uwa.edu.au}, \cr \url{http://www.maths.uwa.edu.au/Members/mueller}

Kaspar Rufibach gratefully acknowledges support by the Swiss National Science Foundation SNF, \url{http://www.snf.ch}
}
\seealso{Other approaches to estimate \eqn{\gamma} based on the fact that the density is log--concave, thus 
\eqn{\gamma \in [-1,0]}, are available as the functions \code{\link{pickands}}, \code{\link{falk}}.}
\examples{
# generate ordered random sample from GPD
set.seed(1977)
n <- 20
gam <- -0.75
x <- rgpd(n, gam)

# compute tail index estimators
omega <- -1 / gam
falkMVUE(x, omega)
}

\keyword{distribution}
\keyword{htest}
\keyword{nonparametric}


