\name{pOP}
\alias{pOP}
\title{Odd Parteo G distribution}
\description{Cumulative distribution function of OPGA distribution for any base distribution}
\value{An object of the same length as q, giving the cdf values computed at q}
\usage{pOP(q,par,distr,lower.tail = TRUE, log.p = FALSE)}
\arguments{

  \item{q}{vector of quantiles}
  \item{par}{named vector or list of the distribution parameters}
  \item{distr}{a character string specifying the base distribution (for example, "exp" for exponential base distribution).}
  \item{lower.tail}{logical; if TRUE, probability is P(X <= x), otherwise, P(X > x).}
  \item{log.p}{logical; if TRUE, probabilitiy p is given as log(p).}
  }
\references{}
\author{Mohamed Hussein}
\examples{pOP(q=2,par=list(a=1,c=2,rate=3),distr="exp")
pOP(c(2,3,4),par=c(scale=2,shape=2,a=3,c=2),distr="weibull")}

\keyword{odd pareto G distribution, family of distributions}
