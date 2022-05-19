\name{dOP}
\alias{dOP}
\title{Odd Parteo G distribution}
\description{Probability density function of OPAG distribution for any base distribution}
\value{An object of the same length as x, giving the pdf values computed at x }
\usage{dOP(x,par,distr, log = FALSE)}
\arguments{

  \item{x}{vector of quantiles}
  \item{par}{named vector or list of the distribution parameters.}
  \item{distr}{a character string specifying the base distribution (for example, "exp" for exponential base distribution).}
  \item{log}{logical; if TRUE, the values of log(pdf), at x, are given.}
  }
\references{}

\author{Mohamed Hussein}
\examples{dOP(x=2,par=list(a=1,c=2,rate=3),distr="exp")
dOP(c(2,3,4),par=c(scale=2,shape=2,a=3,c=2),distr="weibull")}

\keyword{odd pareto G distribution, family of distributions}


