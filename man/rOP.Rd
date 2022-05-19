\name{rOP}
\alias{rOP}
\title{Odd Parteo G distribution}
\description{Generates random deviates from OPAG distribution for any base distribution }
\value{Random deviates from theOdd Parteo G distribution, length of the result is determined by n}
\usage{rOP(n,par,distr)}
\arguments{

  \item{n}{number of random variates to be generated}
  \item{par}{named vector or list of the distribution parameters}
  \item{distr}{a character string specifying the base distribution (for example, "exp" for exponential base distribution).}
  }

\references{}
\author{Mohamed Hussein}
\examples{x<-rOP(n=500,par=c(rate=2,a=3,c=2),distr="exp")
rOP(n=100,par=c(scale=2,shape=2,a=3,c=2),distr="weibull")}
\keyword{odd pareto G distribution, family of distributions}
