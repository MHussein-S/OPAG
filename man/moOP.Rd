\name{moOP}
\alias{moOP}
\title{Odd Parteo G distribution}
\description{The $n$th ordinary moment of the OPAG distribution for any base distribution}
\value{An object of the same length as x, giving the rth moment}
\usage{moOP(n,par,distr)}
\arguments{

  \item{n}{moment of order n}
  \item{par}{named vector or list of the distribution parameters.}
  \item{distr}{a character string specifying the base distribution (for example, "exp" for exponential base distribution).}
  }

\references{}
\author{Mohamed Hussein}
\examples{moOP(n=2,par=c(rate=2,a=3,c=2),distr="exp")
moOP(n=3,par=c(rate=0.2,a=1,c=2),distr="exp")
moOP(n=1,par=c(scale=2,shape=2,a=3,c=2),distr="weibull")
}
\keyword{odd pareto G distribution, family of distributions}
