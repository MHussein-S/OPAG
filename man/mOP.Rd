\name{mOP}
\alias{mOP}
\title{Odd Parteo G distribution}
\description{The $n$th incomplete moment (E[X^n|X<y]) of the OPAG distribution for any base distribution}
\value{An object of the same length as x, giving the rth moment}
\usage{mOP(n,par,y,distr)}
\arguments{

  \item{n}{moment of order n}
  \item{par}{named vector or list of the distribution parameters.}
  \item{distr}{a character string specifying the base distribution (for example, "exp" for exponential base distribution).}
  \item{y}{upper limit of X}
  }

\references{}
\author{Mohamed Hussein}
\examples{mOP(n=2,par=c(rate=2,a=3,c=2),y=2,distr="exp")
mOP(n=3,par=c(rate=0.2,a=1,c=2),y=3,distr="exp")
mOP(n=1,par=c(scale=2,shape=2,a=3,c=2),y=2,distr="weibull")
}
\keyword{odd pareto G distribution, family of distributions}
