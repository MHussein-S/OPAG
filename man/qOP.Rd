\name{qOP}
\alias{qOP}
\title{Odd Parteo G distribution}
\description{Quantile function of OPAG distribution for any base distribution}
\value{An object of the same length as p, giving the quantile function values computed at p}
\usage{qMPo(p,par,distr,lower=0,upper,lower.tail=TRUE,log.p=FALSE)}
\arguments{
  \item{p}{scalar or vector of probabilities.}
  \item{par}{named vector or list of the distribution parameters.}
  \item{distr}{character string specifying the base distribution (for example, "exp" for exponential base distribution).}
  \item{lower}{the lower end point of the interval to be searched.}
  \item{upper}{the upper end point of the interval to be searched.}
  \item{lower.tail}{logical; if TRUE, probability are P(X <= x), otherwise, P(X > x).}
  \item{log.p}{logical; if TRUE, probabilities p are given as log(p).}
}
\references{}
\author{Mohamed Hussein}
\examples{qOP(p=0.5,c(rate=0.2,a=2,c=0.5),"exp",upper=100)
  qOP(0.25,c(rate=0.2,a=2,c=1.5),"exp",upper=100)
  qOP(0.75,c(a=2,c=2),"norm",upper=100)
  qOP(log(0.5),c(a=5,c=1,rate=0.5),"exp",lower=0,upper=100,log.p=TRUE)
  qOP(0.10,c(a=1.5,c=3,rate=0.5),"exp",lower=0,upper=100,lower.tail=FALSE)
  qOP(p=0.5,c(rate=0.2,a=2,c=2),"exp",upper=100)
}
\keyword{odd pareto G distribution, family of distributions}
