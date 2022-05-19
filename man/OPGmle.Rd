\name{OPGmle}
\alias{OPGmle}
\title{Odd Parteo G distribution}
\description{MLE of parameters, KS test statistic and p-value, AIC, CAIC, BIC, HQIC, -log-likelihood function and convergence status for OPAGA distribution}
\value{Maximum likelihood estimates, standard error, 95\% CI, Cramer-von Mises statistic, Anderson Darling statistic, Kolmogorov Smirnov test statistic and p-value,  Akaike Information Criterion, Consistent Akaikes Information Criterion, Bayesian Information Criterion, Hannan-Quinn information criterion,  minimum value of the negative log-likelihood function and convergence status when the OPAGA distribution is fitted to some data.}

\usage{OPGAmle(data,start)}
\arguments{
  \item{data}{a vector of data values for which the distribution is to be fitted}
  \item{start}{named vector or list of the starting values of the distribution parameters}
}

\references{}
\author{Mohamed Hussein}
\examples{
#{
data<-rOP(n=500,par=c(a=3,c=2,shape=2,rate=2),distr="gamma")
OPGAmle(data,start=c(a=0.1,c=2,shape=0.1,rate=0.1))
#}
}
\keyword{odd pareto gamma distribution, family of distributions}
