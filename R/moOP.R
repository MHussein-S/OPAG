#ordinary moment
moOPAG<-function(n,par,distr)
{
  if(!is.list(par))
    par<- as.list(par)
  if (is.null(names(par)))
    stop("'par' must be a named list")
  ddistname <- paste("d", distr, sep = "")
  pdistname <- paste("p", distr, sep = "")
  qdistname <- paste("q", distr, sep = "")
  if (!exists(ddistname, mode="function"))
    stop(paste("The ", ddistname, " distribution is not defined"))
  cpar<-match("c",names(par))
  apar<-match("a",names(par))
  if(is.na(cpar))
    stop(" 'c' parameter is not defined")
  else if (is.na(apar))
    stop(" 'a' parameter is not defined")
  args <- names(formals(ddistname))
  c<-par$c
  a<-par$a
  distparn<-setdiff(names(par),c("c","a"))
  distpar<-par[distparn]
  m <- match(distparn,args)
  if (any(is.na(m)))
    stop("you specifies names of parameters which are not valid for ",ddistname)
  
  ########################## integrand ##########################
  integrand <- function(u,distr,par,n)
  {
    L1<-qOP(p=u,par=par,distr=distr)
    L1^n
  }
  #-------------------------------------------
  
  a<-integrate(integrand, lower = 0,upper = 1, distr=distr ,par= par,n=n)
  return(a$value)
}


