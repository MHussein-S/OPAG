qOP<-function(p,par,distr,lower.tail=TRUE,log.p=FALSE)
{
  if(!is.list(par))
    par<- as.list(par)
  if (is.null(names(par)))
    stop("'par' must be a named list")
  ddistname <- paste("d", distr, sep = "")
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
  distparn<-setdiff(names(par),c("a","c"))
  distpar<-par[distparn]
  m <- match(distparn,args)
  if (any(is.na(m)))
    stop("you specifies names of parameters which are not valid for ",ddistname)
  if (log.p==TRUE)
    p<-exp(p)
  if (lower.tail==FALSE)
    p<-1-p
  p1<-c-c*(1-p)^(1/a)
  p1<-p1/(c+(1-c)*(1-p)^(1/a))
  q<-do.call(qdistname,c(list(p1), as.list(distpar)))
  return(q)
  }


