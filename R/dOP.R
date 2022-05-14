dOP<-function (x,par,distr, log = FALSE)
{
  if(!is.list(par))
    par<- as.list(par)
  if (is.null(names(par)))
    stop("'par' must be a named list")
  ddistname <- paste("d", distr, sep = "")
  pdistname <- paste("p", distr, sep = "")

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
  g<-do.call(ddistname, c(list(x), as.list(distpar)))
  G<-do.call(pdistname, c(list(x), as.list(distpar)))
  G_bar<-1-G
  const<-a*c^a
  d<-const*g/G_bar^2
  d<-d*(G_bar/(G+c*G_bar))^(a+1)
  if (log)
    d <- log(d)
  return(d)
}
