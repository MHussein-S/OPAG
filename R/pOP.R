pOP<-function (q,par,distr, lower.tail = TRUE, log.p = FALSE )
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
  G<-do.call(pdistname, c(list(q), as.list(distpar)))
  G_bar<-1-G
  p<-1-(c*G_bar/(G+c*G_bar))^a
  if (!lower.tail)
    p <- 1 - p
  if (log.p)
    p <- log(p)
  return(p)
}
