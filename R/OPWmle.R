OPWmle12<-function(data, starts, method="BFGS"){
  library(AdequacyModel)
  den=function(par,x){b=par[3];k=par[4]; dweibull(x,scale=b,shape=k)}
  cum=function(par,x){b=par[3];k=par[4]; pweibull(x,scale=b,shape=k)}
  pdf0<-function(par,x){
    f0=den(par,x)
    c0=cum(par,x)
    a=par[1]
    c1=par[2] #c1=1/c
    const=a*c1
    c0_bar=1-c0
    val1=const*f0/c0_bar^2
    val2=c0_bar/(c0_bar+c1*c0)
    val1*val2^(a+1) #new pdf
  }
  cdf0<-function(par,x){
    f0=den(par,x)
    c0=cum(par,x)
    a=par[1]
    c1=par[2]#c1=1/c
    c0_bar=1-c0
    val=c0_bar/(c0_bar+c1*c0)
    1-val^a #new cdf
  }

  med=suppressWarnings(goodness.fit(pdf=pdf0, cdf=cdf0, starts=starts, data=data, method=method, mle=NULL))
  aux=cbind(med$mle,med$Erro,med$mle+qnorm(0.025)*med$Erro,med$mle+qnorm(0.975)*med$Erro)
  colnames(aux)=c("MLE","Std. Dev.","Inf. 95% CI","Sup. 95% CI")

  aux1=cbind(med$AIC, med$CAIC, med$BIC, med$HQIC, med$W, med$A, med$Value)
  colnames(aux1)=c("AIC","CAIC","BIC","HQIC","W","A", "Min(-log(Likelihood))")
  rownames(aux1)=c("")

  aux2=cbind(med$KS$statistic,med$KS$p.value)
  colnames(aux2)=c("KS Statistic","KS p-value")
  rownames(aux2)=c("")

  aux3=cbind(if(med$Convergence==0){"Algorithm Converged"} else {"Algorithm Not Converged"})
  colnames(aux3)=c("")
  rownames(aux3)=c("")

  list("Estimates"=aux,"Measures"=aux1,"Kolmogorov-Smirnov Test"=aux2,"Convergence Status"=aux3)
}
