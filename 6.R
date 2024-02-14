uvtrain <- function(hdata) 
{ 
  xv=vector(mode="numeric", length=0) 
  pv=vector(mode="numeric", length=0) 
  hmin = min(hdata)-15
  hmax = max(hdata)+15 
  m = mean(hdata); 
  v = var(hdata); 
  cat("Mean of Height", m ,"\n") 
  cat("Variance of Height",v) 
  for(x in hmin:hmax) 
  { 
    r = (x-m)^2/v 
    p = (1/(sqrt(2*pi*v)))*exp(-0.5*r); 
    xv <- c(xv, x) 
    pv <- c(pv, p) 
  } 
  plot(xv,pv,xlab="Height of Person",ylab="p(x)",main="Univariate Normal Density",col = "blue") 
  return(list(m,v)) 
}
uvtest <- function(m,v,ht) 
{ 
  r = (ht-m)^2/v 
  pt = (1/(sqrt(2*pi*v)))*exp(-0.5*r) 
  if (pt >= 0.00005) 
    print("The given height of person is an adult") 
  else 
    print("The given height of person is not an adult") 
}
hdata <- c(165, 170, 160, 154, 175, 155, 167, 177, 158, 178) 
mv = uvtrain(hdata)
ht = as.numeric(readline(prompt ='Enter the height of person =')) 
m = as.numeric(mv[1]) 
v = as.numeric(mv[2]) 
uvtest(m,v,ht)
