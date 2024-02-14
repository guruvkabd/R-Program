#library("rgl")
#install.packages("scatter3D")
#install.packages("htmlwidgets", type = "binary")
mvtrain <- function(hwdata) 
{ 
  nd=2 
  hv=vector(mode="numeric", length=0) 
  wv=vector(mode="numeric", length=0) 
  pv=vector(mode="numeric", length=0) 
  hmin = min(hwdata[,1])-15 
  hmax = max(hwdata[,1])+15 
  wmin = min(hwdata[,2])-15 
  wmax = max(hwdata[,2])+15 
  mv = colMeans(hwdata); 
  cv = cov(hwdata); 
  cat("Mean Vector", mv ,"\n") 
  cat("Covariance of Height",cv) 
  for(h in hmin:hmax) 
  { 
    for(w in wmin:wmax) 
    { 
      d = c(h,w)-mv 
      r = ((t(d) %*% solve(cv)) %*% (d)) 
      p = 1/(2*pi*sqrt(det(cv)))*exp(-0.5*r) 
      hv <- c(hv, h) 
      wv <- c(wv, w) 
      pv <- c(pv, p) 
    } 
  } 
  Scatter3d(x = hv, y = wv, z = pv, col ="blue", xlab="Height", ylab="Weight",zlab="p(h,w)") 
  return (mvdata=data.frame(mv=mv,cv=cv)) 
} 
mvtest <- function(mvdata,hwdata) 
{ 
  mv = mvdata$mv 
  cv = cbind(mvdata$cv.1, mvdata$cv.2) 
  d = hwdata - mv 
  r = ((t(d) %*% solve(cv)) %*% (d)) 
  pt = 1/(2*pi*sqrt(det(cv)))*exp(-0.5*r) 
  if (pt >= 0.00005) 
    print("person is an adult based on H & W") 
  else 
    print("person is not an adult based on H & W") 
} 
hwdata <- cbind(c(165, 170, 160, 154, 175, 155, 167, 177, 158, 178), c(78, 71, 60, 53, 72, 51, 64, 65, 55, 69)) 
mvdata = mvtrain(hwdata)
ht= as.numeric(readline(prompt ='Enter the Height of person =')) 
wt= as.numeric(readline(prompt ='Enter the Weight of person =')) 
hwdata = c(ht,wt)
mvtest(mvdata,hwdata)