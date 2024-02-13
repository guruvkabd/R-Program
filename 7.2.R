head(mtcars,32)
co = cov(mtcars$wt, mtcars$mpg)
print("Covariance")
print(co)
cc = cor(mtcars$wt, mtcars$mpg)
print("Pearson's Correlation Coefficient")
print(cc)
ccs = cor(mtcars$wt, mtcars$mpg,method = "spearman")
print("Spearman's Correlation Coefficient")
print(ccs)
scatter.smooth(mtcars$wt, mtcars$mpg, main="Scatter Plot", xlab="CarWeight", ylab="Mileage")
if(cc > 0){
  print("Relationship b/w Car Weight and Mileage is Positive")
} else
{print("Relationship b/w Car Weight and Mileage is Negative")}