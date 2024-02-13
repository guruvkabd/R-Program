A <- matrix(data=c(3,2,9,5), nrow=2, ncol=2, byrow=TRUE)
B <- matrix(data=c(7,1,6,4), nrow=2, ncol=2, byrow=FALSE)
C <- rbind(c(1,2,3),c(6,5,4)) 
D <- cbind(c(9,5,1),c(3,2,7))
print("Matrix of A") 
A
print("Matrix of B")
B
print("Matrix of C") 
C
print("Matrix of D") 
D
print("Resultant Matrices")
print("Addition of Two Matrices=") 
A+B
print("Subtraction of Two Matrices=") 
A-B
print("Scalar Multiplication of A Matrix=")
3*A
print("Multiplication of Two Matrices C & D=")
C%*%D
print("Diagonal Matrix of A")
diag(A)
cat("Transpose of C") 
t(C)
cat("Inverse of B")
solve(B)