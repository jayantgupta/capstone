## Simulating Newton-Raphson Inverse Algorithm.

A<-matrix(c(3,2,1,4,6,5,7,8,9), nrow=3, ncol=3, byrow=TRUE);
I<-matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3, byrow=TRUE);
inv <-solve(A);
print(A);
print(inv);
#print(I)
c=18
X_prev=I/c
for (i in 1:7){
#				print(i)
#				print(X_prev)
				X_new = 2*X_prev - (X_prev %*% X_prev %*% A)
#				print(X_new)
				X_prev <- X_new
#				X <- scan()
}
print( X_new)
print(X_new %*% A)
