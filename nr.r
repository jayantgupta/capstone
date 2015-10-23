nr <- function(a, iter){
	X<-c();
	Y <-c();
	x_old <- 0.01;
	for(i in 1:iter){
		X <- c(X, x_old);
		Y <- c(Y, i);
		x_new <- x_old*(2 - a*x_old);
		x_old <- x_new;
	}
	jpeg('nr.jpeg');
	plot(Y, X, xlab="Iteration #", ylab="Inverse", main=paste0("Newton-Raphson Simulation for ", a));
	dev.off();
}

nr(3,10);
