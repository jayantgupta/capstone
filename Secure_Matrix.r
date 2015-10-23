# Author : Jayant Gupta
# Date : October 15, 2015

# N = Number of samples.
# P = Number of features.
# Using Iris to demonstrate the functioning
# of the Homomorphic Encryption functionality.

library(permute)
library(HomomorphicEncryption)

p <- pars("FandV")
k <- keygen(p)
ENC_MINUS_ONE <- enc(k$pk, -1)
ENC_ONE <- enc(k$pk, 1)
ENC_ZERO <- enc(k$pk, 0)

perm_parity <- function(n, arr, check){
	sum <- 0
	for(i in 1:n){
		j <- i
		k <- 0
		while(check[j] == 0){
			check[j] <- 1
			k <- k + 1
			j <- arr[j]
		}
		if(k > 0){
			sum <- sum + ((k-1) %% 2)
		}
	}
	p <- sum %% 2
	if (p == 0){
		return(ENC_ONE)
	}else{
		return(ENC_MINUS_ONE)
	}
}

calculate_det <- function(M){
	# Leibnitz Formula.
	if(ncol(M) != nrow(M)){
		print("|| Incorrect Dimensions ||")
		return
	}
	L <- ncol(M)
	perms <- allPerms(c(1:L))
	perms <- rbind(c(1:L), perms)
	sign <- rep(ENC_ONE,nrow(perms))
	for(i in 1:nrow(perms)){
		sign[i] <- perm_parity(L, perms[i,], rep(0, L))
	}
	det <- ENC_ZERO
	for(i in 1:nrow(perms)){
		val <- ENC_ONE
		for(j in 1:L){
			val <- val * M[j,perms[i,j]]
#			print(dec(k$sk, val))
		}
#		print(dec(k$sk, val * sign[i]))
#		val <- val * sign[i]
		det <- det + val*ENC_ONE
	}
	return(det)
}

cofactor_matrix <- function(M){
	if(ncol(M) != nrow(M)){
		print("|| Incorrect Dimensions ||")
		return
	}
	cofactor = matrix(0, nrow = nrow(M), ncol = ncol(M))
	for(i in 1:nrow(M)){
		for(j in 1:ncol(M)){
			M_ = M[-i,-j]
			cofactor[i][j] <- calculate_det(M_)
		}
	}
	return(cofactor)
}

apply_filter <- function(M){
	if(ncol(M) != nrow(M)){
		print("|| Incorrect Dimensions ||")
		return
	}
	for(i in 1:nrow(M)){
		for(j in 1:ncol(M)){
			if(i+j %% 2 == 1){
				M[i][j] <- M[i][j] * MINUS_ONE
			}
		}
	}
	return(M)
}

secure_inverse <- function(M){
	M <- cofactor_matrix(M)
	M <- apply_filter(M)
	M_t <- t(M)
	det <- calculate_det(M)
	return(list(M_t, det))
	# Did not divide by det(M)
}

mod_iris_data <- function(data){
	data[,1]<-data[,1]*10
	data[,2]<-data[,2]*10
	data[,3]<-data[,3]*10
	data[,4]<-data[,4]*10
	temp <- data[,5]
	temp <- as.character(temp)
	temp[temp=="setosa"]<-1
	temp[temp=="versicolor"]<-2
	temp[temp=="virginica"]<-3
	temp <- as.numeric(temp)
	temp <- as.factor(temp)
	data[,5] <- temp
	data <- matrix(unlist(data), ncol=5, byrow=FALSE)
	data <- list(data[,-5], data[,5])
	return(data)
}

#------------------#
# CLIENT side code #
#------------------#
#iris<-mod_iris_data(iris)
#features <- iris[1]
#vals <- iris[2]
#M <- matrix(c(-2, 2, -3, -1, 1, 3, 2, 0 ,-1), nrow=3, ncol=3, byrow=TRUE)
M <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
enc_M <- enc(k$pk, M)
ans <- calculate_det(enc_M)
print("Getting the answer")
print(dec(k$sk, ans))


# Encrypt the complete dataset.
# ENC_features <- enc(k$pk, features)
# ENC_vals <- enc(k$pk, vals)
# MINUS_ONE <- enc(k$pk, -1)
# DEC_features <- dec(k$sk, ENC_features)
# DEC_vals <- dec(k$sk, ENC_vals)
# print(all.equal(DEC_features, features))
# print(all.equal(DEC_vals, vals))

#------------------#
# SERVER side code #
#------------------#

# Transpose the Matrix
# T_ENC_features <- t(ENC_features)

# Compute Matrix multiplication.
# SQ_features <- T_ENC_features %*% ENC_features

# Compute Matrix Inverse.
# INV_SQ_features_det <- secure_inverse(SQ_features)
# INV_SQ_features <- INV_SQ_features_det[1]
# ENC_det <- INV_SQ_features_det[2]

# Compute the parameters.
# ENC_B <-(INV_SQ_features %*% T_ENC_features) %*% ENC_vals

# Model formation complete.
# DEC_B <- dec(k$sk, ENC_B)
# DEC_det <- dec(k$sk, ENC_det)
