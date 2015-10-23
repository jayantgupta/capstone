library("HomomorphicEncryption")

ptm<- proc.time()
# -- CLIENT Side code -- #
# Generating keys
#p <- pars("FandV", d=4096, qpow=128, t=2**30);
p <- parsHelp("FandV", lambda=128, L=4);
k <- keygen(p);

# Parsing the data
data <- read.csv("parsed.csv");
m_p <- data[data$survived == 1,];
m_p <- m_p[,c(1,3,4,5)]
m_p <- m_p[sample(1:nrow(m_p), 200, replace=FALSE),]
m_n <- data[data$survived == -1,];
m_n <- m_n[,c(1,3,4,5)]
m_n <- m_n[sample(1:nrow(m_n), 200, replace=FALSE),]
m_p <- matrix(unlist(m_p), ncol=4);
m_n <- matrix(unlist(m_n), ncol=4);

# Encrypting the data
enc_m_p <- enc(k$pk, m_p);
enc_m_n <- enc(k$pk, m_n);

l_p <- nrow(m_p);
l_n <- nrow(m_n);
enc_l_p <- enc(k$pk, l_p);
enc_l_n <- enc(k$pk, l_n);

# Initializing Constant encrypted values
enc_0 <- enc(k$pk, 0);
enc_2 <- enc(k$pk, 2);

print("Encryption of the dataset complete");

# Server Starts now
# Assuming all the encrypted values are trasmitted
# to the server.

s_p = rep(enc_0, 4);
for( i in 1:nrow(m_p)){
	for( j in 1:4){
		s_p[j] = s_p[j] + enc_m_p[i,j];
	}
}

s_n = rep(enc_0, 4);
for( i in 1:nrow(m_n)){
	for( j in 1:4){
		s_n[j] = s_n[j] + enc_m_n[i,j]
	}
}

s_p_ <- enc_l_n * s_p; # modified (+ve) mean vector
s_n_ <- enc_l_p * s_n; # modified (-ve) mean vector

w_ <- s_p_ - s_n_; # Weight vector
X_0 <- s_p_ + s_n_; # X0

c_ <- enc_0; # Constant
for(i in 1:4){
	temp <- w_[i] * X_0[i];
	c_ <- c_ + temp;
#	print(dec(k$sk, temp));
}
print("Model creation complete!!");
# Creating of model complete
cat("s_p_:", dec(k$sk, s_p_), "\n");
cat("s_n_:", dec(k$sk, s_n_), "\n");
cat("w_:", dec(k$sk, w_), "\n");
cat("X_0:", dec(k$sk, X_0), "\n");
cat("c_:", dec(k$sk, c_), "\n");

print("Test Classification begins!!");

# Classification
X_1 = enc(k$pk, m_p[1,]);
X_0 = enc(k$pk, c(1,2,1,2));

ans <- enc_0;
for(i in 1:4){
	temp <- X_1[i]*w_[i];
	ans <- ans + temp;
}
const <- enc_l_p * enc_l_n;
const <- const * enc_2;
ans <- ans*const;
ans <- ans - c_;

# -- Client Gets the Answer -- #
x_class <- dec(k$sk, ans);
print("Binary class of the input");
print(sign(x_class));

print( proc.time() - ptm )
