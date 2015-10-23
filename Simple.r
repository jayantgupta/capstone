library(HomomorphicEncryption)
p <- pars("FandV")
k <- keygen(p)

c1 <- enc(k$pk, 2)
c2 <- enc(k$pk, -1)
cres1 <- c1 * c2
print(dec(k$sk, cres1))
#c1 <- enc(k$pk, c(42, 34))
#c2 <- enc(k$pk, c(7, 5))
#cres1 <- c1 + c2
#cres2 <- c1 * c2
#print(dec(k$sk, cres1))
#print(dec(k$sk, cres2))
