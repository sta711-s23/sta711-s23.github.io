# family-wise error rate for different methods when all hypotheses are null

alpha <- 0.05
nsim <- 1000
errors_sidak <- c()
errors_bonferroni <- c()
errors_holm <- c()
for(j in 1:nsim){
  nsamp <- 100
  n <- 50
  pvals <- rep(0, nsamp)
  for(i in 1:nsamp){
    x <- rnorm(n)
    test_stat <- sqrt(n)*mean(x)
    pvals[i] <- 2*pnorm(abs(test_stat), lower.tail=F)
  }
  
  errors_sidak[j] <- sum(pvals < 1 - (1 - alpha)^(1/nsamp)) > 0
  errors_bonferroni[j] <- sum(pvals < alpha/nsamp) > 0
  errors_holm[j] <- sum(p.adjust(pvals, "holm") < alpha) > 0
  #print(j)
}
mean(errors_sidak)
mean(errors_bonferroni)
mean(errors_holm)


# now compare power to reject true alternatives

alpha <- 0.05
nsim <- 1000
power_sidak <- c()
power_bonferroni <- c()
power_holm <- c()
for(j in 1:nsim){
  nsamp <- 100
  n <- 50
  pvals <- rep(0, nsamp)
  for(i in 1:nsamp){
    x <- rnorm(n, mean=0.5)
    test_stat <- sqrt(n)*mean(x)
    pvals[i] <- 2*pnorm(abs(test_stat), lower.tail=F)
  }
  
  power_sidak[j] <- mean(pvals < 1 - (1 - alpha)^(1/nsamp))
  power_bonferroni[j] <- mean(pvals < alpha/nsamp)
  power_holm[j] <- mean(p.adjust(pvals, "holm") < alpha)
  #print(j)
}
mean(power_sidak)
mean(power_bonferroni)
mean(power_holm)
