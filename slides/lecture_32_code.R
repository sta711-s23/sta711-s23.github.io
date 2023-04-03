# demonstration that 2arcsin(sqrt(x)) is a variance stabilizing transformation

nsim <- 1000
phats <- rep(NA, nsim)
n <- 100
p <- 0.8 # change to demonstrate how the variance depends on p
for(i in 1:nsim){
  x <- rbinom(n, 1, p)
  phats[i] <- mean(x)
}

hist(phats)
sd(phats)
sqrt(p*(1-p)/n)

# now add transformation

nsim <- 1000
transformed_phats <- rep(NA, nsim)
n <- 100
p <- 0.8 # change to demonstrate how the variance no longer depends on p
for(i in 1:nsim){
  x <- rbinom(n, 1, p)
  transformed_phats[i] <- 2*asin(sqrt(mean(x)))
}

hist(transformed_phats)
sd(transformed_phats)
1/sqrt(n)


# now let's check coverage

nsim <- 10000
# try values of p like 0.1, 0.5, 0.8. For n = 100, it seems like the transformation
# has slightly higher coverage
p <- 0.1 
n <- 100
covers_original <- rep(NA, nsim)
covers_transformed <- rep(NA, nsim)
for(i in 1:nsim){
  x <- rbinom(n, 1, p)
  phat <- mean(x)
  
  lower_original <- phat - 1.96*sqrt(phat*(1-phat)/n)
  upper_original <- phat + 1.96*sqrt(phat*(1-phat)/n)
  
  lower_transformed <- 2*asin(sqrt(phat)) - 1.96/sqrt(n)
  upper_transformed <- 2*asin(sqrt(phat)) + 1.96/sqrt(n)
  
  covers_original[i] <- lower_original < p & p < upper_original
  covers_transformed[i] <- sin((lower_transformed)/2)^2 < p & p < sin((upper_transformed)/2)^2
}

mean(covers_original)
mean(covers_transformed)

