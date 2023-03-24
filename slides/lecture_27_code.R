## (Q1) First, want to show that W and G are approximately chi square
## under H0

n <- 100
beta0 <- 0.5
beta1 <- 0

nsim <- 1000
wald_stats <- rep(0, nsim)
lrt_stats <- rep(0, nsim)

for(i in 1:nsim){
  x <- rnorm(n)
  p <- exp(beta0 + beta1 * x)/(1 + exp(beta0 + beta1 * x))
  y <- rbinom(n, 1, p)
  
  m1 <- glm(y ~ x, family = binomial)
  
  wald_stats[i] <- (summary(m1)$coefficients[2,3])^2
  lrt_stats[i] <- m1$null.deviance - m1$deviance
}

hist(wald_stats)
hist(lrt_stats)

data.frame(wald_stats) %>%
  ggplot(aes(sample = wald_stats)) +
  stat_qq(distribution = qchisq, dparams = 1) +
  stat_qq_line(distribution = qchisq, dparams = 1)

data.frame(lrt_stats) %>%
  ggplot(aes(sample = lrt_stats)) +
  stat_qq(distribution = qchisq, dparams = 1) +
  stat_qq_line(distribution = qchisq, dparams = 1)


### (Q2) Now change n

n <- 50
beta0 <- 0.5
beta1 <- 0

nsim <- 1000
wald_stats <- rep(0, nsim)
lrt_stats <- rep(0, nsim)

for(i in 1:nsim){
  x <- rnorm(n)
  p <- exp(beta0 + beta1 * x)/(1 + exp(beta0 + beta1 * x))
  y <- rbinom(n, 1, p)
  
  m1 <- glm(y ~ x, family = binomial)
  
  wald_stats[i] <- (summary(m1)$coefficients[2,3])^2
  lrt_stats[i] <- m1$null.deviance - m1$deviance
}

data.frame(wald_stats) %>%
  ggplot(aes(sample = wald_stats)) +
  stat_qq(distribution = qchisq, dparams = 1) +
  stat_qq_line(distribution = qchisq, dparams = 1)

data.frame(lrt_stats) %>%
  ggplot(aes(sample = lrt_stats)) +
  stat_qq(distribution = qchisq, dparams = 1) +
  stat_qq_line(distribution = qchisq, dparams = 1)


### (Q3) Plot W against G

plot(lrt_stats, wald_stats)
abline(a = 0, b = 1)

## now change n

n <- 500
beta0 <- 0.5
beta1 <- 0

nsim <- 1000
wald_stats <- rep(0, nsim)
lrt_stats <- rep(0, nsim)

for(i in 1:nsim){
  x <- rnorm(n)
  p <- exp(beta0 + beta1 * x)/(1 + exp(beta0 + beta1 * x))
  y <- rbinom(n, 1, p)
  
  m1 <- glm(y ~ x, family = binomial)
  
  wald_stats[i] <- (summary(m1)$coefficients[2,3])^2
  lrt_stats[i] <- m1$null.deviance - m1$deviance
}

plot(lrt_stats, wald_stats)
abline(a = 0, b = 1)


## (Q4) now simulate under Ha

n <- 100
beta0 <- 0.5
beta1 <- 0.5

nsim <- 1000
wald_stats <- rep(0, nsim)
lrt_stats <- rep(0, nsim)

for(i in 1:nsim){
  x <- rnorm(n)
  p <- exp(beta0 + beta1 * x)/(1 + exp(beta0 + beta1 * x))
  y <- rbinom(n, 1, p)
  
  m1 <- glm(y ~ x, family = binomial)
  
  wald_stats[i] <- (summary(m1)$coefficients[2,3])^2
  lrt_stats[i] <- m1$null.deviance - m1$deviance
}

hist(wald_stats)
hist(lrt_stats)

plot(lrt_stats, wald_stats)
abline(a = 0, b = 1)

## Local alternative

n <- 100
beta0 <- 0.5
beta1 <- 2/sqrt(n)

nsim <- 1000
wald_stats <- rep(0, nsim)
lrt_stats <- rep(0, nsim)

for(i in 1:nsim){
  x <- rnorm(n)
  p <- exp(beta0 + beta1 * x)/(1 + exp(beta0 + beta1 * x))
  y <- rbinom(n, 1, p)
  
  m1 <- glm(y ~ x, family = binomial)
  
  wald_stats[i] <- (summary(m1)$coefficients[2,3])^2
  lrt_stats[i] <- m1$null.deviance - m1$deviance
}

plot(lrt_stats, wald_stats)
abline(a = 0, b = 1)
