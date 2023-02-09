set.seed(3)
credits <- rpois(1000, 60)
lambda <- exp(-1 + 0.02*credits)
cards <- rtpois(1000, lambda, a=0)

output <- data.frame(cards = cards, credits = credits)
write.csv(output, "~/Documents/Teaching/sta711-s23.github.io/exams/deacon_cards.csv",
          row.names = F)

X <- cbind(1, credits)
y <- cards

U <- function(beta){
  lambda_hat <- exp(X %*% beta)
  return(t(X) %*% (y - lambda_hat/(1 - exp(-lambda_hat))))
}

I <- function(beta){
  lambda_hat <- exp(X %*% beta)
  W <- diag(c(lambda_hat * exp(lambda_hat) * 
              (-lambda_hat + exp(lambda_hat) - 1)/((exp(lambda_hat) - 1)^2)))
  return(t(X) %*% W %*% X)
}


beta = c(0,0)
for(i in 1:10){
  beta = beta + solve(I(beta)) %*% U(beta)
}
beta
