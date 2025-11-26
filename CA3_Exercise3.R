# Exercise 3 part1
df <- with(mtcars, data.frame(y = mpg, x1 = disp, x2 = hp, x3 = wt))
df

#part 2
nll_lm <- function(params,df,...)
{
  beta0 <- params[1]
  beta1 <- params[2]
  beta2 <- params[3]
  beta3 <- params[4]
  sigma2 <- exp(params[5])
  
  mu <- beta0 + beta1*df$x1 + beta2*df$x2 + beta3*df$x3
  res <- df$y - mu
  llik <- dnorm(res, mean = 0, sd = sqrt(sigma2),log = TRUE)
  
  return(-sum(llik))
}

mod1<-lm(y ~ x1 + x2 +x3,data = df)
mod1

# part 3
test <-c(mean(df$y),0,0,0,log(var(df$y)))

lower_bound <-c(-Inf,-Inf,-Inf,-Inf,log(1e-06))
upper_bound <- c(Inf,Inf,Inf,Inf,log(1e06))

fit <- optim(test, nll_lm, df = df,
             hessian = TRUE,method = "L-BFGS-B",lower =c(-Inf,-Inf,-Inf,-Inf,log(1e-06)),upper = c(Inf,Inf,Inf,Inf,log(1e06)))
fit

# part 4
# we used negative log likelihood as optim only
# minimises a function and we were looking
# to maximise a function. Minimising a negative
# loglikelihood is the same as maximising a likelihood 

#part 5
des_x <- cbind(1,df$x1,df$x2,df$x3)
des_y <- df$y

beta_LS3 <- function(x, y) {
  solve(crossprod(x), crossprod(x, y))
}

beta_hat<-beta_LS3(des_x,des_y)
beta_hat_optim <- fit$par[1:4]
beta_hat
beta_hat_optim

#part 6
y_hat <- des_x %*% beta_hat
df_res <- nrow(des_x) - ncol(des_x)
sigma2_hat <- (crossprod(des_y) - 2 * crossprod(des_y, y_hat) + crossprod(y_hat)) / df_res
sigma_hat <- sqrt(sigma2_hat)

sigma_hat

sigma_hat_optim <- sqrt(exp(fit$par[5]))
sigma_hat_optim

# part 7
# For the beta values for matrix operations 
# and optim both minimise the sum 
# of residuals in similar way so answers are 
# close in answer
# but for sigma using optim differs as it only 
# divides by n whereas the matrix operations 
# divide by degrees of freedom n-p and 
# that is why the answers differ

#part 8
fit <- optim(test, nll_lm, df = df,
             hessian = TRUE,method = "L-BFGS-B",lower =c(-Inf,-Inf,-Inf,-Inf,log(1e-06)),upper = c(Inf,Inf,Inf,Inf,log(1e06)))

std_er <- sqrt(diag(solve(fit$hessian)))[1:4]
std_er
summary(mod1)

