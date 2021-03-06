library(MASS)
library(broom)
library(modelr)
library(tidyverse)
Sigma <- matrix(c(1, 0.7, 0.7, 1), 2)
X <- mvrnorm(n=n, c(0, 0), Sigma = Sigma)
y <- 1.2 + 0.3*X[,1]+1.1*X[,2]+rnorm(n)

sim_reg <- function(n=100L, B=200){
  Sigma <- matrix(c(1, 0.7, 0.7, 1), 2)
  X <- mvrnorm(n=n, c(0, 0), Sigma = Sigma)
  y <- 1.2 + 0.3*X[,1]+1.1*X[,2]+rnorm(n)
  df <- data_frame(y=y, X1=X[,1], X2=X[,2])
  
  ## reg
  reg <- lm(y~X1+X2, data=df)
  reg_su <- summary(reg)
  reg_co <- tidy(reg_su)
  co_X1 <- reg_co$estimate[2]
  co_X2 <- reg_co$estimate[3]
  ratio <- co_X1/co_X2
  ## Delta
  R <- t(cbind(0,1/1.1,-1*0.3/(1.1^2)))
  v <- as.numeric(t(R) %*% vcov(reg_su) %*% R)
  # W-test
  W_test <- t(co_X1/co_X2-0.3/1.1)*v^(-1)*(co_X1/co_X2-0.3/1.1)
  ## boot it:
  x_boot <- modelr::bootstrap(df, n=B) %>%
    mutate(reg=map(strap, ~lm(y~X1+X2, data=.)),
           reg_su = map(reg, summary),
           reg_co = map(reg_su, tidy),
           ratio_b = map_dbl(reg_co, as.numeric(~.$estimate[2]/.$estimate[3])),
           R = map(reg_co, t(cbind(0, ~.1/.$estimate[3], -1*~.$estimate[2]/(.$estimate[3]^2)))),
           vcov_b = map2_dbl(reg_su, R, as.numeric(t(R) %*% vcov(reg_su) %*% R)) ,
           W_test_b = map2_dbl(ratio_b, vcov_b, t(ratio_b-0.3/1.1)%*%(vcov_b^-1)%*%(ratio_b-0.3/1.1) ) ,
           W_test_b2 = map2_dbl(ratio_b, vcov_b, t(ratio_b-ratio)%*%(vcov_b^-1)%*%(ratio_b-ratio)))
x_boot2 <- x_boot %>%
  summarise(boot_sd=sd(ratio_b),
            boot_sd_v = mean(vcov_b),
            boot_mean=mean(ratio_b),
            wtest_cval=COMPUTE,
            wtest2_cval=COMPUTE)
x_boot2
## results: original Wald and va, and boot aggregates
data_frame(co_X1=co_X1, co_X2=co_X2, ratio=ratio,
           W_test=W_test,
           v=v) %>%
  bind_cols(x_boot2)
}

sim_reg()
