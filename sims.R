install.packages("simpleboot")
library(simpleboot)
library(tidyverse)
library(broom)

set.seed(123)
n=100
epsilon <- rnorm(n)
x1 <- rnorm(n)
x2 <- rnorm(n)

x <- cbind(x1,x2)

y <- 1.2 + 0.3*x1 + 1.1*x2 + epsilon


reg <- function(x,y) {
  x1 <- x[,1]
  x2 <- x[,2]
  r <- tidy(lm(y~x1+x2))
  theta_x <- r$estimate[2]/r$estimate[3]
  return(theta_x)
}

reg(x,y)

pairs.boot(x,y,reg(x=x,y=y,R=200)