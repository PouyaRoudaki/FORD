## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(FORD)     # Our package
library(FOCI)     # For comparison
library(ggplot2)  # For visualization

## ----continuous---------------------------------------------------------------
n <- 1000
x <- matrix(runif(n * 3), nrow = n)
y <- (x[, 1] + x[, 2]) %% 1

irdc(y, x[, 1])
irdc(y, x[, 2])
irdc(y, x[, 3])

## ----discrete-1---------------------------------------------------------------
n <- 10000
s <- 0.1
x1 <- c(rep(0, n * s), runif(n * (1 - s)))
x2 <- runif(n)
y <- x1

irdc(y, x1, dist.type.X = "discrete")
irdc(y, x2)

## ----discrete-2---------------------------------------------------------------
n <- 10000
x1 <- runif(n)
y1 <- rbinom(n, 1, 0.5)
y2 <- as.numeric(x1 >= 0.5)

irdc(y1, x1, dist.type.X = "discrete")
irdc(y2, x1, dist.type.X = "discrete")

FOCI::codec(y1, x1)
FOCI::codec(y2, x1)

## ----hurdle-vs-gamma----------------------------------------------------------
r_hurdle_poisson <- function(n, p_zero = 0.3, lambda = 2) {
  is_zero <- rbinom(n, 1, p_zero)
  rztpois <- function(m, lambda) {
    samples <- numeric(m)
    for (i in 1:m) {
      repeat {
        x <- rpois(1, lambda)
        if (x > 0) {
          samples[i] <- x
          break
        }
      }
    }
    samples
  }
  result <- numeric(n)
  result[is_zero == 0] <- rztpois(sum(is_zero == 0), lambda)
  result
}

set.seed(123)
n <- 1000
p_zero <- 0.4
lambda <- 10

hurdle <- r_hurdle_poisson(n, p_zero, lambda)
gamma_mix <- c(rep(0, round(p_zero * n)), rgamma(round((1 - p_zero) * n), shape = lambda, rate = 1))

df <- data.frame(
  value = c(hurdle, gamma_mix),
  source = rep(c("Hurdle Poisson", "Gamma Mixture"), each = n)
)

ggplot(df, aes(x = value, fill = source)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 40) +
  labs(title = "Comparison: Hurdle Poisson vs Gamma Mixture",
       x = "Value", y = "Count", fill = "Distribution") +
  theme_bw()

## ----discrete-3---------------------------------------------------------------
x1 <- sort(gamma_mix)
y1 <- rbinom(n, 1, 0.5)
y2 <- sort(hurdle)

irdc(y1, x1, dist.type.X = "discrete")
irdc(y2, x1, dist.type.X = "discrete")

FOCI::codec(y1, x1)
FOCI::codec(y2, x1)

## ----discrete-4---------------------------------------------------------------
x1 <- sort(hurdle)
y1 <- rbinom(n, 1, 0.5)
y2 <- sort(gamma_mix)

irdc(y1, x1, dist.type.X = "discrete")
irdc(y2, x1, dist.type.X = "discrete")

FOCI::codec(y1, x1)
FOCI::codec(y2, x1)

