set.seed(5)
n <- 20
x <- runif(n, 0, 10)
e <- rnorm(n, 0, 12)
y <- 12 + 3 * x + e

plot(x, y)


fit <- lm(y ~ x)
summary(fit)