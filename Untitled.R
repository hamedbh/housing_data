x <- sample(1:1000, 100, replace = TRUE)
mean(x)
sd(x)

y <- sample(1:1e6, 100, replace = TRUE)
y
z <- (x - mean(x))/sd(x)

w <- (y - mean(y))/sd(y)

difference <- z - w
