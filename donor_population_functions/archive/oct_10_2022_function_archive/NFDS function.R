
# hood river data 
x <- seq(2, 8 , 0.1)
m <- -1.001
b <- 4.4395
y <- (m*x) + b
plot(x, y, xlim = c(4, 7))


#back transformed
plot(exp(x), exp(y))


# example
n.adults <- 50
y <- exp(m*(log(n.adults)) + b)  # per capita reproductive success for 50 adults

# negative binomial function == should not be needed
hist(rnbinom(50000, 0.5, 0.1), breaks = 50)
vals <- rnbinom(50000, 0.5, 0.1)
mean(sample(vals, 500, replace = TRUE))

# iteratively down sample 
OUT <- NULL



