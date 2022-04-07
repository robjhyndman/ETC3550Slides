# y[t] = a * y[t-1] + b * y[t-2]

# First, compute SSE for a specific phi
y <- rnorm(50)
a <- 0.5
b <- 0.7
fit <- rep(NA_real_, length(y))
for(i in 3:length(y)) {
  fit[i] <- a * y[i-1] + b * y[i-2]
}
sse <- sum((y-fit)^2, na.rm=TRUE)

# Function to compute SSE given y and phi as arguments
ar2_sse <- function(y, ab) {
  fit <- rep(NA_real_, length(y))
  for(i in 3:length(y)) {
    fit[i] <- ab[1] * y[i-1] + ab[2] * y[i-2]
  }
  return(sum((y-fit)^2, na.rm=TRUE))
}

# Check that it works
ar2_sse(y, c(0.5, 0.7))

# Function to find the optimal phi to give smallest SSE
ar2_fit <- function(y) {
  fit <- optim(c(0,0), ar2_sse, y=y)
  return(fit$par)
}

# Check that it works
ab <- ar2_fit(y)
ar2_sse(y, ab)

# Putting it all together
ar2 <- function(y) {
  ab <- ar2_fit(y)
  n <- length(y)
  return(ab[1] * y[n-1] + ab[2] * y[n-2])
}

ar2(y)
