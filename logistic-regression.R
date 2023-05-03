# Logistic Regression from Scratch in R
# Bastiaan Quast
# bquast@gmail.com

# Load the mtcars dataset
data(mtcars)

# make variables available in the Global Environment (.GlobalEnv)
hp <- mtcars$hp
wt <- mtcars$wt
am <- mtcars$am

# define the (logistic) sigmoid function
sigmoid <- function(x)
  1 / (1 + exp(-x))

# cost function for a logisic model
logistic_cost <- function(y, y_hat) {
  -mean(y * log(y_hat) + (1 - y) * log(1 - y_hat))
}

# gradient descent
gradient_descent <- function(X, y, learning_rate, n_iterations) {
  m <- nrow(X)
  n <- ncol(X)
  theta <- matrix(0, n, 1)
  
  for (i in 1:n_iterations) {
    z <- X %*% theta
    y_hat <- sigmoid(z)
    gradient <- t(X) %*% (y_hat - y) / m
    theta <- theta - learning_rate * gradient
  }
  
  return(theta)
}

# Add a column of 1s to the data matrix (the constant)
X <- cbind(1, hp, wt)

# Reshape the response variable
y <- matrix(am, length(am), 1)

# Set the learning rate and the number of iterations
learning_rate <- 0.001
n_iterations <- 100000

# Run the gradient descent
theta <- gradient_descent(X, y, learning_rate, n_iterations)
