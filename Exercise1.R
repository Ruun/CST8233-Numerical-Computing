# Define the function
f <- function(x) {
  0.1 * exp(x) * cos(x) + 2 * log(abs(x))
}

# Create the vector cVec
x_values <- seq(3, 6, by = 0.1)
cVec <- f(x_values)

# Calculate the sum
sum_cVec <- sum(cVec)
print(paste("The sum of this vector is:", format(sum_cVec, digits = 7)))

# Plot the vector
plot(x_values, cVec, type = "l", main = "My First Plot", xlab = "x", ylab = "f(x)")
