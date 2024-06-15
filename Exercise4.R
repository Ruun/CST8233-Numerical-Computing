# Define the function
myFun <- function(x) {
  ifelse(x < 0, 
         x^2 + 2*x + 3,
         ifelse(x < 2,
                x + 3,
                x^2 + 4*x - 7))
}
# Create the vector Vec1 for plotting
Vec1 <- seq(-4, 4, by = 0.1)
# Evaluate the function at the values of Vec1
y_values <- myFun(Vec1)
# Plot the function
plot(Vec1, y_values, type = "l", main = "Plot of myFun", xlab = "x", ylab = "f(x)")
# Add vertical lines at the transition points for better visualization
abline(v = 0, col = "red", lty = 2)
abline(v = 2, col = "red", lty = 2)
