# Calculate the summation
sum_result <- sum((2^(1:25) / (1:25)) + (3^(1:25) / (1:25)^2))

# Print the result in the specified format
print(paste("The sum of this summation is:", format(sum_result, scientific = FALSE, digits = 10)))
