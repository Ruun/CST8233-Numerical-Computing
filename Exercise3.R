# Set the random seed
set.seed(75)

# Generate the vectors
Vec1 <- sample(0:999, 100, replace = TRUE)
Vec2 <- sample(0:999, 100, replace = TRUE)

# a. Extract values greater than 600 from Vec2
Vec2a <- Vec2[Vec2 > 600]

# b. Find the index positions of these values in Vec2
Vec2b <- which(Vec2 > 600)

# c. Find the corresponding values in Vec1
Vec1c <- Vec1[Vec2b]

# d. Count how many numbers in Vec1 are divisible by 2
count_divisible_by_2 <- sum(Vec1 %% 2 == 0)

# Print the results
print(paste("Values in Vec2 greater than 600:", toString(Vec2a)))
print(paste("Index positions in Vec2 of these values:", toString(Vec2b)))
print(paste("values in Vec1 which correspond to the values in Vec2
which are greater than 600:", toString(Vec1c)))
print(paste("Number of values in Vec1 divisible by 2:", count_divisible_by_2))

