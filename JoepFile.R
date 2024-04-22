# Set the parameters
N <- 10000  # Number of random values
M <- 100   # Mean
S <- 5    # Standard deviation

# Generate random values
Medicine_Users <- rnorm((N / 2), mean = M, sd = S)
Placebo_Users <- rnorm((N / 2), mean = M, sd = S)
All_Users <- c(Medicine_Users, Placebo_Users)

# Create histogram
hist(All_Users, 
     main = "Histogram of Random Values", 
     xlab = "Blood Pressure", 
     ylab = "Frequency")


# Hypothesis
# H0 : Mean_Med = Mean_Plac
# H1 : Mean_Med != Mean_Plac
# t = (x-μ) / (s/√n) 
# t.test(x, y, alternative = "two.sided", var.equal = FALSE)


t_test <- t.test(Medicine_Users,
                 Placebo_Users, alternative = "two.sided",
                 var.equal = FALSE, paired = FALSE)
print(t_test)