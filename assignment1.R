# Testing a null hypothesis with Questionable Research Practices
# Program for course 'Research Methods in AI',
# Bachelor DS&AI, Leiden University, The Netherlands
# By David Moerdijk & Joep Nelissen

simulate_false_rejections <- function(num_simulations, n, mean, sd, QRP_committed = NULL) {
  # Create vectors to store results
  p_values <- numeric(num_simulations)
  
  # Loop through simulations
  for (i in 1:num_simulations) {
    # Simulate fake data under the null hypothesis
    simulated_data <- rnorm(n = n, mean = mean, sd = sd)
    
    # Perform t-test (assuming null hypothesis is true)
    t_test_result <- t.test(simulated_data, mu = mean)
    
    # Store p-value
    p_values[i] <- t_test_result$p.value
    
    # committing QRP(s) is handled here
    if (!is.null(QRP_committed)) {
      if (QRP_committed == "round_p_values") {
        # Round down p-values (e.g., p of .056 becomes p ≤ .05)
        # chang all values between 0.05 and 0.058 to 0.049
        p_values[i] <- ifelse(p_values[i] > 0.05 & p_values[i] < 0.058, 0.049, p_values[i])
      } else if (QRP_committed == "sequential_testing") {
        # Implement sequential testing with optional stopping (not implemented here)
      } else if (QRP_committed == "remove_outliers") {
        # Remove outliers with different criteria, depending on the results (not implemented here)
      } else if (QRP_committed == "multiple_dependent_variables") {
        # Use multiple dependent variables and report only those giving desirable results (not implemented here)
      } else if (QRP_committed == "specific_levels_reporting") {
        # Report on specific levels (groups) of a nominal independent variable, depending on the results (not implemented here)
      } else if (QRP_committed == "remove_covariates") {
        # Remove covariates or add them to the model to get a lower p-value for the main independent variable (not implemented here)
      }
      # Add more QRPs here if needed
    }
  }
  
  # Count how many times the null hypothesis is falsely rejected
  false_rejections <- sum(p_values < 0.05)
  
  # Return the proportion of false rejections
  return(false_rejections / num_simulations)
}


simulate_average_false_rejections <- function(num_runs, num_simulations, n, mean, sd, QRP_committed = NULL) {
  args <- list(num_simulations=num_simulations, n=n, mean=mean, sd=sd, QRP_committed=QRP_committed)

  total_proportions <- numeric(num_runs)

  for (i in 1:num_runs) {
    proportion_false_rejections <- do.call(simulate_false_rejections, args)
    total_proportions[i] <- proportion_false_rejections
  }

  # get mean, standard deviation, and 95% confidence interval
  mean_proportion <- mean(total_proportions)
  sd_proportion <- sd(total_proportions)
  ci <- quantile(total_proportions, c(0.025, 0.975))
  
  return(list(mean=mean_proportion, sd=sd_proportion, ci=ci))

}


get_results_df <- function() {
  # initialize empty df to store results
  results_df <- data.frame(QRP = character(), mean = numeric(), CI = numeric(), SD = numeric(), stringsAsFactors = FALSE)


  possible_QRPs <- c("correct_practices", "round_p_values", "sequential_testing", "remove_outliers", "multiple_dependent_variables", "specific_levels_reporting", "remove_covariates")

  for (QRP in possible_QRPs) {
    args = list(num_runs = 10,num_simulations = 30000, n = 100, mean = 65.5, sd = 7.7, QRP_committed = QRP)
    results = do.call(simulate_average_false_rejections, args)
    print(paste("CI: ", results$ci))
    print(paste("SD: ", results$sd))
    print(paste("Mean proportion of false rejections under QRP ", QRP, " : ", results$mean))

    results_df <- rbind(results_df, data.frame(QRP = QRP, mean = results$mean, CI = results$ci[2], SD = results$sd, stringsAsFactors = FALSE))
  }
  return (results_df)
}


# Set to TRUE if you want to load results from file, expected execution time is 2 minutes
load_from_file <- FALSE


if (load_from_file) { 
  results_df <- read.csv("results.csv")
} else {
  results_df <- get_results_df()
  write.csv(results_df, "results.csv")
}

# plot mean and standard deviation for all results in results_df
library(ggplot2) 

ggplot(results_df, aes(x=QRP, y=mean)) + 
geom_bar(position = position_dodge(), stat="identity", colour="black", fill="skyblue") +
geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.2) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # change angle of x-axis QRP strings
ggtitle("Mean proportion of false rejections under different QRP's")