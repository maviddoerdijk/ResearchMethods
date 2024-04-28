# Testing a null hypothesis with Questionable Research Practices
# Program for course 'Research Methods in AI',
# Bachelor DS&AI, Leiden University, The Netherlands
# By David Moerdijk & Joep Nelissen

simulate_false_rejections <- function(num_simulations, n, mean, sd, QRP_committed = NULL) {
  # Create vectors to store results
  p_values <- numeric(num_simulations)

  # Loop through simulations
  for (i in 1:num_simulations) {

    if(QRP_committed == "sequential_testing_low" || QRP_committed == "sequential_testing_medium" || QRP_committed == "sequential_testing_high"){
        # Implement sequential testing with optional stopping
        # Take test in steps of 2 and check when result is significant
        if(QRP_committed == 'sequential_testing_low'){
            custom_sample_step = n / 10
            custom_sample_size = n / 1.2
        }else if(QRP_committed == 'sequential_testing_medium'){
            custom_sample_step = n / 10
            custom_sample_size = n / 1.5
        }else if(QRP_committed == 'sequential_testing_high'){
            custom_sample_step = n / 10
            custom_sample_size = n / 2
        }

        while( custom_sample_size < n ){
            simulated_data <- rnorm(n = custom_sample_size, mean = mean, sd = sd)
            t_test_result <- t.test(simulated_data, mu = mean)
            if(t_test_result$p.value < 0.05){
                break
            }
            custom_sample_size <- custom_sample_size + custom_sample_step
        }
    } else {

    # Simulate fake data under the null hypothesis
    simulated_data <- rnorm(n = n, mean = mean, sd = sd)

    }

    if (!is.null(QRP_committed)) {
        if (QRP_committed == "remove_outliers_low" || QRP_committed == "remove_outliers_medium" || QRP_committed == "remove_outliers_high"){
            # Remove outliers with different criteria, depending on the results
            # remove values that are more than 3 standard deviations away from the mean
            if(QRP_committed == 'remove_outliers_low'){
                cutoff_max <- 2.5*sd
                cutoff_min <- 2.8*sd
            } else if(QRP_committed == 'remove_outliers_medium'){
                cutoff_max <- 2.5*sd
                cutoff_min <- 3*sd  
            } else if(QRP_committed == 'remove_outliers_high'){
                cutoff_max <- 2.5*sd
                cutoff_min <- 3.2*sd
            }
            simulated_data <- simulated_data[simulated_data < mean + cutoff_max & simulated_data > mean - cutoff_min]
        }
    }
    
    # Perform t-test (assuming null hypothesis is true)
    # this code is performing a one-sample t-test.
    # the t.test function in R performs a t-test, and the mu parameter specifies the true mean under the null hypothesis. 
    # n a one-sample t-test, we compare the mean of a single sample of data to a known value (in this case, the mean parameter).
    t_test_result <- t.test(simulated_data, mu = mean)
    
    # Store p-value
    p_values[i] <- t_test_result$p.value
    
    # committing QRP(s) is handled here
    if (!is.null(QRP_committed)) {
      if (QRP_committed == "round_p_values_low" || QRP_committed == "round_p_values_medium" || QRP_committed == "round_p_values_high") {
        # Round down p-values (e.g., p of .056 becomes p ≤ .05)
        # change all values between 0.05 and 0.058 to 0.049
        if(QRP_committed == 'round_p_values_low'){
            p_values[i] <- ifelse(p_values[i] > 0.05 & p_values[i] < 0.052, 0.049, p_values[i])
        } else if(QRP_committed == 'round_p_values_medium'){
            p_values[i] <- ifelse(p_values[i] > 0.05 & p_values[i] < 0.055, 0.049, p_values[i])
        } else if(QRP_committed == 'round_p_values_high'){
            p_values[i] <- ifelse(p_values[i] > 0.05 & p_values[i] < 0.06, 0.049, p_values[i])
        }
      }
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

  # Set true to make results reproducible
  set_seed <- TRUE
  if (set_seed) {
    set.seed(123)
  }
  results_df <- data.frame(QRP = character(), mean = numeric(), CI = numeric(), SD = numeric(), stringsAsFactors = FALSE)


  #possible_QRPs <- c("correct_practices", "round_p_values", "sequential_testing", "remove_outliers", "multiple_dependent_variables", "specific_levels_reporting", "remove_covariates")
    possible_QRPs <- c("correct_practices",
     "round_p_values_low", "round_p_values_medium", "round_p_values_high",
     "sequential_testing_low", "sequential_testing_medium", "sequential_testing_high",
     "remove_outliers_low", "remove_outliers_medium", "remove_outliers_high")

  for (QRP in possible_QRPs) {
    args = list(num_runs = 10,num_simulations = 30000, n = 10000, mean = 65.5, sd = 7.7, QRP_committed = QRP)
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

p <- ggplot(results_df, aes(x=QRP, y=mean)) + 
geom_bar(position = position_dodge(), stat="identity", colour="black", fill="skyblue") +
geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.2) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # change angle of x-axis QRP strings
ggtitle("Mean proportion of false rejections under different QRP's")

# Save the plot as a PNG image (easier to use in latex than pdf)
ggsave(filename = "plot.png", plot = p, width = 10, height = 10, dpi = 300)