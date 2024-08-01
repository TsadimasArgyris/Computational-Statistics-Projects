# Define parameters
n_values <- c(200, 300, 400)
k_values <- c(5, 6, 7, 8)
K <- c(4, 7, 11)
M <- c(0.12, 0.15, 0.18, 0.20)

# Function to simulate the number of packs to complete the collection
simulate_packs1 <- function(n, k) {
  collected <- rep(FALSE, n)
  packs <- 0
  while (sum(collected) < n) {
    stickers <- sample(1:n, k, replace=FALSE)
    collected[stickers] <- TRUE
    packs <- packs + 1
  }
  return(packs)
}

# Initialize matrix for results
results_matrix <- matrix(0, nrow = length(n_values) * length(k_values), ncol = 4)
colnames(results_matrix) <- c("Total # of stickers",
                              "# of stickers per pack",
                              "Expected # of Packages",
                              "Profit")

# Calculate profit for each combination
row_index <- 1
for (i in 1:length(n_values)) {
  for (j in 1:length(k_values)) {
    Y_sum <- 0
    for (sim in 1:1000) {
      Y_sum <- Y_sum + simulate_packs1(n_values[i], k_values[j])
    }
    Y <- as.integer(Y_sum / 1000)
    profit <- M[j] * Y - (K[i] + 0.02 * k_values[j] * Y)
    # Store results in the matrix
    results_matrix[row_index, ] <- c(n_values[i], k_values[j], Y, profit)
    row_index <- row_index + 1
  }
}

results_df <- as.data.frame(results_matrix)
colnames(results_df) <- c("Total # of stickers", "# of stickers per pack", "Expected # of Packages", "Profit")
print(results_df, row.names = FALSE)



# Find the row with the maximum profit
max_profit_row <- results_matrix[which.max(results_matrix[, "Profit"]), ]

# Print the maximum profit and its corresponding n and k values
print(paste("Maximum Profit:", round(max_profit_row["Profit"], 2)))
print(paste("This maximum profit is achieved with", max_profit_row["Total # of stickers"], 
            "stickers and", max_profit_row["# of stickers per pack"], "stickers per pack."))

# Convert the matrix to a data frame for easier manipulation
results_df <- as.data.frame(results_matrix)
colnames(results_df) <- c("TotalStickers", "StickersPerPack", "ExpectedPackages", "Profit")

# Create a label for each n-k combination
results_df$Combination <- paste(results_df$TotalStickers,",", results_df$StickersPerPack, sep = "")

# Create the bar plot
barplot(results_df$Profit, 
        names.arg = results_df$Combination,
        col = "blue", 
        main = "Profit for Each Combination of Total Stickers and Stickers Per Pack", 
        xlab = "Combination of Total Stickers and Stickers Per Pack", 
        ylab = "Profit",
        cex.names = 0.7,
        cex.main = 0.8)


# Calculate probability of completing the collection within 40 euros
# Initialize matrix for probability results
probability_results <- matrix(0, nrow = length(n_values) * length(k_values), ncol = 3)
colnames(probability_results) <- c("Total # of stickers", "# of stickers per pack", "Probability")

# Calculate probabilities for each combination
row_index <- 1
for (i in 1:length(n_values)) {
  for (j in 1:length(k_values)) {
    success <- 0
    for (sim in 1:1000) {
      Y <- simulate_packs1(n_values[i], k_values[j])
      cost <- M[j] * Y
      if (cost <= 40) success <- success + 1
    }
    probability <- success / 1000
    probability_results[row_index, ] <- c(n_values[i], k_values[j], probability)
    row_index <- row_index + 1
  }
}

# Convert to data frame for better printing
probability_results_df <- as.data.frame(probability_results)

# Print the results data frame without row names
print(probability_results_df, row.names = FALSE)
# Find the row with the maximum probability
max_prob_row <- probability_results_df[which.max(probability_results_df$Probability), ]

# Print the maximum probability and its corresponding n and k values
print(paste("Maximum Probability:", max_prob_row$Probability))
print(paste("This maximum probability is achieved with", max_prob_row$`Total # of stickers`, 
            "stickers and", max_prob_row$`# of stickers per pack`, "stickers per pack."))

# Create a combination column for n and k
probability_results_df$Combination <- paste(probability_results_df$`Total # of stickers`, ",", probability_results_df$`# of stickers per pack`, sep = "")

# Create the bar plot for probabilities
barplot(probability_results_df$Probability, 
        names.arg = probability_results_df$Combination,
        col = "blue", 
        main = "Probability of Completing Collection Within 40 Euros for Each Combination", 
        xlab = "Combination of Total Stickers and Stickers Per Pack", 
        ylab = "Probability",
        cex.names = 0.7,
        cex.main = 0.8)




# Function to simulate the number of packs to complete the collection
simulate_packs2 <- function(n, k) {
  collected <- rep(FALSE, n)
  packs <- 0
  while (sum(collected) < n) {
    stickers <- sample(1:n, k, replace=T)
    collected[stickers] <- TRUE
    packs <- packs + 1
  }
  return(packs)
}


# Initialize matrix for results
results_matrix2 <- matrix(0, nrow = length(n_values) * length(k_values), ncol = 4)
colnames(results_matrix2) <- c("Total # of stickers", "# of stickers per pack", "Expected # of Packages", "Profit")

# Calculate profit for each combination using simulate_packs2
row_index <- 1
for (i in 1:length(n_values)) {
  for (j in 1:length(k_values)) {
    Y_sum <- 0
    for (sim in 1:1000) {
      Y_sum <- Y_sum + simulate_packs2(n_values[i], k_values[j])
    }
    Y <- as.integer(Y_sum / 1000)
    profit <- M[j] * Y - (K[i] + 0.02 * k_values[j] * Y)
    # Store results in the matrix
    results_matrix2[row_index, ] <- c(n_values[i], k_values[j], Y, profit)
    row_index <- row_index + 1
  }
}

# Convert the matrix to a data frame
results_df2 <- as.data.frame(results_matrix2)
colnames(results_df2) <- c("TotalStickers", "StickersPerPack", "ExpectedPackages", "Profit")
print(results_df2, row.names = FALSE)

# Find the row with the maximum profit
max_profit_row2 <- results_matrix2[which.max(results_matrix2[, "Profit"]), ]

# Print the maximum profit and its corresponding n and k values
print(paste("Maximum Profit:", round(max_profit_row2["Profit"], 2)))
print(paste("This maximum profit is achieved with", max_profit_row2["Total # of stickers"], 
            "stickers and", max_profit_row2["# of stickers per pack"], "stickers per pack."))

# Create a label for each n-k combination
results_df2$Combination <- paste(results_df2$TotalStickers,",", results_df2$StickersPerPack, sep = "")

# Create the bar plot
barplot(results_df2$Profit, 
        names.arg = results_df2$Combination,
        col = "yellow", 
        main = "Profit for Each Combination of Total Stickers and Stickers Per Pack", 
        xlab = "Combination of Total Stickers and Stickers Per Pack", 
        ylab = "Profit",
        cex.names = 0.7,
        cex.main = 0.8)



# Calculate probability of completing the collection within 40 euros using simulate_packs2
# Initialize matrix for probability results
probability_results2 <- matrix(0, nrow = length(n_values) * length(k_values), ncol = 3)
colnames(probability_results2) <- c("Total # of stickers", "# of stickers per pack", "Probability")

# Calculate probabilities for each combination
row_index <- 1
for (i in 1:length(n_values)) {
  for (j in 1:length(k_values)) {
    success <- 0
    for (sim in 1:1000) {
      Y <- simulate_packs2(n_values[i], k_values[j])
      cost <- M[j] * Y
      if (cost <= 40) success <- success + 1
    }
    probability <- success / 1000
    probability_results2[row_index, ] <- c(n_values[i], k_values[j], probability)
    row_index <- row_index + 1
  }
}

# Convert to data frame for better printing
probability_results_df2 <- as.data.frame(probability_results2)

# Print the results data frame without row names
print(probability_results_df2, row.names = FALSE)

# Find the row with the maximum probability
max_prob_row2 <- probability_results_df2[which.max(probability_results_df2$Probability), ]

# Print the maximum probability and its corresponding n and k values
print(paste("Maximum Probability:", max_prob_row2$Probability))
print(paste("This maximum probability is achieved with", max_prob_row2$`Total # of stickers`, 
            "stickers and", max_prob_row2$`# of stickers per pack`, "stickers per pack."))

# Create a combination column for n and k
probability_results_df2$Combination <- paste(probability_results_df2$`Total # of stickers`, ",", probability_results_df2$`# of stickers per pack`, sep = "")

# Create the bar plot for probabilities
barplot(probability_results_df2$Probability, 
        names.arg = probability_results_df2$Combination,
        col = "yellow", 
        main = "Probability of Completing Collection Within 40 Euros for Each Combination", 
        xlab = "Combination of Total Stickers and Stickers Per Pack", 
        ylab = "Probability",
        cex.names = 0.7,
        cex.main = 0.8)





# Function to calculate the probability of a duplicate existing in a pack
duplicate_card <- function(n, k) {
  collected <- rep(FALSE, n)
  packs <- 0
  duplicate_exists <- 0
  while (sum(collected) < n) {
    stickers <- sample(1:n, k, replace=TRUE)
    if (length(unique(stickers)) < k) {
      duplicate_exists <- duplicate_exists + 1 # Increment if duplicates found
    }
    collected[stickers] <- TRUE
    packs <- packs + 1
  }
  # Return the probability of finding a duplicate
  duplicate_prob <- (duplicate_exists / packs)
  return(duplicate_prob)
}

# Initialize matrix for duplicate probabilities
duplicate_probabilities_matrix <- matrix(0, nrow = length(n_values) * length(k_values), ncol = 3)
colnames(duplicate_probabilities_matrix) <- c("Total # of stickers", "# of stickers per pack", "Probability of Duplicate")

# Calculate probabilities of duplicates for each combination
row_index <- 1
for (i in 1:length(n_values)) {
  for (j in 1:length(k_values)) {
    prob_counter <- 0
    for (sim in 1:1000) {
      D <- duplicate_card(n_values[i], k_values[j])
      prob_counter <- prob_counter + D
    }
    probability_of_duplicate <- prob_counter / 1000
    duplicate_probabilities_matrix[row_index, ] <- c(n_values[i], k_values[j], probability_of_duplicate)
    row_index <- row_index + 1
  }
}

# Convert to data frame for better printing
duplicate_probabilities_df <- as.data.frame(duplicate_probabilities_matrix)

# Print the results data frame without row names
print(duplicate_probabilities_df, row.names = FALSE)

