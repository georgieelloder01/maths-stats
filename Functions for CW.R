

# Function for Q2)

play_mc_full <- function(start_pos = 0, max_turns = 100, slippery_squares = c(9, 18, 27, 36, 45, 54, 63), climbing_squares = c(16, 32, 48, 64), slip_back_range = 1:3, climb_up_amount = 5, max_pos = 70) { # Here all the variables have been defined within the game parameters.
  
  pos <- start_pos
  pos_history <- c(pos)
  turns_taken <- 0
  
  while(pos < max_pos && turns_taken < max_turns) {
    
    
    die <- sample(1:6, 1) # Sample will produce a random integer between 1 & 6 and assign it to the argument die.
    pos <- pos + die  # This adds the value of die to pos.
    turns_taken <- turns_taken + 1 # This increments turns taken by 1.
    if (pos %in% slippery_squares) { # Check if the current position is a slippery square value
      pos <- max(0, pos - sample(slip_back_range, 1)) #If above statement is TRUE, a random integer from predefined slip back range is deducted from pos.
    }
    else if (pos %in% climbing_squares) { # Check if the current position is a climbing square
      pos <- pos + climb_up_amount #If above statement is true, add 5 to pos.
    }
    
    pos_history <- append(pos_history, pos) #Records the positions for every turn.
    
    
  }
  
  #print(paste("You Win! Turns taken:", turns_taken)) # Prints a message showing the total number of turns taken to win
  #print(paste("Positions taken: ", pos_history)) # Print the sequence of positions landed on during the game
  
  return(list(pos_history, turns_taken))
  
  
}



# Q3 function) 

simulate_climbs <- function(n_simulations = 100, max_turns = 100, slippery_squares = c(9, 18, 27, 36, 45, 54, 63), climbing_squares = c(16, 32, 48, 64), slip_back_range = 1:3, climb_up_amount = 5, max_pos = 70) {
  
  turns_vector <- c() # Creates a vector to store the number of turns taken in each simulation
  all_histories <- list() # Creates a vector to store the history of positions from each simulated game
  
  #Loop over simulations
  for (i in 1:n_simulations) { 
    result <- play_mc_full() #Calls play_mc_full and assigns value to result
    all_histories[[i]] <- result[[1]] #Adds history to all_histories list
    turns_vector <- c(turns_vector, result[[2]]) #
  }
  
  turns_mean <- mean(turns_vector) # Calculates the average number of turns taken across all simulations
  turns_median <- median(turns_vector) # Calculates median of turns taken across all simulations
  turns_sd = sd(turns_vector) # Calculates standard deviation of turns taken across all simulations
  
  # Below is just a sanity check
  print(turns_mean)
  print(turns_median)
  print(turns_sd)
  
  turns_stats <- list(mean = turns_mean, median = turns_median, sd = turns_sd)
  
  return(list(turns_vector = turns_vector,
              turns_stats = turns_stats,
              all_histories = all_histories))
}



# Q4 function


simulate_pos_at <- function(n_simulations = 100, max_turns = 100, slippery_squares = c(9, 18, 27, 36, 45, 54, 63), climbing_squares = c(16, 32, 48, 64), slip_back_range = 1:3, climb_up_amount = 5, max_pos = 70, n_turns = 10) {
  
  turns_vector <- c() # Creates a vector to store the number of turns taken in each simulation
  all_histories <- list() # Creates a vector to store the history of positions from each simulated game
  positions_at_n_turns <- c() # Creates a vector to store the position of the player at each turn
  
  #Loop over simulations
  for (i in 1:n_simulations) { 
    result <- play_mc_full() #Calls play_mc_full and assigns value to result
    all_histories[[i]] <- result[[1]] #Adds history to all_histories list
    if (length(all_histories) < n_turns) {
      pos_at_n_turns <- result[[1]][n_turns]
      positions_at_n_turns <- append(positions_at_n_turns, pos_at_n_turns)
    }
    
    turns_vector <- c(turns_vector, result[[2]]) #
  }
  
  turns_mean <- mean(turns_vector) # Calculates the average number of turns taken across all simulations
  turns_median <- median(turns_vector) # Calculates median of turns taken across all simulations
  turns_sd = sd(turns_vector) # Calculates standard deviation of turns taken across all simulations
  turns_variance = turns_sd^2 # Calculates the variance from the standard deviation
  
  # Below is just a sanity check
  print(turns_mean)
  print(turns_median)
  print(turns_sd)
  print(turns_variance)
  
  
  turns_stats <- list(mean = turns_mean, median = turns_median, sd = turns_sd, variance = turns_variance)
  
  return(list(turns_vector = turns_vector,
              turns_stats = turns_stats,
              all_histories = all_histories, 
              positions_at_n_turns = positions_at_n_turns))
}



#Question 6 function: 

update_bmi <- function(data) {
  
  # new column replaces bmi column in dataset with correct calculation for BMI
  data <- data %>%
    mutate(bmi = weight / ((height / 100)^2))
  
  # Ensure weight and height are not NA or zero needed for the BMI calculation
  # also removes any rows where any numeric column contains NAs
  numeric_cols <- sapply(data, is.numeric)
  data <- data %>%
    filter(!is.na(weight) & !is.na(height) & height != 0) %>%
    filter(if_all(where(is.numeric), ~ !is.na(.)))
  
  pca_input <- data[, -1] #Only selects numeric rows for PCA and remove first column of bmi_data for PCA analysis
  pca_input <- pca_input[sapply(pca_input, is.numeric)] 
  
  
  # NA or infinite values removed from dataset
  pca_input <- data.frame(lapply(pca_input, function(x) replace(x, !is.finite(x), NA)))
  pca_input <- na.omit(pca_input)
  #Original rows kept following NA and infinite value removal, so this stores original row numbers to map pca outliers to correct rows for outlier reomoval.
  original_rows <- as.numeric(rownames(pca_input))
  
  # PCA on dataset
  p <- prcomp(scale(pca_input))
  pc_df <- as.data.frame(p$x) # PCA results stored in dataframe pc_df
  
  # IQR calculated for PC1 of dataset
  Q1_PC1 <- quantile(pc_df$PC1, 0.25)
  Q3_PC1 <- quantile(pc_df$PC1, 0.75)
  IQR_PC1 <- Q3_PC1 - Q1_PC1
  lower_PC1 <- Q1_PC1 - 1.5 * IQR_PC1
  upper_PC1 <- Q3_PC1 + 1.5 * IQR_PC1
  
  # IQR calculated for PC2 of dataset
  Q1_PC2 <- quantile(pc_df$PC2, 0.25)
  Q3_PC2 <- quantile(pc_df$PC2, 0.75)
  IQR_PC2 <- Q3_PC2 - Q1_PC2
  lower_PC2 <- Q1_PC2 - 1.5 * IQR_PC2
  upper_PC2 <- Q3_PC2 + 1.5 * IQR_PC2
  
  # Locate outliers in PC1 or PC2 
  outliers <- (pc_df$PC1 < lower_PC1 | pc_df$PC1 > upper_PC1) |
    (pc_df$PC2 < lower_PC2 | pc_df$PC2 > upper_PC2)
  
  # Remove outlier rows from dataset
  outlier_rows <- original_rows[outliers]
  cleaned_data <- data[-outlier_rows, ]
  
  return(cleaned_data)
  
}










