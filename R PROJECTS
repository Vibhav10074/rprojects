# Student Performance Analysis System in R
# Main analysis functions

# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

# Function to load and prepare student data
load_student_data <- function(file_path) {
  # Read CSV file
  data <- read.csv(file_path)
  
  # Check for missing values
  if(sum(is.na(data)) > 0) {
    print("Warning: Dataset contains missing values")
    # Handle missing values - replace with mean
    for(col in names(data)) {
      if(is.numeric(data[[col]])) {
        data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
      }
    }
  }
  
  return(data)
}

# Function to calculate basic statistics
calculate_stats <- function(data, subjects) {
  stats_list <- list()
  
  for(subject in subjects) {
    if(subject %in% names(data)) {
      subject_stats <- data.frame(
        Subject = subject,
        Mean = mean(data[[subject]], na.rm = TRUE),
        Median = median(data[[subject]], na.rm = TRUE),
        SD = sd(data[[subject]], na.rm = TRUE),
        Min = min(data[[subject]], na.rm = TRUE),
        Max = max(data[[subject]], na.rm = TRUE)
      )
      stats_list[[subject]] <- subject_stats
    }
  }
  
  # Combine all statistics
  all_stats <- do.call(rbind, stats_list)
  return(all_stats)
}

# Function to identify top and bottom performers
identify_performers <- function(data, subject, top_n = 5) {
  if(!(subject %in% names(data))) {
    return(NULL)
  }
  
  # Extract student ID, name and score
  performer_data <- data %>%
    select(StudentID, Name, !!sym(subject)) %>%
    arrange(desc(!!sym(subject)))
  
  # Top performers
  top_performers <- head(performer_data, top_n)
  
  # Bottom performers
  bottom_performers <- tail(performer_data, top_n)
  
  return(list(top = top_performers, bottom = bottom_performers))
}

# Function to analyze pass rates
analyze_pass_rates <- function(data, subjects, pass_mark = 60, distinction_mark = 85) {
  pass_rates <- data.frame(Subject = character(),
                           Pass_Rate = numeric(),
                           Distinction_Rate = numeric(),
                           stringsAsFactors = FALSE)
  
  for(subject in subjects) {
    if(subject %in% names(data)) {
      # Calculate pass and distinction rates
      pass_count <- sum(data[[subject]] >= pass_mark)
      distinction_count <- sum(data[[subject]] >= distinction_mark)
      total_count <- nrow(data)
      
      pass_rate <- (pass_count / total_count) * 100
      distinction_rate <- (distinction_count / total_count) * 100
      
      # Add to results
      pass_rates <- rbind(pass_rates, data.frame(
        Subject = subject,
        Pass_Rate = pass_rate,
        Distinction_Rate = distinction_rate
      ))
    }
  }
  
  return(pass_rates)
}

# Function to calculate correlation between subjects
calculate_correlation <- function(data, subjects) {
  # Extract only the subject columns
  subject_data <- data %>% select(all_of(subjects))
  
  # Calculate correlation matrix
  cor_matrix <- cor(subject_data, use = "pairwise.complete.obs")
  
  return(cor_matrix)
}

# Function to create score distribution visualization
plot_score_distribution <- function(data, subject) {
  if(!(subject %in% names(data))) {
    return(NULL)
  }
  
  # Create histogram
  p <- ggplot(data, aes_string(x = subject)) +
    geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
    labs(title = paste("Score Distribution for", subject),
         x = "Score",
         y = "Number of Students") +
    theme_minimal()
  
  return(p)
}

# Function to create subject comparison visualization
plot_subject_comparison <- function(data, subjects) {
  # Prepare data in long format
  long_data <- data %>%
    select(StudentID, all_of(subjects)) %>%
    pivot_longer(cols = all_of(subjects),
                names_to = "Subject",
                values_to = "Score")
  
  # Create boxplot
  p <- ggplot(long_data, aes(x = Subject, y = Score, fill = Subject)) +
    geom_boxplot() +
    labs(title = "Score Comparison Across Subjects",
         x = "Subject",
         y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Function to create correlation heatmap
plot_correlation_heatmap <- function(cor_matrix) {
  # Create correlation plot
  corrplot(cor_matrix, method = "color", type = "upper",
          order = "hclust", tl.col = "black", tl.srt = 45,
          addCoef.col = "black", number.cex = 0.7)
}

# Main analysis function
analyze_student_performance <- function(file_path) {
  # Load data
  data <- load_student_data(file_path)
  
  # Identify subject columns (assuming they are numeric)
  subject_cols <- names(data)[sapply(data, is.numeric)]
  subject_cols <- subject_cols[!(subject_cols %in% c("StudentID", "Age"))]
  
  # Calculate statistics
  stats <- calculate_stats(data, subject_cols)
  
  # Analyze pass rates
  pass_rates <- analyze_pass_rates(data, subject_cols)
  
  # Calculate correlations
  correlations <- calculate_correlation(data, subject_cols)
  
  # Compile results
  results <- list(
    data = data,
    subjects = subject_cols,
    statistics = stats,
    pass_rates = pass_rates,
    correlations = correlations
  )
  
  return(results)
}
