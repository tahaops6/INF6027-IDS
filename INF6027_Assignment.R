# =============================
# Data Loading
# =============================


# Set main project directory (adjust this path as needed)
setwd("C:/Users/tahao/OneDrive/Documents/MSc Data Science/Introduction to Data Science(INF6027)/Coursework/Coursework R")

# Function to load all CSV files from a folder into a named list
load_data <- function(folder_path) {
  # Get all CSV file paths in the folder
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Load each CSV file and store it in a named list
  data_list <- lapply(files, read.csv)
  names(data_list) <- sub("\\.csv$", "", basename(files))  # Remove .csv extension for naming
  
  return(data_list)
}

# Load health and technology sector datasets into separate lists
health_data <- load_data("Health")
technology_data <- load_data("Technology")

# Check structure of a specific dataset, e.g., ABBV in health sector
str(health_data$ABBV)

# Check names of datasets loaded in each sector
names(health_data)
names(technology_data)

# Function to detect and handle missing values
clean_data <- function(data) {
  # Check for missing values
  missing_summary <- sapply(data, function(x) sum(is.na(x)))
  print("Summary of missing values:")
  print(missing_summary)
  
  # Decide on handling method based on percentage of missing values
  missing_percent <- missing_summary / nrow(data) * 100
  print("Percentage of missing values:")
  print(missing_percent)
  
  # Example handling:
  # If a column has < 5% missing values, fill with median (numeric) or mode (categorical)
  # If > 5% and < 20%, consider filling or removing based on data context
  # If > 20%, consider removing the column or imputing with a more advanced method
  
  for (col in names(data)) {
    if (missing_percent[col] < 5) {
      # Numeric: Fill with median
      if (is.numeric(data[[col]])) {
        data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
      } 
      # Factor or character: Fill with mode
      else if (is.factor(data[[col]]) || is.character(data[[col]])) {
        mode_value <- as.character(names(sort(table(data[[col]]), decreasing = TRUE)[1]))
        data[[col]][is.na(data[[col]])] <- mode_value
      }
    } else if (missing_percent[col] >= 5 && missing_percent[col] < 20) {
      # Fill based on context, could use mean, median, or KNN imputation (for example)
      data[[col]] <- zoo::na.approx(data[[col]], na.rm = FALSE)  # Linear interpolation for numeric
    } else {
      # High missingness: consider removing column
      data[[col]] <- NULL
    }
  }
  return(data)
}

# Apply the cleaning function to all datasets
health_data <- lapply(health_data, clean_data)
technology_data <- lapply(technology_data, clean_data)








# =============================
# Data Preprocessing
# =============================

calculate_daily_returns <- function(data) {
  data$Daily_Return <- c(NA, diff(log(data$Adjusted.Close)))
  return(data)
}
# Apply to each dataset
health_data <- lapply(health_data, calculate_daily_returns)
technology_data <- lapply(technology_data, calculate_daily_returns)




library(dplyr)

# Function to filter data from 2010 onwards
filter_date_range <- function(data, start_date = "2005-01-01") {
  data <- data %>% filter(as.Date(Date, format = "%d-%m-%Y") >= as.Date(start_date))
  return(data)
}

# Apply the filter function to all datasets
filtered_health_data <- lapply(health_data, filter_date_range)
filtered_technology_data <- lapply(technology_data, filter_date_range)



library(lubridate)

# Function to calculate VWAP for each week
aggregate_to_weekly_with_vwap <- function(data) {
  data <- data %>%
    mutate(Week = floor_date(as.Date(Date, format = "%d-%m-%Y"), "week")) %>%
    group_by(Week) %>%
    summarize(
      Weekly_Volume = sum(Volume, na.rm = TRUE),
      Weekly_High = max(High, na.rm = TRUE),
      Weekly_Low = min(Low, na.rm = TRUE),
      Weekly_Close = last(Adjusted.Close),
      Weekly_Return = sum(Daily_Return, na.rm = TRUE),
      Weekly_Volatility = sd(Daily_Return, na.rm = TRUE),
      VWAP = sum(Volume * Adjusted.Close, na.rm = TRUE) / sum(Volume, na.rm = TRUE)
    )
  return(data)
}

# Apply to filtered datasets
health_data_weekly <- lapply(filtered_health_data, aggregate_to_weekly_with_vwap)
technology_data_weekly <- lapply(filtered_technology_data, aggregate_to_weekly_with_vwap)






# =============================
# Feature Engineering
# =============================

# Function to add lagged variables
add_lagged_variables <- function(data, lag = 1) {
  data <- data %>%
    mutate(
      Lagged_Weekly_Return = lag(Weekly_Return, n = lag),
      Lagged_Weekly_Volatility = lag(Weekly_Volatility, n = lag)
    )
  return(data)
}

# Apply the lag function to weekly aggregated datasets
health_data_weekly <- lapply(health_data_weekly, add_lagged_variables)
technology_data_weekly <- lapply(technology_data_weekly, add_lagged_variables)






library(zoo)

# Function to calculate moving averages for 4-week and 8-week windows, avoiding integer overflow
add_moving_averages <- function(data) {
  data <- data %>%
    mutate(
      MA4_Weekly_Volume = rollmean(as.numeric(Weekly_Volume), k = 4, fill = NA, align = "right"),
      MA8_Weekly_Volume = rollmean(as.numeric(Weekly_Volume), k = 8, fill = NA, align = "right"),
      MA4_Weekly_Close = rollmean(Weekly_Close, k = 4, fill = NA, align = "right"),
      MA8_Weekly_Close = rollmean(Weekly_Close, k = 8, fill = NA, align = "right")
    )
  return(data)
}

# Apply moving averages to each dataset
health_data_weekly <- lapply(health_data_weekly, add_moving_averages)
technology_data_weekly <- lapply(technology_data_weekly, add_moving_averages)









# Function to compute weekly price change percentages
add_weekly_price_change <- function(data) {
  data <- data %>%
    mutate(
      Weekly_Price_Change = (Weekly_Close - lag(Weekly_Close)) / lag(Weekly_Close) * 100
    )
  return(data)
}

# Apply price change calculation to each dataset
health_data_weekly <- lapply(health_data_weekly, add_weekly_price_change)
technology_data_weekly <- lapply(technology_data_weekly, add_weekly_price_change)





add_rsi <- function(data, n = 14) {
  data <- data %>%
    mutate(
      Change = Weekly_Close - lag(Weekly_Close),
      Gain = ifelse(Change > 0, Change, 0),
      Loss = ifelse(Change < 0, abs(Change), 0),
      Avg_Gain = rollmean(Gain, k = n, fill = NA, align = "right"),
      Avg_Loss = rollmean(Loss, k = n, fill = NA, align = "right"),
      RS = Avg_Gain / Avg_Loss,
      RSI = 100 - (100 / (1 + RS))
    )
  return(data)
}
health_data_weekly <- lapply(health_data_weekly, add_rsi)
technology_data_weekly <- lapply(technology_data_weekly, add_rsi)





add_macd <- function(data, short = 12, long = 26, signal = 9) {
  data <- data %>%
    mutate(
      EMA_Short = zoo::rollmean(Weekly_Close, k = short, fill = NA, align = "right"),
      EMA_Long = zoo::rollmean(Weekly_Close, k = long, fill = NA, align = "right"),
      MACD = EMA_Short - EMA_Long,
      Signal_Line = zoo::rollmean(MACD, k = signal, fill = NA, align = "right")
    )
  return(data)
}
health_data_weekly <- lapply(health_data_weekly, add_macd)
technology_data_weekly <- lapply(technology_data_weekly, add_macd)





add_atr <- function(data, n = 14) {
  data <- data %>%
    mutate(
      TR = pmax(Weekly_High - Weekly_Low, abs(Weekly_High - lag(Weekly_Close)), abs(Weekly_Low - lag(Weekly_Close))),
      ATR = rollmean(TR, k = n, fill = NA, align = "right")
    )
  return(data)
}
health_data_weekly <- lapply(health_data_weekly, add_atr)
technology_data_weekly <- lapply(technology_data_weekly, add_atr)




add_bollinger_bands <- function(data, n = 20) {
  data <- data %>%
    mutate(
      MA_Close = rollmean(Weekly_Close, k = n, fill = NA, align = "right"),
      Std_Dev = rollapply(Weekly_Close, width = n, FUN = sd, fill = NA, align = "right"),
      Upper_Band = MA_Close + (2 * Std_Dev),
      Lower_Band = MA_Close - (2 * Std_Dev)
    )
  return(data)
}
health_data_weekly <- lapply(health_data_weekly, add_bollinger_bands)
technology_data_weekly <- lapply(technology_data_weekly, add_bollinger_bands)









library(ggplot2)
ggplot(health_data_weekly[[1]], aes(x = Week, y = Weekly_Close)) +
  geom_line() +
  ggtitle("Weekly Close Prices in Health Sector") +
  theme_minimal()



# library(dplyr)
# # Aggregate function to combine sector data
# aggregate_sector_data <- function(data_list) {
#   aggregated_data <- do.call(rbind, data_list) %>%
#     group_by(Week) %>%
#     summarize(
#       Sector_Weekly_Volume = sum(Weekly_Volume, na.rm = TRUE),
#       Sector_Weekly_High = max(Weekly_High, na.rm = TRUE),
#       Sector_Weekly_Low = min(Weekly_Low, na.rm = TRUE),
#       Sector_Weekly_Close = mean(Weekly_Close, na.rm = TRUE),
#       Sector_Weekly_Return = mean(Weekly_Return, na.rm = TRUE),
#       Sector_Weekly_Volatility = mean(Weekly_Volatility, na.rm = TRUE),
#       Sector_VWAP = sum(Weekly_Volume * Weekly_Close, na.rm = TRUE) /
#         sum(Weekly_Volume, na.rm = TRUE)
#     )
#   return(aggregated_data)
# }
# 
# # Apply aggregation to health and technology data
# aggregated_health_data <- aggregate_sector_data(health_data_weekly)
# aggregated_technology_data <- aggregate_sector_data(technology_data_weekly)



# =======================================
# Data Aggregation & Feature Engineering
# =======================================

library(dplyr)

# Enhanced aggregation function
aggregate_sector_data <- function(data_list, weights = NULL) {
  
  # Helper function to detect and remove outliers using IQR
  remove_outliers <- function(data, column) {
    Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    data <- data %>% filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
    return(data)
  }
  
  # Bind all individual datasets into one
  combined_data <- do.call(rbind, data_list)
  
  # Handle outliers in Weekly_High and Weekly_Low
  combined_data <- remove_outliers(combined_data, "Weekly_High")
  combined_data <- remove_outliers(combined_data, "Weekly_Low")
  
  # Aggregate metrics
  aggregated_data <- combined_data %>%
    group_by(Week) %>%
    summarize(
      Sector_Weekly_Volume = sum(Weekly_Volume, na.rm = TRUE),
      Sector_Weekly_High = max(Weekly_High, na.rm = TRUE),
      Sector_Weekly_Low = min(Weekly_Low, na.rm = TRUE),
      
      # Weighted averages
      Sector_Weekly_Close = if (!is.null(weights)) {
        sum(Weekly_Close * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
      } else {
        mean(Weekly_Close, na.rm = TRUE)
      },
      
      Sector_Weekly_Return = if (!is.null(weights)) {
        sum(Weekly_Return * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
      } else {
        mean(Weekly_Return, na.rm = TRUE)
      },
      
      Sector_Weekly_Volatility = mean(Weekly_Volatility, na.rm = TRUE),
      
      # Volume-weighted average price (VWAP)
      Sector_VWAP = sum(Weekly_Volume * Weekly_Close, na.rm = TRUE) / sum(Weekly_Volume, na.rm = TRUE)
    )
  
  return(aggregated_data)
}

# Apply the enhanced aggregation function to health and technology data
aggregated_health_data <- aggregate_sector_data(health_data_weekly)
aggregated_technology_data <- aggregate_sector_data(technology_data_weekly)














library(zoo)

add_lagged_variables <- function(data, lag = 1) {
  data <- data %>%
    mutate(
      Lagged_Sector_Weekly_Return = lag(Sector_Weekly_Return, n = lag),
      Lagged_Sector_Weekly_Volatility = lag(Sector_Weekly_Volatility, n = lag)
    )
  return(data)
}

# Apply lagged variables
aggregated_health_data <- add_lagged_variables(aggregated_health_data)
aggregated_technology_data <- add_lagged_variables(aggregated_technology_data)





add_moving_averages <- function(data) {
  data <- data %>%
    mutate(
      MA4_Sector_Weekly_Volume = rollmean(Sector_Weekly_Volume, k = 4, fill = NA, align = "right"),
      MA8_Sector_Weekly_Volume = rollmean(Sector_Weekly_Volume, k = 8, fill = NA, align = "right"),
      MA4_Sector_Weekly_Close = rollmean(Sector_Weekly_Close, k = 4, fill = NA, align = "right"),
      MA8_Sector_Weekly_Close = rollmean(Sector_Weekly_Close, k = 8, fill = NA, align = "right")
    )
  return(data)
}

# Apply moving averages
aggregated_health_data <- add_moving_averages(aggregated_health_data)
aggregated_technology_data <- add_moving_averages(aggregated_technology_data)




add_weekly_price_change <- function(data) {
  data <- data %>%
    mutate(
      Sector_Weekly_Price_Change = (Sector_Weekly_Close - lag(Sector_Weekly_Close)) / lag(Sector_Weekly_Close) * 100
    )
  return(data)
}

# Apply weekly price change calculation
aggregated_health_data <- add_weekly_price_change(aggregated_health_data)
aggregated_technology_data <- add_weekly_price_change(aggregated_technology_data)




add_rsi <- function(data, n = 14) {
  data <- data %>%
    mutate(
      Change = Sector_Weekly_Close - lag(Sector_Weekly_Close),
      Gain = ifelse(Change > 0, Change, 0),
      Loss = ifelse(Change < 0, abs(Change), 0),
      Avg_Gain = rollmean(Gain, k = n, fill = NA, align = "right"),
      Avg_Loss = rollmean(Loss, k = n, fill = NA, align = "right"),
      RS = Avg_Gain / Avg_Loss,
      RSI = 100 - (100 / (1 + RS))
    )
  return(data)
}

# Apply RSI
aggregated_health_data <- add_rsi(aggregated_health_data)
aggregated_technology_data <- add_rsi(aggregated_technology_data)





add_macd <- function(data, short = 12, long = 26, signal = 9) {
  data <- data %>%
    mutate(
      EMA_Short = rollmean(Sector_Weekly_Close, k = short, fill = NA, align = "right"),
      EMA_Long = rollmean(Sector_Weekly_Close, k = long, fill = NA, align = "right"),
      MACD = EMA_Short - EMA_Long,
      Signal_Line = rollmean(MACD, k = signal, fill = NA, align = "right")
    )
  return(data)
}

# Apply MACD
aggregated_health_data <- add_macd(aggregated_health_data)
aggregated_technology_data <- add_macd(aggregated_technology_data)



add_atr <- function(data, n = 14) {
  data <- data %>%
    mutate(
      TR = pmax(Sector_Weekly_High - Sector_Weekly_Low, 
                abs(Sector_Weekly_High - lag(Sector_Weekly_Close)), 
                abs(Sector_Weekly_Low - lag(Sector_Weekly_Close))),
      ATR = rollmean(TR, k = n, fill = NA, align = "right")
    )
  return(data)
}

# Apply ATR
aggregated_health_data <- add_atr(aggregated_health_data)
aggregated_technology_data <- add_atr(aggregated_technology_data)




add_bollinger_bands <- function(data, n = 20) {
  data <- data %>%
    mutate(
      MA_Close = rollmean(Sector_Weekly_Close, k = n, fill = NA, align = "right"),
      Std_Dev = rollapply(Sector_Weekly_Close, width = n, FUN = sd, fill = NA, align = "right"),
      Upper_Band = MA_Close + (2 * Std_Dev),
      Lower_Band = MA_Close - (2 * Std_Dev)
    )
  return(data)
}

# Apply Bollinger Bands
aggregated_health_data <- add_bollinger_bands(aggregated_health_data)
aggregated_technology_data <- add_bollinger_bands(aggregated_technology_data)







library(ggplot2)
ggplot() +
  geom_line(data = aggregated_health_data, aes(x = Week, y = Sector_Weekly_Close, color = "Health Sector")) +
  geom_line(data = aggregated_technology_data, aes(x = Week, y = Sector_Weekly_Close, color = "Technology Sector")) +
  labs(
    title = "Weekly Close Prices: Health vs Technology",
    x = "Week",
    y = "Average Weekly Close Price",
    color = "Sector"
  ) +
  theme_minimal()





ggplot() +
  geom_line(data = aggregated_health_data, aes(x = Week, y = Sector_Weekly_Volatility, color = "Health Sector")) +
  geom_line(data = aggregated_technology_data, aes(x = Week, y = Sector_Weekly_Volatility, color = "Technology Sector")) +
  labs(
    title = "Weekly Volatility: Health vs Technology",
    x = "Week",
    y = "Average Weekly Volatility",
    color = "Sector"
  ) +
  theme_minimal()






install.packages("plotly")
library(plotly)
bubble_plot <- ggplot() +
  geom_point(
    data = aggregated_health_data,
    aes(x = Sector_Weekly_Volume, y = Sector_VWAP, size = Sector_Weekly_Return, color = "Health Sector"),
    alpha = 0.6
  ) +
  geom_point(
    data = aggregated_technology_data,
    aes(x = Sector_Weekly_Volume, y = Sector_VWAP, size = Sector_Weekly_Return, color = "Technology Sector"),
    alpha = 0.6
  ) +
  labs(
    title = "Volume vs VWAP: Health vs Technology",
    x = "Sector Weekly Volume",
    y = "Sector VWAP",
    color = "Sector",
    size = "Weekly Return"
  ) +
  theme_minimal()

ggplotly(bubble_plot)





# Correlation Analysis for the Health Sector
health_correlation <- cor(aggregated_health_data[, c("Sector_Weekly_Volume", 
                                                     "Sector_Weekly_Price_Change", 
                                                     "Sector_Weekly_Volatility")], 
                          use = "complete.obs")
print("Correlation Matrix for Health Sector:")
print(health_correlation)

# Correlation Analysis for the Technology Sector
technology_correlation <- cor(aggregated_technology_data[, c("Sector_Weekly_Volume", 
                                                             "Sector_Weekly_Price_Change", 
                                                             "Sector_Weekly_Volatility")], 
                              use = "complete.obs")
print("Correlation Matrix for Technology Sector:")
print(technology_correlation)



# Scatterplot for Health Sector
pairs(aggregated_health_data[, c("Sector_Weekly_Volume", 
                                 "Sector_Weekly_Price_Change", 
                                 "Sector_Weekly_Volatility")],
      main = "Scatterplot Matrix: Health Sector",
      pch = 19, col = "blue")

# Scatterplot for Technology Sector
pairs(aggregated_technology_data[, c("Sector_Weekly_Volume", 
                                     "Sector_Weekly_Price_Change", 
                                     "Sector_Weekly_Volatility")],
      main = "Scatterplot Matrix: Technology Sector",
      pch = 19, col = "red")





install.packages("GGally")
library(GGally)




# Pair Plot for Health Sector
ggpairs(aggregated_health_data[, c("Sector_Weekly_Volume", 
                                   "Sector_Weekly_Price_Change", 
                                   "Sector_Weekly_Volatility")],
        title = "Pair Plot: Health Sector")

# Pair Plot for Technology Sector
ggpairs(aggregated_technology_data[, c("Sector_Weekly_Volume", 
                                       "Sector_Weekly_Price_Change", 
                                       "Sector_Weekly_Volatility")],
        title = "Pair Plot: Technology Sector")







# =============================
# Model Development
# =============================


install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)



# Handle missing values by removing rows with missing values
aggregated_health_data <- na.omit(aggregated_health_data)
aggregated_technology_data <- na.omit(aggregated_technology_data)

# Train Random Forest model for the Health Sector
health_rf <- randomForest(Sector_Weekly_Volatility ~ Sector_Weekly_Volume + Sector_Weekly_Price_Change + 
                            Sector_VWAP + Lagged_Sector_Weekly_Return + MA4_Sector_Weekly_Close + 
                            MA8_Sector_Weekly_Close + RSI + MACD, 
                          data = aggregated_health_data, ntree = 500, importance = TRUE)

# Train Random Forest model for the Technology Sector
tech_rf <- randomForest(Sector_Weekly_Volatility ~ Sector_Weekly_Volume + Sector_Weekly_Price_Change + 
                          Sector_VWAP + Lagged_Sector_Weekly_Return + MA4_Sector_Weekly_Close + 
                          MA8_Sector_Weekly_Close + RSI + MACD, 
                        data = aggregated_technology_data, ntree = 500, importance = TRUE)

# Print model summaries
print("Health Sector Random Forest Model:")
print(health_rf)

print("Technology Sector Random Forest Model:")
print(tech_rf)

# Extract and visualize feature importance for the Health Sector
health_importance <- importance(health_rf)
barplot(health_importance[, "IncNodePurity"], 
        main = "Feature Importance: Health Sector", 
        horiz = TRUE, 
        names.arg = rownames(health_importance), 
        las = 1, col = "blue")

# Extract and visualize feature importance for the Technology Sector
tech_importance <- importance(tech_rf)
barplot(tech_importance[, "IncNodePurity"], 
        main = "Feature Importance: Technology Sector", 
        horiz = TRUE, 
        names.arg = rownames(tech_importance), 
        las = 1, col = "red")

# Line Plot: Relationships between Variables for Health Sector
ggplot(aggregated_health_data, aes(x = Week)) +
  geom_line(aes(y = Sector_Weekly_Volatility, color = "Volatility"), size = 1) +
  geom_line(aes(y = Lagged_Sector_Weekly_Return, color = "Lagged Return"), size = 1) +
  geom_line(aes(y = MA4_Sector_Weekly_Close, color = "MA4 Close"), size = 1) +
  geom_line(aes(y = RSI, color = "RSI"), size = 1) +
  scale_color_manual(
    values = c("Volatility" = "purple", "Lagged Return" = "blue", 
               "MA4 Close" = "green", "RSI" = "red"),
    breaks = c("Volatility", "Lagged Return", "MA4 Close", "RSI")
  ) +
  labs(
    title = "Relationships: Indicators vs Volatility (Health Sector)", 
    x = "Week", y = "Value", color = "Variable"
  ) +
  theme_minimal()

# Line Plot: Relationships between Variables for Technology Sector
ggplot(aggregated_technology_data, aes(x = Week)) +
  geom_line(aes(y = Sector_Weekly_Volatility, color = "Volatility"), size = 1) +
  geom_line(aes(y = Lagged_Sector_Weekly_Return, color = "Lagged Return"), size = 1) +
  geom_line(aes(y = MA4_Sector_Weekly_Close, color = "MA4 Close"), size = 1) +
  geom_line(aes(y = RSI, color = "RSI"), size = 1) +
  scale_color_manual(
    values = c("Volatility" = "purple", "Lagged Return" = "blue", 
               "MA4 Close" = "green", "RSI" = "red"),
    breaks = c("Volatility", "Lagged Return", "MA4 Close", "RSI")
  ) +
  labs(
    title = "Relationships: Indicators vs Volatility (Technology Sector)", 
    x = "Week", y = "Value", color = "Variable"
  ) +
  theme_minimal()







# Install and load ggplot2
install.packages("ggplot2")
library(ggplot2)

# Convert feature importance to a data frame for visualization
health_importance_df <- data.frame(
  Feature = rownames(health_importance),
  Importance = health_importance[, "IncNodePurity"]
)

tech_importance_df <- data.frame(
  Feature = rownames(tech_importance),
  Importance = tech_importance[, "IncNodePurity"]
)

# Fancy Feature Importance Bar Plot for Health Sector
ggplot(health_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Feature Importance: Health Sector",
    x = "Features",
    y = "Importance (Node Purity)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed")
  )

# Fancy Feature Importance Bar Plot for Technology Sector
ggplot(tech_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "coral", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Feature Importance: Technology Sector",
    x = "Features",
    y = "Importance (Node Purity)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed")
  )


























library(caret)  # For data splitting and model evaluation
library(dplyr)  # For data manipulation
library(tidyr)  # For reshaping data
library(ggplot2)  # For visualization
library(plotly)  # For interactive plots

# Function to prepare data for regression
prepare_data <- function(data) {
  # Select relevant predictors and target variable
  data <- data %>%
    select(Sector_Weekly_Volume, Sector_Weekly_Price_Change, Sector_VWAP, 
           Lagged_Sector_Weekly_Return, Sector_Weekly_Volatility) %>%
    filter(!is.na(Sector_Weekly_Volatility))  # Ensure no missing target variable
  return(data)
}

# Prepare the health and technology sector data
health_model_data <- prepare_data(aggregated_health_data)
tech_model_data <- prepare_data(aggregated_technology_data)

# Split data into training and testing sets (80-20 split)
set.seed(123)  # Ensure reproducibility
train_index_health <- createDataPartition(health_model_data$Sector_Weekly_Volatility, p = 0.8, list = FALSE)
train_index_tech <- createDataPartition(tech_model_data$Sector_Weekly_Volatility, p = 0.8, list = FALSE)

health_train <- health_model_data[train_index_health, ]
health_test <- health_model_data[-train_index_health, ]
tech_train <- tech_model_data[train_index_tech, ]
tech_test <- tech_model_data[-train_index_tech, ]

# Train regression models
health_model <- lm(Sector_Weekly_Volatility ~ Sector_Weekly_Volume + Sector_Weekly_Price_Change + 
                     Sector_VWAP + Lagged_Sector_Weekly_Return, data = health_train)

tech_model <- lm(Sector_Weekly_Volatility ~ Sector_Weekly_Volume + Sector_Weekly_Price_Change + 
                   Sector_VWAP + Lagged_Sector_Weekly_Return, data = tech_train)

# Display model summaries
cat("\nHealth Sector Model Summary:\n")
summary(health_model)

cat("\nTechnology Sector Model Summary:\n")
summary(tech_model)

# Function to evaluate model performance
evaluate_model <- function(actual, predicted) {
  data.frame(
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted)),
    R2 = cor(actual, predicted)^2
  )
}

# Predictions and evaluations for health sector (training)
health_train_predictions <- predict(health_model, newdata = health_train)
health_train_evaluation <- evaluate_model(health_train$Sector_Weekly_Volatility, health_train_predictions)

# Predictions and evaluations for health sector (testing)
health_test_predictions <- predict(health_model, newdata = health_test)
health_test_evaluation <- evaluate_model(health_test$Sector_Weekly_Volatility, health_test_predictions)

# Predictions and evaluations for technology sector (training)
tech_train_predictions <- predict(tech_model, newdata = tech_train)
tech_train_evaluation <- evaluate_model(tech_train$Sector_Weekly_Volatility, tech_train_predictions)

# Predictions and evaluations for technology sector (testing)
tech_test_predictions <- predict(tech_model, newdata = tech_test)
tech_test_evaluation <- evaluate_model(tech_test$Sector_Weekly_Volatility, tech_test_predictions)

# Calculate accuracy in percentage from R²
health_train_accuracy <- health_train_evaluation$R2 * 100
health_test_accuracy <- health_test_evaluation$R2 * 100

tech_train_accuracy <- tech_train_evaluation$R2 * 100
tech_test_accuracy <- tech_test_evaluation$R2 * 100

# Print evaluation metrics and accuracies
cat("\nHealth Sector Training Evaluation:\n")
print(health_train_evaluation)
cat("Health Sector Training Accuracy: ", round(health_train_accuracy, 2), "%\n")

cat("\nHealth Sector Testing Evaluation:\n")
print(health_test_evaluation)
cat("Health Sector Testing Accuracy: ", round(health_test_accuracy, 2), "%\n")

cat("\nTechnology Sector Training Evaluation:\n")
print(tech_train_evaluation)
cat("Technology Sector Training Accuracy: ", round(tech_train_accuracy, 2), "%\n")

cat("\nTechnology Sector Testing Evaluation:\n")
print(tech_test_evaluation)
cat("Technology Sector Testing Accuracy: ", round(tech_test_accuracy, 2), "%\n")

# Prepare data for health sector visualization
health_test$Predicted_Volatility <- health_test_predictions
health_long <- health_test %>%
  mutate(Index = 1:nrow(health_test)) %>%
  pivot_longer(cols = c(Sector_Weekly_Volatility, Predicted_Volatility),
               names_to = "Type", values_to = "Volatility")

# Prepare data for technology sector visualization
tech_test$Predicted_Volatility <- tech_test_predictions
tech_long <- tech_test %>%
  mutate(Index = 1:nrow(tech_test)) %>%
  pivot_longer(cols = c(Sector_Weekly_Volatility, Predicted_Volatility),
               names_to = "Type", values_to = "Volatility")

# Create line plot for health sector
health_plot <- ggplot(health_long, aes(x = Index, y = Volatility, color = Type)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Health Sector: Actual vs Predicted Volatility",
    x = "Index",
    y = "Volatility",
    color = "Type"
  ) +
  scale_color_manual(
    values = c("Sector_Weekly_Volatility" = "blue", "Predicted_Volatility" = "red"),
    labels = c("Actual Volatility", "Predicted Volatility")
  ) +
  theme_minimal()

# Convert to interactive plot using Plotly
health_plot_interactive <- ggplotly(health_plot)

# Create line plot for technology sector
tech_plot <- ggplot(tech_long, aes(x = Index, y = Volatility, color = Type)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Technology Sector: Actual vs Predicted Volatility",
    x = "Index",
    y = "Volatility",
    color = "Type"
  ) +
  scale_color_manual(
    values = c("Sector_Weekly_Volatility" = "blue", "Predicted_Volatility" = "red"),
    labels = c("Actual Volatility", "Predicted Volatility")
  ) +
  theme_minimal()

# Convert to interactive plot using Plotly
tech_plot_interactive <- ggplotly(tech_plot)

# Display interactive plots
print(health_plot_interactive)
print(tech_plot_interactive)














# Install and load necessary libraries
install.packages("xgboost")
# Install necessary libraries
# Install necessary libraries
install.packages("xgboost")
install.packages("caret")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("plotly")
library(xgboost)
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Function to prepare data for XGBoost
prepare_xgb_data <- function(data) {
  predictors <- data %>%
    select(Sector_Weekly_Volume, Sector_Weekly_Price_Change, Sector_VWAP, Lagged_Sector_Weekly_Return)
  target <- data$Sector_Weekly_Volatility
  predictors_matrix <- as.matrix(predictors)
  return(list(predictors = predictors_matrix, target = target))
}

# Prepare health sector data
health_train_xgb <- prepare_xgb_data(health_train)
health_test_xgb <- prepare_xgb_data(health_test)

# Prepare technology sector data
tech_train_xgb <- prepare_xgb_data(tech_train)
tech_test_xgb <- prepare_xgb_data(tech_test)

# Train XGBoost models
health_xgb <- xgboost(
  data = health_train_xgb$predictors,
  label = health_train_xgb$target,
  nrounds = 100,
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)

tech_xgb <- xgboost(
  data = tech_train_xgb$predictors,
  label = tech_train_xgb$target,
  nrounds = 100,
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)

# Function to evaluate model performance
evaluate_model <- function(actual, predicted) {
  data.frame(
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted)),
    R2 = cor(actual, predicted)^2
  )
}

# Evaluate on training and testing data for health sector
health_train_predictions <- predict(health_xgb, newdata = health_train_xgb$predictors)
health_train_evaluation <- evaluate_model(health_train_xgb$target, health_train_predictions)

health_test_predictions <- predict(health_xgb, newdata = health_test_xgb$predictors)
health_test_evaluation <- evaluate_model(health_test_xgb$target, health_test_predictions)

# Evaluate on training and testing data for technology sector
tech_train_predictions <- predict(tech_xgb, newdata = tech_train_xgb$predictors)
tech_train_evaluation <- evaluate_model(tech_train_xgb$target, tech_train_predictions)

tech_test_predictions <- predict(tech_xgb, newdata = tech_test_xgb$predictors)
tech_test_evaluation <- evaluate_model(tech_test_xgb$target, tech_test_predictions)

# Calculate accuracy in percentage from R²
health_test_accuracy <- health_test_evaluation$R2 * 100
tech_test_accuracy <- tech_test_evaluation$R2 * 100

# Print evaluations with accuracy percentage
cat("\nHealth Sector Training Evaluation:\n")
print(health_train_evaluation)

cat("\nHealth Sector Testing Evaluation:\n")
print(health_test_evaluation)
cat("Health Sector Test Accuracy: ", round(health_test_accuracy, 2), "%\n")

cat("\nTechnology Sector Training Evaluation:\n")
print(tech_train_evaluation)

cat("\nTechnology Sector Testing Evaluation:\n")
print(tech_test_evaluation)
cat("Technology Sector Test Accuracy: ", round(tech_test_accuracy, 2), "%\n")

# Visualization for health sector
health_test$Predicted_Volatility <- health_test_predictions
health_long <- health_test %>%
  mutate(Index = 1:nrow(health_test)) %>%
  pivot_longer(cols = c(Sector_Weekly_Volatility, Predicted_Volatility),
               names_to = "Type", values_to = "Volatility")

health_plot <- ggplot(health_long, aes(x = Index, y = Volatility, color = Type)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Health Sector: Actual vs Predicted Volatility (XGBoost)",
    x = "Index",
    y = "Volatility",
    color = "Type"
  ) +
  scale_color_manual(
    values = c("Sector_Weekly_Volatility" = "blue", "Predicted_Volatility" = "red"),
    labels = c("Actual Volatility", "Predicted Volatility")
  ) +
  theme_minimal()

health_plot_interactive <- ggplotly(health_plot)

# Visualization for technology sector
tech_test$Predicted_Volatility <- tech_test_predictions
tech_long <- tech_test %>%
  mutate(Index = 1:nrow(tech_test)) %>%
  pivot_longer(cols = c(Sector_Weekly_Volatility, Predicted_Volatility),
               names_to = "Type", values_to = "Volatility")

tech_plot <- ggplot(tech_long, aes(x = Index, y = Volatility, color = Type)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Technology Sector: Actual vs Predicted Volatility (XGBoost)",
    x = "Index",
    y = "Volatility",
    color = "Type"
  ) +
  scale_color_manual(
    values = c("Sector_Weekly_Volatility" = "blue", "Predicted_Volatility" = "red"),
    labels = c("Actual Volatility", "Predicted Volatility")
  ) +
  theme_minimal()

tech_plot_interactive <- ggplotly(tech_plot)

# Display plots
print(health_plot_interactive)
print(tech_plot_interactive)


















install.packages("shapper")
library(xgboost)
library(DALEX)
library(randomForest)

# Train Random Forest models
health_rf <- randomForest(Sector_Weekly_Volatility ~ ., data = aggregated_health_data, ntree = 500, importance = TRUE)
tech_rf <- randomForest(Sector_Weekly_Volatility ~ ., data = aggregated_technology_data, ntree = 500, importance = TRUE)

# Extract feature importance
health_rf_importance <- importance(health_rf)
tech_rf_importance <- importance(tech_rf)

# Convert to data frames for visualization
health_rf_importance_df <- data.frame(
  Feature = rownames(health_rf_importance),
  Importance = health_rf_importance[, "IncNodePurity"]
)

tech_rf_importance_df <- data.frame(
  Feature = rownames(tech_rf_importance),
  Importance = tech_rf_importance[, "IncNodePurity"]
)

# Visualize feature importance
library(ggplot2)

# Health Sector Feature Importance Plot
ggplot(health_rf_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Feature Importance: Health Sector (Random Forest)",
    x = "Features",
    y = "Importance (Node Purity)"
  ) +
  theme_minimal()

# Technology Sector Feature Importance Plot
ggplot(tech_rf_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "coral", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Feature Importance: Technology Sector (Random Forest)",
    x = "Features",
    y = "Importance (Node Purity)"
  ) +
  theme_minimal()












install.packages("ggplot2")
install.packages("ggiraph")
library(ggiraph)
library(ggplot2)

# Prepare data
radial_data <- data.frame(
  Feature = rownames(tech_importance),
  Importance = tech_importance[, "IncNodePurity"]
)

# Add angle positions for radial bars
radial_data$Angle <- seq(0, 360, length.out = nrow(radial_data) + 1)[-1]

# Create a radial bar chart
radial_bar_plot <- ggplot(radial_data, aes(x = factor(Feature), y = Importance, fill = Feature)) +
  geom_bar(stat = "identity", width = 1, interactive = TRUE, aes(tooltip = Feature)) +
  coord_polar() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(values = colorRampPalette(c("blue", "purple", "red"))(nrow(radial_data))) +
  labs(
    title = "Radial Bar Chart: Feature Importance",
    subtitle = "Technology Sector",
    caption = "Feature importance from Random Forest Model"
  )

# Make the plot interactive
interactive_plot <- girafe(ggobj = radial_bar_plot)

# Display the interactive plot
print(interactive_plot)





# =============================
# Model Evaluation
# =============================




# =============================
# Data Visualisation
# =============================