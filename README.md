# INF6027 - Introduction to Data Science 📊🎨

## Introduction 🔍

Stock price volatility is a key market indicator that reflects economic conditions, market sentiment, and liquidity dynamics. This project investigates the drivers of volatility in the Health and Technology sectors, exploring sector-specific influences such as regulatory decisions and innovation cycles. Using advanced feature engineering and predictive modeling techniques, this research uncovers patterns and evaluates the performance of models such as Random Forest and XGBoost. The findings offer valuable insights for investors and policymakers, enhancing their understanding of sector-specific risks.

---

## Research Questions 🔧

1. **What are the key patterns and relationships between weekly trading volume, price movements, and stock price volatility in the Health and Technology sectors?**
2. **How do features such as lagged values, moving averages, and momentum indicators influence stock price volatility in these sectors?**
3. **How do predictive models perform in forecasting stock price volatility, and what are the sector-specific differences in their predictive accuracy?**

---

## Key Findings 🌍📊

- Higher trading volumes strongly correlate with price volatility, particularly in the Technology sector.
- Features such as VWAP, Bollinger Bands, and RSI are key predictors of volatility.
- XGBoost outperforms Random Forest, achieving higher R² values and lower RMSE across both sectors.
- Technology stocks exhibit greater predictive accuracy due to higher feature variability, while Health stocks require stability-focused features.
- Lagged weekly returns are pivotal in forecasting stock price volatility.

---

## Project Files 📂

This repository contains the following files:
- **INF6027_Assignment.R**: Main R script for data analysis.
- **Health.zip**: Dataset for the Health sector.
- **Technology.zip**: Dataset for the Technology sector.
- **.gitignore**: Specifies files to be ignored by Git.

---

## Instructions for Running the Code 🔄

### Step 1: Clone the Repository 🔐
- Clone or download the repository:
  ```bash
  git clone https://github.com/tahaops6/INF6027-IDS.git
  ```
- Alternatively, download as a ZIP file and extract it.

---

### Step 2: Extract Data Files 📦
- Extract the contents of `Health.zip` and `Technology.zip` into separate folders within the repository:
  - **Health Folder:** Contains Health sector data in CSV format.
  - **Technology Folder:** Contains Technology sector data in CSV format.

---

### Step 3: Install Prerequisites ⚙️
1. Install **R** and **RStudio**:
   - [Download R](https://cran.r-project.org/)
   - [Download RStudio](https://posit.co/downloads/)
2. Install required packages by running:
   ```R
   install.packages(c("dplyr", "ggplot2", "randomForest", "caret", "xgboost", "reshape2", "iml", "zoo", "plotly", "GGally", "lubridate"))
   ```

---

### Step 4: Run the Code 🔄
1. Open the `INF6027_Assignment.R` script in RStudio.
2. Set the working directory to the folder where the repository is located:
   ```R
   setwd("C:/path/to/INF6027-IDS")
   ```
3. Run the entire script by clicking **Source** in RStudio or execute specific sections step-by-step.

---

## Repository Link 🔗
- [GitHub Repository: INF6027-IDS](https://github.com/tahaops6/INF6027-IDS)

---

## Contact 📢
For questions or feedback, feel free to reach out via the **GitHub Issues** section.
