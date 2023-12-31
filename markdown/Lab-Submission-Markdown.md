Business Intelligence Lab Submission Markdown
================
Uzair Farooq
24-11-2023

- [Student Details](#student-details)
- [Step 1: Setup Chunk](#setup-chunk)
- [STEP 2 Loading and pre-processing the
  dataset](#step-2-loading-and-pre-processing-the-dataset)
- [STEP 3. Descriptive and Inferential
  Statistics](#step-3-descriptive-and-inferential-statistics)
- [STEP 4. Basic Visualization](#step-4-basic-visualization)
- [STEP 5. Check for missing values](#step-5-check-for-missing-values)
- [STEP 6. Training the model and visualising the
  rules](#step-6-training-the-model-and-visualising-the-rules)
- [STEP 7. Hyper-Parameter Tuning and Ensembling
  Methods](#step-7-hyper-parameter-tuning-and-ensembling-methods)
- [STEP 8. Saving and reading the
  Model](#step-8-saving-and-reading-the-model)

# Student Details

|                                                   |                                                               |     |     |
|---------------------------------------------------|---------------------------------------------------------------|-----|-----|
| **Student ID Numbers and Names of Group Members** | 134253 - Uzair Farooq                                         |     |     |
| **Course Code**                                   | BBT4206                                                       |     |     |
| **Course Name**                                   | Business Intelligence II                                      |     |     |
| **Program**                                       | Bachelor of Business Information Technology                   |     |     |
| **Semester Duration**                             | 16<sup>th</sup> October 2023 to 28<sup>th</sup> November 2023 |     |     |

# Step 1: Setup Chunk

We start by installing all the required packages

``` r

if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# STEP 1. Install and Load the Required Packages ----
## arules ----
if (require("arules")) {
  require("arules")
} else {
  install.packages("arules", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## arulesViz ----
if (require("arulesViz")) {
  require("arulesViz")
} else {
  install.packages("arulesViz", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## tidyverse ----
if (require("tidyverse")) {
  require("tidyverse")
} else {
  install.packages("tidyverse", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## readxl ----
if (require("readxl")) {
  require("readxl")
} else {
  install.packages("readxl", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## knitr ----
if (require("knitr")) {
  require("knitr")
} else {
  install.packages("knitr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## ggplot2 ----
if (require("ggplot2")) {
  require("ggplot2")
} else {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## lubridate ----
if (require("lubridate")) {
  require("lubridate")
} else {
  install.packages("lubridate", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## plyr ----
if (require("plyr")) {
  require("plyr")
} else {
  install.packages("plyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## dplyr ----
if (require("dplyr")) {
  require("dplyr")
} else {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## naniar ----
if (require("naniar")) {
  require("naniar")
} else {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## RColorBrewer ----
if (require("RColorBrewer")) {
  require("RColorBrewer")
} else {
  install.packages("RColorBrewer", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
if (!require("randomForest")) {
  install.packages("randomForest", dependencies = TRUE)
}
if (!require("caret")) {
  install.packages("caret", dependencies = TRUE)
}
if (!require("mlbench")) {
  install.packages("mlbench", dependencies = TRUE)
}
# install.packages("plotly")
# Install and load required packages
if (!require("plumber")) install.packages("plumber", dependencies = TRUE)
if (!require("arules")) install.packages("arules", dependencies = TRUE)

library(plumber)
library(arules)
library(randomForest)
library(caret)
library(mlbench)
library(arules)
library(arulesViz)
library(tidyverse)
library(readr)
library(ggplot2)
library(knitr)
library(magrittr)
library(dplyr)
library(plyr)
library(writexl)
library(plotly)
```

------------------------------------------------------------------------

**Note:** the following “*KnitR*” options have been set as the defaults
in this markdown:  
`knitr::opts_chunk$set(echo = TRUE, warning = FALSE, eval = TRUE, collapse = FALSE, tidy.opts = list(width.cutoff = 80), tidy = TRUE)`.

More KnitR options are documented here
<https://bookdown.org/yihui/rmarkdown-cookbook/chunk-options.html> and
here <https://yihui.org/knitr/options/>.

``` r
knitr::opts_chunk$set(
    eval = TRUE,
    echo = TRUE,
    warning = FALSE,
    collapse = FALSE,
    tidy = TRUE
)
```

------------------------------------------------------------------------

**Note:** the following “*R Markdown*” options have been set as the
defaults in this markdown:

> output:
>
> github_document:  
> toc: yes  
> toc_depth: 4  
> fig_width: 6  
> fig_height: 4  
> df_print: default
>
> editor_options:  
> chunk_output_type: console

# STEP 2 Loading and pre-processing the dataset

``` r
sales <- read_csv("data/Updated_sales.csv")


View(sales)
#Descriptive statistics
dim(sales)

#Rename Fields/Columns
colnames(sales)[1] <- "Order_ID"
colnames(sales)[3] <- "Quantity_Ordered"
colnames(sales)[4] <- "Price_Each"
colnames(sales)[5] <- "Order_Date"

str(sales)

print(sales)
summary(sales)

# Convert Quantity Ordered to numeric
sales$Quantity_Ordered <- as.numeric(sales$Quantity_Ordered)

# Convert Price Each to numeric
sales$Price_Each <- as.numeric(sales$Price_Each)

# Convert Date to Date type
sales$Order_Date <- as.Date(sales$Order_Date, format = "%m/%d/%Y")  

# Check the updated data types
str(sales)
```

# STEP 3. Descriptive and Inferential Statistics

``` r
# Summary statistics for numeric columns
summary(sales$Quantity_Ordered)
summary(sales$Price_Each)

# Summary statistics for categorical columns
table(sales$Product)

# Descriptive statistics for Date
summary(as.Date(sales$Order_Date))


# Measures of Central Tendency for numeric variables (e.g., Quantity Ordered, Price Each)
mean_quantity <- mean(sales$Quantity_Ordered)
median_quantity <- median(sales$Quantity_Ordered)

mean_price <- mean(sales$Price_Each)
median_price <- median(sales$Price_Each)

# Display the results
cat("Mean Quantity Ordered:", mean_quantity, "\n")
cat("Median Quantity Ordered:", median_quantity, "\n")
cat("Mean Price Each:", mean_price, "\n")
cat("Median Price Each:", median_price, "\n")


# Measures of Distribution for numeric variables (e.g., Quantity Ordered, Price Each)
sd_quantity <- sd(sales$Quantity_Ordered)
range_quantity <- range(sales$Quantity_Ordered)

sd_price <- sd(sales$Price_Each)
range_price <- range(sales$Price_Each)

# Display the results
cat("Standard Deviation Quantity Ordered:", sd_quantity, "\n")
cat("Range Quantity Ordered:", range_quantity, "\n")
cat("Standard Deviation Price Each:", sd_price, "\n")
cat("Range Price Each:", range_price, "\n")


# checking the relationship between Quantity Ordered and Price Each
correlation <- cor(sales$Quantity_Ordered, sales$Price_Each)

# Display the result
cat("Correlation between Quantity Ordered and Price Each:", correlation, "\n")


# Perform ANOVA
anova_result <- aov(Quantity_Ordered ~ Product, data = sales)

# Display the ANOVA results
summary(anova_result)
```

# STEP 4. Basic Visualization

``` r
# Univariate plot for Quantity Ordered
ggplot(sales, aes(x = Quantity_Ordered)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Quantity Ordered", x = "Quantity Ordered", y = "Frequency")

# Univariate plot for Price Each
ggplot(sales, aes(x = Price_Each)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Price Each", x = "Price Each", y = "Frequency")


# Multivariate plot for Quantity Ordered and Price Each
ggplot(sales, aes(x = Quantity_Ordered, y = Price_Each)) +
  geom_point(color = "red", alpha = 0.5) +
  labs(title = "Scatter Plot of Quantity Ordered vs. Price Each", x = "Quantity Ordered", y = "Price Each")


# Multivariate 3D scatter plot for Quantity Ordered, Price Each, and Product
plot_ly(sales, x = ~Quantity_Ordered, y = ~Price_Each, z = ~Product, color = ~Product,
        type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "Quantity Ordered"),
                      yaxis = list(title = "Price Each"),
                      zaxis = list(title = "Product")))

#heatmap(sales[, c("Quantity_Ordered", "Price_Each")],
 #       col = colorRamp2(c(min(sales$Quantity_Ordered, na.rm = TRUE), mean(sales$Quantity_Ordered, na.rm = TRUE), max(sales$Quantity_Ordered, na.rm = TRUE)),
  #                       c("blue", "white", "red")),
   #     show_row_names = FALSE,
    #    show_column_names = TRUE)
```

# STEP 5. Check for missing values

``` r
# Check for missing values using summary
summary(sales)

# Check for missing values using is.na

any_missing <- any(is.na(sales))

if (any_missing) {
  cat("There are  missing values in the dataset.\n")
} else {
  cat("There are no missing values in the dataset.\n")
}

#remove null values
#sales <- sales[complete.cases(sales), ]


# Verify if any null values are left in the dataset
#any_missing <- any(is.na(sales))

#if (any_missing) {
#  cat("There are still missing values in the dataset.\n")
#} else {
#  cat("All rows with null values have been removed.\n")
#}
```

# STEP 6. Training the model and visualising the rules

``` r
#remove unwanted columns for association rules
sales_removed_vars <-
  sales %>% dplyr::select(-Address,-Quantity_Ordered,-Price_Each)

dim(sales_removed_vars)
View(sales_removed_vars)

str(sales_removed_vars)
dim(sales_removed_vars)
head(sales_removed_vars)

#Split Data
transaction_data <-
  plyr::ddply(sales_removed_vars,
              c("Order_ID", "Order_Date"),
              function(df1) {
                paste(df1$Product, collapse = ",")
              }
  )


#Only remain with products
transaction_data <-
  transaction_data %>%
  dplyr::select("items" = V1)


anyNA(transaction_data)

## Save the transactions in CSV format ----
write.csv(transaction_data,
          "data/transactions_basket_format.csv",
          quote = FALSE, row.names = FALSE)

## Read the transactions from the CSV file ----
tr <-
  read.transactions("data/transactions_basket_format.csv",
                    format = "basket",
                    header = TRUE,
                    rm.duplicates = TRUE,
                    sep = ","
  )

print(tr)
summary(tr)

# Create an item frequency plot for the top  items 10
itemFrequencyPlot(tr, topN = 10, type = "absolute",
                  col = brewer.pal(8, "Pastel2"),
                  main = "Absolute Item Frequency Plot",
                  horiz = TRUE,
                  mai = c(1, 1, 1, 1))

# We can set the minimum support and confidence levels for rules to be
# generated.

association_rules <- apriori(tr, 
                             parameter = list(support = 0.001,
                                              confidence = 0.4,
                                              maxlen = 10))




# Print the association rules ----

summary(association_rules)
inspect(association_rules)


### Remove redundant rules ----
# We can remove the redundant rules as follows:
# Number of rules in the association_rules
subset_rules <-
  which(colSums(is.subset(association_rules,
                          association_rules)) > 1)
#getting the length
num_rules <- length(subset_rules)
length(subset_rules)

# Create a sequence of indexes for rules to be removed (1st, 3rd, 5th, etc.) hence remaining with one of each
indexes_to_remove <- seq(1, num_rules, by = 2)

# Remove the rules at the specified indexes
association_rules_no_reps <- association_rules[-indexes_to_remove, ]

# Print summary and inspect the non-redundant rules
summary(association_rules_no_reps)
inspect(association_rules_no_reps)
plot(association_rules_no_reps)



write(association_rules_no_reps,
      file = "data/association_rules_based_on_product_name.csv")

#Find specific rules ----
# Which product(s), if bought, result in a customer purchasing
# "Samsung USB Type-C to Type-C"?
USB_C_Charging_Cable <-  
  apriori(tr, parameter = list(supp = 0.001, conf = 0.05),
          appearance = list(default = "lhs",
                            rhs = "Samsung USB Type-C to Type-C"))
inspect(head(USB_C_Charging_Cable))

# Which product(s) are bought if a customer purchases
# "iPhone,Google Phone"?
iPhone_Google_Phone <- # nolint
  apriori(tr, parameter = list(supp = 0.001, conf = 0.05),
          appearance = list(lhs = c("Google Pixel 8 Pro", "iPhone 14 Pro Max"), # nolint
                            default = "rhs"))
inspect(head(iPhone_Google_Phone))

#Visualize the rules ----
# Filter rules with confidence greater than 0.85 or 85%
rules_to_plot <-
  association_rules_no_reps[quality(association_rules_no_reps)$confidence > 0.1] # nolint

View(association_rules_no_reps)

rules <- as(association_rules_no_reps, "data.frame")

# Save the rules to an Excel file
write_csv(rules, "association_rules.csv")

# Save the data frame to an Excel file using the writexl package
library(writexl)
write_xlsx(rules, "association_rules.csv")


#Plot SubRules.
plot(rules_to_plot)
plot(rules_to_plot, method = "two-key plot")

top_rules_to_plot <- head(rules_to_plot, n =13, by = "confidence")
#install.packages("visNetwork")
plot(top_rules_to_plot, method = "graph",  engine = "htmlwidget")

saveAsGraph(head(rules_to_plot, n = 1000, by = "lift"),
            file = "graph/association_rules.graphml")


# Filter top 20 rules with highest lift
rules_to_plot_by_lift <- head(rules_to_plot, n = 20, by = "lift")
plot(rules_to_plot_by_lift, method = "paracoord")

plot(rules_to_plot_by_lift, method = "grouped")
```

# STEP 7. Hyper-Parameter Tuning and Ensembling Methods

``` r
str(sales)
#hyper parameter tuning
# Assuming 'Product' is the correct dependent variable
dependent_variable <- "Product"

# Set seed
set.seed(7)

# STEP 3. Train the Model with Default Parameters ----
model_default <- arules::apriori(sales, parameter = list(support = 0.1, confidence = 0.5))

# Function to calculate confidence
calculate_confidence <- function(model) {
  confidence <- arules::interestMeasure(model, measure = "confidence")
  return(mean(confidence, na.rm = TRUE))  # Use mean confidence as an example
}

# Calculate confidence for the default model
confidence_default <- calculate_confidence(model_default)
print(confidence_default)

# STEP 4. Manually Iterate Over Different Parameter Combinations ----
parameter_grid <- expand.grid(support = seq(0.05, 0.2, 0.05), confidence = seq(0.4, 0.7, 0.1))

best_model <- NULL
best_confidence <- 0

for (i in 1:nrow(parameter_grid)) {
  params <- parameter_grid[i, ]
  model <- arules::apriori(sales, parameter = params)
  current_confidence <- calculate_confidence(model)
  
  if (current_confidence > best_confidence) {
    best_confidence <- current_confidence
    best_model <- model
  }
}

# Print the best model
print(best_model)


#Ensembling
sales_binary <- arules::transactions(sales)

# Create a list to store models
model_list <- list()

# Model 1
model_list[["Model1"]] <- arules::apriori(sales, parameter = list(support = 0.1, confidence = 0.5))

# Model 2
model_list[["Model2"]] <- arules::apriori(sales, parameter = list(support = 0.15, confidence = 0.6))

# ... Add more models as needed

# Stacked model (a simple list in this case)
stacked_model <- list(models = model_list)

# Print the stacked model
print(stacked_model)
```

# STEP 8. Saving and reading the Model

``` r
#Save the model
saveRDS(best_model, file = "model/best_model.rds")


# Load your association rule model (replace "best_model.rds" with the actual file name)
best_model <- readRDS("model/best_model.rds")


#Load the association rules model
model <- readRDS("model/best_model.rds")
```

**END OF MARKDOWN**
