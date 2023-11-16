
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
install.packages("plotly")

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



# STEP 2. Load and pre-process the dataset ----

# The "read.transactions" function in the "arules" package is used to read
# transaction data from a file and create a "transactions object".

# The transaction data can be specified in either of the following 2 formats:
sales <- read_csv("data/Updated_sales.csv")



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

missing_values <- any(is.na(sales[, c("Quantity_Ordered", "Price_Each")])) ||
  any(!is.numeric(sales[, c("Quantity_Ordered", "Price_Each")]))

# Convert Date to Date type
sales$Order_Date <- as.Date(sales$Order_Date, format = "%m/%d/%Y")  

# Check the updated data types
str(sales)

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
