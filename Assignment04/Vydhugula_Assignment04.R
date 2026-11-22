# =====================================================
# Student Name: Prashanth Vydhugula
# Course Code: BDA400
# Course Name: Data Science Tools and Techniques
# Assignment 4: Sales Data Cleaning Using R
# Date: November 22, 2026
# =====================================================

# AI Assistance Declaration:
# I used ChatGPT (GPT-6) for R code generation,
# explanations, debugging and data cleaning guidance.
# The results are verified using RStudio.

# =====================================================
# Step 1: Import Sales Dataset
# =====================================================

# Import CSV file
sales_data <- read.csv(file.choose())

# Display first six records
print(head(sales_data))

# Display dataset dimensions
print(dim(sales_data))

# Display dataset structure
str(sales_data)

# Display summary statistics
print(summary(sales_data))


# =====================================================
# Step 2: Identify Missing Values and Duplicates
# =====================================================

# Check missing values
print(colSums(is.na(sales_data)))

# Check blank values
print(colSums(sales_data == "", na.rm = TRUE))

# Count duplicate records
print(sum(duplicated(sales_data)))

# Display duplicate records
print(sales_data[duplicated(sales_data), ])


# =====================================================
# Step 3: Remove Duplicate Records
# =====================================================

# Remove exact duplicate records
cleaned_data <- unique(sales_data)

# Display original record count
print(nrow(sales_data))

# Display remaining record count
print(nrow(cleaned_data))

# Verify remaining duplicates
print(sum(duplicated(cleaned_data)))


# =====================================================
# Step 4: Identification of Missing Values
# =====================================================

# Identify missing values in each column
print(colSums(is.na(cleaned_data)))

# Identify blank product names
print(sum(trimws(cleaned_data$Product) == "",
          na.rm = TRUE))

# Display records containing missing values
print(head(
  cleaned_data[!complete.cases(cleaned_data), ]
))


# =====================================================
# Step 5: Handling Missing Values
# =====================================================

# Count records before cleaning
print(nrow(cleaned_data))

# Remove records containing NA
cleaned_data <- na.omit(cleaned_data)

# Count remaining records
print(nrow(cleaned_data))

# Verify remaining missing values
print(colSums(is.na(cleaned_data)))


# =====================================================
# Step 6: Identify Invalid Values
# =====================================================

# Check zero and negative quantities
print(sum(cleaned_data$Quantity <= 0))

# Check zero and negative prices
print(sum(cleaned_data$Price <= 0))

# Display invalid records
print(cleaned_data[
  cleaned_data$Quantity <= 0 |
    cleaned_data$Price <= 0, ])


# =====================================================
# Step 7: Handle Invalid Values
# =====================================================

# Count records before cleaning
before <- nrow(cleaned_data)

# Remove zero and negative quantities and prices
# Assume valid sales require positive values

cleaned_data <- cleaned_data[
  cleaned_data$Quantity > 0 &
    cleaned_data$Price > 0, ]

# Count removed records
print(before - nrow(cleaned_data))

# Verify remaining invalid quantities
print(sum(cleaned_data$Quantity <= 0))

# Verify remaining invalid prices
print(sum(cleaned_data$Price <= 0))

# Display remaining records
print(nrow(cleaned_data))


# =====================================================
# Step 8: Standardize Data
# =====================================================

# Remove unnecessary spaces from product names
cleaned_data$Product <- trimws(cleaned_data$Product)

# Standardize product names
cleaned_data$Product <- tools::toTitleCase(
  tolower(cleaned_data$Product)
)

# Standardize customer names
cleaned_data$Customer <- tools::toTitleCase(
  tolower(trimws(cleaned_data$Customer))
)

# Count blank product names
print(sum(cleaned_data$Product == ""))

# Remove records containing blank product names
cleaned_data <- cleaned_data[
  cleaned_data$Product != "", ]

# Display standardized data
print(head(cleaned_data, 10))

# Display unique product names
print(unique(cleaned_data$Product))

# Display unique customer names
print(unique(cleaned_data$Customer))

# Verify remaining blank product names
print(sum(cleaned_data$Product == ""))

# Display remaining records
print(nrow(cleaned_data))


# =====================================================
# End of Step 8
# =====================================================

# Step 9: Final Data Validation

# Check missing values
print(colSums(is.na(cleaned_data)))

# Check blank values
print(colSums(cleaned_data == "", na.rm = TRUE))

# Check duplicate records
print(sum(duplicated(cleaned_data)))

# Check invalid quantities
print(sum(cleaned_data$Quantity <= 0))

# Check invalid prices
print(sum(cleaned_data$Price <= 0))

# Display final dataset dimensions
print(dim(cleaned_data))

# Display summary statistics
print(summary(cleaned_data))