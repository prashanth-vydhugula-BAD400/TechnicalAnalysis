


# =====================================================
# Student Name: Prashanth Vydhugula
# Course Code: BDA400
# Course Name: Data Science Tools and Techniques
# Assignment 3: Prompting R Functions with AI
# Date: November 21, 2026
# =====================================================

# AI Assistance Declaration:
# I used ChatGPT (GPT-6) on November 21, 2026,
# for task design, R function generation, code
# explanations, debugging suggestions, testing
# guidance, and documentation assistance.
#
# Prompts used:
# See the separate AI Prompts Appendix.
#
# Verification methods:
# RStudio, Google Sheets AVERAGE formulas,
# built-in R mean function, edge-case testing,
# input validation, and SUM/COUNT formulas.
#
# All final calculations are done by myself.
# I am responsible for the accuracy and originality
# of this work.

# =====================================================
# Original Task Description
# =====================================================

# Calculate the average of a numeric vector while
# handling missing values.

# =====================================================
# Step 1: Version 1 - Initial AI-generated function
# =====================================================

# Define a function that accepts a numeric vector
calculate_average_v1 <- function(x) {
  
  # Calculate the average of the numeric vector
  result <- mean(x)
  
  # Return the calculated average
  return(result)
}

# Test 1: Normal numeric data
print(calculate_average_v1(c(10, 20, 30, 40, 50)))

# Test 2: Data containing a missing value
print(calculate_average_v1(c(10, 20, NA, 40, 50)))

# Observation:
# The initial function returned 30 for normal data.
# However, it returned NA when a missing value
# was present.

# =====================================================
# Step 2: Version 2 - Improved R function
# =====================================================

# Define the improved function
calculate_average_v2 <- function(x) {
  
  # Validate that the input is numeric
  if (!is.numeric(x)) {
    
    # Display an error for non-numeric input
    stop("Input must be numeric.")
  }
  
  # Check whether the vector is empty or all missing
  if (length(x) == 0 || all(is.na(x))) {
    
    # Return a numeric missing value
    return(NA_real_)
  }
  
  # Calculate the average while ignoring missing values
  result <- mean(x, na.rm = TRUE)
  
  # Return the calculated average
  return(result)
}

# =====================================================
# Step 3: Testing the improved function
# =====================================================

# Test 1: Normal numeric data
print(calculate_average_v2(c(10, 20, 30, 40, 50)))

# Test 2: Data containing missing values
print(calculate_average_v2(c(10, 20, NA, 40, 50)))

# Test 3: Empty vector
print(calculate_average_v2(numeric(0)))

# Test 4: All missing numeric values
print(calculate_average_v2(c(NA_real_, NA_real_)))

# Observation:
# Both normal data and missing-value data returned 30.
# Empty vectors and entirely missing numeric vectors
# returned NA.

# =====================================================
# Step 4: Independent cross-check
# =====================================================

# Verify normal data using the built-in mean function
print(mean(c(10, 20, 30, 40, 50)))

# Verify missing-value data using the built-in function
print(mean(c(10, 20, NA, 40, 50), na.rm = TRUE))

# =====================================================
# Step 5: Invalid input validation
# =====================================================

# Test character input
# try() catches the intentional error and allows
# the script to continue running.

try(calculate_average_v2(c("A", "B", "C")))

# Observation:
# The function displayed the expected error message:
# Input must be numeric.

# =====================================================
# Step 6: Google Sheets Verification
# =====================================================

# Primary calculation formulas:
# =AVERAGE(A2:A6)
# =AVERAGE(B2:B6)

# Self spot-check formulas:
# =SUM(A2:A6)/COUNT(A2:A6)
# =SUM(B2:B6)/COUNT(B2:B6)

# Observation:
# The Google Sheets results matched the RStudio results.
# Screenshots are included in the reflection document.

# =====================================================
# End of Assignment
# =====================================================
