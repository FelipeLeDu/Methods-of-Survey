# 01_treatment.R 
# Data treatment and validation

# Dataframe 

df <- readRDS("data/macae.rds")

# Basic verification 

cat("\nDimensions:\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

cat("\nColumn names:\n")
print(names(df))

cat("\nColumn classes:\n")
print(sapply(df, class))

cat("\nFirst rows:\n")
print(head(df, 10))

cat("\nLast rows:\n")
print(tail(df, 10))

# Missing values

na_table <- data.frame(
  column = names(df),
  missing_values = colSums(is.na(df)))

print(na_table, row.names = FALSE)

# Duplicate checks

duplicated_rows <- sum(duplicated(df))

cat("\nNumber of duplicated full rows:", duplicated_rows, "\n")

if (duplicated_rows > 0) {
  warning("Duplicated full rows found in the dataset.")
} else {
  cat("No duplicated full rows found.\n")
}



