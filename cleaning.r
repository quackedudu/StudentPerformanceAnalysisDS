# 1. Read the csv
file_path <- "C:/Users/najmi/OneDrive/Desktop/PERFORMANCE_REPORT/student_performance.csv"
main_data <- read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE)

# 2. Clean the database
# a) Removing rows if critical columns are missing
# This removes StudentID 2592 because Name is missing
main_data <- main_data[!is.na(main_data$StudentID), ]
main_data <- main_data[!is.na(main_data$Name) & main_data$Name != "", ]
main_data <- main_data[!is.na(main_data$Gender) & main_data$Gender != "", ]
main_data <- main_data[!is.na(main_data$ParentalSupport) & main_data$ParentalSupport != "", ]
main_data <- main_data[!is.na(main_data$"Online Classes Taken") & main_data$"Online Classes Taken" != "", ]

# b) Fill empty cells with rounded mean
target_cols <- c("AttendanceRate", "StudyHoursPerWeek", "PreviousGrade", 
                 "ExtracurricularActivities", "FinalGrade", "Study Hours", "Attendance (%)")

for (each_col in target_cols) {
  # Calculate mean and round it
  mean_val <- mean(main_data[[each_col]], na.rm = TRUE)
  round_mean <- round(mean_val)
  
  # Replace NA values
  main_data[[each_col]][is.na(main_data[[each_col]])] <- round_mean
}

# 3. Save the result
write.csv(main_data, "student_performance_cleaned.csv", row.names = FALSE)