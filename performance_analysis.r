clean_data <- read.csv("student_performance_cleaned.csv", check.names = FALSE)

#categorizing students in pie chart
clean_data$risk_status <- ifelse(clean_data$FinalGrade < 70, "At-Risk", "Stable")
risk_counts <- table(clean_data$risk_status)

#identifies students below 70%
plot(clean_data$PreviousGrade, clean_data$FinalGrade,
     col = ifelse(clean_data$risk_status == "At-Risk", "red", "darkgrey"),
     pch = 16, 
     main = "Intervention Target Map: Quadrant Analysis",
     xlab = "Previous Grade", ylab = "Final Grade",
     xlim = c(60, 95), ylim = c(60, 95))

abline(h = 70, col = "red", lty = 2, lwd = 2)
abline(v = 70, col = "blue", lty = 2, lwd = 2)

text(85, 85, "Consistent", col = "darkgrey", font = 2)
text(85, 65, "Sudden Drop", col = "red", font = 2)
text(65, 85, "Improved", col = "darkgreen", font = 2)
text(65, 65, "Risky", col = "red", font = 2)

#grade by attendance
clean_data$attendance_group <- cut(clean_data$AttendanceRate, breaks = c(0, 75, 85, 100), 
                                   labels = c("Low", "Mid", "High"))
avg_grades <- aggregate(FinalGrade ~ attendance_group, data = clean_data, mean)
barplot(avg_grades$FinalGrade, names.arg = avg_grades$attendance_group,
        main = "Average Grade by Attendance",
        xlab = "Attendance Level", ylab = "Mean Final Grade",
        col = "darkseagreen", ylim = c(0, 100))

#parental support correlation with PREVIOUS marks
clean_data$ParentalSupport <- factor(clean_data$ParentalSupport, 
                                     levels = c("Low", "Medium", "High"))

support_prev_avg <- aggregate(PreviousGrade ~ ParentalSupport, data = clean_data, mean)

plot(support_prev_avg$PreviousGrade, type = "b", 
     main = "Historical Support Context",
     xlab = "Parental Support Level", ylab = "Mean Previous Grade",
     col = "coral", pch = 18, lwd = 3, xaxt = "n", 
     ylim = c(min(support_prev_avg$PreviousGrade) - 5, max(support_prev_avg$PreviousGrade) + 5)) 

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

axis(1, at = 1:3, labels = levels(support_prev_avg$ParentalSupport))

text(1:3, support_prev_avg$PreviousGrade, labels = round(support_prev_avg$PreviousGrade, 1), 
     pos = 3, cex = 0.8, col = "darkred")

#parental support correlation with final grade
clean_data$ParentalSupport <- factor(clean_data$ParentalSupport, 
                                     levels = c("Low", "Medium", "High"))
support_avg <- aggregate(FinalGrade ~ ParentalSupport, data = clean_data, mean)
plot(support_avg$FinalGrade, type = "b", 
     main = "Support as a Risk Offset",
     xlab = "Parental Support Level", ylab = "Mean Final Grade",
     col = "skyblue", pch = 18, lwd = 3, xaxt = "n", 
     ylim = c(min(support_avg$FinalGrade) - 5, max(support_avg$FinalGrade) + 5)) 
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

axis(1, at = 1:3, labels = levels(support_avg$ParentalSupport))

text(1:3, support_avg$FinalGrade, labels = round(support_avg$FinalGrade, 1), pos = 3, cex = 0.8, col = "darkblue")

#percentage of students needing intervention
pie(risk_counts, 
    labels = paste(names(risk_counts), "\n", round(100*risk_counts/sum(risk_counts), 1), "%"),
    main = "Student Risk Distribution",
    col = c("tomato", "lightgrey"))

#MACHINE LEARNING
train_indices <- sample(1:nrow(clean_data), 0.7 * nrow(clean_data))
train_data <- clean_data[train_indices, ]
test_data  <- clean_data[-train_indices, ]

ml_model <- lm(FinalGrade ~ StudyHoursPerWeek + AttendanceRate, data = train_data)

#prediction
predictions <- predict(ml_model, test_data)
actuals_preds <- data.frame(
  Name = test_data$Name, 
  StudentID = test_data$StudentID,
  actual = test_data$FinalGrade, 
  predicted = predictions
)

#calculate error for priority
actuals_preds$error_difference <- abs(actuals_preds$actual - actuals_preds$predicted)

#priorities based on prediction error
#low: < 5, medium: 5-10, high: > 10
actuals_preds$priority <- cut(actuals_preds$error_difference, 
                              breaks = c(-Inf, 5, 10, Inf), 
                              labels = c("Low", "Medium", "High"))

print(head(actuals_preds))
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actual)) / actuals_preds$actual) * 100
print(paste("The error - MAPE is: ", round(mape, digit = 2), "%"))