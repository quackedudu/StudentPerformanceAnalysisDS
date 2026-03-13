clean_data <- read.csv("student_performance_cleaned.csv", check.names = FALSE)

#categorizing students in pie chart
clean_data$risk_status <- ifelse(clean_data$FinalGrade < 70, "At-Risk", "Stable")
risk_counts <- table(clean_data$risk_status)

#grouping attendance
clean_data$attendance_group <- cut(clean_data$AttendanceRate, breaks = c(0, 75, 85, 100), 
                                   labels = c("Low", "Mid", "High"))
avg_grades <- aggregate(FinalGrade ~ attendance_group, data = clean_data, mean)

par(mfrow = c(2, 2))

#identifies students below the 70% threshold
plot(clean_data$PreviousGrade, clean_data$FinalGrade,
     col = ifelse(clean_data$risk_status == "At-Risk", "red", "darkgrey"),
     pch = 16, main = "Intervention Target Map",
     xlab = "Previous Grade", ylab = "Final Grade")
abline(h = 70, col = "red", lty = 2)

#grade by attendance
barplot(avg_grades$FinalGrade, names.arg = avg_grades$attendance_group,
        main = "Average Grade by Attendance",
        xlab = "Attendance Level", ylab = "Mean Final Grade",
        col = "darkseagreen", ylim = c(0, 100))

#parental support correlation with final grade
boxplot(FinalGrade ~ ParentalSupport, data = clean_data,
        main = "Support as a Risk Offset",
        xlab = "Parental Support Level", ylab = "Final Grade",
        col = "skyblue")

#percentage of students needing intervention
pie(risk_counts, 
    labels = paste(names(risk_counts), "\n", round(100*risk_counts/sum(risk_counts), 1), "%"),
    main = "Student Risk Distribution",
    col = c("tomato", "lightgrey"))