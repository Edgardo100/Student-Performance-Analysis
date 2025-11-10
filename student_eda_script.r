library(ggplot2) # for visualizations
library(rpart) # for decision tree
library(rpart.plot) # for plotting decision tree

# Load dataset
setwd("~/Documents/Google Drive/UC3M/Programming in R/Assignment 3") # Set working directory to where the CSV file is located
student_data = read.csv("student-mat.csv", sep = ";") # sep=";" for semicolon separator

head(student_data) # Checking the first few rows to see if it loaded correctly
str(student_data) # Checking structure of data

#---------------------------------------------------- PART A

ggplot(student_data, aes(x = reason, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Reason for Choosing School, by Gender",
       x = "Reason",
       y = "Number of Students",
       fill = "Gender") +
  scale_fill_manual(values = c("F" = "#E69F00", "M" = "#56B4E9")) +
  theme_minimal()

ggplot(student_data, aes(x = G3)) +
  geom_histogram(bins = 20, fill = "darkolivegreen", color = "white") +
  labs(title = "Distribution of Final Grades (G3)",
       x = "Final Grade (G3)",
       y = "Frequency (Number of Students)") +
  theme_minimal()

ggplot(student_data, aes(x = factor(studytime), y = G3, fill = factor(studytime))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Final Grades by Weekly Study Time",
       x = "Study Time (1: <2hrs, 2: 2-5hrs, 3: 5-10hrs, 4: >10hrs)",
       y = "Final Grade") +
  theme_minimal()

ggplot(student_data, aes(x = absences, y = G3)) +
  geom_point(color = "purple", alpha = 0.6) + # alpha adds transparency
  labs(title = "Final Grade (G3) vs. Number of Absences",
       x = "Number of Absences",
       y = "Final Grade (G3)") +
  theme_minimal()

# summary() and table() for summarization
print("Summary of Study Time")
table(student_data$studytime)

print("Summary of G1, G2, G3 (First, Second, Thirds Period Grade respectively)")
summary(student_data$G1)
summary(student_data$G2)
summary(student_data$G3)

#---------------------------------------------------- PART B

student_data$pass = ifelse(student_data$G3 >= 10, "Yes", "No") # Create binary variable 'pass'
student_data$pass = as.factor(student_data$pass) # Convert new column to a factor
table(student_data$pass) # Check the new column

# Build the classification tree
tree_model = rpart(pass ~ . - G3, # Exclude G3 from predictors
                   data = student_data,
                   method = "class")

printcp(tree_model) # print the cross-validation error table

rpart.plot(tree_model, 
           main = "Decision Tree for Predicting Passing Grade (G3 >= 10)",
           yesno = 2) # Use yesno = 2 to add "Yes" and "No" labels