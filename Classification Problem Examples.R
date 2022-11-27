# Classification Examples

# True Positive (TP)
# Both actual and predicted values are Positive.

# True Negative (TN)
# Both actual and predicted values are Negative.

# False Positive (FP)
# The actual value is negative but we predicted it as positive. 

# False Negative (FN)
# The actual value is positive but we predicted it as negative.

# 1. A politician is believed to have a 8 to 2 odds of winning the election. What is the probability of winning for the candidate?
#create matrix

install.packages('caret')

#Import required library
library(caret)

#Creates vectors having data points
expected_value <- factor(c(1,1,1,1,1,1,1,1,0,0))
predicted_value <- factor(c(0,0,0,0,0,0,0,0, 1,1))

#Creating confusion matrix
example <- confusionMatrix(data= predicted_value, reference = expected_value)

#Display results 
example
program <- c('Winner', 'Loser')
outcome <- c('Win', 'Lose')
politics <- matrix(c(0.8, 0.2), nrow=1, ncol=2, byrow=FALSE)

#view matrix
politics

install.packages('epitools')

library(epitools)

#calculate odds ratio
oddsratio(politics)

$measure

#split dataset into training and testing set
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
train <- data[sample, ]
test <- data[!sample, ]

#fit logistic regression model
model <- glm(default~student+balance+income, family="binomial", data=train)



# 2. It is estimated that an appointment with a 10 day lag for a male patient has a predicted probability of 0.1372 of cancelling
# Compare this with the predicted cancellation probability for a female patient who also has an appointment with a 10 day lag.
# Assume that value of gender variable is 1 for male patients and 0 for females. Also, assume that the estimated coefficient for 
# gender is -0.3572 , b0 is -1.6515 , b1 is .01699.

P(Y = 1) = exp(-1.6515 + .01699 * 10)
= 0.2272738

1 + exp(-1.6515 + .01699 * 10)
=  1.227274

# ANSWER = A female is more likely to cancel by 4.8%


# 3. Given the following table, does the shaded quadrant represent true positives, true negatives, false positives, or false negatives?
# Assume cancellation is denoted by 1 and arrival is denoted by 0.

# ANSWER = False positives

# 4. It is believed that we can reverse 60% of cancellations with reminder phone calls. We decided to place reminder calls for all
# appointments that are predicted to cancel. For how many cases can we reverse cancellation (round to the nearest integer) ?
# Assume cancellation is denoted by 1 and arrival is denoted by 0.

# Answer = 90  

# 5. Assume the cost of placing a reminder call is $1 using an automated system and the benefit of serving a patient is $50. What is the profit of placing reminder 
# calls for all appointments predicted to cancel (round to the nearest integer)?

# Answer = 4300 

# 6. Consider the following confusion matrix when answering the next two questions. 
# What is the precision?

Precision (true positives / predicted positives) = TP / TP + FP


18 / ( 18 + 34) = 0.346


# 7. Use the confusion matrix from the previous question. What is the accuracy of the model?
TP + TN / TP + TN + FP + FN

(18 + 54) / (18 + 54 + 34 + 9)

0.626087
