# Drew Whitaker 
# Fundamentals of Data Mining Final
my_dataframe <- read.csv("/Users/14703/Downloads/cybersecurity_intrusion_data.csv")
head(my_dataframe)

#Ensure clean data set
any(duplicated(my_dataframe))
#No duplicates

any(is.na(my_dataframe))
#No null values

library(rpart)
library(rpart.plot)

# Build the decision tree model
class.tree <- rpart(attack_detected ~ ., data = my_dataframe,
                    control = rpart.control(minsplit = 0),
                    method = "class")

# View the results of the tree
print(class.tree)

# Plot the decision tree
rpart.plot(class.tree)

# decision tree uses just under 10,000 objects and only places them in yes or no on attack detected
#Linear regression model

class(data)
str(data)
head(data)

data <- read.csv("/Users/14703/Downloads/cybersecurity_intrusion_data.csv", stringsAsFactors = TRUE)
numeric_data <- data[sapply(data, is.numeric)]
cor(numeric_data)
# only numeric columns are received
pairs(numeric_data)

# network_packet_size login_attempts session_duration
#network_packet_size         1.000000000   -0.001889791      0.021650284
#login_attempts             -0.001889791    1.000000000      0.006392316
#session_duration            0.021650284    0.006392316      1.000000000
#ip_reputation_score         0.002320362   -0.002618222     -0.005076988
#failed_logins              -0.011676379   -0.013507182      0.019374894
#unusual_time_access        -0.001255244    0.007349186      0.012930113
#attack_detected            -0.006797949    0.277320385      0.041601869
#ip_reputation_score failed_logins unusual_time_access
#network_packet_size         0.002320362  -0.011676379        -0.001255244
#login_attempts             -0.002618222  -0.013507182         0.007349186
#session_duration           -0.005076988   0.019374894         0.012930113
#ip_reputation_score         1.000000000   0.015612660        -0.003146119
#failed_logins               0.015612660   1.000000000         0.006131358
#unusual_time_access        -0.003146119   0.006131358         1.000000000
#attack_detected             0.211539983   0.363725533         0.008651520
#attack_detected
#network_packet_size    -0.006797949
#login_attempts          0.277320385
#session_duration        0.041601869
#ip_reputation_score     0.211539983
#failed_logins           0.363725533
#unusual_time_access     0.008651520
#attack_detected         1.000000000

#Scatter Plot matrix also created

model <- glm(attack_detected ~ failed_logins + login_attempts + ip_reputation_score, 
             data = data, family = binomial)
summary(model)
#Failed logins, login attempts, and ip reputation score used a primary predictors

#Coefficients:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         -4.28764    0.10071  -42.57   <2e-16 
#  failed_logins        0.93795    0.02669   35.14   <2e-16 
#  login_attempts       0.38939    0.01353   28.77   <2e-16 
# ip_reputation_score  3.12262    0.14112   22.13   <2e-16 

exp(coef(model))
# odds of increasing attack after each variable
#0.0137 when all predictors are 0
#2.55 for every addition failed login (positive relationship)
#1.48 for each addition login attempt (positive relationship)
#22.71 for each increase in 1 unit of Ip reputation score (very strong relationship)

#convert to %
# Get the exponentiation coefficients (odds ratios)
odds_ratios <- exp(coef(model))
# Adjust ip_reputation_score for a 0.1 increase
odds_ratios["ip_reputation_score"] <- exp(coef(model)["ip_reputation_score"] * 0.1)
#adjust ip rep score to 0.1 increase not 1.0

# Convert odds ratios to percentage changes
percentage_changes <- (odds_ratios - 1) * 100
print(percentage_changes)

#failed logins - 155%
#login attempts - 48%
#ip reputation score - 37%


#confusion matrix
# Convert predicted probabilities into 0/1 classification
predictions <- predict(model, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Create confusion matrix
table(predicted_classes, data$attack_detected)
#Used ChatGPT for Accuracy, precision, and recall formulas

# Define confusion matrix values
TN <- 4500  # True Negatives
FP <- 1100  # False Positives
FN <- 800  # False Negatives
TP <- 3300  # True Positives

# Accuracy: Overall correctness of the model
accuracy <- (TP + TN) / (TP + TN + FP + FN)
print(paste("Accuracy:", round(accuracy, 4)))
# Accuracy - 0.8041

# Precision: How many predicted attacks were correct
precision <- TP / (TP + FP)
print(paste("Precision:", round(precision, 4)))
# Precision - 0.75

# Recall: How many actual attacks were correctly identified
recall <- TP / (TP + FN)
print(paste("Recall:", round(recall, 4)))
#Recall - 0.8049

# F1 Score: Harmonic mean of precision & recall
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1 Score:", round(f1_score, 4)))
#F1 score - 0.7765

#Accuracy ~80% → Model correctly predicts about 80% of cases
#Precision ~75% → When it predicts an attack, it’s right ~75% of the time
#Recall ~80% → The model catches ~80% of actual attacks
#F1 Score ~78% → Balance between precision & recall


# Random Forest Model
install.packages("randomForest") 
library(randomForest)

data$attack_detected <- as.factor(data$attack_detected)

set.seed(42)
rf_model <- randomForest(attack_detected ~ failed_logins + login_attempts + ip_reputation_score,
                         data = data,
                         ntree = 500,
                         mtry = 2,
                         importance = TRUE)

print(rf_model)

# 12.99% OOB error rate, meaning 87.01% accuracy
# 	Predicted 0	Predicted 1	Class Error
# Actual 0	5248	25	0.47% error (very low false positive rate)
# Actual 1	1214	3050	28.5% error (higher false negative rate)

# In simpler terms:
# 5248 True Negatives, correctly predicted no attack
# 25 False Positives, model predicted an attack even though there was none
# 1214 False Negatives, model predicted no attack even though there was one
# 3050 True Positives, model correctly predicted an attack

# Stress danger of False negatives in cyber security in presentation 

# Accuracy - 87.01%
# Precision - 99.18%
# Recall - 71.5%
# F1 - 83.1%

# Adjust threshold to improve recall 
# Predict probabilities (needs a data frame passed to predict)
rf_probs <- predict(rf_model, newdata = data, type = "prob")[,2]

# Lower threshold from 0.5 to 0.3 to catch more attacks (improve recall)
rf_pred_adjusted <- ifelse(rf_probs > 0.3, 1, 0)

# Confusion matrix
conf_matrix_adjusted <- table(Predicted = rf_pred_adjusted, Actual = data$attack_detected)
print("Confusion Matrix with Adjusted Threshold (0.3):")
print(conf_matrix_adjusted)

# Manually calculate metrics
TP <- sum(rf_pred_adjusted == 1 & data$attack_detected == 1)
FP <- sum(rf_pred_adjusted == 1 & data$attack_detected == 0)
TN <- sum(rf_pred_adjusted == 0 & data$attack_detected == 0)
FN <- sum(rf_pred_adjusted == 0 & data$attack_detected == 1)

recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Recall (Adjusted Threshold):", recall, "\n") # 79%
cat("Precision (Adjusted Threshold):", precision, "\n") # 99%
cat("F1 Score (Adjusted Threshold):", f1_score, "\n") # 88%

# Lowered the classification threshold from 0.5 to 0.3 to increase number of predicted positives
# Precision score does drop but by less than 1%, while recall jumps 8% meaing the model catches more legit attacks





