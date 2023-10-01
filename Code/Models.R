### Splitting Data ###

options(repos = c(
  fawda123 = 'https://fawda123.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

install.packages('caTools')
install.packages('lmtest')
install.packages('ggord')
library(ggord)
library(caTools)
library(lmtest)
library(MASS)
library(class)

str(salary)

set.seed(1)
split <- sample.split(salary$salary, 0.7)
training <- subset(salary, split == TRUE)
test <- subset(salary, split == FALSE)

### MODEL 1 - LOGISTIC REGRESSION ###

logmodel1 <- glm(data = training, formula = salary ~ age, family = binomial(link = 'logit'))
summary(logmodel1) # We have that age is highly significant with 0.0414 factor
contrasts(salary$salary)
coefficients <- unname(logmodel1$coefficients)
exp(coefficients[2]) # For every year that you are older, your odds of earning above 50k increase by 4.2%
exp(coefficients[1] + coefficients[2]*20)/(1+exp(coefficients[1] + coefficients[2]*20)) # 12.6% chance of earning above 50k at 20 years old
logmodel1_pred <- predict(logmodel1, test, type = 'response')
logmodel1_pred <- ifelse(logmodel1_pred >0.5, '>50K', '<=50K')
table(logmodel1_pred, test$salary)
mean(logmodel1_pred != test$salary) # We have a 26.3% error rate (we would only have a 24.9% error if we guessed <=50K all the time), but only a 1.8% recall
sum(salary$salary == '<=50K')/nrow(salary)
plot(logmodel1)

logmodel2 <- glm(data = training, formula = salary ~., family = binomial(link = 'logit'))
summary(logmodel3)
logmodel3 <- update(logmodel2, ~. - education - workclass)
lrtest(logmodel2, logmodel3) # education and workclass do not make it better

logmodel3_pred <- predict(logmodel3, test, type = 'response')
logmodel3_pred <- ifelse(logmodel3_pred >0.5, '>50K', '<=50K')
table(logmodel3_pred, test$salary)
mean(logmodel3_pred != test$salary)

count <- 1 
sensitivity <- c()
false_positive <- c()
for(threshold in seq(min(0.05), max(0.95), by = 0.01)){
  logmodel3_pred <- predict(logmodel3, test, type = 'response')
  logmodel3_pred <- ifelse(logmodel3_pred >threshold, '>50K', '<=50K')
  model_table <- table(logmodel3_pred, test$salary)
  sensitivity[count] <- model_table[2,2]/colSums(model_table)[2]
  false_positive[count] <- model_table[2,1]/colSums(model_table)[1]
  count <- count +1 
}
plot(append(false_positive, 1), append(sensitivity,1), xlab = 'False Positive Rate', ylab = 'Sensitivity')
lines(append(1, false_positive), append(1, sensitivity))
abline(a = 0, b = 1, col = 'red', lwd  =2)


# Comments
# - We ended up with an 18.6% error rate with our 3rd model, not the greatest but better than chance though
# - We also have a 47.2% recall, so not terrible
# - The ROC curve shows the model isn't the greatest

### MODEL 2 - LINEAR DISCRIMINANT ANALYSIS ###

ldamodel1 <- lda(data = training, salary ~ age)
ldamodel1
plot(ldamodel1)
ldamodel1_pred <- predict(ldamodel1, test)
ldamodel1_pred <- ldamodel1_pred$class
table(ldamodel1_pred, test$salary)
mean(ldamodel1_pred != test$salary) # This is worse than predicting <=50K every time again, with a 26.3% error rate

ldamodel2 <- lda(data = training, salary ~.)
ldamodel2
plot(ldamodel2)
ldamodel2_pred <- predict(ldamodel2, test)
ldamodel2_pred <- ldamodel2_pred$class
table(ldamodel2_pred, test$salary)
mean(ldamodel2_pred != test$salary) # 18.8% error rate, 46.8% recall

a = ggord(ldamodel2, training$salary, ylim = c(-5,5),
          size = 1, repel = TRUE, txt = 4,
          grp_title = 'Salaries')
a + theme_bw()
?ggord

### MODEL 3 - KNN ###

salary_scaled <- salary %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  mutate(across(where(is.numeric), scale)) %>%
  as.data.frame()
salary_scaled <- salary_scaled[,c(1,4,9,11)]

set.seed(1)
split <- sample.split(salary$salary, 0.7)
training <- subset(salary_scaled, split == TRUE)
test <- subset(salary_scaled, split == FALSE)
training_results <- training$salary
test_results <- test$salary
training <- training[,-ncol(training)]
test <- test[,-ncol(test)]

knn_basepred <- knn(training, test, training_results)
table(knn_basepred, test_results)
mean(knn_basepred != test_results)

errors <- c()
count <- 1
for(i in seq(min(1), max(401), by = 10)){
  knn_basepred <- knn(training, test, training_results, k = i)
  table(knn_basepred, test_results)
  errors[count] <- mean(knn_basepred != test_results)
  count <- count +1
}
errors
plot(seq(min(1), max(401), by = 10), errors, xlab = 'K', ylab = 'Error Rate')
lines(seq(min(1), max(401), by = 10), errors)
min(errors) # 0.204




