### Splitting Data ###
install.packages('caTools')
install.packages('lmtest')
library(caTools)
library(lmtest)

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
summary(logmodel2)
logmodel3 <- update(logmodel2, ~. - education - workclass)
lrtest(logmodel2, logmodel3) # education and workclass do not make it better

logmodel3_pred <- predict(logmodel3, test, type = 'response')
logmodel3_pred <- ifelse(logmodel3_pred >0.5, '>50K', '<=50K')
table(logmodel3_pred, test$salary)
mean(logmodel3_pred != test$salary) 







