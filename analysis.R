Titanic=read.csv(file.choose())
titanic=na.omit(Titanic)
attach(titanic)
head(titanic)
tail(titanic)
dim(titanic)
set.seed(25190)
n <- nrow(titanic)
train_titanic <- sample(1:n,size=0.7*n)
training_data_titanic <- titanic[train_titanic, ]
test_data_titanic=titanic[-train_titanic,]
dim(test_data_titanic)

library(ggplot2)
ggplot(training_data_titanic, aes(x = Sex, y = Age, color = factor(Survived)))+
  geom_point(position = position_jitter(width = 0.2), size = 2.5)+
  labs(title = "Age vs. Sex with Survival Status")

ggplot(training_data_titanic, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point(size = 2.5) +labs(title = "Age vs. Fare with Survival Status") 

ggplot(training_data_titanic, aes(x = Sex, y = Fare, color = factor(Survived))) +
  geom_point(position = position_jitter(width = 0.2), size = 2.5) +
  labs(title = "Sex vs. Fare with Survival Status")

ggplot(training_data_titanic, aes(x = Pclass, y = Fare, color = factor(Survived))) +
  geom_point(position = position_jitter(width = 0.2), size = 2.5) +
  labs(title = "Fare vs. Pclass with Survival Status")

ggplot(training_data_titanic, aes(x = SibSp, y = Age, color = factor(Survived))) +
  geom_point(size = 2.5) +labs(title = "SibSp vs. Age with Survival Status")

ggplot(training_data_titanic, aes(x = factor(Survived), y = Age,
                                  fill = factor(Survived)))+geom_boxplot()+
  labs(title = "Box Plot of Age by Survival Status",x = "Survived",y = "Age")

ggplot(training_data_titanic, aes(x = factor(Survived), y = Fare, 
                                  fill = factor(Survived)))+geom_boxplot()+
  labs(title = "Box Plot of Fare by Survival Status",x = "Survived",y = "Fare")

ggplot(training_data_titanic, aes(x = factor(Survived), y = SibSp,
                                  fill = factor(Survived))) +geom_boxplot() +
  labs(title = "Box Plot of SibSp by Survival Status",x = "Survived",
       y = "Number of Siblings/Spouse on board")

install.packages("dplyr")
library(dplyr)
descriptive_statistics <- training_data_titanic %>% 
  group_by(Survived) %>%
  summarize(Mean_Age = mean(Age),Median_Age = median(Age),Std_Age = sd(Age),
            Mean_Fare = mean(Fare),Median_Fare = median(Fare),Std_Fare = sd(Fare),
            Mean_SibSp = mean(SibSp),Median_SibSp = median(SibSp),Std_SibSp = sd(SibSp))
print(descriptive_statistics)

logistic_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare, 
                      data = training_data_titanic, family = binomial())
summary(logistic_model)

#log(p(Survived=1)/p(Survived=0))= B0 + B1*Pclass + B2*Sex + B3*Age + B4*SibSp + B5*Fare
odds_ratios <- exp(coef(logistic_model))
print(odds_ratios)

passenger_1 <- data.frame(Pclass = 3,Sex = factor("female", levels = c("male", "female")),
                          Age = 25,SibSp = 2,Fare = 50)
predict(logistic_model,newdata = passenger_1,type = "response")

passenger_2 <- data.frame(Pclass = 2, Sex = factor("male", levels = c("male", "female")), 
                          Age = 30,SibSp = 2,Fare = 50)
predicted_prob <- predict(logistic_model, newdata = passenger_2, type = "response")
print(predicted_prob)
Survival_Hat <- ifelse(predicted_prob > 0.5, 1, 0)
if (Survival_Hat == 1) {print("survived.")
} else {print("not survived.")}

#dp/dAge = p_hat(1-p_hat).B*Age
B_age <- coef(logistic_model)["Age"]
Marginal_effect_Age <- predicted_prob * (1 - predicted_prob) * B_age
print(Marginal_effect_Age)

passenger_F <- data.frame(Pclass = 2, Sex = factor("female", levels = c("male", "female")),  # Ensure factor levels match the original data
                          Age = 30,SibSp = 2,Fare = 50)
predicted_prob_F <- predict(logistic_model, newdata = passenger_F, type = "response")
Marginal_effect_Gender <-  predicted_prob_F- predicted_prob
print(Marginal_effect_Gender)

#R^2 = 1- log-likelihood of fitted model/log likeilood of null model
log_likelihood_model <- logLik(logistic_model)
null <- glm(Survived ~ 1, data = training_data_titanic, family = binomial)
log_likelihood_null <- logLik(null)
pseudo_Rsquare <- 1 - (as.numeric(log_likelihood_model) / as.numeric(log_likelihood_null))
print(pseudo_Rsquare)

# or we can find directly through library:

install.packages("pscl")
library(pscl)
pseudo_R2 <- pR2(logistic_model)
print(pseudo_R2)

predicted_prob_testdata <- predict(logistic_model, newdata = test_data_titanic,
                                   type = "response")
survival_predict <- ifelse(predicted_prob_testdata > 0.5, 1, 0)
prediction_of_testdata <- test_data_titanic
prediction_of_testdata$predicted_probability_test <- predicted_prob_testdata
prediction_of_testdata$Survival_prediction <- survival_predict
head(prediction_of_testdata)

#Confusion Matrix:
test.survive <- test_data_titanic$Survived
Tab=table (survival_predict , test.survive)
Tab

Accuracy=(Tab[1,1]+Tab[2,2])/sum(Tab) 
Error_rate = 1 -Accuracy
print(Error_rate)

Precision=Tab[2,2]/(Tab[2,1]+Tab[2,2]) 
Recall=Tab[2,2]/(Tab[1,2]+Tab[2,2])     
F1_Score=(2*Recall*Precision)/(Recall+Precision)
print(Precision)
print(Recall)
print(F1_Score)

#LDA
install.packages("MASS")
library(MASS)
lda.fit = lda(Survived ~ Pclass + Sex + Age + SibSp + Fare,
              data = training_data_titanic, family = binomial())
lda.pred = predict(lda.fit, test_data_titanic)$class
print(lda.fit)
length(lda.pred)

Tab_lda=table(lda.pred , test.survive)
Tab_lda
Accuracy_lda=(Tab_lda[1,1]+Tab_lda[2,2])/sum(Tab_lda)
Error_rate_lda = 1 -Accuracy_lda
print(Error_rate_lda)

Precision_lda=Tab_lda[2,2]/(Tab_lda[2,1]+Tab_lda[2,2]) 
Recall_lda=Tab_lda[2,2]/(Tab_lda[1,2]+Tab_lda[2,2])     
F1_Score_lda=(2*Recall_lda*Precision_lda)/(Recall_lda+Precision_lda)
print(Precision_lda)
print(Recall_lda)
print(F1_Score_lda)

#QDA
qda.fit = qda(Survived ~ Pclass + Sex + Age + SibSp + Fare,
              data = training_data_titanic, family = binomial())
qda.pred = predict(qda.fit, test_data_titanic)$class
print(qda.fit)
length(qda.pred)

Tab_qda=table(qda.pred , test.survive)
Tab_qda
Accuracy_qda=(Tab_qda[1,1]+Tab_qda[2,2])/sum(Tab_qda)
Error_rate_qda = 1 -Accuracy_qda
print(Error_rate_qda)

Precision_qda=Tab_qda[2,2]/(Tab_qda[2,1]+Tab_qda[2,2]) 
Recall_qda=Tab_qda[2,2]/(Tab_qda[1,2]+Tab_qda[2,2])     
F1_Score_qda=(2*Recall_qda*Precision_qda)/(Recall_qda+Precision_qda)
print(Precision_qda)
print(Recall_qda)
print(F1_Score_qda)

# Naive Bays
install.packages("e1071")
library(e1071)
modnb=naiveBayes(Survived ~ Pclass + Sex + Age + SibSp + Fare,
                 data = training_data_titanic, family = binomial())
nbclass=predict(modnb,test_data_titanic,type="class")
print(modnb)
length(nbclass)

Tab_nb=table(nbclass,test.survive)
Tab_nb
Accuracy_nb=(Tab_nb[1,1]+Tab_nb[2,2])/sum(Tab_nb)
Error_rate_nb = 1 -Accuracy_nb
print(Error_rate_nb)

Precision_nb=Tab_nb[2,2]/(Tab_nb[2,1]+Tab_nb[2,2]) 
Recall_nb=Tab_nb[2,2]/(Tab_nb[1,2]+Tab_nb[2,2])     
F1_Score_nb=(2*Recall_nb*Precision_nb)/(Recall_nb+Precision_nb)
print(Precision_nb)
print(Recall_nb)
print(F1_Score_nb)

#KNN (k=3)
install.packages("class")
library(class)
train.X=cbind(Pclass, as.numeric(factor(Sex)), Age, SibSp, Fare)[train_titanic, ]
test.X=cbind(Pclass, as.numeric(factor(Sex)), Age, SibSp, Fare)[-train_titanic, ]
train.survive=Survived[train_titanic]
pred.knn.3= knn(train.X, test.X, train.survive, k = 3)
Tab_knn_3=table(pred.knn.3, test.survive)
Tab_knn_3

Accuracy_knn_3=(Tab_knn_3[1,1]+Tab_knn_3[2,2])/sum(Tab_knn_3) 
Error_rate_knn_3 = 1 -Accuracy_knn_3
print(Error_rate_knn_3)

Precision_knn_3=Tab_knn_3[2,2]/(Tab_knn_3[2,1]+Tab_knn_3[2,2]) 
Recall_knn_3=Tab_knn_3[2,2]/(Tab_knn_3[1,2]+Tab_knn_3[2,2])     
F1_Score_knn_3=(2*Recall_knn_3*Precision_knn_3)/(Recall_knn_3+Precision_knn_3)
print(Precision_knn_3)
print(Recall_knn_3)
print(F1_Score_knn_3)

#KNN (k=5)
pred.knn.5= knn(train.X, test.X, train.survive, k = 5)
Tab_knn_5=table(pred.knn.5, test.survive)
Tab_knn_5

Accuracy_knn_5=(Tab_knn_5[1,1]+Tab_knn_5[2,2])/sum(Tab_knn_5) 
Error_rate_knn_5 = 1 -Accuracy_knn_5
print(Error_rate_knn_5)

Precision_knn_5=Tab_knn_5[2,2]/(Tab_knn_5[2,1]+Tab_knn_5[2,2]) 
Recall_knn_5=Tab_knn_5[2,2]/(Tab_knn_5[1,2]+Tab_knn_5[2,2])     
F1_Score_knn_5=(2*Recall_knn_5*Precision_knn_5)/(Recall_knn_5+Precision_knn_5)
print(Precision_knn_5)
print(Recall_knn_5)
print(F1_Score_knn_5)

#logistic model
install.packages("ROCR")
library(ROCR)
prob=predict(logistic_model ,test_data_titanic, type="response")
perf=performance(prediction(prob, test.survive), 'tpr', 'fpr')
plot(perf, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)
#area Under ROC curve
install.packages("Metrics")
library(Metrics)
auc(test.survive, prob) 

#LDA
prob_lda = predict(lda.fit, test_data_titanic)$posterior[,2]  
perf_lda=performance(prediction(prob_lda, test.survive), 'tpr', 'fpr')
plot(perf_lda, main = "ROC curve for LDA", colorize = T)
abline(a = 0, b = 1)
#area Under ROC curve
auc(test.survive, prob_lda)  

#QDA
prob_QDA = predict(qda.fit, test_data_titanic)$posterior[,2]
perf_QDA=performance(prediction(prob_QDA , test.survive), 'tpr', 'fpr')
plot(perf_QDA, main = "ROC curve for QDA", colorize = T)
abline(a = 0, b = 1)
#area Under ROC curve
auc(test.survive, prob_QDA)  

#Naive Bays
prob_Naive=predict(modnb,test_data_titanic,type="raw")[,2] 
perf_Naive=performance(prediction(prob_Naive, test.survive), 'tpr', 'fpr')
plot(perf_Naive, main = "ROC curve for Naive Bays", colorize = T)
abline(a = 0, b = 1)
#area Under ROC curve
auc(test.survive, prob_Naive) 

#KNN (K=3)
prob3= knn(train.X, test.X, train.survive, k = 3, prob=TRUE)
prob_3=attr(prob3, "prob")
perf=performance(prediction(prob_3, test.survive), 'tpr', 'fpr')
plot(perf, main = "ROC curve for KNN(K=3)", colorize = T)
abline(a = 0, b = 1)
#area Under ROC curve
auc(test.survive, prob_3) 

#KNN (k=5)
prob5= knn(train.X, test.X, train.survive, k = 5, prob=TRUE)
prob_5=attr(prob5, "prob")
perf5=performance(prediction(prob_5, test.survive), 'tpr', 'fpr')
plot(perf5, main = "ROC curve for KNN(K=5)", colorize = T)
abline(a = 0, b = 1)
#area Under ROC curve
auc(test.survive, prob_5) 
