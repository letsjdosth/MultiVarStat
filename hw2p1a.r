# Info: https://hastie.su.domains/ElemStatLearn/datasets/vowel.info.txt
vowel_train = read.table("https://hastie.su.domains/ElemStatLearn/datasets/vowel.train", header=TRUE, sep=",")
vowel_test = read.table("https://hastie.su.domains/ElemStatLearn/datasets/vowel.test", header=TRUE, sep=",")
head(vowel_train)
head(vowel_test)
dim(vowel_test)
vowel_train = vowel_train[,2:12] #delete index
vowel_test = vowel_test[,2:12] #delete index
dim(vowel_test) #462, 11

#LDA
library(MASS)
?lda
lda_fit = lda(y ~ ., data=vowel_train)
plot(lda_fit)
lda_fit
lda_train = predict(lda_fit, vowel_train[,2:11])
lda_test = predict(lda_fit, vowel_test[,2:11])
lda_test$class
dim(lda_test$posterior) #(462, 11), a matrix whose kth column contains the posterior probability that the corresponding observation belongs to the kth class

mean(vowel_train$y == lda_train$class)
mean(vowel_test$y == lda_test$class) #correct classification
table(vowel_test$y, lda_test$class)

#QDA
?qda
qda_fit = qda(y ~ ., data=vowel_train)
qda_fit
qda_train = predict(qda_fit, vowel_train[,2:11])
qda_test = predict(qda_fit, vowel_test[,2:11])
qda_test$class
dim(qda_test$posterior)

mean(vowel_train$y == qda_train$class)
mean(vowel_test$y == qda_test$class) #correct classification
table(vowel_test$y, qda_test$class)

#logistic regression (multinomial)
library(nnet)
multinom_fit <- multinom(y ~ ., data = vowel_train)
summary(multinom_fit)
multinom_train = predict(multinom_fit, newdata = vowel_train[,2:11], "class")
multinom_test = predict(multinom_fit, newdata = vowel_test[,2:11], "class")

mean(vowel_train$y == multinom_train)
mean(vowel_test$y == multinom_test) #correct classification
table(vowel_test$y, multinom_test)
