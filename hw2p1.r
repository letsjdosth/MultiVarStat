# Info: https://hastie.su.domains/ElemStatLearn/datasets/vowel.info.txt
vowel_train = read.table("https://hastie.su.domains/ElemStatLearn/datasets/vowel.train", header=TRUE, sep=",")
vowel_test = read.table("https://hastie.su.domains/ElemStatLearn/datasets/vowel.test", header=TRUE, sep=",")
head(vowel_train)
head(vowel_test)
dim(vowel_test)
vowel_train = vowel_train[,2:12] #delete index
vowel_test = vowel_test[,2:12] #delete index
dim(vowel_test) #462, 11

#p1(a)###################################################
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
?table

#logistic regression (multinomial)
library(nnet)
multinom_fit = multinom(y ~ ., data = vowel_train)
summary(multinom_fit)
multinom_train = predict(multinom_fit, newdata = vowel_train[,2:11], "class")
multinom_test = predict(multinom_fit, newdata = vowel_test[,2:11], "class")

mean(vowel_train$y == multinom_train)
mean(vowel_test$y == multinom_test) #correct classification
table(vowel_test$y, multinom_test)


#p1(b)###################################################
library(mclust)

quickfit = function(model_name_code){
    fit = MclustDA(vowel_train[,2:11], vowel_train[,1], modelNames = model_name_code)
    cat(model_name_code, '\n')
    train_err = summary(fit, newdata = vowel_test[,2:11], newclass=vowel_test[,1])$ce
    test_err = summary(fit, newdata = vowel_test[,2:11], newclass=vowel_test[,1])$ce.newdata
    cat("train.err", train_err, '\n')
    cat("test.err", test_err, '\n')
    return(c(train_err, test_err))
}

all_codes = c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", 
            "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV")
all_test_err = rep(0, length(all_codes))
for(i in 1:length(all_codes)){
    err_vec = quickfit(all_codes[i])
    all_test_err[i] = err_vec[2]
}
all_test_err
#  [1] 0.4761905 0.4740260 0.4329004 0.5000000 0.5281385 0.5627706 0.5541126
#  [8] 0.5974026 0.5584416 0.6168831 0.6818182 0.6709957 0.6125541 0.6385281
all_codes[which.min(all_test_err)] #EEI
all_test_err[which.min(all_test_err)] #0.4329004


#p1(c))###################################################
vowel_train_r = vowel_train[(vowel_train$y==6 | vowel_train$y==11),]
vowel_test_r = vowel_test[(vowel_test$y==6 | vowel_test$y==11),]
vowel_train_r$y = ifelse(vowel_train_r$y==6, 0, 1)
vowel_test_r$y = ifelse(vowel_test_r$y==6, 0, 1)

#lda
lda_fit_r = lda(y ~., data=vowel_train_r)
plot(lda_fit_r)
lda_fit_r
lda_train_r = predict(lda_fit_r, vowel_train_r[,2:11])
lda_test_r = predict(lda_fit_r, vowel_test_r[,2:11])
lda_test_r$class
dim(lda_test_r$posterior) #(84, 2), a matrix whose kth column contains the posterior probability that the corresponding observation belongs to the kth class

mean(vowel_train_r$y == lda_train_r$class)
mean(vowel_test_r$y == lda_test_r$class) #correct classification
table(vowel_test_r$y, lda_test_r$class)

#qda
qda_fit_r = qda(y ~ ., data=vowel_train_r)
qda_train_r = predict(qda_fit_r, vowel_train_r[,2:11])
qda_test_r = predict(qda_fit_r, vowel_test_r[,2:11])
qda_test_r$class
dim(qda_test_r$posterior)

mean(vowel_train_r$y == qda_train_r$class)
mean(vowel_test_r$y == qda_test_r$class) #correct classification
table(vowel_test_r$y, qda_test_r$class)

#logistic reg
logistic_fit_r = glm(y ~ ., family = binomial, data = vowel_train_r)
summary(logistic_fit_r)
logistic_train_r_prob = predict(logistic_fit_r, vowel_train_r[,1:11], type="response") #probs for 11
logistic_train_r = ifelse(logistic_train_r_prob>0.5, 1, 0)
logistic_test_r_prob = predict(logistic_fit_r, vowel_test_r[,1:11], type="response")
logistic_test_r = ifelse(logistic_test_r_prob>0.5, 1, 0)
mean(vowel_train_r$y == logistic_train_r)
mean(vowel_test_r$y == logistic_test_r) #correct classification
table(vowel_test_r$y, logistic_test_r)

library(pROC)
plot(roc(vowel_test_r$y, lda_fit_r)) #Area under the curve: 0.8265
plot(roc(vowel_test_r$y, qda_fit_r)) #Area under the curve: 0.8265
plot(roc(vowel_test_r$y, logistic_test_r_prob)) #Area under the curve: 0.8265

#eigenval decomp 
mcluster_fit_r = MclustDA(vowel_train_r[,2:11], vowel_train_r[,1], modelNames = "EEI")
train_err_r = summary(mcluster_fit_r, newdata = vowel_test_r[,2:11], newclass=vowel_test_r[,1])$ce
test_err_r = summary(mcluster_fit_r, newdata = vowel_test_r[,2:11], newclass=vowel_test_r[,1])$ce.newdata
summary(mcluster_fit_r, newdata = vowel_test_r[,2:11], newclass=vowel_test_r[,1])
cat("EEI\ntrain.acc", 1-train_err_r, "\ntest.acc", 1-test_err_r, "\n")

