# Info: https://hastie.su.domains/ElemStatLearn/datasets/vowel.info.txt
vowel_train = read.table("https://hastie.su.domains/ElemStatLearn/datasets/vowel.train", header=TRUE, sep=",")
vowel_test = read.table("https://hastie.su.domains/ElemStatLearn/datasets/vowel.test", header=TRUE, sep=",")
head(vowel_train)
head(vowel_test)
dim(vowel_test)
vowel_train = vowel_train[,2:12] #delete index
vowel_test = vowel_test[,2:12] #delete index
dim(vowel_test) #462, 11

library(mclust)
