# Split into test/train set + mind groups (all groups should be well-represented in both train/test data)
# Do not use sample.split. Does not achieve at representing all job categories in both train/test data.
library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.textmodels)
library(spacyr)
library(stringr)
library(caret)
library(seededlda)
library(caTools)
library(text2vec)
set.seed(123) # Redundant but let it be
ind_train <- lapply(split(seq(1:nrow(FullData_cleaned)), FullData_cleaned$Party), function(x) sample(x, floor(.7*length(x))))
ind_test <- mapply(function(x,y) setdiff(x,y), x = split(seq(1:nrow(FullData_cleaned)), FullData_cleaned$Party), y = ind_train)

test <- FullData_cleaned[unlist(ind_test),]
train <- FullData_cleaned[unlist(ind_train),]

corp_party_test <- corpus(test, text_field = "terms")
corp_party_train <- corpus(train, text_field = "terms")

toks_party_test <- tokens(corp_party_test)
toks_party_train <- tokens(corp_party_train)

dfmt_party_train <- dfm(toks_party_train)
dfmt_party_test <- dfm(toks_party_test)

tmod_nb_train <- textmodel_nb(dfmt_party_train, dfmt_party_train@docvars[["Party"]])
summary(tmod_nb_train)
dfmat_matched <- dfm_match(dfmt_party_test, features = featnames(dfmt_party_train))

actual_class <- dfmat_matched@docvars[["Party"]]
predicted_class <- predict(tmod_nb_train, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class

confusionMatrix(tab_class, mode = "everything")


###Train Linear SVM. Interpret. (Optionally: Perform Grid Search). Explore confusion matrix. Comment
tmod_svm_train <- textmodel_svm(dfmt_party_train, dfmt_party_train@docvars[["Party"]])
summary(tmod_svm_train)

actual_class_svm <- dfmat_matched@docvars[["Party"]]
predicted_class_svm <- predict(tmod_svm_train, newdata = dfmat_matched)
tab_class_svm <- table(actual_class_svm, predicted_class_svm)
tab_class_svm
confusionMatrix(tab_class_svm, mode = "everything")



### logistic
tmod_lr_train <- textmodel_lr(dfmt_party_train, dfmt_party_train@docvars[["Party"]])
summary(tmod_lr_train)
actual_class_lr <- dfmat_matched@docvars[["Party"]]
predicted_class_lr <- predict(tmod_lr_train, newdata = dfmat_matched)
tab_class_lr <- table(actual_class_lr, predicted_class_lr)
tab_class_lr
confusionMatrix(tab_class_lr, mode = "everything")
