
df_red <- read.csv("winequality-red.csv", sep=";")
df_white <- read.csv("winequality-white.csv", sep=";")

df_red <- cbind(df_red, "red")
df_white <- cbind(df_white, "white")
names(df_red)[13] <- "color"
names(df_white)[13] <- "color"

df <- rbind(df_red, df_white)

#randomly split data 70/30
set.seed(1)
split <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7, 0.3))
df_train <- df[split,]
df_test <- df[!split,]

res <- kmeans(df_train[,1:12], centers = 2)
table(df_train$color, res$cluster)
(1047+2523)/sum(table(df_train$color, res$cluster)) #79%

#predict classes in test data
res_test <- clue::cl_predict(res, df_test[,-13])
table(df_test$color, res_test)
(464+1108)/sum(table(df_test$color, res_test)) #79%

#accuracy is suboptimal for both data sets


#Instead Latent Profile Analysis
library(mclust)

model <- Mclust(df_train[,-13], modelNames = "EEV", G=2)
summary(model)
tab <- summary(model, parameters = T)


table(df_train$color, model[["classification"]])
(1088+3326)/sum(table(df_train$color, model[["classification"]])) # 98%


test_pred <- predict.Mclust(model,df_test[,-13])
(470+1494)/sum(table(df_test$color, test_pred[["classification"]])) # 98%

#LPA performs a LOT better