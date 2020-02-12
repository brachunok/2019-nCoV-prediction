#EDA

df <- read.csv("../data/geoprocessed/china_centroids_compiled.csv")

# remove columns 1,2,3
df <- df[,-c(1,2,3,4,5)]

# get rid of weird negative rows

bad_rows <- which(df[,1]<0)
bad_rows <- c(bad_rows,which(df$china_pop_<0))

df <- df[-bad_rows,]

# start by predicting outbreak vs non outbreak
df$cases_binary <- 0
df$cases_binary[which(df$ncov_cas_1>0)] <-1
df$cases_binary <- as.factor(df$cases_binary)

df$death_binary <- 0
df$death_binary[which(df$ncov_dea_1>0)]<-1
df$death_binary <- as.factor(df$death_binary)

library('randomForest')
library('pROC')

# try a classifier

rf1 <- randomForest(cases_binary ~airport__1+airport__2+health_c_2+health_c_3+china_pop_,data=df )

rf1.roc<-roc(df$cases_binary,rf1$votes[,2])
plot(rf1.roc)
auc(rf1.roc)

rf2 <- randomForest(death_binary ~airport__1+airport__2+health_c_2+health_c_3+china_pop_,data=df )
rf2.roc<-roc(df$death_binary,rf2$votes[,2])
plot(rf2.roc)
auc(rf2.roc)

# pretty bad class imbalance

library("ROSE")

# do an actual cross validation and measure AUC
set.seed(1)
folds <- 5
df$folds <- sample(1:folds,size=nrow(df),replace=T)

for(f in 1:folds){
  df_train <- df[which(df$folds!=f),]
  df_test <- df[which(df$folds==f),]
  
  # oversample the training data
  #df_train_over <- ovun.sample(cases_binary~.,data=df_train,method="both",N=1500)$data
  df_train_over <- ROSE(cases_binary~.,data=df_train)$data
  rf1_over <- randomForest(cases_binary ~airport__1+airport__2+health_c_2+health_c_3+china_pop_,data=df_train_over )
  
  # predict on test data
  rf1_test <- predict(object = rf1_over,newdata = df_test)
  print(roc.curve(df_test$cases_binary,rf1_test))
  
  
  rf2 <- randomForest(ncov_cas_1~airport__1+airport__2+health_c_2+health_c_3+china_pop_,data=df_train_over )
  rf2_test <- predict(rf2,newdata = df_test)  
  print(cor(rf2_test,df_test$ncov_cas_1)  )
}
