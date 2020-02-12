#EDA

df <- read.csv("../data/china_centroids_complied.csv")

# remove columns 1,2,3
df <- df[,-c(1,2,3)]

# get rid of weird negative rows

df <- df[-c(763:nrow(df)),]

bad_rows <- which(df$acc_to_c<0)
bad_rows <- c(bad_rows,which(df$pop_dens<0))

df <- df[-bad_rows,]

# start by predicting outbreak vs non outbreak
df$cases_binary <- 0
df$cases_binary[which(df$ncov_cases>0)] <-1
df$cases_binary <- as.factor(df$cases_binary)

df$death_binary <- 0
df$death_binary[which(df$ncov_death>0)]<-1
df$death_binary <- as.factor(df$death_binary)

library('randomForest')
library('pROC')

# try a classifier

rf1 <- randomForest(cases_binary ~acc_to_c+hc_dist+hc_dens+airport_de+airport_di+pop_dens,data=df )

rf1.roc<-roc(df$cases_binary,rf1$votes[,2])
plot(rf1.roc)
auc(rf1.roc)

rf2 <- randomForest(death_binary ~acc_to_c+hc_dist+hc_dens+airport_de+airport_di+pop_dens,data=df )
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
  rf1_over <- randomForest(cases_binary ~acc_to_c+hc_dist+hc_dens+airport_de+airport_di+pop_dens,data=df_train_over )
  
  # predict on test data
  rf1_test <- predict(object = rf1_over,newdata = df_test)
  print(roc.curve(df_test$cases_binary,rf1_test))
  
  
  rf2 <- randomForest(ncov_cases~acc_to_c+hc_dist+hc_dens+airport_de+airport_di+pop_dens,data=df_train_over )
  rf2_test <- predict(rf2,newdata = df_test)  
  cor(rf2_test,df_test$ncov_cases)  
}
