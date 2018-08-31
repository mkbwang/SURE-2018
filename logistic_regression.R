library(dplyr)
library(glmnet)

# read all the data
alldata = read.csv('/home/wangmk/UM/Research/SURE/cleaned_data/full_cleaned_data.csv')
bool_cols = c('True_state','Alert','Identification','Top_Left_Enemy',
              "Top_Right_Enemy",'Bottom_Left_Enemy','Bottom_Right_Enemy','Top_Left_Dark',
              'Top_Right_Dark','Bottom_Left_Dark','Bottom_Right_Dark')
alldata[bool_cols] = alldata[bool_cols]=='True'
alldata$performance = alldata$RMS<=60
alldata$Top_Left_Hard = alldata$Top_Left_Dark & alldata$Top_Left_Enemy
alldata$Top_Right_Hard = alldata$Top_Right_Dark & alldata$Top_Right_Enemy
alldata$Bottom_Left_Hard = alldata$Bottom_Left_Dark & alldata$Bottom_Left_Enemy
alldata$Bottom_Right_Hard = alldata$Bottom_Right_Dark & alldata$Bottom_Right_Enemy

alldata$First_toggle_time = alldata$First_toggle_time / 10000
alldata$Trial = alldata$Trial / 100

alldata = tbl_df(alldata)
normal = alldata %>% filter(performance == TRUE)
abnormal = alldata %>% filter(performance == FALSE)

bootstrap = sample(3840,160)
newdata = rbind(normal[bootstrap,],abnormal)
train_id = sample(320,320*0.7)
train_data = newdata[train_id,]
test_data = newdata[-train_id,]
xtrain = train_data[,!names(train_data) %in% c("RMS", "performance")]
ytrain = train_data[,'performance']
xtest = test_data[,!names(test_data) %in% c("RMS", "performance")]
ytest = test_data[,'performance']
lasso.fit = cv.glmnet(as.matrix(xtrain),as.matrix(ytrain),lambda = c(0.001, 0.01, 0.1, 1, 10,100),nfolds=5,type.measure="auc")
fit = glmnet(x = as.matrix(xtrain),as.matrix(ytrain),family='binomial',lambda = lasso.fit$lambda.min,alpha=1)
