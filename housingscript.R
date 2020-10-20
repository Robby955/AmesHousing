suppressMessages(library(tidyverse))
suppressMessages(library(caret))
suppressMessages(library(caTools))
suppressMessages(library(ggplot2))
suppressMessages(library(MASS))
suppressMessages(library(glmnet))
suppressMessages(library(plotly))
suppressMessages(library(reshape2))
suppressMessages(library(gridExtra))
suppressMessages(library(gbm))

setwd("C:/Users/User/Desktop/Kaggle") #I saved the csv files to a folder on my desktop.

housing=read.csv(file="housing_train.csv",stringsAsFactors = TRUE, na.strings = c("NA","null","Null"))
housing_testing=read.csv(file="housing_testing.csv",stringsAsFactors = TRUE)
housing_testing$SalePrice=NA #Fill in NA for missing values of test sale price.

housing_full=rbind(housing,housing_testing) #Create a full dataset to transform predictors later on.

paste("The number of observations in the training data set is",nrow(housing))
paste("The number of observations in the test data set is",nrow(housing_testing))
paste("The number of predictors in the data (including Id) is,",ncol(housing %>% select(-SalePrice)))

#Expected: 1460 observations in training, 1459 in test set and 80 predictors.


# Get Summary tells us the min, max and mean price of a house in each distinct neighborhood.

j=1
getSummary=data.frame(matrix(ncol=4,nrow=length(levels(housing$Neighborhood))))
names(getSummary)=c("Neighborhood","$ Minimum","$ Maximum","$ Mean")
for(l in levels(housing$Neighborhood)){
  getSummary[j,]=rbind(c(l,min((housing%>%filter(Neighborhood==l))$SalePrice),max((housing%>%filter(Neighborhood==l))$SalePrice),round(mean((housing%>%filter(Neighborhood==l))$SalePrice),2)
  ))
  j=j+1
}


par(mfrow=c(1,2))
hist(housing$SalePrice,xlab='housing price',ylab='Frequency',main="Histrogram of Sale Price",col='red')

hist(log(housing$SalePrice),xlab='Log(housing price)',ylab='Frequency',main="Histrogram of Log Sale Price",col='red') 


# We note we can remove some outliers from the dataset. in particular only 1 test house is larger than 4000 sqft so we won't train on houses that large. This is noted in the documentation.
housing_full=housing_full[-which(housing_full$GrLivArea>4000 & housing_full$SalePrice < 800000),]
housing=housing[-which(housing$GrLivArea>4000 & housing$SalePrice < 800000),]


# More Explatory Analysis

g1=ggplot(housing)+
  geom_boxplot(mapping=aes(x=SaleType,y=log(SalePrice)))+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g2=ggplot(housing)+
  geom_boxplot(mapping=aes(x=SaleCondition,y=log(SalePrice)))+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g3=ggplot(housing)+
  geom_point(mapping=aes(x=OverallQual,y=log(SalePrice)))+theme_bw()

g4=ggplot(housing)+
  geom_point(mapping=aes(x=OverallCond,y=SalePrice))+theme_bw()


g5=ggplot(housing)+
  geom_boxplot(mapping=aes(x=Street,y=log(SalePrice)))+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g6=ggplot(housing)+
  geom_boxplot(mapping=aes(x=Neighborhood,y=log(SalePrice)))+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


g7=ggplot(housing)+
  geom_point(mapping=aes(x=FullBath,y=log(SalePrice)))+theme_bw()



breaks=c(0,1,2)

g8=ggplot(housing)+
  geom_point(mapping=aes(x=HalfBath,y=log(SalePrice)))+scale_x_continuous(breaks=breaks,
                                                                          labels=c("0", "1", "2"))+theme_bw()

g9=ggplot(housing)+
  geom_boxplot(mapping=aes(x=GarageQual,y=log(SalePrice)))+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


g10=ggplot(housing)+
  geom_boxplot(mapping=aes(x=HouseStyle,y=log(SalePrice)))+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


g11=ggplot(housing)+
  geom_point(mapping=aes(x=YearRemodAdd,y=log(SalePrice)))+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g12=ggplot(housing)+
  geom_boxplot(mapping=aes(x=GarageQual,y=log(SalePrice)))+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

grid.arrange(g1,g2,g3,g4,nrow=2)
grid.arrange(g5,g6,g7,g8,nrow=2)
grid.arrange(g9,g10,g11,g12,nrow=2)


# Make heat map of numeric predictors

housing_numeric=housing%>%
  dplyr::select_if(is.numeric)%>%
  drop_na()


cc2=cor(housing_numeric)

cc2_melt=melt(cc2)

gz=ggplot(cc2_melt,mapping=aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(text = element_text(size=8))+
  ggtitle("Heat Map for Housing Data: Numeric Predictors")+
  ylab("")+
  xlab("")+
  scale_fill_distiller(palette = "RdPu") 

ggplotly(gz, tooltip="text")


#Define a function that checks for how many missing entries we have in our dataframe.

displayMissing=function(){
  for(l in 2:ncol(housing_full)-1){
    print(paste(names(housing_full)[l], 'has', sum(is.na(housing_full[,l])),'missing values'))
  }
}

## Imputation

# For some features , no value tells us information. NA for fence means no fence, NA for alley means no alley access, etc. Hence, we keep these features for now as they may convey some information.

#Impute Fence
housing_full$Fence=addNA(housing_full$Fence)
levels(housing_full$Fence)=c(levels(housing_full$Fence)[-length(levels(housing_full$Fence))], 'None')


# Impute Alley
housing_full$Alley=addNA(housing_full$Alley)
levels(housing_full$Alley)=c(levels(housing_full$Alley)[-length(levels(housing_full$Alley))], 'None')


# Impute Fireplace Qu
housing_full$FireplaceQu=addNA(housing_full$FireplaceQu)
levels(housing_full$FireplaceQu)=c(levels(housing_full$FireplaceQu)[-length(levels(housing_full$FireplaceQu))], 'None')

#Remove Misc feature and PoolQC. To many missing values at this point, easier to remove than attempt to impute.

housing_full=housing_full%>%
  dplyr::select(-MiscFeature,-PoolQC)


#Impute lot frontage based off median lot frontage for that neighborhood

for(i in 1:nrow(housing_full)){
  if(is.na(housing_full[i,'LotFrontage'])){
    select_area=housing_full[i,'Neighborhood']
    s=housing_full %>%
      dplyr::filter(Neighborhood==select_area)
    con_med=median(s$LotFrontage,na.rm=TRUE)
    housing_full[i,'LotFrontage']=con_med
  }
}

# Fix numeric to factor

housing_full$MSSubClass=as.factor(housing_full$MSSubClass)




# Basment Imputation

# Basment: Several features relating to basements are missing. This could be because the house has no basment, hence missing values in this case actually may convey some infromation. We impute them as missing.

#For missing factors we can impute as an NA for missing.
housing_full$BsmtQual=addNA(housing_full$BsmtQual)
levels(housing_full$BsmtQual) <- c(levels(housing_full$BsmtQual)[-length(levels(housing_full$BsmtQual))], 'None')

housing_full$BsmtCond=addNA(housing_full$BsmtCond)
levels(housing_full$BsmtCond)=c(levels(housing_full$BsmtCond)[-length(levels(housing_full$BsmtCond))], 'None')

housing_full$BsmtExposure=addNA(housing_full$BsmtExposure)
levels(housing_full$BsmtExposure)=c(levels(housing_full$BsmtExposure)[-length(levels(housing_full$BsmtExposure))], 'None')

housing_full$BsmtFinType1=addNA(housing_full$BsmtFinType1)
levels(housing_full$BsmtFinType1)=c(levels(housing_full$BsmtFinType1)[-length(levels(housing_full$BsmtFinType1))], 'None')

housing_full$BsmtFinType2=addNA(housing_full$BsmtFinType2)
levels(housing_full$BsmtFinType2)=c(levels(housing_full$BsmtFinType2)[-length(levels(housing_full$BsmtFinType2))], 'None')

#For missing numeric, no basment is equivelent to zero SF.

for(i in 1:nrow(housing_full)){
  if(is.na(housing_full[i,'BsmtFinSF1'])){
    housing_full[i,'BsmtFinSF1']=0
  }
  if(is.na(housing_full[i,'BsmtFinSF2'])){
    housing_full[i,'BsmtFinSF2']=0
  }
  if(is.na(housing_full[i,'BsmtUnfSF'])){
    housing_full[i,'BsmtUnfSF']=0
  }
  if(is.na(housing_full[i,'TotalBsmtSF'])){
    housing_full[i,'TotalBsmtSF']=0
  }
  if(is.na(housing_full[i,'BsmtFullBath'])){
    housing_full[i,'BsmtFullBath']=0
  }
  
  if(is.na(housing_full[i,'BsmtHalfBath'])){
    housing_full[i,'BsmtHalfBath']=0
  }
}


# Garage Imputation

# Garage

housing_full$GarageType=addNA(housing_full$GarageType)
levels(housing_full$GarageType) <- c(levels(housing_full$GarageType)[-length(levels(housing_full$GarageType))], 'None')

housing_full$GarageFinish=addNA(housing_full$GarageFinish)
levels(housing_full$GarageFinish) <- c(levels(housing_full$GarageFinish)[-length(levels(housing_full$GarageFinish))], 'None')

housing_full$GarageQual=addNA(housing_full$GarageQual)
levels(housing_full$GarageQual) <- c(levels(housing_full$GarageQual)[-length(levels(housing_full$GarageQual))], 'None')

housing_full$GarageCond=addNA(housing_full$GarageCond)
levels(housing_full$GarageCond) <- c(levels(housing_full$GarageCond)[-length(levels(housing_full$GarageCond))], 'None')



for(i in 1:nrow(housing_full)){
  if(is.na(housing_full[i,'GarageYrBlt'])){
    housing_full[i,'GarageYrBlt']=0
  }
  if(is.na(housing_full[i,'GarageArea'])){
    housing_full[i,'GarageArea']=0
  }
  if(is.na(housing_full[i,'GarageCars'])){
    housing_full[i,'GarageCars']=0
    
  }
}


# Fix predictors that should be factors.
housing_full$MasVnrType=addNA(housing_full$MasVnrType)
levels(housing_full$MasVnrType) <- c(levels(housing_full$MasVnrType)[-length(levels(housing_full$MasVnrType))], 'None')

for(i in 1:nrow(housing_full)){
  if(is.na(housing_full[i,'MasVnrArea'])){
    housing_full[i,'MasVnrArea']=0
  }}

# Assign to most common factor if missing as these are overwhelming

housing_full[which(is.na(housing_full$MSZoning)),'MSZoning']='RL'
housing_full[which(is.na(housing_full$Functional)),'Functional']='Typ'
housing_full[which(is.na(housing_full$Electrical)),'Electrical']='SBrkr'
housing_full[which(is.na(housing_full$KitchenQual)),'KitchenQual']='TA'
housing_full[which(is.na(housing_full$SaleType)),'SaleType']="WD"
housing_full[which(is.na(housing_full$Exterior1st)),'Exterior1st']='VinylSd'
housing_full[which(is.na(housing_full$Exterior2nd)),'Exterior2nd']="VinylSd"


# Remove ID and Utilities, Utilities is identical for all but a few, and has diffirent levels in test set.


housing_full=housing_full%>%
  dplyr::select(-Utilities,-Id)




# Reform the train and test set, get ready for model training

order=nrow(housing)+1
housing=housing_full[1:nrow(housing),]
housing_predictors=housing[,-length(housing)]
housing_sale=housing[,length(housing)]
housing_testing=housing_full[order:nrow(housing_full),]


# Make matrix versions that are used for prediction algorithms.

housing_predictors_Matrix=housing%>%
  dplyr::select(-SalePrice)%>%
  data.matrix()

housing_testing_Matrix=housing_testing%>%
  dplyr::select(-SalePrice)%>%
  data.matrix()

housing_Matrix=housing%>%
  data.matrix()



# Scaling. We scale for mean and centre on sd for numeric predictors. 

housing_pred=housing%>%
  select(-SalePrice)

# Don't scale Output
for(j in 1:ncol(housing_pred)-1){
  if(class(housing_pred[,j])=='integer' | class(housing_pred[,j])=='numeric'){
    housing_pred[,j]=as.vector(scale(housing_pred[,j]))
  }
}


housing_testing_pred=housing_testing%>%
  select(-SalePrice)

# we also scale the testing set seperately.
for(j in 1:ncol(housing_testing_pred)){
  if(class(housing_testing_pred[,j])=='integer' | class(housing_testing_pred[,j])=='numeric'){
    housing_testing_pred[,j]=as.vector(scale(housing_testing_pred[,j]))
  }
}



## Random Forest Select mtry using num(predictors)/3 rule, hence around 25, I prefer more so 30.

library(randomForest)

#housing_pred=housing%>%
#  select(-SalePrice)


model_rf <- randomForest(log(housing$SalePrice) ~ ., data = housing_pred, ntree = 1000, mtry = 30, importance = TRUE)


p_rf=exp(predict(model_rf,newdata=housing_testing_pred))

ptrain_rf=predict(model_rf,newdata=housing_pred)

MSE_rf=sum((log(housing$SalePrice)-ptrain_rf)^2) # VERY LOW IN TRAIN ERROR , possibly Overfit training, so combine with models to blend.




## Elastic Net

cv_en<-cv.glmnet(x=housing_predictors_Matrix,y=log(housing_Matrix[,'SalePrice']),alpha=0.5)
fit_elastic<-glmnet(x=housing_predictors_Matrix, y=log(housing$SalePrice), alpha = 0.5, lambda = cv_en$lambda.min)
pelastic=exp(predict(fit_elastic,newx=housing_testing_Matrix,type='response',s=cv_en$lambda.min))


# Train Error
ptrain_en=predict(fit_elastic,newx=housing_predictors_Matrix,type='response',s=cv_en$lambda.min)


MSE_elastic=sum((log(housing$SalePrice)-ptrain_en)^2)



## Gradient Boosting

model_gbm=gbm(log(housing$SalePrice)~.,data=housing_pred,cv.folds = 5,distribution ="gaussian",shrinkage=0.3,n.trees = 300)
pgbm=exp(predict(model_gbm,newdata=housing_testing_pred))

ptrain_gbm=predict(model_gbm,newdata=housing)
MSE_gbm=sum((log(housing$SalePrice)-ptrain_gbm)^2)


## Support Vector Machine


library(e1071) # For Support Vector Machine

fit_svm=svm(log(housing$SalePrice)~.,data=housing_pred)

psvm=exp(predict(fit_svm,newdata =housing_testing_pred))


# Train Error
ptrain_svm=predict(fit_svm,newdata=housing) # Very low train error, fits our training set well. Possible overfit?


MSE_svm=sum((log(housing$SalePrice)-ptrain_svm)^2)




# Ridge, Lasso, and Elastic need our data in a special form in R

housing_predictors=housing_pred%>%
  data.matrix()

housing_new_testing=housing_testing_pred%>%
  data.matrix()


## Ridge Regression


cv_ridge <- cv.glmnet(x=housing_predictors, y=log(housing$SalePrice), alpha = 0)
fit_ridge <- glmnet(x=housing_predictors, y=log(housing$SalePrice), alpha = 0, lambda = cv_ridge$lambda.min)
pridge=exp(predict(fit_ridge,newx=housing_new_testing,type='response',s=cv_ridge$lambda.min))


#Train Error

ptrain_ridge=predict(fit_ridge,newx=housing_predictors,type='response',s=cv_ridge$lambda.min)
MSE_Ridge=sum((log(housing$SalePrice)-ptrain_ridge)^2)

## Lasso

cv_lasso <- cv.glmnet(x=housing_predictors, y=log(housing$SalePrice), alpha = 1)
fit_lasso<-glmnet(x=housing_predictors, y=log(housing$SalePrice), alpha = 1, lambda = cv_lasso$lambda.min)
plasso=exp(predict(fit_lasso,newx=housing_new_testing,type='response',s=cv_lasso$lambda.min))


ptrain_lasso=predict(fit_lasso,newx=housing_predictors,type='response',s=cv_lasso$lambda.min)
MSE_Lasso=sum((log(housing$SalePrice)-ptrain_lasso)^2)


#Train Error

## Elastic Net

cv_en<-cv.glmnet(x=housing_predictors,y=log(housing$SalePrice),alpha=0.5)
fit_elastic<-glmnet(x=housing_predictors, y=log(housing$SalePrice), alpha = 0.5, lambda = cv_en$lambda.min)
pelastic=exp(predict(fit_elastic,newx=housing_new_testing,type='response',s=cv_en$lambda.min))

# Train Error
ptrain_en=predict(fit_elastic,newx=housing_predictors,type='response',s=cv_en$lambda.min)
MSE_elastic=sum((log(housing$SalePrice)-ptrain_en)^2)




## Blend Predictions and write to csv.

pzz=(1/5)*(p_rf+pgbm+pelastic+plasso+psvm)

df.sub=data.frame(1461:2919,pzz)
colnames(df.sub)<-c('Id','SalePrice')
write.csv(df.sub,'housingprice1.csv',row.names=FALSE)  #0.129.
