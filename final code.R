setwd('C:\\Users\\김문정\\Desktop\\2018_2\\ESC ppt')
dt<-read.csv("C:\\Users\\김문정\\Desktop\\2018_2\\ESC ppt\\final project\\test.csv") ##data with all the '?'s converted to NA's
################################################################################


for(i in 1:dim(dt)[2]){
  dt[,i][which(dt[,i]=="?")]=NA 
}
dt[,125]

dt[,125][is.na(dt[,125])]=0
dt[,125]=factor(as.numeric(as.character(dt[,125])))
dt[,125]

for(i in 1:dim(dt)[2]){
  dt[,i]=as.numeric(dt[,i])
}

dt[15,]
##Scaling/Imputation/Skewness/pca
###divide 'population' to some variables to convert to "percentile per pop" form
###delete some irrelvant variables
colnames(dt)[125]

dt[,c(15,32,53,55,75,95,96,122,128,129,130,131)]=dt[,c(15,32,53,55,75,95,96,122,128,129,130,131)]/dt[,5]
dt1=dt[,-c(1,3,4,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,92,119,123,16,15,95,96,122,124,127,126)]
dt[,27][which(dt[,27]==0)]=NA
###divide into continuous/categorical/response variable(s)
dim(dt1)
response=dt1[,101]

colnames(dt1)

contin_dt=dt1[,-c(96,1)]
contin_dt[1598,99]=median(dt1[-1598,101])
colnames(contin_dt)
colnames(dt1)
cate_dt=dt1[,c(96,1)] 
###make a vector "med" consisting of medians of each column
###imputation(NA's into median)
med=vector(length=0)
for(i in 1:dim(contin_dt)[2]){
  med[i]=median(contin_dt[,i],na.rm = TRUE)
}
for(i in 1:dim(contin_dt)[2]){
  contin_dt[,i][is.na(contin_dt[,i])]=med[i]
}
sum(is.na(contin_dt)) #0

sum(is.na(cate_dt)) #1595

###define X and Y
colnames(contin_dt)
cate_dt[1][is.na(cate_dt[1])]=0


X=cbind(contin_dt,cate_dt)
y=response
sum(is.na(X))
sum(is.na(contin_dt))

library(car)
names<-colnames(contin_dt)
colnames(contin_dt)<-names
#오래 걸림
scatterplotMatrix(X[,c(1,2,7,8,9,10,93)])

###skewness
###skewness function
skewness=function(x){
  uh=fivenum(x)[4]
  md=fivenum(x)[3]
  lh=fivenum(x)[2]
  skew=((uh-md)-(md-lh))/((uh-md)+(md-lh))  
  return(skew)
}

skew_dt=vector(length=0)
for (i in 1:dim(contin_dt)[2]){
  skew_dt[i]=skewness(contin_dt[,i])
}

###Check skewness(|x|>0.5) 
plot(sort(skew_dt,decreasing = T),type='o') 
order(skew_dt)
skew_dt[order(skew_dt)]
a<-colnames(contin_dt)[order(skew_dt,decreasing = FALSE)]
z<-cbind(a,order(skew_dt),skew_dt[order(skew_dt)])
z #able to find the skewness of each variable


trans_contin_dt=contin_dt
for(i in 1:dim(contin_dt)[2]){
  if(skew_dt[i]>0.1){
    for(j in 1:dim(contin_dt)[1]){
      if(contin_dt[j,i]==0){contin_dt[j,i]=0.03}
    }
    trans_contin_dt[,i]=log(contin_dt[,i])
    colnames(trans_contin_dt)[i]=paste('log',colnames(trans_contin_dt)[i]) 
    
  }
  if(skew_dt[i]<(-0.1)){trans_contin_dt[,i]=(contin_dt[,i])^2
  colnames(trans_contin_dt)[i]=paste('sq',colnames(trans_contin_dt)[i])}
}
colnames(trans_contin_dt)

head(trans_contin_dt)

trans_skew_dt=vector(length=0)
for (i in 1:dim(trans_contin_dt)[2]){
  trans_skew_dt[i]=skewness(trans_contin_dt[,i])
}
trans_skew_dt

plot(sort(trans_skew_dt,decreasing = T),type='o') 
order(trans_skew_dt)
skew_dt[order(trans_skew_dt)]
a<-colnames(contin_dt)[order(trans_skew_dt,decreasing = FALSE)]
z<-cbind(a,order(trans_skew_dt),skew_dt[order(trans_skew_dt)])
z #able to find the skewness of each variable
#
dim(trans_contin_dt)
colnames(trans_contin_dt)
library(car)
#population category
#오래걸림
scatterplotMatrix(contin_dt[,c(1,2,7,8,9,10,93)])
scatterplotMatrix(trans_contin_dt[,c(1,2,7,8,9,10,93)])
#population education category
scatterplotMatrix(contin_dt[,28:35])
scatterplotMatrix(trans_contin_dt[,28:35])

write.csv(cbind(trans_contin_dt,cate_dt),'final_dat.csv')
X.new=cbind(trans_contin_dt[-1],cate_dt)


#########scaling(x-median/max-min)
scaled_trans_contin_dt=matrix(0,dim(trans_contin_dt)[1],dim(trans_contin_dt)[2])
for(i in 1:dim(trans_contin_dt)[2]){
  scaled_trans_contin_dt[,i]=(trans_contin_dt[,i]-median(trans_contin_dt[,i]))/(max(trans_contin_dt[,i])-min(trans_contin_dt[,i]))  
}
head(scaled_trans_contin_dt)







library(elasticnet)
########
scaled_trans_X_contin=scaled_trans_contin_dt[,-99]
scaled_trans_y=data.frame(scaled_trans_contin_dt[,99])

names<-colnames(trans_contin_dt[,-99])
colnames(scaled_trans_X_contin)<-names

names<-colnames(trans_contin_dt)[99]
colnames(scaled_trans_y)<-names
head(scaled_trans_X_contin)
colnames(scaled_trans_X_contin)
scaled_trans_X_contin
X.new=cbind(scaled_trans_X_contin,cate_dt)
y.new=scaled_trans_y

data.new=cbind(X.new,y.new)




head(data.new)
write.csv(data.new,'transformed_data.csv')
#######


#5combi
spca_result2=spca(scaled_trans_X_contin,K=10,sparse='varnum',para=rep(5,10))
spca_result2
loadings2=spca_result2$loadings
dim(loadings2)




coef_names_pc=function(K,loading){
  combination=list()
  for (i in 1:10){
    
    combination[[i]]=loading[,i][which(loading[,i]!=0)]
    
  }
  
  
  return(combination)
}

summary_spca=function(list,p){
  summ=matrix(0,p,10)
  for(i in 1:10){
    summ[,i]=list[[i]]}
  
}



coef_names_pc(5,loadings2)


#5

PC_X2=scaled_trans_X_contin%*%loadings2 

####Simple Regression
PCA_Data=cbind(PC_X2,cate_dt,y.new)
set.seed(100)
sample_data <- sample(nrow(PCA_Data),nrow(PCA_Data)*0.7)
train <- PCA_Data[sample_data,]
test <- PCA_Data[-sample_data,]

lm_spca=lm(data=train,train$`log violentPerPop`~.)
summary(lm_spca)

test_lm=predict(lm_spca,test,se.fit = TRUE)
y[(y==0)]=0.03

org.test.y=exp(test$`log violentPerPop`*(max(log(y))-min(log(y)))+median(log(y)))
org.fit.y=exp(test_lm$fit*(max(log(y))-min(log(y)))+median(log(y)))
mean((org.test.y-org.fit.y)^2)







####Decision Tree
install.packages('rattle')
library(rattle)
library(rpart)

fit <- rpart(train$`log violentPerPop`~ ., data = train, method = "anova",
             control = rpart.control(cp = 0,minsplit = 200))
plot(fit, uniform=TRUE, 
     main="Regression Tree for monami 1-1")
text(fit, use.n=TRUE, cex = .6)
printcp(fit)
plotcp(fit)
fancyRpartPlot(fit)


####Random forest
library(randomForest)
set.seed(100)
rf=randomForest(train$`log violentPerPop`~.,data=train,ntree=100,mtry=5
                ,importance=TRUE)
summary(rf)
importance(rf)
test_rf=predict(rf,newdata = test)


org.test.y=exp(test$`log violentPerPop`*(max(log(y))-min(log(y)))+median(log(y)))
org.fit.y.rf=exp(test_rf*(max(log(y))-min(log(y)))+median(log(y)))
mean((org.test.y-org.fit.y.rf)^2)


###factor analysis-Not needed
fit <- factanal(scaled_trans_X_contin, factors=4, rotation="varimax")
print(fit, sort=TRUE,cutoff=0.3)

Factors=scaled_trans_X_contin%*%fit$loadings
cor(PC_X2,Factors)
library(car)

plot(Factors[,1],PC_X2[,5])
plot(Factors[,2],PC_X2[,10])
plot(Factors[,3],PC_X2[,2])
plot(Factors[,4],PC_X2[,10])

######









