#训练模型

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(Matrix)

newdata=read.csv('F:/test/KDD/newdata.csv')  #训练集
testdata=read.csv('F:/test/KDD/testdata.csv')   #测试集
data=newdata[,-1]
colnames(data)
colnames(testdata)

xdata=data.matrix(newdata[,c(1:8,9:29,32,33)]) 
xdata2=Matrix(xdata,sparse=T)
head(xdata)

lab=newdata[,31]
traindat=list(data=xdata2,label=lab) 
dtrain=xgb.DMatrix(data=traindat$data,label=traindat$label) 

xgb=xgboost(data=dtrain,max_depth=15, eta=0.3,objective='multi:softmax',num_class=12,nround=10)

nrow(testdata)
click_mode=vector(length=94358)
click_mode=as.data.frame(click_mode)
dim(click_mode)
d=cbind(testdata,click_mode)
dim(d)
colnames(d) #测试集


#d=merge(d,npro,by="pid",all.x = T)
#colnames(d)
#d=d[,-2]
#colnames(xdata)


testset1=data.matrix(d[,c(1:8,10:31)]) 
testset2=Matrix(testset1,sparse=T)
testset3=d[,32]
testset4=list(data=testset2,label=testset3) 
dtest=xgb.DMatrix(data=testset4$data,label=testset4$label) 

pre_xgb=predict(xgb,newdata=dtest)
head(pre_xgb)
pre_xgb=as.data.frame(pre_xgb)
dim(pre_xgb)

result1=data.frame(d$sid,pre_xgb)
dim(result1)
colnames(result1)=c("sid","recommend_mode")
colnames(result1)
head(result1)
write.csv(result1,'F:/test/KDD/result1.csv')


