#1.train_queries鏁版嵁闆?
queries=read.csv('F:/test/data_set_phase1/train_queries.csv')
nrow(queries)
head(queries)

library(stringr)  #鏁版嵁鍒嗗壊

m=str_split_fixed(queries$o,",", 2)
m=as.data.frame(m)
head(m)
names(m)=c('o1','o2')
head(m)

n=str_split_fixed(queries$d,",", 2)
n=as.data.frame(n)
head(n)
names(n)=c('d1','d2')
head(n)

nq=cbind(queries[,c(1,2,3,4,5)],m)
nq=cbind(nq,n)
head(nq)


#鎺㈢储缂哄け鍊?
miss=c()   
for(i in 1:ncol(nq)){
  sum=sum(is.na(nq[,i]))
  miss[i]=sum
}
miss

library(mice)
md.pattern(nq)


sum(is.na(nq$pid))/nrow(nq)  #鏁版嵁鐨勫垎甯冪殑姣斾緥,濡傛灉涓嶅敮涓€,涔熷氨鎰忓懗鐫€鍑虹幇浜嗛噸澶嶇殑琛屼负
frq=c()
for(i in 1:ncol(nq)){
  le=length(unique(nq[,i]))/nrow(nq)
  frq[i]=le
}
frq

num1=as.data.frame(table(queries$o))
tail(num1[order(num1$Freq),])
num1[which(num1[,2]==max((num1)[,2])),]  #o涓嚭鐜版鏁版渶澶氱殑鍦版柟鐨勭粡绾害

num2=as.data.frame(table(queries$d))
tail(num2[order(num2$Freq),])
num2[which(num2[,2]==max((num2)[,2])),]  #d涓嚭鐜版鏁版渶澶氱殑鍦版柟鐨勭粡绾害

km_result=kmeans(m,6)
km_result$cluster
library(ggplot2)
min=min(as.numeric(as.character(m$o1)))
max=max(as.numeric(as.character(m$o1)))
ggplot(m,aes(x=m$o1,y=m$o2,color=km_result$cluster),
       scale_x_continous(limits=c(min,max),breaks=seq(min,max,0.5)))+geom_point(shape=1)

km_result1=kmeans(m,6)
min1=min(as.numeric(as.character(m$o1)))
max1=max(as.numeric(as.character(m$o1)))
ggplot(m,aes(x=m$o1,y=m$o2,color=km_result1$cluster),
       scale_x_continous(limits=c(min,max),breaks=seq(min,max,0.5)))+geom_point(shape=1)

km_result2=kmeans(n,6)
min2=min(as.numeric(as.character(m$o2)))
max2=max(as.numeric(as.character(m$o2)))
ggplot(m,aes(x=m$o1,y=m$o2,color=km_result2$cluster),
       scale_x_continous(limits=c(min,max),breaks=seq(min,max,0.5)))+geom_point(shape=1)


#2.train_plans鏁版嵁闆?
tplans=read.csv('F:/test/data_set_phase1/train_plans.csv');str(tplans)
nm=nrow(tplans);nm
sid=as.data.frame(tplans$sid)
plantime=as.data.frame(tplans$plan_time);head(plantime)
nrow(plantime);nrow(sid)
head(tplans)
View(tplans)


##鎻愬彇plans鐗瑰緛

p=as.character(tplans$plans)
head(p)
library(tidyjson)
library(dplyr)  
plans=p%>%as.tbl_json%>%gather_array%>%spread_values(
  distance=jnumber("distance"),
  price=jnumber("price"),
  eta=jnumber("eta"),
  tr_mode=jnumber("transport_mode")
)
df=as.data.frame(plans)
head(df)
write.csv(df,"F:/test/KDD/df.csv")
nrow(df);ncol(df)


#mat=matrix(unlist(tplans$plans))
#head(mat)
library(RJSONIO)
#df=data.frame()
#for(i in 1:nm){
#  l=fromJSON(mat[i,])
#  d=data.frame(matrix(unlist(l), ncol=4,byrow=T))
#  df=rbind(df,d)
#}
#df


sum(is.na(df$price))/nrow(df)
sum(is.na(df$eta))/nrow(df)
sum(is.na(df$distance))/nrow(df)

colnames(df)=c("id","array.index","distance","price","eta","tr_mode")


#鎻掕ˉprice缂哄け閮ㄥ垎
median_pri=aggregate(price~id,data=df,FUN="median");colnames(median_pri)=c("id","median_price")
df[24,]
qs=df[which(is.na(df$price)==1),]
nrow(qs);nrow(median_pri)
head(qs)
head(median_pri)
library(dplyr)
cq=left_join(qs,median_pri,by="id");nrow(cq)
head(cq)
cq$price=cq$median_price;head(cq)
cq=cq[,-7] 
nrow(cq)


ndf=df[-which(is.na(df$price)==1),];nrow(ndf)
nrow(ndf)+nrow(cq)
cdf=bind_rows(ndf,cq)
nrow(cdf)
head(cdf)
cdf=cdf%>%arrange(id)
sum(is.na(cdf$price))  #浠呮湁鍗曚釜plan鐨勬牱鏈病鏈変腑浣嶆暟鍙互鎻掑€硷紝鍙互灏嗙己澶卞€艰缃负0
cdf$price[is.na(cdf$price)]=0


max_dis=aggregate(distance~id,data=cdf,FUN="max");colnames(max_dis)=c("id","max_distance")
min_dis=aggregate(distance~id,data=cdf,FUN="min");colnames(min_dis)=c("id","min_distance")
mean_dis=aggregate(distance~id,data=cdf,FUN="mean");colnames(mean_dis)=c("id","mean_distance")
sd_dis=aggregate(distance~id,data=cdf,FUN="sd");colnames(sd_dis)=c("id","sd_distance")
nrow(max_dis);nrow(min_dis);nrow(mean_dis);nrow(sd_dis)

max_pri=aggregate(price~id,data=cdf,FUN="max");colnames(max_pri)=c("id","max_price")
min_pri=aggregate(price~id,data=cdf,FUN="min");colnames(min_pri)=c("id","min_price")
mean_pri=aggregate(price~id,data=cdf,FUN="mean");colnames(mean_pri)=c("id","mean_price")
sd_pri=aggregate(price~id,data=cdf,FUN="sd");colnames(sd_pri)=c("id","sd_price")
nrow(max_pri);nrow(min_pri);nrow(mean_pri);nrow(sd_pri)

max_eta=aggregate(eta~id,data=cdf,FUN="max");colnames(max_eta)=c("id","max_eta")
min_eta=aggregate(eta~id,data=cdf,FUN="min");colnames(min_eta)=c("id","min_eta")
mean_eta=aggregate(eta~id,data=cdf,FUN="mean");colnames(mean_eta)=c("id","mean_eta")
sd_eta=aggregate(eta~id,data=cdf,FUN="sd");colnames(sd_eta)=c("id","sd_eta")
nrow(max_eta);nrow(min_eta);nrow(mean_eta);nrow(sd_eta)


#mode
head(cdf)
merge1=left_join(cdf,max_dis,by="id");head(merge1)
nrow(merge1)
td1=aggregate(distance~id,data=merge1,FUN="duplicated");head(td1)
un=unlist(td1$distance);head(un)
sum(un)  #distance閲嶅鐨勮鏁?
cd1=merge1[!un,];head(cd1)
nrow(cd1)
trm=cd1[,c(1,6)]
head(trm)

maxd_mode=trm[which(cd1$distance==cd1$max_distance),];maxd_mode=as.data.frame(maxd_mode)
head(maxd_mode)
nrow(maxd_mode)
colnames(maxd_mode)=c("id","maxd_mode")
#maxdmode=(left_join(cdf[,-7],maxd_mode,by="id"))$tr_mode;maxdmode=as.data.frame(maxdmode)
#head(maxdmode)
#nrow(maxdmode)

merge2=left_join(cdf,min_dis,by="id");head(merge2)
nrow(merge2)
td2=aggregate(distance~id,data=merge2,FUN="duplicated");head(td2)
un2=unlist(td2$distance);head(un2)
sum(un2)  #distance閲嶅鐨勮鏁?
cd2=merge2[!un2,];head(cd2)
nrow(cd2)
trm2=cd2[,c(1,6)]

mind_mode=trm2[which(cd2$distance==cd2$min_distance),];mind_mode=as.data.frame(mind_mode)
head(mind_mode)
nrow(mind_mode)
colnames(mind_mode)=c("id","mind_mode")

merge3=left_join(cdf,max_pri,by="id");head(merge3)
nrow(merge3)
td3=aggregate(price~id,data=merge3,FUN="duplicated");head(td3)
un3=unlist(td3$price);head(un3)
sum(un3)  #price閲嶅鐨勮鏁?
cd3=merge3[!un3,];head(cd3)
nrow(cd3)
trm3=cd3[,c(1,6)]

maxp_mode=trm3[which(cd3$price==cd3$max_price),];maxp_mode=as.data.frame(maxp_mode)
head(maxp_mode)
nrow(maxp_mode)
colnames(maxp_mode)=c("id","maxp_mode")

merge4=left_join(cdf,min_pri,by="id");head(merge4)
nrow(merge4)
td4=aggregate(price~id,data=merge4,FUN="duplicated");head(td4)
un4=unlist(td4$price);head(un4)
sum(un4)  #price閲嶅鐨勮鏁?
cd4=merge4[!un4,];head(cd4)
nrow(cd4)
trm4=cd4[,c(1,6)]

minp_mode=trm4[which(cd4$price==cd4$min_price),];minp_mode=as.data.frame(minp_mode)
head(minp_mode)
nrow(minp_mode)
colnames(minp_mode)=c("id","minp_mode")             

merge5=left_join(cdf,max_eta,by="id");head(merge5)
nrow(merge5)
td5=aggregate(eta~id,data=merge5,FUN="duplicated");head(td5)
un5=unlist(td5$eta);head(un5)
sum(un5)  #eta閲嶅鐨勮鏁?
cd5=merge5[!un5,];head(cd5)
nrow(cd5)
trm5=cd5[,c(1,6)]

maxe_mode=trm5[which(cd5$eta==cd5$max_eta),];maxe_mode=as.data.frame(maxe_mode)
head(maxe_mode)
nrow(maxe_mode)
colnames(maxe_mode)=c("id","maxe_mode")

merge6=left_join(cdf,min_eta,by="id");head(merge6)
nrow(merge6)
td6=aggregate(eta~id,data=merge6,FUN="duplicated");head(td6)
un6=unlist(td6$eta);head(un6)
sum(un6)  #eta閲嶅鐨勮鏁?
cd6=merge6[!un6,];head(cd6)
nrow(cd6)
trm6=cd6[,c(1,6)]

mine_mode=trm6[which(cd6$eta==cd6$min_eta),];mine_mode=as.data.frame(mine_mode)
head(mine_mode)
nrow(mine_mode)
colnames(mine_mode)=c("id","mine_mode")

f_mode=cdf$tr_mode[which(cdf$array.index==1)]
f_mode=as.data.frame(f_mode)
head(f_mode)
nrow(f_mode)



nrow(maxd_mode);nrow(mind_mode);nrow(maxp_mode);nrow(minp_mode);nrow(maxe_mode);nrow(mine_mode);nrow(f_mode)
nrow(max_dis);nrow(min_dis);nrow(mean_dis);nrow(sd_dis)
nrow(max_pri);nrow(min_pri);nrow(mean_pri);nrow(sd_pri)
nrow(max_eta);nrow(min_eta);nrow(mean_eta);nrow(sd_eta)


ndf=left_join(max_dis,min_dis,by="id");ndf=left_join(ndf,mean_dis,by="id")
ndf=left_join(ndf,sd_dis,by="id");ndf=left_join(ndf,max_pri,by="id")
ndf=left_join(ndf,min_pri,by="id");ndf=left_join(ndf,mean_pri,by="id")
ndf=left_join(ndf,sd_pri,by="id");ndf=left_join(ndf,max_eta,by="id")
ndf=left_join(ndf,min_eta,by="id");ndf=left_join(ndf,mean_eta,by="id")
ndf=left_join(ndf,sd_eta,by="id");ndf=left_join(ndf,maxd_mode,by="id");
ndf=left_join(ndf,mind_mode,by="id")
ndf=left_join(ndf,maxp_mode,by="id");ndf=left_join(ndf,minp_mode,by="id")
ndf=left_join(ndf,maxe_mode,by="id");ndf=left_join(ndf,mine_mode,by="id")
f_mode=cbind(ndf[,1],f_mode);colnames(f_mode)=c("id","f_mode")
ndf=left_join(ndf,f_mode,by="id")
nrow(ndf)
head(ndf)

write.csv(ndf,"F:/test/KDD/ndf.csv")

tp=data.frame(sid,plantime,ndf)
nrow(tp)
names(tp$tplans.sid)="sid"
head(tp)




#3. train_clicks鏁版嵁闆?
clicks=read.csv('F:/test/data_set_phase1/train_clicks.csv')
str(clicks)
nrow(clicks)
head(clicks)

sum(is.na(clicks$sid));sum(is.na(clicks$click_time));sum(is.na(clicks$click_mode))
library(mice)
md.pattern(clicks)

sum(is.na(clicks$sid))/nrow(clicks)  #鏁版嵁鐨勫垎甯冪殑姣斾緥,濡傛灉涓嶅敮涓€,涔熷氨鎰忓懗鐫€鍑虹幇浜嗛噸澶嶇殑琛屼负
length(unique(clicks$click_time))/nrow(clicks)
length(unique(clicks$click_mode))/nrow(clicks)
table(clicks$click_mode)

library(ggplot2)
ggplot(clicks)+geom_histogram(aes(x=clicks$click_mode))

type=as.data.frame(table(clicks$click_mode))$Var1
value=as.numeric(as.data.frame(table(clicks$click_mode))$Freq)
cl=data.frame(type=type,value=value)
ggplot(cl,aes(x="",y=value,fill=type))+
  geom_bar(stat="identity",width=1)+
  coord_polar("y",start=0)


#4.profiles
profiles=read.csv("F:/test/data_set_phase1/profiles.csv")
colnames(profiles)
dim(profiles)

inputdata=profiles[,-1]
svdresults=svd(inputdata)
D=diag(svdresults$d);D
dim(D)
U=svdresults$u;U
dim(U)
V=svdresults$v;V
dim(V)

u2=as.matrix(U[, 1:50])
d2=as.matrix(D[1:50, 1:50])
v2=as.matrix(V[, 1:50])
pro=u2 %*% d2 %*% t(v2)
pro=as.data.frame(pro);dim(pro)
colnames(pro)
npro=cbind(profiles$pid,pro)
colnames(npro)[1]="pid"
colnames(npro)


#pc.cr=princomp(inputdata, cor = TRUE)
#summary(pc.cr)
#loadings(pc.cr)



#鍚堝苟鏁版嵁闆?

dim(nq)
dim(tp)
colnames(nq) #queries
colnames(tp)  #train_plans
colnames(clicks)


colnames(profiles)
colnames(tp)[1]="sid"

dat=left_join(nq,tp,by="sid")
dat=left_join(dat,clicks,by="sid")


dim(dat)
head(dat)
colnames(dat)
table(dat$click_mode,useNA = "ifany")
dat$click_mode[is.na(dat$click_mode)]=0
table(dat$click_mode,useNA = "ifany")

write.csv(dat,'F:/test/KDD/dat.csv')

data=dat[,-11]
library(lubridate)
time=data$req_time   #鏃堕棿鐗瑰緛
req_week=wday(time);head(req_week)
req_hour=hour(time);head(req_hour)
newdata=cbind(data,req_week)
newdata=cbind(newdata,req_hour)
head(newdata)
newdata=newdata[,-3]


dim(newdata);colnames(newdata)  #璁粌闆唍ewdata
colnames(profiles)
dim(profiles)
#data=left_join(data,profiles,by="pid")
write.csv(newdata,'F:/test/KDD/newdata.csv')


#xdata=merge(data,npro,by="pid",all.x = T)
#xdata=xdata[,-2]
#colnames(xdata)
#dim(xdata)


