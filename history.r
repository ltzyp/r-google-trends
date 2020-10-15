print(df$location)
print(kr[,c(1,4)])
merge( df,kr,by.x="location", all=TRUE)
print(df)
}
df
}
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr1=gr[keyword==key1,1:2]
kr2=gr[keyword==key2,1:2]
#kr0=cbind(kr1[order(kr1$location),],kr2[order(kr2$location),])
kr0=merge(kr1,kr2,by="location",all=TRUE)
kr = cbind(kr0, ratio=kr0$hits.x/kr0$hits.y)
print(df$location)
print(kr[,c(1,4)])
merge( df,kr,by.x="location", all=TRUE)
print(df)
}
df
}
keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="month")
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr1=gr[keyword==key1,1:2]
kr2=gr[keyword==key2,1:2]
#kr0=cbind(kr1[order(kr1$location),],kr2[order(kr2$location),])
kr0=merge(kr1,kr2,by="location",all=TRUE)
kr = cbind(kr0, ratio=kr0$hits.x/kr0$hits.y)
print(df$location)
print(kr[,c(1,4)])
merge( df,kr[,c(1,4)],by="location",all=TRUE)
print(df)
}
df
}
keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="month")
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr1=gr[keyword==key1,1:2]
kr2=gr[keyword==key2,1:2]
#kr0=cbind(kr1[order(kr1$location),],kr2[order(kr2$location),])
kr0=merge(kr1,kr2,by="location",all=TRUE)
kr = cbind(kr0, ratio=kr0$hits.x/kr0$hits.y)
print(df$location)
print(kr[,c(1,4)])
merge( df,kr[,c(1,4)],all=TRUE)
print(df)
}
df
}
keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="month")
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr1=gr[keyword==key1,1:2]
kr2=gr[keyword==key2,1:2]
#kr0=cbind(kr1[order(kr1$location),],kr2[order(kr2$location),])
kr0=merge(kr1,kr2,by="location",all=TRUE)
kr = cbind(kr0, ratio=kr0$hits.x/kr0$hits.y)
print(df$location)
print(kr[,c(1,4)])
merge( df,kr[,c(1,4)],all=TRUE)
print(df)
}
kr
}
res =keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="month")
res
df
merge(df,res,all=TRUE
)
merge(df,res,by.x='location',all=TRUE)
merge(df,res[,c(1,4)],by.x='location',all=TRUE)
df = data.frame(location=character(),stringsAsFactors = FALSE)
df
merge(df,res[,c(1,4)],by.x='location',all=TRUE)
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr1=gr[keyword==key1,1:2]
kr2=gr[keyword==key2,1:2]
#kr0=cbind(kr1[order(kr1$location),],kr2[order(kr2$location),])
kr0=merge(kr1,kr2,by="location",all=TRUE)
kr = cbind(kr0, ratio=kr0$hits.x/kr0$hits.y)
print(df$location)
print(kr[,c(1,4)])
df<-merge( df,kr[,c(1,4)],all=TRUE)
}
df
}
res =keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="month")
timeseq=seq(ISOdate(2020,1,1),by="month",length=3)
timeseq[1]
format(timeseq[1],'YM')
strftime(timeseq[1],'YM')
strftime(timeseq[1],'%y%m')
strftime(timeseq[1],'%yy%m')
strftime(timeseq[1],'%Y%m')
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr1=gr[keyword==key1,1:2]
kr2=gr[keyword==key2,1:2]
#kr0=cbind(kr1[order(kr1$location),],kr2[order(kr2$location),])
kr0=merge(kr1,kr2,by="location",all=TRUE)
kr = cbind(kr0, ratio=kr0$hits.x/kr0$hits.y)
colnames(kr,4)<-strftime(moment,'%Y%m')
print(kr[,c(1,4)])
df<-merge( df,kr[,c(1,4)],all=TRUE)
}
df
}
colnames(kr,2)
colnames(df,2)
colnames(df)
colnames(kr)
colnames(kr,2)<-strftime(Sys.Date(),'%Y%m')
colnames(kr,2)<-'sdfss'
colnames(kr)[2]<-'sdfss'
colnames(kr)[2]<-strftime(Sys.Date(),'%Y%m')
kr
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr1=gr[keyword==key1,1:2]
kr2=gr[keyword==key2,1:2]
#kr0=cbind(kr1[order(kr1$location),],kr2[order(kr2$location),])
kr0=merge(kr1,kr2,by="location",all=TRUE)
kr = cbind(kr0, ratio=kr0$hits.x/kr0$hits.y)
colnames(kr)[4]<-strftime(moment,'%Y%m')
print(kr[,c(1,4)])
df<-merge( df,kr[,c(1,4)],all=TRUE)
}
df
}
res =keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="month")
testTimeSeq=seq(ISOdate(2020,1,1),by="month",length=3)
res =keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="month")
df
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr1=gr[keyword==key1,1:2]
kr2=gr[keyword==key2,1:2]
#kr0=cbind(kr1[order(kr1$location),],kr2[order(kr2$location),])
kr0=merge(kr1,kr2,by="location",all=TRUE)
kr = cbind(kr0, ratio=kr0$hits.x/kr0$hits.y)
colnames(kr)[4]<-strftime(moment,'%Y%m')
#print(kr[,c(1,4)])
df<-merge( df,kr[,c(1,4)],by='location',all=TRUE)
df
}
df
}
res =keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="month")
res
x=.3
y=.6
(x-y)/(x+y)
1+(x-y)/(x+y)
1+(y-x)/(x+y)
1+(y-x)/(x+y)/2
(1+(y-x)/(x+y))/2
(1+(x-y)/(x+y))/2
x=.8
y=1
(1+(x-y)/(x+y))/2
x=90
y=70
(100+(x-y)/(x+y))/2
x=.90
y=.70
(1+(x-y)/(x+y))/2
y=.70
y=70
x=90
(100+(x-y)/(x+y))/2
(100+(x-y)/(x+y))
(x-y)/(x+y)
(1+(x-y)/(x+y))*50
z=c(x,y)
diff(z)
diff(z)/mean(z)
mad(z)
atan(x,y)
arctan(x,y)
atan2(x,y)
atan(x/y)
atan2(x,y)/pi()
pi()
atanpi(x/y)
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr1=gr[keyword==key1,1:2]
kr2=gr[keyword==key2,1:2]
kr0=merge(kr1,kr2,by="location",all=TRUE)
kr = cbind(kr0, ratio=(kr0$hits.x-kr0$hits.y )/(kr0$hits.x+kr0$hits.y)
colnames(kr)[4]<-strftime(moment,'%Y%m')
#print(kr[,c(1,4)])
df<-merge( df,kr[,c(1,4)],by='location',all=TRUE)
df
}
df
}
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr1=gr[keyword==key1,1:2]
kr2=gr[keyword==key2,1:2]
kr0=merge(kr1,kr2,by="location",all=TRUE)
kr = cbind(kr0, ratio=(kr0$hits.x-kr0$hits.y )/(kr0$hits.x+kr0$hits.y))
colnames(kr)[4]<-strftime(moment,'%Y%m')
#print(kr[,c(1,4)])
df<-merge( df,kr[,c(1,4)],by='location',all=TRUE)
df
}
df
}
res =keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="month")
res
testTimeSeq=seq(ISOdate(2018,1,1),by="6 months",length=5)
testTimeSeq
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
moment = timeseq[i]
di = paste(as.Date.POSIXct(seq(moment,by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
#kr1=gr[keyword==key1,1:2]
#kr2=gr[keyword==key2,1:2]
kr0=merge(gr[keyword==key1,1:2],gr[keyword==key2,1:2],by="location",all=TRUE)
kr = cbind(kr0, ratio=(kr0$hits.x-kr0$hits.y )/(kr0$hits.x+kr0$hits.y))
colnames(kr)[4]<-strftime(moment,'%Y%m')
df<-merge( df,kr[,c(1,4)],by='location',all=TRUE)
}
df
}
res =keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="6 months")
res
keyratio_by_region <- function(key1,key2,geo,timeseq,by) {
df = data.frame(location=character(),stringsAsFactors = FALSE)
for ( i in seq_along(timeseq) ) {
di = paste(as.Date.POSIXct(seq(timeseq[i],by=by,length=2)),collapse=" ")
gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
kr0=merge(gr[keyword==key1,1:2],gr[keyword==key2,1:2],by="location",all=TRUE)
kr = cbind(kr0, ratio=(kr0$hits.x-kr0$hits.y )/(kr0$hits.x+kr0$hits.y))
colnames(kr)[4]<-strftime(di[1],'%Y%m')
df<-merge( df,kr[,c(1,4)],by='location',all=TRUE)
}
df
}
res =keyratio_by_region(key1,key2,geo='UA',timeseq=testTimeSeq,by="6 months")
res
keyratio_by_region()
keyratio_by_region
d=data.frame(x(5:9),y(15:19))
d=data.frame(x=(5:9),y=(15:19))
d
apply(d[,'y'],1,function(x){x**2})
apply(d,1,function(x){x**2})
apply(d,2,function(x){x**2})
apply(d[,2],2,function(x){x**2})
apply(d[2,],2,function(x){x**2})
apply(d[2,],1,function(x){x**2})
plot(df)
plot(df,xlim=c(-1,1))
plot(df[1,],xlim=c(-1,1))
plot(df[1,],xlim=c(1,5),ylim=c(-1,1))
plot(df[,1],xlim=c(1,5),ylim=c(-1,1))
df[,1]
df[,2]
df[2,]
df[2,1]
df[5,4]
df
kr
res
plot(res)
plot(res,xlim=c(1,5),ylim=c(-1,1))
plot(res[1,],xlim=c(1,5),ylim=c(-1,1))
res[1,]
res[1,2:6]
plot(res[1,2:6],xlim=c(1,5),ylim=c(-1,1))
plot(res[1,2:6],type='l',xlim=c(1,5),ylim=c(-1,1))
plot(res[1,2:6],type='l',ylim=c(-1,1))
plot(res[1,2:6],type='p',ylim=c(-1,1))
plot(res[1,2:6])
plot(res[1,2:6],type='l')
plot((2:5),type='l')
plot(res[2,2:6],type='l')
plot(c(0.2048193,   0.14 ,-0.06024096, -0.1333333, -0.07526882),type='l')
plot(c(0.2048193,   0.14 ,-0.06024096, -0.1333333, -0.07526882),type='l',color='red')
plot(c(0.2048193,   0.14 ,-0.06024096, -0.1333333, -0.07526882),type='l',color=80)
ggplot(res[2,2:6])
require(ggplot2)
ggplot(res[2,2:6])
ggplot(res)
cars
ggplot(cars)
mtcars
as.factor(mtcars$cyl)
mtcars.cyl <-  as.factor(mtcars$cyl)
mtcars
class(mtcars.cyl)
plot(mtcars)
plot(mpg~hp,data=mtcars.col=cyl)
plot(mpg~hp,data=mtcars,col=cyl)
plot(mpg~hp,data=mtcars,col=cyl,pch= c(4,6,8)[mtcars$cyl],cx=1.2)
plot(mpg~hp,data=mtcars,col=cyl,pch= c(4,6,8)[mtcars$cyl],cex=1.2)
plot(mpg~hp,data=mtcars,col=cyl,cex=1.2)
legend("topright",legend=levels(mtcars$cyl),pch=c(4,6,8),col=levels(mtcars$cyl))
levels[mtcars$cyl]
levels(mtcars$cyl)
levels(mtcars$.cyl)
mtcars$cyl
plot(mpg~hp,data=mtcars,col=cyl,pch= c(4,6,8)[mtcars$cyl],cex=1.2)
legend("topright",legend=levels(mtcars$cyl),pch=c(4,6,8),col=levels(mtcars$cyl))
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl,shape=cyl)+geom_point(size=2)
)
class(mtcars)
data(mtcars,package='datasets')
class(mtcars)
mtcars
class(mtcars.cyl)
str(mtcars)
plot(mpg~hp,data=mtcars,col=cyl,cex=1.2)
legend("topright",legend=levels(mtcars$cyl),pch=c(4,6,8),col=levels(mtcars$cyl))
levels(c(1,2,3,4,5,6,1,3,4))
library(ggplot2)
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl,shape=cyl))+geom_point(size=2)
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl,shape=cyl))
rlang::last_error()
ggplot(mtcars,aes(x=hp,y=mpg))
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl))
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl))+geom_point(size=2)
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl,shape=gear))+geom_point(size=2)
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl, shape=gear))+geom_point(size=2)
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl, shape=am))+geom_point(size=2)
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl, shape=1))+geom_point(size=2)
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl, shape=TRUE))+geom_point(size=2)
ggplot(mtcars,aes(x=hp,y=mpg,color=cyl))+geom_point(size=2)+geom_smooth(method='lm',aes(fill=cyl))
ggplot(mtcars,aes(y=mpg,color=cyl))+geom_point(size=2)
seq_along(5:10)
seq_along(mtcars)
seq_alog(mtcars$mpg)
seq_along(mtcars$mpg)
ggplot(mtcars,aes(x=seq_along(mtcars$mpg),y=mpg,color=cyl))+geom_point(size=2)
ggplot(res,aes(x=seq_along(res[1,]),y=res[2,],color='red'))+geom_point(size=2)
melt(res)
library(reshape2)
install.packages("reshape2")
libary("reshape2")
libary(reshape2)
library(reshape2)
melt(res)
save.image("~/R/source/workspace_reshape.RData")
t(res)
tres=t(res)
ggplot(tres,aes(x=seq_along(tres[,1]),y=res[,2],color='red'))+geom_point(size=2)
class(tres)
tres=as.data.frame(t(res))
ggplot(tres,aes(x=seq_along(res[,1]),y=res[,2],color='red'))+geom_point(size=2)
res[,2]
tres[2,]
tres[,2]
tres[,1]
ggplot(tres,aes(x=seq_along(tres[,2]),y=res[,2],color='red'))+geom_point(size=2)
ggplot(tres,aes(x=seq_along(tres[,2]),y=tres[,2],color='red'))+geom_point(size=2)
ggplot(tres,aes(x=seq_along(tres[,3]),y=tres[,3],color='red'))+geom_point(size=2)
mres=melt(res)
head(mres)
ggplot(mres(aes(variable,value))+geom_line(colour=location)
ggplot(mres(aes(variable,value))+geom_line(aes(colour=location)
)
ggplot(mres,aes(variable,value))+geom_line(aes(colour=location))
ggplot(head(mres),aes(variable,value))+geom_line(aes(colour=location))
mres[order(mres$variable)]
order(mres$variable)
order(mres$location,mres$valiable)
order(mres$location,mres$variable)
mres[order(mres$location,mres$variable),]
mores =mres[order(mres$location,mres$variable),]
ggplot(mores,aes(variable,value))+geom_line(aes(colour=location))
ggplot(mores,aes(variable,value,group=1))+geom_line(aes(colour=location))
ggplot(mres,aes(variable,value,group=1))+geom_line(aes(colour=location))
ggplot(mres,aes(variable,value,group=5))+geom_line(aes(colour=location))
ggplot(head(mres),aes(variable,value,group=5))+geom_line(aes(colour=location))
head(mres)
head(mores)
ggplot(head(mores),aes(variable,value,group=5))+geom_line(aes(colour=location))
head(mores,10)
ggplot(head(mores,10),aes(variable,value,group=5))+geom_line(aes(colour=location))
ggplot(head(mores,10),aes(variable,value,group=1))+geom_line(aes(colour=location))
ggplot(head(mores,10),aes(variable,value))+geom_line(aes(colour=location))
ggplot(head(mores,10),aes(variable,value,group=location))+geom_line(aes(colour=location))
ggplot(mres,aes(variable,value,group=location))+geom_line(aes(colour=location))
mean(c(5,3,4,8,9))
mean(c(5,3,4))
res
res
res[1,]
mean(res[1,])
mean(res[1,2:6])
res[1,2:6]
as.numeric(res[1,2:6])
mean(as.numeric(res[1,2:6]))
mean(as.numeric(res[,2:6]))
mean(as.numeric(res[20,2:6]))
res[,2:6]
mean(res[,2:6])
mean(c(2,NA,4))
mean(c(2,NA,4),na.rm=TRUE)
mean(res[,2:6],na.rm=TRUE)
apply(res[,2:6],function(x )=mean(x)
apply(res[,2:6],function(x ){mean(x)})
apply(res[,2:6],1,function(x ){mean(x)})
res[order(apply(res[,2:6],1,function(x ){mean(x)}))]
res[order(apply(res[,2:6],1,function(x ){mean(x)})),]
ores =res[order(apply(res[,2:6],1,function(x ){mean(x)})),]
res$mean<-apply(res[,2:6],1,function(x ){mean(x)})
res
order(res$mean<-apply(res[,2:6],1,function(x ){mean(x)}))
ores<-res[order(res$mean<-apply(res[,2:6],1,function(x ){mean(x)}))]
ores<-res[order(res$mean<-apply(res[,2:6],1,function(x ){mean(x)})),]
ores<-res[order(res$mean<-apply(res[,2:6],1,function(x ){mean(x)})),]
mean(2:5,trim=0.01)
mean(2:5,trim=1)
mean(2:5/10,trim=0.01)
mean(2:5/10,trim=1)
mean(c(1.2,1.3),trim=1)
mean(c(1.22,1.25),trim=1)
mean(c(1.22,1.25),trim=0.1)
kmeans(c(2,6,3,11,15,1,1,0,20),3)
k=kmeans(c(2,6,3,11,15,1,1,0,20),3)
k
k.cluster
k$cluster
res$means
ores$means
ores
ores$mean
kmean(ores$mean,4)
kmeans(ores$mean,4)
kmean(ores$mean,4,na.rm=TRUE)
kmeans(ores$mean,4,na.rm=TRUE)
k=kmeans(c(2,6,3,11,15,NA,1,1,0,20),3)
k=kmeans(na.omit(c(2,6,3,11,15,NA,1,1,0,20)),3)
k$cluster
kmeans(na.omit(c(2,6,3,11,15,NA,1,1,0,20)),3)
kmeans(na.omit(ores$mean),4)
kmeans(na.omit(ores$mean),5)
kmeans(na.omit(ores$mean),5)$cluster
ores$cluster<-kmeans(na.omit(ores$mean),5)$cluster
ores$mean
ores$mean[is.na<-1]
ores$mean[is.na]<-1
ores$mean
ores<-res[order(res$mean<-apply(res[,2:6],1,function(x ){mean(x)})),]
ores$mean
ores[is.na]
ores[ores$mean.is.na]
ores[is.na(ores$mean)]
ores[is.na(ores$mean),]
ores$mean[is.na(ores$mean),]
ores[is.na(ores$mean),]$mean
ores[is.na(ores$mean),]$mean<-1
ores
kmeans(ores$mean,5)
ores$cluster=kmeans(ores$mean,5)$cluster
ores
ores$cluster=kmeans(ores$mean,6)$cluster
ores
ggplot(mres,aes(variable,value,group=location))+geom_line(aes(colour=ores$cluster[location]))
ores[8,]
ores[,8]
ggplot(mres,aes(variable,value,group=location))+geom_line(aes(colour=ores[location,8]))
savehistory("~/R/source/history.r")
