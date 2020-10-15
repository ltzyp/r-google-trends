ratioForKeywords function(key1,key2,geo,timeseq,by) {
    df = data.frame(timeslot=character(),location=character(),ratio=numeric() )
    for ( i in seq_along(timeseq) ) {
        di = paste(as.Date.POSIXct(seq(timeseq[i],by=by,length=2)),collapse=" ")
        gr = gtrends(keyword=c(key1,key2),geo=geo,time=di)$interest_by_region
        kr0=merge(gr[keyword==key1,1:2],gr[keyword==key2,1:2],by="location",all=TRUE)
        kr = cbind(kr0, ratio=(kr0$hits.x-kr0$hits.y )/(kr0$hits.x+kr0$hits.y))
        kr$timeslot <- strftime(di[1],"%Y%m")
        k <- kr[,c(5,1,4)]
        df<-rbind(df,k)
    } 
    df
}
function(key1,key2,geo,timeseq,by) {
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
function(key1,key2,geo,time) {
    gr = gtrends(keyword=c(key1,key2),geo=geo,time=time)$interest_by_region
    merge(gr[keyword==key1,1:2],gr[keyword==key2,1:2],by='location')
}

clusterize <-  function(df,numClusters=6) {
  mtrx<-dcast(df,location~timeslot,value.var='ratio')                #todo -names  
  mtrx$means<-apply(mtrx[,2:length(mtrx)],1,function(x){mean(x)})
  mtrx[is.na(mtrx$means),]$means<-1
  mtrx$clusters <- 1                                                  #todo -where this values must be calculated
  mtrx$clusters<-as.factor(kmeans(mtrx$means,numClusters)$cluster)
  mtrx$rank<-0 
  for ( lev in levels(mtrx$clusters)) {
    i =0;
    for (loc in mtrx[mtrx$clusters==lev,]$location) { 
      mtrx[mtrx$location==loc,]$rank<-(i=i+1) 
    }
  }
  mtrx$rank<-as.factor(mtrx$rank)
  melt(mtrx,c("location","means","clusters","rank"))
}

draw <- function(mtrx ,  group,cluster, rank ,variable, value) {
  ggplot(mtrx,aes(variable,value,group=location))+geom_line(aes(colour=cluster,linetype=rank))
} 
