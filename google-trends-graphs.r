source('time-intervals.r')
require('gtrendsR')
require('reshape2')
require('ggplot2')
default_transformer <-  function(a,b) { (a-b)/(a+b)  }

GT_LocationsHistory <- function(words,geo,timeseq,func=default_transformer) {
    func_ <- function(a,b) {tryCatch( func(a,b) ,error=function(e) NA   ) }
    tf <- TI_frame(timeseq) 
    df <- data.frame(timeslot=character(),location=character(),value=numeric() )
    attr(df,'wordList') <- words
    for ( i in seq_along(tf[,1]) ) {
        di <-  paste(format(tf[i,],'%Y-%m-%d'),collapse=" ")
        gt <-  gtrends(keyword=words,geo=geo,time=di)$interest_by_region
        fr <-  dcast(gt,'location~keyword',value.var='hits')
        fr$value <-  func_( fr[[words[1]]], fr[[words[2]]])
        fr$timeslot <- format(tf[i,1],"%Y%m")
        df<-rbind(df, fr[,c('timeslot','location','value')])
    };  
    class(df) <- c('GT_LocationsHistory',class(df))
    return(df)
}

GT_LocationsClusterized <-  function(df,numClusters=6) {
  mtrx<-dcast(df,location~timeslot,value.var='value')                #todo -names  
  mtrx$means<-apply(mtrx[,2:length(mtrx)],1,function(x){m <-  mean(x); ifelse(is.na(m),1,m)  })
#  mtrx[is.na(mtrx$means),]$means<-1
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
  lc <- melt(mtrx,c("location","means","clusters","rank"))
  class(lc) <- c('GT_LocationsClusterized',class(lc))
  return(lc)
}

plot.GT_LocationsClusterized <- function(mtrx ) {
     ggplot(mtrx,aes(variable,value,Adjusted,group=location,colour=clusters)) +
       geom_line(aes(colour=clusters,linetype=as.factor(rank)))  +
        theme(legend.position='none') +
        labs(title='UA',x='year',y='percent' )+
        scale_color_discrete(guide='none')+             
        geom_dl(aes(label=substr(location,1,3)),method=list(dl.combine('first.bumpup','last.bumpup'),cex=0.7) )     
}

plot.GT_LocationsHistory <- function( lh,numClusters =6 ) {
  lc <- GT_LocationsClusterized( lh, numClusters )   
  plot(lc)
}

