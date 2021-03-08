source('setup.r')

diff_transformer <-  function(a,b) { (a-b)/(a+b)  }
default_transformer <-  function(a,b) { b/(a+b)  }


GT_LocationsHistory <- function(words,geo,timeseq,func=default_transformer) {
  func_ <- function(a,b) {tryCatch( func(a,b) ,error=function(e) NA   ) }
  tf <- TI_frame(timeseq) 
  df <- data.frame(timeslot=character(),location=character(),value=numeric() )
  attr(df,'wordList') <- words
  for ( i in seq_along(tf[,1]) ) {
    di <-  paste(format(tf[i,],'%Y-%m-%d'),collapse=" ")
    gt <-  gtrends(keyword=words,geo=geo,time=di)$interest_by_region
    gt$keyword <- gsub('\\+','_', gsub('\\s+','', gt$keyword))
    fr <-  dcast(gt,'location~keyword',value.var='hits')
    fr$value <-  func_( fr[[2]], fr[[3]])
    fr$timeslot <- format(tf[i,1],degree.get_format(tf))
    df<-rbind(df, fr[,c('timeslot','location','value')])
  };  
  class(df) <- c('GT_LocationsHistory',class(df))
  return(df)
}

GT_LocationsClusterized <-  function(df,numClusters=5) {
  mtrx<-dcast(df,location~timeslot,value.var='value')                #todo -names  
  # mtrx$means<-apply(mtrx[,2:length(mtrx)],1,function(x){m <-  mean(x); ifelse(is.na(m),1,m)  })
  #  mtrx[is.na(mtrx$means),]$means<-1
#  mtrx$clusters <- 1                                                  #todo -where this values must be calculated
#  mtrx$clusters<-as.factor(kmeans(mtrx$means,numClusters)$cluster)
  mtrx <- na.omit(mtrx)
  ncols <- c(2:ncol(mtrx))
  km <- kmeans(mtrx[ncols],numClusters,nstart=64)
  mtrx$clusters<-as.factor(km$cluster)
  mtrx$rank<-0 
  for ( lev in levels(mtrx$clusters)) {
    i =1;
    sbst <- mtrx[mtrx$clusters==lev,]
    for (loc in sbst[order(sbst[,tail(ncols,1)]),]$location) { 
      mtrx[mtrx$location==loc,]$rank<-(i=i+1) 
    }
  }
  location <- paste('cluster',rownames(km$centers))
  rank <-1 
  clusters <- as.integer(rownames(km$centers))
  c <-cbind(location,km$centers,clusters,rank)
    mtrx <- rbind(mtrx,c)
  mtrx$rank<-as.factor(mtrx$rank)
  lc <- melt(mtrx,c("location","clusters","rank"))
  lc$value <- as.numeric(lc$value)
  class(lc) <- c('GT_LocationsClusterized',class(lc))
  return(lc)
}

plot.GT_LocationsClusterized <- function(mtrx,yl = c(0,1) ) {
  ggplot(mtrx,aes(variable,value,Adjusted,group=location,colour=clusters)) +
    scale_linetype_manual(values=c(1,c(2:6),c(2:6))) + 
    scale_size_manual(values=c(2,rep(.3,5),rep(.4,5))) + 
    scale_alpha_manual(values=c(.2,rep(1,5),rep(.7,5))) +
    geom_line(aes(colour=clusters,linetype=as.factor(rank),alpha=as.factor(rank),size=as.factor(rank) ))  +
    theme(legend.position='none') +
    labs(title='UA',x='year',y='percent' )+
#    theme(axis.text.y=element_blank())+
    scale_y_continuous()+
    ylim(yl) +
    scale_color_discrete(guide='none')+             
#    geom_dl(aes(label=paste(clusters,rank)),method=list(dl.combine('first.bumpup','last.bumpup'),cex=0.7) )     
    geom_dl(aes(label=abbreviate(location,3)),method=list(dl.combine('first.bumpup','last.bumpup'),cex=0.7) )     
}

plot.GT_LocationsHistory <- function( lh,numClusters =6 ) {
  lc <- GT_LocationsClusterized( lh, numClusters )   
  plot(lc)
}
