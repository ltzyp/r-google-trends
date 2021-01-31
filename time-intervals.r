#todo syntax Y~[Y]&[seq]
#todo add granularity attribute to output
TI_frame <- function(timeseq,until_now=FALSE) {
  if(typeof(timeseq)=='character') timeseq <- TI_parse_time(timeseq)
  df <- data.frame(starts = head(timeseq,-1),stops = tail(timeseq,-1))
  return( degree.from(df,timeseq) )
}


TI_parse_time <- function(string) {
  str_to_date <-   function(ch) {
    num <- unlist( strsplit(ch,'.',fixed=TRUE))
    len <- length(num)
    full<- c(num,tail(c('1','1','0','0','0'),6-len))
    degree.set( do.call('ISOdate',as.list(full)),len )
  }
    tokens <- strsplit(strsplit(string,'<~',fixed=TRUE)[[1]],c(':','*'),fixed=TRUE)
    tokens <- lapply(tokens,function(x) { c( x,rep(NA,2-length(x))) })
    start <- str_to_date(tokens[[1]][1])
    stop <- str_to_date(tokens[[1]][2])
    ab <-  attr(start,'granularity')
    step <-( if (!is.na(tokens[[2]][2])) {
         sprintf('%s %s',tokens[[2]][2],degree.name(start))
    }  else NA)
    params <- list(from=start,
                   to=stop,
                   length=as.integer(tokens[[2]][1]),
                   by=step 
    )
   degree.from( do.call('seq',params[!is.na(params)]), start)
}

degrees<-c('Year','month','day','Hour','Min','Sec')
degree <- function( obj ) {
  return(attr(obj ,'degree' ) )
}

degree.get_format <- function( obj ) {
  a = attr(obj,'degree')
  l <- degrees[1:match(degree(obj),degrees)]
  paste('%',substr(l,1,1),sep='',collapse='')
}

degree.set <- function(obj,pos) {
  attr(obj,'degree')<-degrees[pos]
  return(obj)
}

degree.name <- function(obj) {
  tolower(attr(obj,'degree'))
}

degree.from <- function(dst,src) {
  attr(dst,'degree')<-attr(src,'degree')
  return(dst)
  }
