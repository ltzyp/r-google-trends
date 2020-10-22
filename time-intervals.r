TI_frame <- function(timeseq,until_now=FALSE) {
  data.frame(starts = head(timeseq,-1),stops = tail(timeseq,-1))

TI_parse_time <- function(string) {
    tokens <- strsplit(string,'<~',fixed=TRUE)[[1]]
     dateTokens  <- strsplit(strsplit(tokens[1],':')[[1]],'.',fixed = TRUE)
     factorTokens<- strsplit(tokens[2],'*',fixed=TRUE)[[1]]
     tokens <- list(start=dateTokens[1][[1]],stop=dateTokens[2][[1]],times=factorTokens[1],step=factorTokens[2])
     parsed <-tokens[sapply(tokens,function(x ) { length(x[nchar(x)])>0 && !is.na(x) } )]
     if ( (numTokens<- length(parsed)) <3 ) stop(sprintf("not enough agruments need 3 found %s ",numTokens) )
     if ( (numTokens<- length(parsed)) >3 ) stop(sprintf("too much agruments need 3 found %s ",numTokens) )
  # todo   if ( (numTokens<- length(parsed[2:4])) <>2 ) stop(sprintf("just 2 agrs except 1st must be defined ",numTokens) )
     start <- do.call('ISOdate',as.list(parsed[[1]]))
     granularity <- 'day'
    # print(parsed)
    if ( is.null(parsed$stop) ) {
      length<-as.integer(parsed$times[1])
      by<-sprintf("%s %s",parsed$step[1], granularity)
      return( seq(start, length=length, by=by))
    }
     if ( is.null(parsed$times) ) {
       by<-sprintf("%s %s",parsed$step[1], granularity)
       stop <- do.call('ISOdate',as.list(parsed$stop))
       return( seq(start, to=stop, by=by))
     }
     if ( is.null(parsed$step) ) {
       stop <- do.call('ISOdate',as.list(parsed$stop))
       length<-as.integer(parsed$times[1])
       return( seq(start, to=stop, length=length))
     }
}
