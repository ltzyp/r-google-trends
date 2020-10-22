TI_frame <- function(timeseq,until_now=FALSE) {
  data.frame(starts = head(timeseq,-1),stops = tail(timeseq,-1))

TI_parse_time <- function(string) {
    tokens <- strsplit(string,'<~',fixed=TRUE)[[1]]
     dateTokens  <- strsplit(strsplit(tokens[1],':')[[1]],'.',fixed = TRUE)
     factorTokens<- strsplit(tokens[2],'*',fixed=TRUE)[[1]]
     tokens <- list(from=dateTokens[1][[1]],to=dateTokens[2][[1]],length=factorTokens[1],by=factorTokens[2])
     parsed <-tokens[sapply(tokens,function(x ) { length(x[nchar(x)])>0 && !is.na(x) } )]
     if ( (numTokens<- length(parsed)) <3 ) stop(sprintf("not enough agruments need 3 found %s ",numTokens) )
     if ( (numTokens<- length(parsed)) >3 ) stop(sprintf("too much agruments need 3 found %s ",numTokens) )
  # todo   if ( (numTokens<- length(parsed[2:4])) <>2 ) stop(sprintf("just 2 agrs except 1st must be defined ",numTokens) )
     granularity <- c('year','month','day','hour','min','sec')[length(parsed$from)]
     parsed$from <- do.call('ISOdate',as.list(parsed[[1]]))
#     parsed$length<-as.integer(parsed$length[1])
     parsed$by<-sprintf("%s %s",parsed$by[1], granularity)
     parsed$to <- do.call('ISOdate',as.list(parsed$to))
     print(parsed)
 
         return( do.call('seq',parsed) )
}
