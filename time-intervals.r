TI_frame <- function(timeseq,until_now=FALSE) {
  if(typeof(timeseq)=='character') timeseq <- TI_parse_time(timeseq)
  data.frame(starts = head(timeseq,-1),stops = tail(timeseq,-1))
}

TI_parse_time <- function(string) {
  str_to_date <-   function(ch) {
    num <- unlist( strsplit(ch,'.',fixed=TRUE))
    len <- length(num)
    full<- c(num,tail(c('1','1','0','0','0'),6-len))
    dat <- do.call('ISOdate',as.list(full))
    attr(dat,'granularity')<-c('year','month','day','hour','min','sec')[len]
    return(dat)
  }
    tokens <- strsplit(strsplit(string,'<~',fixed=TRUE)[[1]],c(':','*'),fixed=TRUE)
    tokens <- lapply(tokens,function(x) { c( x,rep(NA,2-length(x))) })
    start <- str_to_date(tokens[[1]][1])
    step <-( if (!is.na(tokens[[2]][2])) {
         sprintf('%s %s',tokens[[2]][2],attr(start,'granularity'))
    }  else NA)
    params <- list(from=start,
                   to=str_to_date(tokens[[1]][2]),
                   length=as.integer(tokens[[2]][1]),
                   by=step 
    )
   do.call('seq',params[!is.na(params)])
}
