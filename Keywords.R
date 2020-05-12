#####################################################
### Code for getting schedules and keywords ready ###
#####################################################

library(dplyr)
library(readxl)
library(foreach)

### NOTE - for special characters, the RDS must be made from an XLSX file!!!
X <- read_excel("WSTC5 Final Schedule_Grant_new.xlsx", sheet=1)
Z <- read_excel("WSTC5 Final Schedule_Grant_new.xlsx", sheet=2)

#for(tt in unique(X$Session)){
keywords.out <- foreach(tt = unique(X$Session),.combine='rbind') %do% {  
  Y <- X[X$Session == tt,]
  
  
  out <- foreach(m = 1:nrow(Y),.combine='rbind') %do%{
    out1 <- foreach(i = strsplit(Y$Keywords[m],' ;')[[1]],.combine='rbind') %do%{
      j <-gsub(' ','',i)
      xx <- data.frame(j)
      names(xx)<-c('yy')
      return(xx)
    }
    
    return(out1)  
    
  }
  out <- unique(out$yy) 
  KEYS <- paste(unlist(out,use.names = FALSE),collapse=' ')
  KEYS <- gsub(x=KEYS,pattern='#Seabirds ',replacement = '')
  
}


Z$Keywords <- keywords.out

saveRDS(Z,'WSTC5_Overview.rds')
saveRDS(X,'WSTC5_Abstracts.rds')



#### list of keywords

out <- foreach(m = 1:nrow(X),.combine='rbind') %do%{
  out1 <- foreach(i = strsplit(X$Keywords[m],' ;')[[1]],.combine='rbind') %do%{
    j <-gsub(' ','',i)
    xx <- data.frame(j)
    names(xx)<-c('yy')
    return(xx)
  }
  
  return(out1)  
  
}


yy <- unique(out$yy)
yy <- as.character(yy[!is.na(yy)])
yy <- yy[order(substr(yy, start = 2, stop = max(nchar(yy))))]
yy <- yy[yy!='#Seabirds']
yy <- data.frame(keywords = yy)

saveRDS(yy,'WSTC5_keywords.rds')








