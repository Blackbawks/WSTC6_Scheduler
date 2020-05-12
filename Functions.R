############################################################
### Functions for converting from XLSX to format for app ###
############################################################
#### 
## Run this to update the files 
##


require(tidyverse)
library(readxl)
library(foreach)
require(googlesheets4)

# Converted directly from the Google spreadsheet
X <- readxl::read_xlsx("WSTC6_Final_Schedule.xlsx", sheet=1)

X <- X %>% dplyr::filter(!is.na(Timezone),!is.na(First))

data.out <- foreach(tt = unique(X$Session),.combine='rbind') %do% {  
  Y <- X[X$Session == tt,]
  ntalks <- nrow(Y)
  tstart <- substr(strsplit(as.character(Y$Time[1])," ")[[1]][2],1,5)
  tend <- substr(strsplit(as.character(Y$Time[nrow(Y)])," ")[[1]][2],1,5)
  Day <- as.character(format(Y$Date[1],format="%b %d"))
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
  
  dfout <- data.frame(Date = Day,Start=tstart,End=tend,
                      Session = tt, Talks=ntalks,Keywords=KEYS)
  
  
}

saveRDS(data.out,'WSTC6_Overview.rds')
saveRDS(X,'WSTC6_Abstracts.rds')


###########
# Get list of keywords

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

saveRDS(yy,'WSTC6_keywords.rds')

