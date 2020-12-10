source("packages.R")
# source("usefulFunctions.R")

day10A <- read_table2("day10A.txt", col_names = FALSE)
adapters<-day10A$X1

# A
sort(adapters)->adapters
lags<-adapters-lag(adapters)
lags[1]<-adapters[1]
(sum(lags==3)+1)*(sum(lags==1))
#2380

# B
lags<-c(lags,3)   
# there are only gaps of 1 or 3
threes<-c(0,which(lags==3)) # add the plug (mandatory)
consecOnes<-threes-lag(threes)-1 # first one is NA
consecOnes<-consecOnes[2:length(consecOnes)]

possib<-function(x) case_when(x==1 ~ 1,
                              x==2 ~ 2,
                              x==3 ~ 4,
                              TRUE ~ possib(x-3)+possib(x-2)+possib(x-1))

possibVect<-unlist(sapply(X =(1:10),FUN = possib))
prod(possibVect[consecOnes])
# ou bien prod(unlist(sapply(X = consecOnes,FUN = possib)))
# 48358655787008
