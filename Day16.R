source('~/Perso/AdventOfCode/AoC2020/packages.R')

rules <- read_table2("~/Perso/AdventOfCode/AoC2020/Day16rules.txt", col_names = FALSE)
nearbyT <- read_csv("~/Perso/AdventOfCode/AoC2020/day16nearbyTickets.txt", col_names = FALSE)


# A

rules %>% mutate(max1=cutBefore(X3,'-'),
                 min1=cutAfter(X3,'-'),
                 max2=cutBefore(X5,'-'),
                 min2=cutAfter(X5,'-'),
) -> rules

range<-c(0)
addRange<-function(min1,min2,max1,max2){
  range<<-unique(c(range,min1:min2,max1:max2))
}

mapply(addRange,rules$min1,rules$min2,rules$max1,rules$max2)

range<-sort(range)

myT<-c(173,191,61,199,101,179,257,79,193,223,139,97,83,197,251,53,89,149,181,59)
myT %in% range

wrong<-c(0)
wrongTickets<-c(0)

nTickets<-nrow(nearbyT)
DF<-t(nearbyT)
for(i in 1:nTickets){
  vect<-DF[,i]
  wrong<-c(wrong,vect[!(vect %in% range)])
  wrongTickets<-c(wrongTickets,i)
}
sum(wrong)
# 21996


# B

rules <- read_table2("~/Perso/AdventOfCode/AoC2020/Day16rulesSimple.txt", col_names = FALSE)
nearbyT <- read_csv("~/Perso/AdventOfCode/AoC2020/day16nearbyTicketsSimple.txt", col_names = FALSE)

rules <- read_table2("~/Perso/AdventOfCode/AoC2020/Day16rules.txt", col_names = FALSE)
nearbyT <- read_csv("~/Perso/AdventOfCode/AoC2020/day16nearbyTickets.txt", col_names = FALSE)

myT<-c(173,191,61,199,101,179,257,79,193,223,139,97,83,197,251,53,89,149,181,59)

rules %>% mutate(max1=cutBefore(X3,'-'),
                 min1=cutAfter(X3,'-'),
                 max2=cutBefore(X5,'-'),
                 min2=cutAfter(X5,'-'),
) -> rules

# does number x respect rule i ? 
valid<-function(x,i){
  rule<-rules[i,]
  x %in% c(rule$min1:rule$max1,rule$min2:rule$max2)
}

nTickets<-nrow(nearbyT)
DF<-t(nearbyT)

range<-c(0)
addRange<-function(min1,min2,max1,max2){
  range<<-unique(c(range,min1:min2,max1:max2))
}
mapply(addRange,rules$min1,rules$min2,rules$max1,rules$max2)
range<-sort(range)

wrong<-c(0)
wrongTickets<-c(0)

wrongTickets<-c(0)
for(i in 1:nTickets){
  vect<-DF[,i]
  if(any(!(vect %in% range)))  wrongTickets<-c(wrongTickets,i)
}
nearbyT <- nearbyT %>% mutate(numTicket=1:nTickets) %>% filter(! (numTicket %in% wrongTickets))

nTickets<-nrow(nearbyT)
nbRules<-nrow(rules)

possibMat<-matrix(data = TRUE,nrow = nbRules,ncol=nbRules)
for(i in 1:nbRules){
  for(j in 1:nbRules){
    for(k in 1:nTickets){
      print(paste("i=",i,"j=",j,"k=",k))
      if(!(valid(nearbyT[k,j],i))) possibMat[i,j]<-FALSE
    }
  }
}

rules$pos<-0
notFoundRules<-1:nbRules
possibMat<-t(possibMat)
found<-TRUE
while(found){
  found<-FALSE
  for(i in notFoundRules){
    if(sum(possibMat[,i])==1){
      j<-which(possibMat[,i])
      possibMat[j,]<-FALSE
      possibMat[j,i]<-TRUE 
      rules$pos[i]<-j
      notFoundRules<-notFoundRules[notFoundRules!=i]
      found<-TRUE
    }
  }
}

rules %>% filter(X1=="departure") %>% select(pos) ->dep
prod(myT[dep$pos])

# 650080463519 


