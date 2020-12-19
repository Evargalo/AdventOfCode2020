# DOCUMENTATION
# https://www.irif.fr/~vjuge/af4/automates-1.pdf

source('~/Perso/AdventOfCode/AoC2020/packages.R')

Msg <- read_csv("~/Perso/AdventOfCode/AoC2020/Day19simpleMsg.txt", col_names = FALSE)
Rul <- read_csv("~/Perso/AdventOfCode/AoC2020/Day19simpleRul.txt", col_names = FALSE)

Msg <- read_csv("~/Perso/AdventOfCode/AoC2020/Day19Msg.txt", col_names = FALSE)
Rul <- read_csv("~/Perso/AdventOfCode/AoC2020/Day19Rul.txt", col_names = FALSE)
Rul <- read_csv("~/Perso/AdventOfCode/AoC2020/Day19RulB.txt", col_names = FALSE)

rulNumbers<-mapply(cutAfter,Rul,pattern=":")
Rul[rulNumbers %in% c("110","92"),] -> endpoints
Rul[!rulNumbers %in% c("110","92"),] -> infRUles

# # For simple
# Rul[rulNumbers %in% c("4","5"),] -> endpoints
# Rul[!rulNumbers %in% c("4","5"),] -> infRUles

# For real
endpoints$numbers<-mapply(cutAfter,endpoints$X1,pattern=":")
endpoints$value<-mapply(substr,endpoints$X1,nchar(endpoints$X1)-1,nchar(endpoints$X1)-1)

infRulNumbers<-mapply(cutAfter,infRUles,pattern=":",USE.NAMES = FALSE)

mapply(gsub,"\\s",".",infRUles)->prov1
mapply(gsub,"\\|","£",prov1)->prov2
names(prov2)<-NULL

cutBefore(prov2,":")->prov3
mapply(paste0,prov3,".",USE.NAMES = FALSE)->prov3

# # For simple
# mapply(gsub,".4.",".a.",prov3,USE.NAMES = FALSE)->prov4
# while(any(grepl("5",prov4))){
#   mapply(gsub,".5.",".b.",prov4,USE.NAMES = FALSE)->prov4
# }
# while(any(grepl("4",prov4))){
#   mapply(gsub,".4.",".a.",prov4,USE.NAMES = FALSE)->prov4
# }

# For real
mapply(gsub,".110.",".a.",prov3,USE.NAMES = FALSE)->prov4
while(any(grepl("92",prov4))){
  mapply(gsub,".92.",".b.",prov4,USE.NAMES = FALSE)->prov4
}
while(any(grepl("110",prov4))){
  mapply(gsub,".110.",".a.",prov4,USE.NAMES = FALSE)->prov4
}

# Recherche descendante, en profondeur, à gauche
checkMsg<-function(msg,test){
  # print(test)
  testWODOt<-rmAllDots(test)
  elem<-strsplit(test,"\\.")[[1]]
  if(any(elem %in% rulNumbers)){
    lastRk<-last(which(elem %in% rulNumbers))
    if(lastRk<length(elem)){
      suffix<-paste(elem[(lastRk+1):length(elem)],collapse = "")
      if(suffix!="" & substr(msg,nchar(msg)+1-nchar(suffix),nchar(msg))!=suffix) return(FALSE) 
    }
    rkToChange<-first(which(elem %in% rulNumbers))
    prefix<-paste(elem[1:(rkToChange-1)],collapse = "")
    if(prefix!="" & substr(msg,1,nchar(prefix))!=prefix) return(FALSE)
    
    rule<-prov4[elem[rkToChange]==infRulNumbers]
    newNodes<-data.frame(inside=".",stringsAsFactors = FALSE)
    for(currentRule in strsplit(rule,"£")[[1]]){
      inside<-elem
      inside[rkToChange]<-substr(currentRule,2,nchar(currentRule)-1)
      inside<-paste0(paste(inside,collapse = "."),".")
      newNodes %>% add_row(inside=inside) ->newNodes
    }
    newNodes %>% filter(container!=".")->newNodes
    return(any(mapply(checkMsg,msg,newNodes$inside)))
  }
  else return(testWODOt==msg)
}

k<-1
checkFullTree<-function(msg) {print(k);k<<-k+1;checkMsg(msg,".0.")}

valid<-sapply(X = Msg$X1,FUN = checkFullTree,USE.NAMES = FALSE)
sum(valid)

# A
# 190

# B
# 311




#################################################################
# Construire tout le langage. C'est trop long (au moins 10^6 mots...)
#################################################################

treeData<-data.frame(container="0",inside=".8.11.",treated=FALSE,stringsAsFactors = FALSE)

while(!all(treeData$treated)){
  i<-first(which(!treeData$treated))
  test<-treeData$inside[i]
  elem<-strsplit(test,"\\.")[[1]]
  if(any(elem %in% rulNumbers)){
    rkToChange<-first(which(elem %in% rulNumbers))
    rule<-prov4[elem[rkToChange]==infRulNumbers]
    newNodes<-data.frame(container=".",inside=".",treated=FALSE,stringsAsFactors = FALSE)
    for(currentRule in strsplit(rule,"£")[[1]]){
      inside<-elem
      inside[rkToChange]<-substr(currentRule,2,nchar(currentRule)-1)
      inside<-paste0(paste(inside,collapse = "."),".")
      newNodes %>% add_row(container=test,inside=inside,treated=FALSE) ->newNodes
    }
    newNodes %>% filter(container!=".")->newNodes
    treeData<-treeData %>% bind_rows(newNodes)
  }
  treeData$treated[i]<-TRUE
  if((treeData %>% nrow) %% 1000 == 0) print(treeData %>% nrow)
}

treeData %>% filter(grepl("^[ab\\.]+$*",inside)) %>% 
  select(inside) -> langage
sapply(FUN=rmAllDots, X=langage$inside,USE.NAMES = FALSE)  -> langage
intersect(langage,Msg$X1)
#####################################################################

