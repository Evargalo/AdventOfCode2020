source('~/Perso/AdventOfCode/AoC2020/packages.R')

Day14 <- read_table2("~/Perso/AdventOfCode/AoC2020/Day14.txt", col_names = FALSE)
Day14 <- read_table2("~/Perso/AdventOfCode/AoC2020/Day14simple2.txt", col_names = FALSE)

mask<-Day14$X3[1]
m<-nchar(mask)
X1<-Day14$X1[2]
X3<-Day14$X3[2]

Day14 %>% select(X1,X3) -> Day14

BinToDec <- function(x, charactersForOnes) sum(2^(which(rev(unlist(strsplit(as.character(x), "")) %in% charactersForOnes))-1))

# A

# Registre mémoire
values<-data.frame(rank=c(-1),val=0)

updateMask<-function(X3){
  mask<<-unlist(strsplit(as.character(X3), ""))
}

sumBits<-function(binVal,maskBit){
  ifelse(maskBit=="X", binVal, maskBit)
}

applyMask<-function(X3){
  #l<-nchar(X3)
  #binVal<-unlist(strsplit(as.character(X3), ""))
  binVal<-unlist(strsplit(bits(as.integer(X3)), ""))
  l<-length(binVal)
  if(m>l) binVal<-c(rep('0',times=m-l),binVal)
  valBits<-mapply(sumBits,binVal,mask)
  BinToDec(valBits,"1")
}

execMask<-function(X1,X3){
  if(X1=="mask"){
    updateMask(X3)
  }
  else{
    adress<-as.integer(substring(X1,5,nchar(X1)-1))
    val<-applyMask(X3)
    values %>% filter(rank!=adress) %>% add_row(rank=adress,val=val) ->> values
  }
}
  
mapply(execMask,Day14$X1,Day14$X3)

values %>% filter(rank>0) %>% summarise(sum(val))
# 17028179706934

# B

Day14 <- read_table2("~/Perso/AdventOfCode/AoC2020/Day14simple2.txt", col_names = FALSE)
Day14 <- read_table2("~/Perso/AdventOfCode/AoC2020/Day14.txt", col_names = FALSE)

mask<-Day14$X3[1]
m<-nchar(mask)

Day14 %>% select(X1,X3) -> Day14

# registre mémoire
values<-data.frame(rank=c(-1),val=0)

updateMask<-function(X3){
  mask<<-unlist(strsplit(as.character(X3), ""))
  allMaskValues<<-c(BinToDec(mask,"1"))
  for(rk in which(mask=="X")){
    allMaskValues<<-c(allMaskValues,allMaskValues+2^(m-rk))
  }
}

# All the adresses to be written
calcAdress<-function(binAdr){
  binAdr[which(mask%in% c("X","1"))]<-0
  BinToDec(binAdr,"1")+allMaskValues
}

execMask<-function(X1,X3){
  if(X1=="mask"){
    updateMask(X3)
  }
  else{
    binAdr<-bits(as.integer(substring(X1,5,nchar(X1)-1)))
    l<-nchar(binAdr)
    binAdr<-unlist(strsplit(binAdr,""))
    if(m>l) binAdr<-c(rep('0',times=m-l),binAdr)
    newAdress<-calcAdress(binAdr)
    values %>% filter(!(rank %in% newAdress)) %>% 
      bind_rows(data.frame(rank=newAdress,val=as.integer(X3))) ->> values
  }
}

mapply(execMask,Day14$X1,Day14$X3)

values %>% filter(rank>0) %>% summarise(sum(val))

# 3683236147222
