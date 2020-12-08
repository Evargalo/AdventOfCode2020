source("packages.R")

day08A <- read_table2("day08A.txt", col_names = FALSE)

# A
ExecProg<-function(){
  accum<-0
  index<-1
  positions<-c()
  while(!(index%in%positions) & (index<611)){
    positions<-c(positions,index)
    action<-day08A$X1[index]
    x<-day08A$X2[index]
    if(action== "nop") {
      index<-index+1
    }
    if(action== "acc"){
      accum<-accum+x
      index<-index+1
    }
    if(action== "jmp"){
      index<-index+x
    }
  }
  if(index>610) return(c(1,accum))
  else return(c(0,accum))
}
ExecProg()
# 1217

# B
positionsFirstTry<-positions

pos<-1
try<-0
res<-0
found<-0
test<-c()
while((!found) & (pos<189)){
  try<-positionsFirstTry[pos]
  action<-day08A$X1[try]
  if(action!="acc"){
    if(action=="nop") day08A$X1[try]<-"jmp"
    if(action=="jmp") day08A$X1[try]<-"nop"
    test<-ExecProg()
    found<-test[1]
    if(found) res<-(test[2])
    if(action=="nop") day08A$X1[try]<-"nop"
    if(action=="jmp") day08A$X1[try]<-"jmp"
  }
  pos<-pos+1
}
res
# 501

