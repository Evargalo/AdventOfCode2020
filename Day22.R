source('~/Perso/AdventOfCode/AoC2020/packages.R')

P1 <- read_delim("~/Perso/AdventOfCode/AoC2020/day22P1.txt", 
                 "µ", escape_double = FALSE, col_names = TRUE, 
                 trim_ws = TRUE)

P2 <- read_delim("~/Perso/AdventOfCode/AoC2020/day22P2.txt", 
                 "µ", escape_double = FALSE, col_names = TRUE, 
                 trim_ws = TRUE)

P1<-P1 %>% unlist

P2<-P2 %>% unlist

###############################################
# A

# NB: The player with the highest card will always win
# But we need to find the arrangement of his deck at the end

rmFirst<-function(v){
  index<-1:length(v)
  v[index>1]
}

play<-function(){
  if(length(P1)==0) {print("P2 wins"); fini<<-TRUE; return()}
  if(length(P2)==0) {print("P1 wins"); fini<<-TRUE; return()}
  if(P1[1]>P2[1]){
    P1<<-c(rmFirst(P1),P1[1],P2[1])
    P2<<-rmFirst(P2)
  }
  else{
    P2<<-c(rmFirst(P2),P2[1],P1[1])
    P1<<-rmFirst(P1)
  }
  play()
}

play()

sum(P1 * (length(P1):1))
# 32472

###############################################
# B

# P1<-c(9,2,6,3,1)
# P2<-c(5,8,4,7,10)

stringFromVect<-function(v){paste(v,collapse = "-")}

play<-function(){
  print(paste("match",stringFromVect(P1),"vs",stringFromVect(P2)))
  if(length(P1)==0) {print("P2 wins"); fini<<-TRUE; return()}
  if(length(P2)==0) {print("P1 wins"); fini<<-TRUE; return()}
  
  if((length(P1)>P1[1]) & (length(P2)>P2[1])){
    winner<-subGame(rmFirst(P1)[1:P1[1]],rmFirst(P2)[1:P2[1]],list())
  }
  else{
    if(P1[1]>P2[1]){winner<-1}
    if(P1[1]<P2[1]){winner<-2}
  }
  if(winner==1){
    P1<<-c(rmFirst(P1),P1[1],P2[1])
    P2<<-rmFirst(P2)
  }
  if(winner==2){
    P2<<-c(rmFirst(P2),P2[1],P1[1])
    P1<<-rmFirst(P1)
  }
  
}

subGame<-function(P1,P2,previousStates){
#  print(paste("submatch",stringFromVect(P1),"vs",stringFromVect(P2)))
  P1string<-paste(P1,collapse = "-")
  if(P1string %in% previousStates){return(1)}
  previousStates<-c(previousStates,P1string)
  if(length(P1)==0) {return(2)}
  if(length(P2)==0) {return(1)}
  if((length(P1)>P1[1]) & (length(P2)>P2[1])){
    winner<-subGame(rmFirst(P1)[1:P1[1]],rmFirst(P2)[1:P2[1]],list())
  }
  else{
    if(P1[1]>P2[1]){winner<-1}
    if(P1[1]<P2[1]){winner<-2}
  }
  if(winner==1){
    P1<-c(rmFirst(P1),P1[1],P2[1])
    P2<-rmFirst(P2)
  }
  if(winner==2){
    P2<-c(rmFirst(P2),P2[1],P1[1])
    P1<-rmFirst(P1)
  }
  return(subGame(P1,P2,previousStates))
}
  
previousStates<-list()
fini<-FALSE
i<-0
while(i<1000 & !fini){
    i<-i+1
    print(paste("round",i,"of main game"))
    play()
    P1string<-stringFromVect(P1)
    if(P1string %in% previousStates){print("P1 wins"); break}
    previousStates<<-c(previousStates,P1string)
}
P1  
P2
sum(P1 * (length(P1):1))
# 36463
