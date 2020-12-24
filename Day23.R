source('~/Perso/AdventOfCode/AoC2020/packages.R')

input<-"389125467"
input<-"463528179"
cups<-strsplit(input,"") %>% unlist %>% as.numeric

###########################################################
# Correct solution : store the follower of each cup
###########################################################

circle<-function(x) ( (x-1) %% l ) + 1 

#A
l<-9
nextCup<-c(7,8,5,6,2,3,9,1,4)
index<-4

play<-function(){
  val<<-nextCup[index]
  plus3<-nextCup[nextCup[val]]
  toCut<-c(val,nextCup[val],plus3)
  dest<-circle(index-1)
  while(dest %in% toCut) {dest<-circle(dest-1)}
  nextCup[index]<<-nextCup[plus3]
  nextCup[plus3]<<-nextCup[dest]
  nextCup[dest]<<-val
  index<<-nextCup[index]
}

for(i in 1:100){play()}

# B
l<-1000000
nextCup<-c(7,8,5,6,2,3,9,1,4)
nextCup<-c(nextCup[1:8],10:l,4)
index<-4

for(i in 1:10000000){play()}

nextCup[1]*nextCup[nextCup[1]]
# 8456532414

###################################################
# Pedestrian solution; ok for A, too slow for B
##################################################

cups<-strsplit(input,"") %>% unlist %>% as.numeric

# A

circle<-function(x) ( (x-1) %% 9 ) + 1 
circle(0)

play<-function(){
  pickUp<-cups[(2):(4)]
  destination<-circle(cups[1]-1)
  while(destination %in% pickUp){ destination<- circle(destination-1)}
  index<-which(cups==destination)
  if(index<9)  cups<<-c(cups[5:index],pickUp,cups[(index+1):9],cups[1])
  if(index==9) cups<<-c(cups[5:index],pickUp,cups[1])
  print(cups)
}
  
for(i in 1:100){play()}
#52937846

# B
l<-1000000

cups<-strsplit(input,"") %>% unlist %>% as.numeric
cups<-c(cups,10:l)

circle<-function(x) ( (x-1) %% l ) + 1 
circle(0)

play<-function(){
  pickUp<-cups[(2):(4)]
  destination<-circle(cups[1]-1)
  while(destination %in% pickUp){ destination <- circle(destination-1)}
  index<-which(cups==destination)
  if(index<l)  cups<<-c(cups[5:index],pickUp,cups[(index+1):l],cups[1])
  if(index==l) cups<<-c(cups[5:index],pickUp,cups[1])
}
# for(i in 1:100){print(i);play()}
# Of course too slow



##############################
# Various attempts, too slow
##############################
#A
l<-9
positions<-c(7,5,3,1,4,2,8,6,9)
#nb<-1:l

play2<-function(){
  fst<-which(positions==1)
  dest<-circle(fst-1)
  pick2<-which(positions==2)
  pick3<-which(positions==3)
  pick4<-which(positions==4)
  pickUp<-c(pick2,pick3,pick4)
  while(dest %in% pickUp){ dest <- circle(dest-1)}
  posDest<-positions[dest]
  positions[positions<=posDest]<<-positions[positions<=posDest]-3
  positions[pick2]<<-posDest-2
  positions[pick3]<<-posDest-1
  positions[pick4]<<-posDest
  positions[fst]<<-1
  positions<<-circle(positions-1)
}

play2()

for(i in 1:100){play2()}
#52937846

#B

l<-1000000
positions<-c(7,5,3,1,4,2,8,6,9,10:1000000)

for(i in 1:1000){play2()}
# not faster at all! ~2 min for 1000 rep

# result valid
cups1000[1:20]
order(positions)[1:20]
#############################################
