source('~/Perso/AdventOfCode/AoC2020/packages.R')

######################################
# A

CardPubKey<-16915772
DoorPubKey<-18447943

value<-7
subNumber<-7

# No idea why initialisation counts as first step, but it does. 
for(k in 2:20201227){
  value<-(value*subNumber) %% 20201227
  if (value %in% c(CardPubKey,DoorPubKey)) print(paste(k,":",value))
}

# if k starts at 1
#[1] "4618529 : 16915772"
#[1] "6662322 : 18447943"
# 
# if k starts at 2
# [1] "4618530 : 16915772"
# [1] "6662323 : 18447943"

value<-18447943
subNumber<-18447943
for(k in 1:4618529){
  value<-(value*subNumber) %% 20201227
}
value
# 6011069

value<-16915772
subNumber<-16915772
for(k in 1:6662322){
  value<-(value*subNumber) %% 20201227
}
value
# 6011069


##################
# Jeu test
CardPubKey<-5764801
DoorPubKey<-17807724

for(k in 1:11){
  value<-(value*subNumber) %% 20201227
  if (value %in% c(CardPubKey,DoorPubKey)) print(paste(k,":",value))
}
# [1] "7 : 5764801"
# instead of 8 loops

value<-5764801
subNumber<-5764801
for(k in 1:12){
  value<-(value*subNumber) %% 20201227
  print(paste(k,":",value))
}
value
#10 ok

######################################
# B

# Finally we are not toying with arithmetics modulo 20201227 ?
# Thank you reindeer !

