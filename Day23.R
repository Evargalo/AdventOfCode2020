cups<-strsplit(input,"") %>% unlist %>% as.numeric

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
nextCup

# B

l<-1000000
nextCup<-c(7,8,5,6,2,3,9,1,4)
nextCup<-c(nextCup[1:8],10:l,4)
nextCup[1:20]
index<-4

for(i in 1:10000000){if(i%%100000==0) print(i);play()}

nextCup[1]
nextCup[nextCup[1]]
nextCup[1]*nextCup[nextCup[1]]
# 8456532414
