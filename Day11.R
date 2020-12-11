source("packages.R")
# source("usefulFunctions.R")
options(digits = 20)

day11 <- read_table2("day11.txt", col_names = FALSE)
rawData<-day11$X1

rawData %>% sapply(function(x) str_replace_all(string = x, pattern = "#", replacement = "B")) %>% 
  unname() -> rawData
mazeInfo<-buildMaze(rawData)

directions<-list(c(1,0),c(0,1),c(1,1),c(-1,1),c(1,-1),c(-1,-1),c(-1,0),c(0,-1))

# A mieux

tableNeighbour1<-data.frame(a=c(0),b=c(0),c=c(0),d=c(0))
for(j in 1:nrow(maze)){
  a<-maze$x[j]
  b<-maze$y[j]
  for (dir in directions){
    place<-c(a,b)+dir
    tableNeighbour1 %>% add_row(a=a,b=b,c=place[1],d=place[2]) ->tableNeighbour1
  }
  if(j%%20 == 0){
    print(j)
  }
}

tableNeighbour1 %>% filter(c>0 & d>0 & c<limx+1 & d<limy+1) ->tableNeighbour1
names(tableNeighbour1)<-c("x","y","c","d")

finish<-FALSE
nbStep<-0
maze<-mazeInfo$Maze %>% filter(t!=".")
maze$nbNeigh<-0

while(!finish & nbStep<1000){
  newMaze<-maze
  mazeO<-maze %>% filter(t=="O")
  if (nrow(mazeO)>0){
    newMaze<-newMaze %>% select(-nbNeigh)
    mazeO %>% left_join(tableNeighbour1,by=c("x","y")) %>% 
      group_by(c,d) %>% count %>% rename(x=c,y=d) %>% right_join(newMaze,by=c("x","y")) %>% 
      rename(nbNeigh=n) %>% mutate(nbNeigh=ifelse(is.na(nbNeigh),0,nbNeigh))->newMaze
  }
  newMaze %>% mutate(newT=ifelse(((t=="L" & nbNeigh==0) | (t=="O" & nbNeigh<4)),"O","L"))->newMaze
  nbStep<-nbStep+1
  if(nbStep%%10 == 0){
    print(nbStep)
    print(sum(maze$t=="O"))
  }
  if(sum(newMaze$t != newMaze$newT)==0) finish<-TRUE
  newMaze %>% mutate(t=newT) ->maze
}

sum(maze$t=="O")
# 2324

drawMaze(maze)

# B

maze<-mazeInfo$Maze %>% filter(t!=".")
refMaze<-mazeInfo$Maze 

limx<-max(maze$x)
limy<-max(maze$y)

tableNeighbour<-data.frame(a=c(0),b=c(0),c=c(0),d=c(0))
for(j in 1:nrow(maze)){
  a<-maze$x[j]
  b<-maze$y[j]
  for (dir in directions){
    t<-"A"
    place<-c(a,b)+dir
    if(all(place>0) & (place[1]<limx+1) & (place[2]<limy+1)) {t<-(refMaze %>% filter(x==place[1]&y==place[2]))$t} 
    while(t=="." & all(place>0) & (place[1]<limx+1) & (place[2]<limy+1)){
      place<-place+dir
      if(all(place>0) & (place[1]<limx+1) & (place[2]<limy+1)) {t<-(refMaze %>% filter(x==place[1]&y==place[2]))$t} 
    }
   tableNeighbour %>% add_row(a=a,b=b,c=place[1],d=place[2]) ->tableNeighbour
  }
  if(j%%20 == 0){
    print(j)
  }
}

tableNeighbour %>% filter(c>0 & d>0 & c<limx+1 & d<limy+1) ->tableNeighbour
names(tableNeighbour)<-c("x","y","c","d")

finish<-FALSE
nbStep<-0
maze<-mazeInfo$Maze %>% filter(t!=".")
maze$nbNeigh<-0

while(!finish & nbStep<1000){
  newMaze<-maze
  mazeO<-maze %>% filter(t=="O")
  if (nrow(mazeO)>0){
    newMaze<-newMaze %>% select(-nbNeigh)
    mazeO %>% left_join(tableNeighbour,by=c("x","y")) %>% 
      group_by(c,d) %>% count %>% rename(x=c,y=d) %>% right_join(newMaze,by=c("x","y")) %>% 
      rename(nbNeigh=n) %>% mutate(nbNeigh=ifelse(is.na(nbNeigh),0,nbNeigh))->newMaze
  }
  newMaze %>% mutate(newT=ifelse(((t=="L" & nbNeigh==0) | (t=="O" & nbNeigh<5)),"O","L"))->newMaze
  nbStep<-nbStep+1
  if(nbStep%%10 == 0){
    print(nbStep)
    print(sum(maze$t=="O"))
  }
  if(sum(newMaze$t != newMaze$newT)==0) finish<-TRUE
  newMaze %>% mutate(t=newT) ->maze
}

sum(maze$t=="O")
# 2068

drawMaze(maze)

