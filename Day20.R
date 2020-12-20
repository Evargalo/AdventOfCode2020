source('~/Perso/AdventOfCode/AoC2020/packages.R')

Day20 <- read_csv("~/Perso/AdventOfCode/AoC2020/Day20.txt", col_names = FALSE)
Day20 <- read_csv("~/Perso/AdventOfCode/AoC2020/Day20simple.txt", col_names = FALSE)

X1<-Day20$X1
length(X1)
(1584)/11
# 1584 rows
# 144 tiles of 10x10, image of 12 tiles x 12 tiles 

###############################################
# A

tiles<-data.frame(id="tile 0:",N="",E="",S="",W="",stringsAsFactors = FALSE)
i<-1
for(i in seq(1,1584,by=11)){
  id<-rmFirstWord(X1[i])
  mazeInfo<-buildMaze(X1[(i+1):(i+10)])
  maze<-mazeInfo$Maze
  North<-maze %>% filter(x==1)
  N<-paste(North$t,collapse = "")
  South<-maze %>% filter(x==10)
  S<-paste(South$t,collapse = "")
  West<-maze %>% filter(y==1)
  W<-paste(West$t,collapse = "")
  East<-maze %>% filter(y==10)
  E<-paste(East$t,collapse = "")
  tiles %>% add_row(id=id,N=N,S=S,E=E,W=W) ->tiles
}

tiles %>% filter(id!="tile 0:") %>% 
  mutate(num=as.double(cutAfter(rmFirstWord(id),":"))) ->tiles

flip<-function(E){
  vect<-unlist(strsplit(E,""))
  paste(rev(vect),collapse="")
}

S; flip(S)

allBorders<-tiles %>% select(num,N) %>% rename(B=N) %>% mutate(posInit="N") %>% 
  bind_rows(tiles %>% select(num,E) %>% rename(B=E)%>% mutate(posInit="E")) %>% 
  bind_rows(tiles %>% select(num,S) %>% rename(B=S)%>% mutate(posInit="S")) %>% 
  bind_rows(tiles %>% select(num,W) %>% rename(B=W)%>% mutate(posInit="W"))

allBorders<-allBorders %>% mutate(flipped=FALSE) %>% 
  bind_rows(allBorders %>% rowwise %>% mutate(B=flip(B),flipped=TRUE)) 

countMatches<-function(Test){
  res<-allBorders %>% filter(B==Test) %>% nrow
  if(Test==flip(Test)) res<-res/2
  res
}
Test<- ".#.###.#.."
countMatches(Test = ".#.###.#..")

tiles %>% rowwise %>% mutate(matches=countMatches(N)+countMatches(W)+countMatches(S)+countMatches(E)) -> tiles

table(tiles$matches) # each border is either unique or shared by only two tiles
tiles %>% arrange(matches) %>% head

3023*1571*1709*3457
# 28057939502729

###############################################
# B

# Place all tiles in jigsaw

tiles %>% rowwise %>% mutate(x=0,y=0,rot="",found=FALSE) %>% arrange(matches)-> tiles

tiles$x[1]<-1
tiles$y[1]<-1
tiles$found[1]<-TRUE
tiles$rot[1]<-"SF"
# rot defined by where the North side will land
# rot in c("N","E","W","S","NF","EF","WF","SF")
# means that the North side is now facing N/E/S/W, flipped or not

jigsaw<-data.frame(x=0,y=0,num=0,rot=".",stringsAsFactors = FALSE)
jigsaw %>% add_row(x=1,y=1,num=3023,rot="SF") ->jigsaw

borderSouth<-function(num,rot){
  tile<-tiles[tiles$num==num,]
  if(rot=="N") return(tile$S)
  if(rot=="E") return(flip(tile$E))
  if(rot=="S") return(tile$N)
  if(rot=="W") return(tile$E)
  if(rot=="NF") return(flip(tile$S))
  if(rot=="EF") return(flip(tile$W))
  if(rot=="SF") return(flip(tile$N))
  if(rot=="WF") return(tile$W)
} # What lands on the SOuth after applying rot to tile num

borderEast<-function(num,rot){
  tile<-tiles[tiles$num==num,]
  if(rot=="N") return(tile$E)
  if(rot=="E") return(tile$N)
  if(rot=="S") return(flip(tile$E))
  if(rot=="W") return(tile$S)
  if(rot=="NF") return(tile$W)
  if(rot=="EF") return(flip(tile$N))
  if(rot=="SF") return(flip(tile$W))
  if(rot=="WF") return(flip(tile$S))
}

buildRotForNorth<-function(posInit,flipped){
  if(!flipped){
    if(posInit=="N") return("N")
    if(posInit=="E") return("WF")
    if(posInit=="S") return("S")
    if(posInit=="W") return("W")
  }
  if(flipped){
    if(posInit=="N") return("NF")
    if(posInit=="E") return("EF")
    if(posInit=="S") return("SF")
    if(posInit=="W") return("E")
  }
} # What rotation brings posInit to the North, flipped or not

buildRotForWest<-function(posInit,flipped){
  if(!flipped){
    if(posInit=="N") return("W")
    if(posInit=="E") return("NF")
    if(posInit=="S") return("E")
    if(posInit=="W") return("N")
  }
  if(flipped){
    if(posInit=="N") return("WF")
    if(posInit=="E") return("SF")
    if(posInit=="S") return("EF")
    if(posInit=="W") return("S")
  }
}

num<-3023
rot<-"SF"
x<-1 ; y<-1

addNeighbours<-function(x,y,num,rot){
  print(paste("x=",x,"y=",y))
  xx<-x
  yy<-y
  givenNum<-num
  if(xx<12 & nrow(tiles %>% filter(x==xx+1,y==yy))==0){
    borderSouth<-borderSouth(givenNum,rot)
    newBorder<-allBorders %>% filter (B==borderSouth,num!=givenNum)
    newNum<-newBorder$num
    newRot<-buildRotForNorth(newBorder$posInit,newBorder$flipped)
    tiles$x[tiles$num==newNum]<<-xx+1
    tiles$y[tiles$num==newNum]<<-yy
    tiles$rot[tiles$num==newNum]<<-newRot
    tiles$found[tiles$num==newNum]<<-TRUE
    jigsaw %>% add_row(x=xx+1,y=yy,num=newNum,rot=newRot)->>jigsaw
    addNeighbours(xx+1,yy,newNum,newRot)
  }
  if(yy<12 & nrow(tiles %>% filter(x==xx,y==yy+1))==0){
    borderEast<-borderEast(givenNum,rot)
    newBorder<-allBorders %>% filter (B==borderEast,num!=givenNum)
    newNum<-newBorder$num
    newRot<-buildRotForWest(newBorder$posInit,newBorder$flipped)
    tiles$x[tiles$num==newNum]<<-xx
    tiles$y[tiles$num==newNum]<<-yy+1
    tiles$rot[tiles$num==newNum]<<-newRot
    tiles$found[tiles$num==newNum]<<-TRUE
    jigsaw %>% add_row(x=xx,y=yy+1,num=newNum,rot=newRot)->>jigsaw
    addNeighbours(xx,yy+1,newNum,newRot)
  }
}

addNeighbours(1,1,num,rot)

jigsaw %>% filter(rot!=".")->jigsaw

######
# Big Image

bigJigsaw<-data.frame(x=0,y=0,t="bidon",stringsAsFactors = FALSE)
i<-1

flipMaze<-function(maze,rot){
  limx<-max(maze$x)+1
  limy<-max(maze$y)+1
  if(rot=="N") return (maze)
  if(rot=="NF") return (maze %>% mutate(y=limy-y))
  if(rot=="S") return (maze %>% mutate(x=limx-x))
  if(rot=="SF") return (maze %>% mutate(x=limx-x,y=limy-y))
  if(rot=="E") return (maze %>% mutate(z=y,y=limx-x,x=z) %>% select(-z))
  if(rot=="W") return (maze %>% mutate(z=y,y=x,x=z) %>% select(-z))
  if(rot=="EF") return (maze %>% mutate(z=y,y=limx-x,x=limy-z) %>% select(-z))
  if(rot=="WF") return (maze %>% mutate(z=y,y=x,x=limy-z) %>% select(-z))
}

for(i in seq(1,1584,by=11)){
  id<-rmFirstWord(X1[i])
  mazeInfo<-buildMaze(X1[(i+1):(i+10)])
  maze<-mazeInfo$Maze
  rot<-tiles$rot[tiles$id==id]
  maze2<-flipMaze(maze,rot)
  spotx<-tiles$x[tiles$id==id]
  spoty<-tiles$y[tiles$id==id]
  newBJS<-maze2 %>% mutate(x=x+10*(spotx-1),y=y+10*(spoty-1))
  bigJigsaw<- bigJigsaw %>% bind_rows(newBJS)
}

bigJigsaw<- bigJigsaw %>% filter(t!="bidon") %>%  filter(x%%10 %in% 2:9,y%%10 %in% 2:9)

correctCoord<-function(coord){
  coord-2*(coord%/%10)-1
}
correctCoord(12)
bigJigsaw<- bigJigsaw %>% rowwise %>% mutate(x=correctCoord(x),y=correctCoord(y))

table(bigJigsaw$x)

drawMaze(bigJigsaw)

#######
# SeeMonster

SM<-read_csv("~/Perso/AdventOfCode/AoC2020/Day20SeaMonster.txt", col_names = FALSE,trim_ws = FALSE)
(SM$X1) %>% sapply(function(x) str_replace_all(string = x, pattern = "#", replacement = "B")) %>%
  sapply(function(x) str_replace_all(string = x, pattern = "\\s", replacement = ".")) %>%
  unname()  %>% buildMaze -> SMfull
SeeMonster<-SMfull$CharBSpots
 SeeMonsterX<-SeeMonster$x-1
 SeeMonsterY<-SeeMonster$y-1
# SeeMonsterX<-c(0,rep(1,8),rep(2,6))
# SeeMonsterY<-c(18,0,5,6,11,12,17,18,19,1,4,7,10,13,16)
length(SeeMonsterY)

M<-matrix(data = ".",nrow = 96,ncol=96)
fillMat<-function(x,y,t) M[x,y]<<-t
pmap(bigJigsaw,fillMat)
M

searchSM<-function(x,y){
  if(x>94 | y>77) return(FALSE)
  for(i in 1:15) { if(M[x+SeeMonsterX[i],y+SeeMonsterY[i]] != "#") return(FALSE)}
  return(TRUE)
}

searchSM(x,y)

sum(unlist(pmap(bigJigsaw %>% select(x,y), searchSM)))

for(rot in c("N","E","W","S","NF","EF","WF","SF")){
  bigJS<-flipMaze(bigJigsaw,rot)
  M<-matrix(data = ".",nrow = 96,ncol=96)
  pmap(bigJS,fillMat)
  s<-sum(unlist(pmap(bigJigsaw %>% select(x,y), searchSM)))
  print(paste("rot",rot,"s",s))
}
# 18 seeMonsters for rot="W", 0 otherwise

drawMaze(bigJigsaw)
drawMaze(bigJS)

sum(bigJigsaw$t=="#")-18*15
#2489
