source('~/Perso/AdventOfCode/AoC2020/packages.R')

Day17 <- read_table2("~/Perso/AdventOfCode/AoC2020/Day17.txt", col_names = FALSE)
Day17 <- read_table2("~/Perso/AdventOfCode/AoC2020/Day17simple.txt", col_names = FALSE)

X1<-Day17$X1


# A

buildMaze<-function(rawData){
  Maze<-data.frame(x=c(),y=c(),z=c(),t=c(),stringsAsFactors = FALSE)
  n<-length(rawData)
  p<-nchar(rawData[1])
  # Data
  for(k in 1:n){
    line<-unlist(strsplit(rawData[k],split = "")) 
    for (l in 1:p){
      newline<-data.frame(x=k,y=l,z=0,t=line[l],stringsAsFactors = FALSE)
      Maze<-Maze %>% bind_rows(newline)
    }
  }
  res<-list("Maze"=Maze,"foundChar"=unique(Maze$t))
  for (c in unique(Maze$t)){
    assign(x = paste0("Char",c,"Spots"),
           Maze %>% filter(t==c) %>% 
             select(x,y,z))
    res[[paste0("Char",c,"Spots")]]<-get(x = paste0("Char",c,"Spots"))
  }
  res
}

mazeInfo<-buildMaze(rawData = X1)
maze<-mazeInfo$Maze

directions<-list(c(1,0,0),c(0,1,0),c(1,1,0),c(-1,1,0),c(1,-1,0),c(-1,-1,0),c(-1,0,0),c(0,-1,0),
                 c(1,0,-1),c(0,1,-1),c(1,1,-1),c(-1,1,-1),c(1,-1,-1),c(-1,-1,-1),c(-1,0,-1),c(0,-1,-1),
                 c(1,0,1),c(0,1,1),c(1,1,1),c(-1,1,1),c(1,-1,1),c(-1,-1,1),c(-1,0,1),c(0,-1,1),
                 c(0,0,1),c(0,0,-1)
                 )

maze<-mazeInfo$Maze
nx<-max(maze$x)
ny<-max(maze$y)
rep<-6

bigMaze<-data.frame(x=(1-rep):(nx+rep),oui=1) %>% left_join(data.frame(y=(1-rep):(ny+rep),oui=1)) %>% 
  left_join(data.frame(z=(-rep:rep),oui=1))
bigMaze$t<-as.character(".") 

for(j in 1:nrow(maze)){
  a<-maze$x[j]
  b<-maze$y[j]
  c<-maze$z[j]
  t<-maze$t[j]
 bigMaze$t[bigMaze$x==a & bigMaze$y==b & bigMaze$z==c] <- t
}
sum(bigMaze$t=="#")

tableNeighbour1<-data.frame(a=c(0),b=c(0),c=c(0),d=c(0),e=c(0),f=c(0))
for (dir in directions){
  place<-c(1,1,0)+dir
  tableNeighbour1 %>% add_row(a=1,b=1,c=0,d=place[1],e=place[2],f=place[3]) ->tableNeighbour1
}
tableNeighbour1 %>% filter(row_number()!=1) ->tableNeighbour1

tableNeigh<-tableNeighbour1 
for(x in c((-rep:-1),1:(nx+rep-1))) {
  tableNeighbour2<- tableNeighbour1 %>% mutate(a=a+x,d=d+x)
  tableNeigh<-tableNeigh %>% bind_rows(tableNeighbour2)
  if(any(tableNeigh$c>0)) print(x)
}
tableNeighbour1<-tableNeigh %>% filter( (d>(-rep)) & (d<(nx+rep+1)) & (a>(-rep)) & (a<(nx+rep+1)) &
                                        (e>(-rep)) & (e<(ny+rep+1))  & (b>(-rep)) & (b<(ny+rep+1)) &
                                        (f>(-rep-1)) & (f<(rep+1)) & (c>(-rep-1)) & (c<(rep+1)) )

tableNeigh<-tableNeighbour1

for(y in c((-rep:-1),1:(ny+rep-1))) {
  tableNeighbour2<-tableNeighbour1 %>% mutate(b=b+y,e=e+y)
  tableNeigh<-tableNeigh %>% bind_rows(tableNeighbour2)
}
tableNeighbour1<-tableNeigh %>% filter( (d>(-rep)) & (d<(nx+rep+1)) & (a>(-rep)) & (a<(nx+rep+1)) &
                                          (e>(-rep)) & (e<ny+rep+1)  & (b>(-rep)) & (b<ny+rep+1) &
                                          (f>(-rep-1)) & (f<rep+1) & (c>(-rep-1)) & (c<rep+1) )
tableNeigh<-tableNeighbour1

for(z in c((-rep:-1),1:rep)) {
  tableNeighbour2<-tableNeighbour1 %>% mutate(c=c+z,f=f+z)
  tableNeigh<-tableNeigh %>% bind_rows(tableNeighbour2)
}
tableNeighbour1<-tableNeigh %>% filter( (d>(-rep)) & (d<(nx+rep+1)) & (a>(-rep)) & (a<(nx+rep+1)) &
                                          (e>(-rep)) & (e<ny+rep+1)  & (b>(-rep)) & (b<ny+rep+1) &
                                          (f>(-rep-1)) & (f<rep+1) & (c>(-rep-1)) & (c<rep+1) )

tableNeighbour1 %>% rename(x=a,y=b,z=c) -> tableNeighbour
tableNeighbour %>% unique -> tableNeighbour

countNeigh<-function(xx,yy,zz){
  tableNeighbour %>% filter(d==xx & e==yy & f==zz & t=="#")%>% nrow()
}

mazeAct<-bigMaze %>% filter(t=="#")
tableNeighbour %>% left_join(mazeAct,by=c("x","y","z")) %>% 
  select(x,y,z,d,e,f,t) %>% mutate(t=ifelse(is.na(t),".",t)) -> tableNeighbour

bigMaze$nbNeigh<-mapply(countNeigh,bigMaze$x,bigMaze$y,bigMaze$z)
table(bigMaze$nbNeigh)
sum(bigMaze$nbNeigh)


bigMaze %>% filter(x %in% 0:4, y %in% 0:4, z %in% (-1):1 ) %>% arrange(z) %>% select(-oui)
tableNeighbour %>% filter(d==2 & e==2 & f==-1)

for(i in 1:rep){
  newMaze<-bigMaze
  mazeO<-bigMaze %>% filter(t=="#")
  if (nrow(mazeO)>0){
    newMaze<-newMaze %>% select(-nbNeigh)
    mazeO %>% left_join(tableNeighbour,by=c("x","y","z")) %>% 
      group_by(d,e,f) %>% count %>% rename(x=d,y=e,z=f) %>% right_join(newMaze,by=c("x","y","z")) %>% 
      rename(nbNeigh=n) %>% ungroup %>% mutate(nbNeigh=ifelse(is.na(nbNeigh),0,nbNeigh)) ->newMaze
  }
  newMaze %>% mutate(newT=ifelse(nbNeigh==3 | (nbNeigh==2 & t=="#"),
                                        "#",
                                        "."))->newMaze
  newMaze %>% mutate(t=newT) ->bigMaze
}

sum(bigMaze$t=="#")
# 424

###############################
# B

buildMaze<-function(rawData){
  Maze<-data.frame(x=c(),y=c(),z=c(),t=c(),stringsAsFactors = FALSE)
  n<-length(rawData)
  p<-nchar(rawData[1])
  # Data
  for(k in 1:n){
    line<-unlist(strsplit(rawData[k],split = "")) 
    for (l in 1:p){
      newline<-data.frame(x=k,y=l,z=0,t=line[l],stringsAsFactors = FALSE)
      Maze<-Maze %>% bind_rows(newline)
    }
  }
  res<-list("Maze"=Maze,"foundChar"=unique(Maze$t))
  for (c in unique(Maze$t)){
    assign(x = paste0("Char",c,"Spots"),
           Maze %>% filter(t==c) %>% 
             select(x,y,z))
    res[[paste0("Char",c,"Spots")]]<-get(x = paste0("Char",c,"Spots"))
  }
  res
}

mazeInfo<-buildMaze(rawData = X1)
maze<-mazeInfo$Maze

maze$w<-0
nx<-max(maze$x)
ny<-max(maze$y)
rep<-6

bigMaze<-data.frame(x=(1-rep):(nx+rep),oui=1) %>% left_join(data.frame(y=(1-rep):(ny+rep),oui=1)) %>% 
  left_join(data.frame(z=(-rep:rep),oui=1) )%>% left_join(data.frame(w=(-rep:rep),oui=1))
bigMaze$t<-as.character(".") 

for(j in 1:nrow(maze)){
  a<-maze$x[j]
  b<-maze$y[j]
  c<-maze$z[j]
  d<-maze$w[j]
  t<-maze$t[j]
  bigMaze$t[bigMaze$x==a & bigMaze$y==b & bigMaze$z==c & bigMaze$w==d] <- t
}
sum(bigMaze$t=="#")

directions<-list(c(1,0,0,0),c(0,1,0,0),c(1,1,0,0),c(-1,1,0,0),c(1,-1,0,0),c(-1,-1,0,0),c(-1,0,0,0),c(0,-1,0,0),
           c(1,0,-1,0),c(0,1,-1,0),c(1,1,-1,0),c(-1,1,-1,0),c(1,-1,-1,0),c(-1,-1,-1,0),c(-1,0,-1,0),c(0,-1,-1,0),
           c(1,0,1,0),c(0,1,1,0),c(1,1,1,0),c(-1,1,1,0),c(1,-1,1,0),c(-1,-1,1,0),c(-1,0,1,0),c(0,-1,1,0),
           c(0,0,1,0),c(0,0,-1,0),
           c(1,0,0,-1),c(0,1,0,-1),c(1,1,0,-1),c(-1,1,0,-1),c(1,-1,0,-1),c(-1,-1,0,-1),c(-1,0,0,-1),c(0,-1,0,-1),
           c(1,0,-1,-1),c(0,1,-1,-1),c(1,1,-1,-1),c(-1,1,-1,-1),c(1,-1,-1,-1),c(-1,-1,-1,-1),c(-1,0,-1,-1),c(0,-1,-1,-1),
           c(1,0,1,-1),c(0,1,1,-1),c(1,1,1,-1),c(-1,1,1,-1),c(1,-1,1,-1),c(-1,-1,1,-1),c(-1,0,1,-1),c(0,-1,1,-1),
           c(0,0,1,-1),c(0,0,-1,-1),
           c(1,0,0,1),c(0,1,0,1),c(1,1,0,1),c(-1,1,0,1),c(1,-1,0,1),c(-1,-1,0,1),c(-1,0,0,1),c(0,-1,0,1),
           c(1,0,-1,1),c(0,1,-1,1),c(1,1,-1,1),c(-1,1,-1,1),c(1,-1,-1,1),c(-1,-1,-1,1),c(-1,0,-1,1),c(0,-1,-1,1),
           c(1,0,1,1),c(0,1,1,1),c(1,1,1,1),c(-1,1,1,1),c(1,-1,1,1),c(-1,-1,1,1),c(-1,0,1,1),c(0,-1,1,1),
           c(0,0,1,1),c(0,0,-1,1),
           c(0,0,0,1),c(0,0,0,-1)
)

tableNeighbour1<-data.frame(a=c(0),b=c(0),c=c(0),d=c(0),e=c(0),f=c(0),g=c(0),h=c(0))

for (dir in directions){
  place<-c(1,1,0,0)+dir
  tableNeighbour1 %>% add_row(a=1,b=1,c=0,d=0,e=place[1],f=place[2],g=place[3],h=place[4]) ->tableNeighbour1
}
tableNeighbour1 %>% filter(row_number()!=1) ->tableNeighbour1

tableNeigh<-tableNeighbour1 
for(x in c((-rep:-1),1:(nx+rep-1))) {
  tableNeighbour2<- tableNeighbour1 %>% mutate(a=a+x,e=e+x)
  tableNeigh<-tableNeigh %>% bind_rows(tableNeighbour2)
}
tableNeighbour1<-tableNeigh %>% filter( (e>(-rep)) & (e<(nx+rep+1)) & (a>(-rep)) & (a<(nx+rep+1)) &
                                          (f>(-rep)) & (f<(ny+rep+1))  & (b>(-rep)) & (b<(ny+rep+1)) &
                                          (g>(-rep-1)) & (g<(rep+1)) & (c>(-rep-1)) & (c<(rep+1)) &
                                          (h>(-rep-1)) & (h<(rep+1)) & (d>(-rep-1)) & (d<(rep+1)) )

tableNeigh<-tableNeighbour1

for(y in c((-rep:-1),1:(ny+rep-1))) {
  tableNeighbour2<-tableNeighbour1 %>% mutate(b=b+y,f=f+y)
  tableNeigh<-tableNeigh %>% bind_rows(tableNeighbour2)
}
tableNeighbour1<-tableNeigh %>% filter( (e>(-rep)) & (e<(nx+rep+1)) & (a>(-rep)) & (a<(nx+rep+1)) &
                                          (f>(-rep)) & (f<(ny+rep+1))  & (b>(-rep)) & (b<(ny+rep+1)) &
                                          (g>(-rep-1)) & (g<(rep+1)) & (c>(-rep-1)) & (c<(rep+1)) &
                                          (h>(-rep-1)) & (h<(rep+1)) & (d>(-rep-1)) & (d<(rep+1)) )
tableNeigh<-tableNeighbour1

for(z in c((-rep:-1),1:rep)) {
  tableNeighbour2<-tableNeighbour1 %>% mutate(c=c+z,g=g+z)
  tableNeigh<-tableNeigh %>% bind_rows(tableNeighbour2)
}
tableNeighbour1<-tableNeigh %>% filter( (e>(-rep)) & (e<(nx+rep+1)) & (a>(-rep)) & (a<(nx+rep+1)) &
                                          (f>(-rep)) & (f<(ny+rep+1))  & (b>(-rep)) & (b<(ny+rep+1)) &
                                          (g>(-rep-1)) & (g<(rep+1)) & (c>(-rep-1)) & (c<(rep+1)) &
                                          (h>(-rep-1)) & (h<(rep+1)) & (d>(-rep-1)) & (d<(rep+1)) )

for(w in c((-rep:-1),1:rep)) {
  tableNeighbour2<-tableNeighbour1 %>% mutate(d=d+w,h=h+w)
  tableNeigh<-tableNeigh %>% bind_rows(tableNeighbour2)
}
tableNeighbour1<-tableNeigh %>% filter( (e>(-rep)) & (e<(nx+rep+1)) & (a>(-rep)) & (a<(nx+rep+1)) &
                                          (f>(-rep)) & (f<(ny+rep+1))  & (b>(-rep)) & (b<(ny+rep+1)) &
                                          (g>(-rep-1)) & (g<(rep+1)) & (c>(-rep-1)) & (c<(rep+1)) &
                                          (h>(-rep-1)) & (h<(rep+1)) & (d>(-rep-1)) & (d<(rep+1)) )

tableNeighbour1 %>% rename(x=a,y=b,z=c,w=d) -> tableNeighbour

tableNeighbour %>% summarise_all(min)
tableNeighbour %>% summarise_all(max)

countNeigh<-function(xx,yy,zz,ww){
  tableActiveNeighbour %>% filter(e==xx & f==yy & g==zz & h==ww)%>% nrow()
}

mazeAct<-bigMaze %>% filter(t=="#") %>% select(x,y,z,w,t)
tableNeighbour %>% left_join(mazeAct,by=c("x","y","z","w")) %>% 
  select(x,y,z,w,e,f,g,h,t) %>% mutate(t=ifelse(is.na(t),".",t)) -> tableNeighbour
tableActiveNeighbour <- tableNeighbour %>% filter(t=="#")

bigMazeAInit<-bigMaze %>% filter(w<=1 & z<=1 & w>=(-1) & z>=(-1) & x>=0 & y>=0 & x<=nx+1 & y<=ny+1)
bigMazeOther<-bigMaze %>% filter(!(w<=1 & z<=1 & w>=(-1) & z>=(-1) & x>=0 & y>=0 & x<=nx+1 & y<=ny+1))

bigMazeAInit$nbNeigh<-mapply(countNeigh,bigMazeAInit$x,bigMazeAInit$y,bigMazeAInit$z,bigMazeAInit$w)
bigMazeOther$nbNeigh<-0
bigMaze<-bigMazeAInit %>% bind_rows(bigMazeOther)
table(bigMaze$nbNeigh)

for(i in 1:rep){
  newMaze<-bigMaze
  mazeO<-bigMaze %>% filter(t=="#")
  if (nrow(mazeO)>0){
    newMaze<-newMaze %>% select(-nbNeigh)
    mazeO %>% left_join(tableNeighbour,by=c("x","y","z","w")) %>% 
      group_by(e,f,g,h) %>% count %>% rename(x=e,y=f,z=g,w=h) %>% right_join(newMaze,by=c("x","y","z","w")) %>% 
      rename(nbNeigh=n) %>% ungroup %>% mutate(nbNeigh=ifelse(is.na(nbNeigh),0,nbNeigh)) ->newMaze
  }
  newMaze %>% mutate(newT=ifelse(nbNeigh==3 | (nbNeigh==2 & t=="#"),
                                 "#",
                                 "."))->newMaze
    print(i)
    print(sum(maze$t=="#"))
    newMaze %>% mutate(t=newT) ->bigMaze
}

sum(bigMaze$t=="#")
# 2460
