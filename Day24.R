source('~/Perso/AdventOfCode/AoC2020/packages.R')

day24 <- read_table2("~/Perso/AdventOfCode/AoC2020/day24.txt", col_names = FALSE)
day24 <- read_table2("~/Perso/AdventOfCode/AoC2020/day24simple.txt", col_names = FALSE)

X1<-day24$X1

######################################
# A

allmoves<-data.frame(x=as.numeric(),y=as.numeric())

s<-"seswneswswsenwwnwse"

# Instructions to one character each
changeChar<-function(s){
  s %>% gsub("ne","a",.) %>% gsub("nw","b",.) %>% gsub("se","c",.) %>% gsub("sw","d",.)
}

s<-changeChar(s)

instr<- changeChar(X1)

move<-function(z){
  if(z=="e") return(c(0,1))
  if(z=="w") return(c(0,-1))
  if(z=="a") return(c(-1,1))
  if(z=="b") return(c(-1,0))
  if(z=="c") return(c(1,0))
  if(z=="d") return(c(1,-1))
}

flipATile<-function(s){
  vect<-strsplit(s,"") %>% unlist
  pos<-c(0,0)
  for(z in vect){pos<-pos+move(z)}
  allmoves<<-allmoves %>% add_row(x=pos[1],y=pos[2])
}

sapply(instr,flipATile)

allmoves$flips<-1
allmoves %>% group_by(x,y) %>% summarise(f=sum(flips)) %>% filter(f%%2==1) %>% nrow
# 300

######################################
# B

allmoves %>% group_by(x,y) %>% summarise(t=sum(flips)%%2) -> maze 
sum(maze$t==1)

directions<-list(c(0,1),c(0,-1),c(1,-1),c(-1,0),c(1,0),c(-1,1))

tileEffect<-function(x,y,t){
  affectedTiles<-data.frame(x=as.numeric(),y=as.numeric())
  if(t==1){
    pos<-c(x,y)
    for(dir in directions){
      newPos<-pos+dir
      affectedTiles<-affectedTiles %>% add_row(x=newPos[1],y=newPos[2])
    }
  }
  affectedTiles
}

for(i in 1:100){
  print(i)
  pmap_dfr(maze,tileEffect) %>% mutate(neigh=1) %>% group_by(x,y) %>% 
    summarise(s=sum(neigh)) %>% left_join(maze,by=c("x","y")) %>% group_by(x,y,s) %>% 
    summarise(t=ifelse(is.na(t),0,t)) %>%
    summarise(t=ifelse(t==1,s %in% c(1,2),s==2)) %>% 
    select(x,y,t) %>% ungroup %>% filter(t==1) -> maze 
}

sum(maze$t)
# 3466





