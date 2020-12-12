source("packages.R")
# source("usefulFunctions.R")
options(digits = 20)

day12 <- read_table2("day12simple.txt", col_names = FALSE)
day12 <- read_table2("day12.txt", col_names = FALSE)

day12$instruction<-substr(day12$X1,1,1)
day12$value<-as.numeric(substr(day12$X1,2,4))

# A

day12 %>% filter(instruction=="R") %>% select(value) %>% unique

day12 %>% filter(instruction=="N") %>% select(value) %>% sum ->totN
day12 %>% filter(instruction=="S") %>% select(value) %>% sum ->totS
day12 %>% filter(instruction=="W") %>% select(value) %>% sum ->totW
day12 %>% filter(instruction=="E") %>% select(value) %>% sum ->totE
position<-c(totN-totS,totE-totW,0)

turns <-day12 %>% filter(instruction%in% c("R","L","F"))
n<-nrow(turns)

for (i in 1:n){
  ins<-turns$instruction[i]
  val<-turns$value[i]
   if(ins=="R"){
    ins<-"L"
    val<-360-val
  }
  if(ins=="L"){
    position<-position+c(0,0,val)
    position[3]<-position[3]%%360
    }
  if(ins=="F"){
    dir<-position[3]
    if(dir==0){
      position<-position+c(0,val,0)
    }
    if(dir==180){
      position<-position+c(0,-val,0)
    }
    if(dir==90){
      position<-position+c(val,0,0)
    }
    if(dir==270){
      position<-position+c(-val,0,0)
    }
  }
}

abs(position[1])+abs(position[2])
# 562

# B

# with functions and pmap

day12<-day12 %>% select(instruction,value)
position<-c(0,0)
waypoint<-c(1,10)

flip<-function(instruction,value){
  if (instruction=="R") value<-360-value
  n<-value %/% 90
  for(i in 1:n){
    x<-waypoint[1]
    y<-waypoint[2]
    waypoint<<-c(y,-x)
  }
}

exec<-function(instruction,value){
  if(instruction=="N") waypoint<<-waypoint+c(value,0)
  if(instruction=="S") waypoint<<-waypoint+c(-value,0)
  if(instruction=="W") waypoint<<-waypoint+c(0,-value)
  if(instruction=="E") waypoint<<-waypoint+c(0,value)
  if(instruction=="F") position<<-position+value*waypoint
  if(instruction=="L") flip(instruction,value)
  if(instruction=="R") flip(instruction,value)
  print(position)
  print(waypoint)
}

pmap(day12,exec)

abs(position[1])+abs(position[2])
# 101860
