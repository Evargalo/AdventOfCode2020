source("packages.R")

# A
X3DecembreInput <- read_delim("day03A.txt", delim= ",",
                               escape_double = FALSE, col_names = FALSE)

Maze<-data.frame(x=c(),y=c(),t=c(),stringsAsFactors = FALSE)
n<-nrow(X3DecembreInput)
p<-nchar(X3DecembreInput$X1[1])

trees<-data.frame(x=c(),y=c(),stringsAsFactors = FALSE)
passes<-data.frame(x=c(),y=c(),stringsAsFactors = FALSE)

# Data
for(k in 1:n){
  line<-unlist(strsplit(X3DecembreInput$X1[k],split = "")) 
  for (l in 1:p){
    newline<-data.frame(x=k,y=l,t=line[l],stringsAsFactors = FALSE)
    Maze<-Maze %>% bind_rows(newline)
    if(line[l] == "#"){
      tree<-data.frame(x=k,y=l,stringsAsFactors = FALSE)
      trees<-trees %>% bind_rows(tree)
    }
    if(line[l] == "."){
      pass<-data.frame(x=k,y=l,stringsAsFactors = FALSE)
      passes<-passes %>% bind_rows(pass)
    }
  }
}

xMax=X3DecembreInput %>% nrow
direction<-c(1,3)
position<-c(1,1)
nStep=(xMax-1) %/% direction[1]
positions<-data.frame(step=0:nStep,x=1,y=1)
positions<-positions %>% mutate (x=1+step*direction[1],y=1+step*direction[2])
positions<-positions %>% mutate (y=((y-1) %% p)+1)
onTheWay<-positions %>% inner_join(trees,by=c("x","y"))
onTheWay %>% nrow
# 265

# B
nbTrees<-function(direction){
  nStep=(xMax-1) %/% direction[1]
  positions<-data.frame(step=0:nStep,x=1,y=1)
  positions<-positions %>% mutate (x=1+step*direction[1],y=1+step*direction[2])
  positions<-positions %>% mutate (y=((y-1) %% p)+1)
  onTheWay<-positions %>% inner_join(trees,by=c("x","y"))
  onTheWay %>% nrow
}
nbTrees(direction = c(1,1))
nbTrees(direction = c(1,3))
nbTrees(direction = c(1,5))
nbTrees(direction = c(1,7))
nbTrees(direction = c(2,1))
61*265*82*70*34
# 3154761400
