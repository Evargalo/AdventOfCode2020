source("packages.R")

# Data-frame des relations parent-child
Day07A <- read_delim("Day07A.txt", ";", escape_double = FALSE, 
                     col_names = FALSE, trim_ws = TRUE)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
rmLastWord<-function(x) sub(pattern = "\\s*\\w*$",x = x,replacement = "")
rmFirstWord<-function(x) sub(pattern = "^.*?(\\s)",x = x,replacement = "")

Day07A %>% mutate(container=sub(pattern = "contain.*",x = X1,replacement = "")) %>% 
  mutate(inside=sub(pattern = ".*contain ",x = X1,replacement = "")) %>% 
  mutate(inside=sub(pattern = "\\.",x = inside,replacement = "")) %>% 
  mutate(container=sub(pattern = "\\s*\\w*$",x = trim(container),replacement = "")) %>% 
  select(container,inside)-> Day07A

fillTree<-function(container,inside){
  aTree<-data.frame(parent=c(),child=c(),nb=c())
  children<-rmLastWord(trim(unlist(strsplit(inside,","))))
  for (child  in children){
    nb<-sub(pattern = "\\s.*",x = child,replacement = "")
    if(!(nb%in%c("0","no"))) {
      nb<-as.numeric(nb)
      child<-rmFirstWord(child)
      newRow<-data.frame(parent=container,child=child,nb=nb)
      aTree<-aTree %>% bind_rows(newRow)
    }
  }
  aTree
}

treeDF<-pmap_dfr(.l = Day07A,.f = fillTree)

# B

colorsTree<-Node$new(name = "shiny gold")
colorsTree$nb<-1

addChildren<-function(xNode){
  xNode$Get("name")->x
  treeDF %>% filter(parent==x) -> subDF
  if(nrow(subDF)>0){
    for(i in 1:nrow(subDF)){
      child<-subDF$child[i]
      nb<-subDF$nb[i]
      xNode$AddChild(name=child,nb=nb)
      addChildren(xNode$Climb(name=child))
    }
  }
}

addChildren(colorsTree)

plot(colorsTree)
plot(colorsTree,output = "visNetwork")

bagsInside <- function(node) {
  ifelse(node$isLeaf, node$nb, node$nb * (1+sum(sapply(node$children, bagsInside))))->result
  return(result)
}

colorsTree$Get(bagsInside)
(colorsTree$Get(bagsInside))[1]-1
# 30899

# A 
# Attention, pas un vrai arbre car il y a des circuits -> des noeuds sont dupliqués

colorsTree<-Node$new(name = "shiny gold")
colorsTree$nb<-1

addParents<-function(xNode){
  xNode$Get("name")->x
  treeDF %>% filter(child==x) -> subDF
  if(nrow(subDF)>0){
    for(i in 1:nrow(subDF)){
      parent<-subDF$parent[i]
      nb<-subDF$nb[i]
      xNode$AddChild(name=parent,nb=nb)
      addParents(xNode$Climb(name=parent))
    }
  }
}

addParents(colorsTree)

plot(colorsTree)
plot(colorsTree,output = "visNetwork")

colorsTree$totalCount
# Trop à cause des noeuds dupliqués

names<-c()
addName<-function(x){
  if(!(x$name %in% names)) names<<-c(names,x$name)
  if(!(x$isLeaf)) sapply(x$children,addName)
  return(1)
}
addName(colorsTree)
length(names)-1
#161
