source("packages.R")

day09A <- read_table2("day09A.txt", col_names = FALSE)
vect<-day09A$X1

# A

l<-length(vect)
index<-26:l

possib<-function(i){
  the25<-vect[(i-26):(i-1)]
  add<-function(x,y) x+y
  unique(unlist(lapply(the25,add,the25)))
}

# Moche mais rapide
for(i in index){
  if(!(vect[i] %in% possib(i))) print(vect[i])
}
# 258585477

# Moins moche
correct<-function(i){
  return (vect[i] %in% possib(i))
}
vect[which(!(sapply(index, correct)))+25]
# 258585477

# B
wanted<-258585477

# Moche mais rapide
for(i in 1:length(vect)){
  somme<-0
  j<-i
  while(somme<wanted){
    somme=somme+vect[j]
    j<-j+1
  }
  if (somme==wanted) print(c(i,j-1))
}
min(vect[480:496])+max(vect[480:496])
# 36981213

# Moins moche
allSums<-cumsum(vect)
data.frame(i=1:l,sumi=allSums,a=1) %>% left_join(data.frame(j=1:l,sumj=allSums,a=1)) %>% 
  filter(j>i) %>% mutate(diff=sumj-sumi) %>% filter(diff==wanted) %>% head(1)
min(vect[480:496])+max(vect[480:496])
# 36981213
