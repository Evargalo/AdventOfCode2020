source("packages.R")

# A

Day06A <- read_csv("day06A.txt", col_names = FALSE)

countNb<-function(x) length(unique(strsplit(x,"")[[1]]))
Day06A$nb<-sapply(X = Day06A$X1,FUN = countNb)
sum(Day06A$nb)
# 6506

# B

Day06B <- read_csv("day06B.txt", col_names = FALSE)

countRes<- function(x) {
  vecteur<-strsplit(x,"\\$")[[1]]
  length(vecteur)->nbPers
  setLetters<-unique(strsplit(vecteur[1],"")[[1]])
   for (i in 1:nbPers){
     setLetters<- intersect(setLetters,strsplit(vecteur[i],"")[[1]])
   }
  length(setLetters)
}

Day06B$nb<-sapply(Day06B$X1,countRes)
sum(Day06B$nb)
# 3243
