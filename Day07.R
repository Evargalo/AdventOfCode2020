source("packages.R")

# A

Day07A <- read_delim("Day07A.txt", ";", escape_double = FALSE, 
                     col_names = FALSE, trim_ws = TRUE)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
rmLastWord<-function(x) sub(pattern = "\\s*\\w*$",x = x,replacement = "")
rmFirstWord<-function(x) sub(pattern = "^.*?(\\s)",x = x,replacement = "")
Day07A<-Day07A %>% mutate(container=sub(pattern = "contain.*",x = X1,replacement = ""))
Day07A<-Day07A %>% mutate(inside=sub(pattern = ".*contain ",x = X1,replacement = ""))
Day07A<-Day07A %>% mutate(inside=sub(pattern = "\\.",x = inside,replacement = ""))
Day07A<-Day07A %>% mutate(containerColor=sub(pattern = "\\s*\\w*$",x = trim(container),replacement = ""))

sol<-c("shiny gold")
precSol<-sol
newSol<-(Day07A %>% filter(grepl(pattern = sol,x = inside)))$containerColor
sol<-unique(c(sol,newSol))

while(length(sol)>length(precSol)){
  tempSol<-c()
  for(color in newSol){
    newSolution<-(Day07A %>% filter(grepl(pattern = color,x = inside)))$containerColor
    tempSol<-unique(c(tempSol,newSolution))
  }
  precSol<-sol
  sol<-unique(c(sol,tempSol))
  newSol<-tempSol
}
length(sol)-1
#161

#B

allBags<-data.frame(nb=1,color="shiny gold")

addInsider<-function(color,currentNb){
  newBags<-data.frame(nb=c(),color=c())
  Day07A %>% filter(containerColor==color) -> currentRule
  ul<-trim(unlist(strsplit(currentRule$inside,",")))
  for(insider in rmLastWord(ul)){
    nb<-sub(pattern = "\\s.*",x = insider,replacement = "")
    newCol<-rmFirstWord(insider)
    if(!(nb%in%c("0","no"))) {
      nb<-as.numeric(nb)
      newInsider<-data.frame(nb=nb*currentNb,color=newCol)
      newBags<-newBags %>% bind_rows(newInsider)
    }
  }
  newBags
}

newBags<-allBags

while (0<nrow(newBags)) {
  foundBags<-data.frame(nb=c(),color=c())
  for(i in 1:nrow(newBags)){
    foundBags<-foundBags %>% bind_rows(addInsider(newBags$color[i],newBags$nb[i]))
  }
  newBags<-foundBags
  allBags<-allBags %>% bind_rows(newBags)
}

sum(allBags$nb)-1
# 30899
