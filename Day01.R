source("packages.R")

numbers<-fread("day01A.txt")
numbers<-distinct(numbers)

# Part A
sol<-intersect(numbers,2020-numbers)
sol[1]*sol[2]
# 1014624

# Part B
numbers<-as.vector(numbers$V1)
x=rep(numbers,times=200)
y=rep(numbers,each=200)

table<-data.frame(x,y)
table %>% filter(x != y) -> table
table %>% mutate(somme=x+y) -> table
table %>% filter(somme<=2020) -> table
table %>% mutate(need=2020-somme) -> table
sol<-intersect(numbers,table$need)
sum(sol)
prod(sol)
# 80072256
