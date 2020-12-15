source('~/Perso/AdventOfCode/AoC2020/packages.R')

numbers<-c(15,12,0,14,3,1)
# numbers<-c(3,1,2)

for(i in 1:2020){
  prev<-which(numbers==last(numbers))
  if (length(prev)==1) {new<-0} else {
    new<-last(prev-lag(prev))
  }
  numbers<-c(numbers,new)
  if(i%%20==0) print(i)
}

numbers[2020]
# 249

# B

last0<-3
rk<-rep(0,100) # rank of last occurence of each number >0
# Init
numbers<-c(15,12,0,14,3,1)
for(i in 1:5){
 if(i!=0) rk[numbers[i]]<-i
} 
new<-1
nb<-length(rk)
# Loop
for(i in 6:(30000000-1)){
  if(i %% 100000==0) print(i)
  if(new==0) {new<-i-last0 ; last0<-i ;next}
  if(new>nb) {rk<-c(rk,rep(0,new-nb-1),i); nb<-new; new<-0; next}
  k<-rk[new]
  rk[new]<-i
  ifelse(k==0,new<-0,new<-i-k)
}

new
# 41687
