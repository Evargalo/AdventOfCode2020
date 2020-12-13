source('~/Perso/AdventOfCode/AoC2020/packages.R')

options(digits=22)

# données dans vecteur
dep<-1002618
x<-0
buses<-c(19,x,x,x,x,x,x,x,x,41,x,x,x,37,x,x,x,x,x,367,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,373,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23)
# buses<-c(17,x,13,19)

# A

buses<-buses[buses>0]
buses-(dep %% buses)
6*373
#2238

# B

buses<-c(19,x,x,x,x,x,x,x,x,41,x,x,x,37,x,x,x,x,x,367,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,373,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23)
which(buses!=0)-1
realBuses<-buses[buses>0]

congruences<-(-(which(buses!=0)-1))%%realBuses
isprime(realBuses) # tous premiers

# On cherche le plus petit nombre $sol congru à chaque $congruences modulo chaque $realBuses

# Restes chinois
n<-prod(realBuses)
nHat<-n %/% realBuses

# Bézout
eucl<-function(r, u, v, rr, uu, vv){
  if(rr==0) return(list(r, u, v))
  eucl(rr, uu, vv, r - (r%/%rr)*rr, u - (r%/%rr)*uu, v - (r%/%rr)*vv)  
}
euclide<-function(a,b){
  eucl(a, 1, 0, b, 0, 1)[[3]]
}
# vérif
eucl(17, 1, 0, 15, 0, 1)
-7*17+8*15
euclide(17,15)

# inverses de $nHat modulo $realBuses (Bezout)
inv<-mapply(euclide,realBuses,nHat)

# verif : inv*nHat est congru à 1 par rapport à son bus mais congru à 0 par rapport à tous les autres
(inv*nHat) %% realBuses
(inv*nHat) %% 19
(inv*nHat) %% 373

# Plus qu'à sommer
sol<-sum((congruences * (inv * nHat)%% n)  )
sol %% n -> sol

sol %% realBuses
congruences
# Bizarrement, décalage de 3 
# (ou 5, ou 2, varie à chaque exécution) (arrondis dans les calculs avec grands entiers ?)

(sol-3) %% realBuses
sol-3
# 560214575859998
