source('~/Perso/AdventOfCode/AoC2020/packages.R')

Day18 <- read_csv("~/Perso/AdventOfCode/AoC2020/day18simple.txt", 
                  col_names = FALSE)
Day18 <- read_csv("~/Perso/AdventOfCode/AoC2020/day18.txt", 
                  col_names = FALSE)

X1<-Day18$X1

# A

calcExpr<-function(expr){
  vect<-unlist(strsplit(expr, ""))
  l<-length(vect)
  if (!any(vect %in% c("+","*","("))) {
    return(as.double(reduce(vect,paste0)))
  }
  if(any(vect =="(")){
    Open<-which(vect=="(")
    Closed<-which(vect==")") 
    op<-last(which(vect =="("))
    cl<-Closed[first(which(Closed>op))]
    val<-reduce(vect[(op+1):(cl-1)],paste0)
    if(op==1) {part1<-""} else {part1<-reduce(vect[(1):(op-1)],paste0)}
    if(cl==l) {part3<-""} else {part3<-reduce(vect[(cl+1):(l)],paste0)}
    return(calcExpr(paste0(part1,calcExpr(val),part3)))
  } 
  rankOpe<-last(which(vect %in% c("+","*")))
  val<-reduce(vect[1:(rankOpe-1)],paste0)
  val2<-reduce(vect[(rankOpe+1):length(vect)],paste0)
  ope<-vect[rankOpe]
  if(ope=="+") return(calcExpr(val)+calcExpr(val2))
  return(calcExpr(val)*calcExpr(val2))
}

calc<-function(expr){
  print(expr)
  vect<-unlist(strsplit(expr, ""))
  vect<-vect[vect!=" "]
  expr<-reduce(vect,paste0)
  calcExpr(expr)
}

res<-0
for(i in 1:length(X1)) res[i]<-calc(X1[i])
sum(res)
# 5783053349377


# B

calcExpr<-function(expr){
  print(expr)
  vect<-unlist(strsplit(expr, ""))
  l<-length(vect)
  if (!any(vect %in% c("+","*"))) {
    return(as.double(reduce(vect,paste0)))
  }
  if(any(vect =="(")){
    op<-first(which(vect =="("))
    Open<-which(vect=="(")
    Closed<-which(vect==")") 
    lagClosed<-lag(Closed)
    lagClosed[1]<-l
    if (length(Closed)>1 & any(lagClosed<Open)){
      newPar<-which(lagClosed<Open)[1]
      cl<-Closed[newPar-1]
    } else { cl<-last(Closed) }
    val<-reduce(vect[(op+1):(cl-1)],paste0)
    if(op==1) {part1<-""} else {part1<-reduce(vect[(1):(op-1)],paste0)}
    if(cl==l) {part3<-""} else {part3<-reduce(vect[(cl+1):(l)],paste0)}
    return(calcExpr(paste0(part1,calcExpr(val),part3)))
  } 
  if (any(vect=="*")){
    rankOpe<-first(which(vect =="*")) 
    val<-reduce(vect[1:(rankOpe-1)],paste0)
    val2<-reduce(vect[(rankOpe+1):length(vect)],paste0)
    return(calcExpr(val)*calcExpr(val2))
  }
  rankOpe<-first(which(vect =="+")) 
  val<-reduce(vect[1:(rankOpe-1)],paste0)
  val2<-reduce(vect[(rankOpe+1):l],paste0)
  return(calcExpr(val)+calcExpr(val2))
}

res<-0
for(i in 1:length(X1)) res[i]<-calc(X1[i])
sum(res)
# 74821486966872


