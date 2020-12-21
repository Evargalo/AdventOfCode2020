source('~/Perso/AdventOfCode/AoC2020/packages.R')

Day21 <- read_delim("~/Perso/AdventOfCode/AoC2020/day21simple.txt", 
                    "µ", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)

Day21 <- read_delim("~/Perso/AdventOfCode/AoC2020/day21.txt", 
                    "µ", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)

X1<-Day21$X1
length(X1)

###############################################
# A

rmLastChar<-function(s) substring(s,1,nchar(s)-1)

ingredients<-cutAfter(text = X1,pattern = "\\(contains")
allergens<-cutBefore(text = X1,pattern = "\\(contains") %>% rmLastChar
input<-data.frame(ingredients,allergens)

possibleIngredients<-data.frame(ingredients="ingredients",allergen="allergen",stringsAsFactors = FALSE)
buildCorresp<-function(ingredients,allergens){
  listAllergens<-strsplit(allergens,",")
  for (al in listAllergens){
    possibleIngredients %>% 
      add_row(ingredients=ingredients,allergen=al) ->> possibleIngredients
  }
}

mapply(FUN = buildCorresp,ingredients,allergens)
possibleIngredients %>% filter(ingredients!="ingredients") %>%
  mutate_all(trimws)->> possibleIngredients

allAllerg<-unique(possibleIngredients$allergen)

findIngredient<-function(allerg){
  df<- possibleIngredients %>% filter(allergen==allerg) %>% select(ingredients)
  suspects<-unlist(strsplit(df[1,]," "))
  i<-1
  while(i<nrow(df) && length(suspects)>1){
    i<-i+1
    suspects<-unlist(strsplit(df[i,]," ")) %>% intersect(suspects)
  }
  suspects
}

sapply(X = allAllerg,FUN = findIngredient) %>% unlist %>% unique ->dangerous

strsplit(ingredients," ") %>% unlist -> listIngr

sum(!(listIngr %in% dangerous))
#2874

###############################################
# B

corresp<-data.frame(allAllerg,culprit="",stringsAsFactors = FALSE)

listDangers<-sapply(X = allAllerg,FUN = findIngredient) 

# By hand coz there are only 8 allergens
corresp$culprit[corresp$allAllerg=="fish"]<-"ndkkq"
corresp$culprit[corresp$allAllerg=="sesame"]<-"sgzr"
corresp$culprit[corresp$allAllerg=="dairy"]<-"gfvrr"
corresp$culprit[corresp$allAllerg=="soy"]<-"pkkg"
corresp$culprit[corresp$allAllerg=="peanuts"]<-"bthjz"
corresp$culprit[corresp$allAllerg=="shellfish"]<-"mbkbn"
corresp$culprit[corresp$allAllerg=="nuts"]<-"jxcxh"
corresp$culprit[corresp$allAllerg=="wheat"]<-"mjbtz"

corresp %>% arrange(allAllerg) %>% select(culprit) %>% unlist %>% paste(collapse = ",")
# "gfvrr,ndkkq,jxcxh,bthjz,sgzr,mbkbn,pkkg,mjbtz"

