source("packages.R")

# A
Day05A <- read_csv("day05A.txt", col_names = FALSE)
BinToDec <- function(x) sum(2^(which(rev(unlist(strsplit(as.character(x), "")) %in% c("R","B")))-1))
Day05A$place<-sapply(Day05A$X1,BinToDec)
max(Day05A$place)
# 896

# B
allPlaces<-Day05A$place
B<-data.frame(A=1:(127*8+7))
B %>% filter (!A %in% allPlaces & (A-1)%in%allPlaces & (A+1)%in%allPlaces)
# 659
