source("packages.R")

# A
rules<-fread("day02A.txt",header = FALSE)
rules %>% mutate(min=sub(pattern="-.*","",V1),
                 max=sub(pattern=".*-","",V1),
                 char=sub(pattern=":","",V2)) -> rules
rules %>% mutate(occur=str_count(V3,char),
                 min=as.numeric(min),
                 max=as.numeric(max))->rules
rules %>% filter(min<=occur & occur<=max) %>% nrow
# 638

# B
rules %>% mutate(firstChar=substr(V3,min,min),
                 lastChar=substr(V3,max,max),
                 valid=xor(firstChar==char,lastChar==char))->rules
sum(rules$valid)
# 699
