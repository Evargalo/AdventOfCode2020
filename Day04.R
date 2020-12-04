source("packages.R")

# A
day04A <- read_csv("day04A.txt", col_names = FALSE)
#day04A <- read_csv("day04BInvalid.txt", col_names = FALSE)

day04A %>% mutate(
  hasbyr = str_detect(X1,"byr:"),
  hasiyr = str_detect(X1,"iyr:"),
  haseyr = str_detect(X1,"eyr:"),
  hashgt = str_detect(X1,"hgt:"),
  hashcl = str_detect(X1,"hcl:"),
  hasecl = str_detect(X1,"ecl:"),
  haspid = str_detect(X1,"pid:"),
  hascid = str_detect(X1,"cid:")
)->day04A
day04A %>% mutate(valid=hasbyr & hasiyr & haseyr & hashgt & hashcl & hasecl & haspid) -> day04A
day04A %>% filter(valid) %>% nrow
# 204

# B

validEcl<- c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
validUnit<- c("cm","in")

day04A %>% filter(valid) -> step2

step2 %>% mutate(byr=sub(pattern=".*byr:","",X1)) %>%  
  mutate(byr=sub(pattern="\\s.*","",byr)) %>%
  filter(nchar(byr)==4) %>% 
  
  mutate(iyr=sub(pattern=".*iyr:","",X1)) %>%
  mutate(iyr=sub(pattern="\\s.*","",iyr)) %>%
  filter(nchar(iyr)==4) %>%
  
  mutate(eyr=sub(pattern=".*eyr:","",X1)) %>%   
  mutate(eyr=sub(pattern="\\s.*","",eyr)) %>%
  filter(nchar(eyr)==4) %>%
  
  mutate(hgt=sub(pattern=".*hgt:","",X1)) %>% 
  mutate(hgt=sub(pattern="\\s.*","",hgt)) %>%
  mutate(hgtUnit=substr(hgt,nchar(hgt)-1,nchar(hgt))) %>% 
  mutate(hgtNb=as.numeric(substr(hgt,1,nchar(hgt)-2))) %>% 
  filter(hgtUnit%in%validUnit) %>% 
  mutate(hgtValid=if_else(hgtUnit=="cm",
                          hgtNb>149 & hgtNb<194,
                          hgtNb>58 & hgtNb<77))%>% 
  
  mutate(hcl=sub(pattern=".*hcl:","",X1)) %>%  
  mutate(hcl=sub(pattern="\\s.*","",hcl)) %>%
  filter(nchar(hcl)==7) %>%
  
  mutate(ecl=sub(pattern=".*ecl:","",X1)) %>% 
  mutate(ecl=sub(pattern="\\s.*","",ecl)) %>%
  filter(nchar(ecl)==3) %>%
  
  mutate(pid=sub(pattern=".*pid:","",X1)) %>% 
  mutate(pid=sub(pattern="\\s.*","",pid)) %>%
  filter(nchar(pid)==9) -> step2

step2 %>% 
  filter(byr>1919 & byr<2003) %>% 
  filter(iyr>2009 & iyr<2021) %>% 
  filter(eyr>2019 & eyr<2031)  %>% 
  filter(str_detect(string = hcl,pattern = "#[0-9a-f]{6}")) %>% 
  filter(ecl %in% validEcl) %>% 
  filter(str_detect(string = pid,pattern = "[0-9]{9}")) %>% 
  filter(hgtValid) -> result

result %>% nrow
# 179
