library(haven)
library(tidyverse)
library(BBmisc)
library(dplyr)
library(tidyr)

#### wide data #### 

####  load and prepare data #### 
gesis<-read_dta("GESIS.dta") 

# missing function 
minusf<-function(x){
  ifelse(x<0, NA, x)
}

# 0 to 10 recode function 
myf1to7<- function (x) {
  BBmisc::mapValues(x, from=c(1:7), to=c(7:1))
}
# 0 to 10 recode function 
myf1to10 <- function (x) {
  BBmisc::mapValues(x, from=c(1:10), to=c(10:1))
}

myf0to10 <- function (x) {
  BBmisc::mapValues(x, from=c(0:10), to=c(10:0))
}

# 1 to 5 recode function 
myf1to5 <- function (x) {
  BBmisc::mapValues(x, from=c(1:5), to=c(5:1))
}

# dummy variable (99)
dummy99_f<-function(x){
  ifelse(x==99, 1, ifelse(is.na(x)==FALSE, 0, x))
}

# missing (99)
missing99_f<-function(x){
  ifelse(x==99, NA, x)
}

# missing (98)
missing98_f<-function(x){
  ifelse(x==98, NA, x)
}

#### Main variables #### 
# id = z000001a 
# wave participation variable 
gesis$wave_ga<-minusf(gesis$gaza003a)
gesis$wave_gb<-minusf(gesis$gbza003a)
gesis$wave_gc<-minusf(gesis$gcza003a)

# time wave gabc 
gesis$time_ga<-as.Date(gesis$gazp205a)
gesis$time_gb<-as.Date(gesis$gbzp205a)
gesis$time_gc<-as.Date(gesis$gczp205a)

# MIP 
gesis$MIP_ga<-minusf(gesis$gace067a)
gesis$MIP_gb<-minusf(gesis$gbce156a)
gesis$MIP_gc<-minusf(gesis$gcce052a)

gesis$MIP_ga_b<-ifelse(gesis$MIP_ga>7, 1, 
                    ifelse(gesis$MIP_ga<=7, 0, NA))
gesis$MIP_gb_b<-ifelse(gesis$MIP_gb>7, 1, 
                    ifelse(gesis$MIP_gb<=7, 0, NA))
gesis$MIP_gc_b<-ifelse(gesis$MIP_gc>7, 1, 
                    ifelse(gesis$MIP_gc<=7, 0, NA))

# anti-immigrant sentiment
gesis$imm_att_ga<-minusf(gesis$gace069a)
gesis$imm_att_gb<-minusf(gesis$gbce158a)
gesis$imm_att_gc<-minusf(gesis$gcce054a)
gesis[c(15389:15391)]<-lapply(gesis[c(15389:15391)], myf0to10)



#### control #### 
# 2017 General elections
gesis$BTW_2017_first<-minusf(gesis$eebu124a)
gesis$BTW_2017_second<-minusf(gesis$eebu126a)

gesis$BTW_2017<-ifelse(gesis$BTW_2017_first==1 | gesis$BTW_2017_second== 1, "CDU", 
                        ifelse(gesis$BTW_2017_first==2 | gesis$BTW_2017_second== 2, "SPD", 
                               ifelse(gesis$BTW_2017_first==6 | gesis$BTW_2017_second== 6, "AFD", "others")))


# satisfaction federal government 
gesis$satisfe_ga<-minusf(gesis$gace070a)
gesis$satisfe_gb<-minusf(gesis$gbce159a)
gesis$satisfe_gc<-minusf(gesis$gcce055a)


# satisfaction EU 
gesis$satiseu_ga<-minusf(gesis$gace072a)
gesis$satiseu_gb<-minusf(gesis$gbce161a)
gesis$satiseu_gc<-minusf(gesis$gcce057a)


# migration status 
gesis<-gesis %>% mutate(migrationstatus = case_when(
  (a11d077d==1 | d11d077d==1 | f11d077d==1) & (a11d075d==1 | d11d075d==1 | f11d075d==1) ~ 1, 
  TRUE ~ 0)
)




# subset 
gesis<-gesis[c(1, 15383:15407)]
gesis<-subset(gesis, wave_ga==1 | wave_gb==1 | wave_gc==1)


#### media salience #### 

media<-read.csv("de_salience.csv")

media_ga<-subset(media, select=c(time_ga, salience_ga))
media_gb<-subset(media, select=c(time_gb, salience_gb))
media_gc<-subset(media, select=c(time_gc, salience_gc))


gesis<-merge(merge(merge(gesis, media_ga, by="time_ga", all.x=T), 
                   media_gb, by="time_gb", all.x=T), 
             media_gc, by="time_gc", all.x=T)


write.csv(gesis, "gesis_widedt.csv")

#### long data #### 

# wave 
gesis$id<-as.factor(gesis$z000001a)

wave_long<-gesis %>%
  dplyr::select(id, contains("wave"))%>% 
  gather(key = "wave", value = "wave_v", -id)


create_new_id <- function(data, id_column, time_column, id_prefix = "") {
  unique_time <- unique(data[[time_column]])
  data$id_new <- NA_character_
  
  for (time_val in unique_time) {
    matching_rows <- data[[time_column]] == time_val
    new_id <- paste0(data[matching_rows, id_column], id_prefix, time_val)
    data$id_new[matching_rows] <- new_id
  }
  
  return(data)
}
wave_long <- create_new_id(wave_long, "id", "wave", "")
wave_long$id_new <- str_replace(wave_long$id_new, "wave", "")
wave_long<-wave_long[c(2:4)]

# time 
time_long<-gesis %>%
  dplyr::select(id, contains("time"))%>% 
  gather(key = "time", value = "time_v", -id)
time_long <- create_new_id(time_long, "id", "time", "")
time_long$id_new <- str_replace(time_long$id_new, "time", "")
time_long<-time_long[-c(2)]

# MIP continuous 
MIP_long<-gesis %>%
  dplyr::select(id, contains("MIP"))
head(MIP_long)
MIP_long<-MIP_long[c(1:4)]
MIP_long<-MIP_long %>% 
  gather(key = "MIP", value = "MIP_v", -id)
MIP_long <- create_new_id(MIP_long, "id", "MIP", "")
MIP_long$id_new <- str_replace(MIP_long$id_new, "MIP", "")
MIP_long<-MIP_long[c(3:4)]


# MIP binary 
MIP_long<-gesis %>%
  dplyr::select(id, contains("MIP"))
MIP_b_long<-MIP_long[c(1, 5:7)]
MIP_b_long<-MIP_b_long %>% 
  gather(key = "MIP_b", value = "MIP_b_v", -id)
MIP_b_long <- create_new_id(MIP_b_long, "id", "MIP_b", "")
MIP_b_long$id_new <- str_replace(MIP_b_long$id_new, "_b", "")
MIP_b_long$id_new <- str_replace(MIP_b_long$id_new, "MIP", "")


# anti-immigration attitudes 
anti_long<-gesis %>%
  dplyr::select(id, contains("imm_att"))%>% 
  gather(key = "imm_att", value = "imm_att_v", -id)
anti_long <- create_new_id(anti_long, "id", "imm_att", "")
anti_long$id_new <- str_replace(anti_long$id_new, "imm_att", "")
anti_long<-anti_long[c(3:4)]


# satis faction: federal government 
satisfe_long<-gesis %>%
  dplyr::select(id, contains("satisfe"))%>% 
  gather(key = "satisfe", value = "satisfe_v", -id)
satisfe_long <- create_new_id(satisfe_long, "id", "satisfe", "")
satisfe_long$id_new <- str_replace(satisfe_long$id_new, "satisfe", "")
satisfe_long<-satisfe_long[c(3:4)]


# satis faction: EU 
satiseu_long<-gesis %>%
  dplyr::select(id, contains("satiseu"))%>% 
  gather(key = "satisEU", value = "satisEU_v", -id)
satiseu_long <- create_new_id(satiseu_long, "id", "satisEU", "")
satiseu_long$id_new <- str_replace(satiseu_long$id_new, "satiseu", "")
satiseu_long<-satiseu_long[c(3:4)]



# migrationstatus 
migration_long<-gesis %>%
  dplyr::select(id, contains("migration"))%>% 
  gather(key = "migration", value = "migration_v", -id)
migration_long<-migration_long[-c(2)]


# salience 
salience_long<-gesis %>%
  dplyr::select(id, contains("all"))%>% 
  gather(key = "salience", value = "salience_v", -id)
salience_long <- create_new_id(salience_long, "id", "salience", "")
salience_long$id_new <- str_replace(salience_long$id_new, "salience", "")
salience_long<-salience_long[c(3:4)]

####  merge everything #### 
longdt<-merge(merge(merge(time_long, wave_long, by="id_new"), 
                    MIP_long, by="id_new"), 
              anti_long, by='id_new') 
longdt<-merge(longdt, MIP_b_long, by="id_new")
longdt<-merge(longdt, anti_long, by="id_new")
longdt<-merge(longdt, satisfe_long, by="id_new")
longdt<-merge(longdt, satiseu_long, by="id_new")
longdt<-merge(longdt, salience_long, by="id_new")
longdt<-merge(longdt, migration_long, by="id")


write.csv(longdt, "gesis_longdt.csv")

