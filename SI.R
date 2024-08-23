library(haven)
library(tidyverse)
library(BBmisc)
library(dplyr)
library(tidyr)

#### data #### 
# load and prepare data
BES<-read_dta('BES2019_W21_Panel_v21.0.dta')


intact<-bes
#### Main variables #### 
# wave 
# wave variable 
wavedt<-bes[c(1,8,9,11)]
# interview date
timedt<-bes[c(1, 30, 31, 33)]
timedt[-c(1)]<-lapply(timedt[-c(1)], as.Date)
names(timedt) = gsub(pattern = "start", replacement = "", x = names(timedt))
# MIP/MII  

mipdt <- bes[, c(1, grep("^mii_catW", names(bes)))]
mipfunction <- function (x) {
  ifelse(x == 9 | x == 12 | x ==13, 1, ifelse(is.na(x)==FALSE, 0, NA))
}  
mipdt[-c(1)]<-lapply(mipdt[-c(1)], mipfunction)
names(mipdt) = gsub(pattern = "mii_cat", replacement = "MIP", x = names(mipdt))
mipdt<-subset(mipdt, select = c("id", "MIPW7", "MIPW8", "MIPW10"))


# anti-immigration attitudes  
immatt<-bes[c("id", "immigEconW7", "immigEconW8",
              "immigEconW10", 
              "immigCulturalW7", "immigCulturalW8",
              "immigCulturalW10")] 
myf9999 <- function (x) {
  ifelse(x == 9999, NA, x)
} 
myf1to7 <- function (x) {
  mapValues(x, from=c(1:7), to=c(7:1))
}  

immatt[-c(1)]<-as.data.frame(lapply(immatt[-c(1)], myf9999))
immatt[-c(1)]<-as.data.frame(lapply(immatt[-c(1)], myf1to7))


immatt <- immatt %>%
  mutate(imm_att_W7 = rowMeans(dplyr::select(., immigCulturalW7, immigEconW7), na.rm = TRUE), 
         imm_att_W8 = rowMeans(dplyr::select(., immigCulturalW8, immigEconW8), na.rm = TRUE),
         imm_att_W10 = rowMeans(dplyr::select(., immigCulturalW10, immigEconW10), na.rm = TRUE))

maindt <- merge(merge(merge(timedt, wavedt, by='id'), mipdt, by='id'), immatt, by='id')


####  control variables #### 
# satisfaction with democracy UK/ EU
satDemUK<-bes %>%
  dplyr::select(id, contains("satDemUK"))
satDemUK[-c(1)]<-lapply(satDemUK[-c(1)], myf9999)
satDemUK<-satDemUK[c(1:4)]

satDemUK<-satDemUK[c("id", "satDemUKW7", "satDemUKW8",
              "satDemUKW10")]

satDemEU<-bes %>%
  dplyr::select(id, contains("satDemEU"))
satDemEU[-c(1)]<-lapply(satDemEU[-c(1)], myf9999)
satDemEU<-satDemEU[c("id", "satDemEUW7", "satDemEUW8",
                     "satDemEUW10")]

# past vote 
p_past_vote<-bes %>% 
  dplyr::select(id, contains("p_past_vote"))   
p_past_vote[-c(1)]<-lapply(p_past_vote[-c(1)], myf9999)   

# ethnicity 
p_ethnicitydt<-bes %>%
  dplyr::select(id, contains("p_ethnicity")) 

mydummy <- function (x) {
  ifelse(x == 1, 1, 0)
}

p_ethnicitydt[-c(1)]<-lapply(p_ethnicitydt[-c(1)], mydummy)
p_ethnicitydt[-c(1)]<-lapply(p_ethnicitydt[-c(1)], as.factor)
p_ethnicitydt<-p_ethnicitydt[c(1,8,9, 11)]

contdt <- merge(merge(merge(satDemUK, p_past_vote, by='id'), p_ethnicitydt, by='id'), satDemEU, by='id')
widedt<-merge(maindt, contdt, by='id')


# media salience data #### 
uk_salience<-read.csv("/Users/ge28ful/Desktop/Iclouds_download/0 DATA/Newsarticles_UK/salience/uk_salience2.csv")
salience_W7<-uk_salience[c(2,3)]
names(salience_W7)<-c("timeW7", "salienceW7")
salience_W8<-uk_salience[c(2,3)]
names(salience_W8)<-c("timeW8", "salienceW8")
salience_W10<-uk_salience[c(2,3)]
names(salience_W10)<-c("timeW10", "salienceW10")

salience_W7$timeW7<-as.Date(salience_W7$timeW7)
salience_W8$timeW8<-as.Date(salience_W8$timeW8)
salience_W10$timeW10<-as.Date(salience_W10$timeW10)

widedt<-merge(widedt, salience_W7, by="timeW7", all.x = T)
widedt<-merge(widedt, salience_W8, by="timeW8", all.x = T)
widedt<-merge(widedt, salience_W10, by="timeW10", all.x = T)

      

write.csv(widedt, "SI_widedt.csv")



widedt<-read.csv("SI_widedt.csv")
#### long data #### 
widedt$id<-as.factor(widedt$id) 
# rearrange variable orders 
idcols <- c("id")
cols <- c(idcols, names(widedt)[-which(names(widedt) %in% idcols)]) 
widedt <- widedt[cols] 


### change wide into long ### 
# main variables #### 
# time 
long_timedt<-widedt %>%
  select(id, contains("timeW"))%>% 
  gather(key = "time", value = "time_v", -id)

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

long_timedt <- create_new_id(long_timedt, "id", "time", "_")
long_timedt$id_new <- str_replace(long_timedt$id_new, "time", "")
long_timedt<-long_timedt[-c(2)]

# wave 
wave_long<-widedt %>%
  dplyr::select(id, contains("wave"))%>% 
  gather(key = "wave", value = "wave_v", -id)
wave_long <- create_new_id(wave_long, "id", "wave", "_")
wave_long$id_new <- str_replace(wave_long$id_new, "wave", "W")
wave_long<-wave_long[-c(1)]

# MIP 
mip_long<-widedt %>%
  dplyr::select(id, contains("MIP"))%>% 
  gather(key = "MIP", value = "MIP_v", -id)
mip_long <- create_new_id(mip_long, "id", "MIP", "_")
mip_long$id_new <- str_replace(mip_long$id_new, "MIP", "")
mip_long<-mip_long[-c(1,2)]


# immigration attitudes 
imm_att_long<-widedt %>%
  select(id, contains("imm_att"))%>% 
  gather(key = "imm_att", value = "imm_att_v", -id)
imm_att_long <- create_new_id(imm_att_long, "id", "imm_att", "")
imm_att_long$id_new <- str_replace(imm_att_long$id_new, "imm_att", "") 
imm_att_long<-imm_att_long[-c(1,2)]

# control variables #### 
satDemUK_long<-widedt %>% 
  select(id, contains("satDemUK"))%>% 
  gather(key = "satDemUK", value = "satDemUK_v", -id)
satDemUK_long <- create_new_id(satDemUK_long, "id", "satDemUK", "")
satDemUK_long$id_new <- str_replace(satDemUK_long$id_new, "satDemUK", "_") 
satDemUK_long<-satDemUK_long[-c(1,2)]


satDemEU_long<-widedt %>% 
  select(id, contains("satDemEU"))%>% 
  gather(key = "satDemEU", value = "satDemEU_v", -id)
satDemEU_long <- create_new_id(satDemEU_long, "id", "satDemEU", "")
satDemEU_long$id_new <- str_replace(satDemEU_long$id_new, "satDemEU", "_") 
satDemEU_long<-satDemEU_long[-c(1,2)]

# ethnicity 
p_ethnicity<-widedt %>% 
  dplyr::select(id, contains("p_ethnicityW"))%>%
  gather(key = "p_ethnicity", value = "p_ethnicity_v", -id)
p_ethnicity_long <- create_new_id(p_ethnicity, "id", "p_ethnicity", "")
p_ethnicity_long$id_new <- str_replace(p_ethnicity_long$id_new, "p_ethnicity", "_") 
p_ethnicity_long<-p_ethnicity_long[-c(1,2)]



#### media data #### 
mediasalience_long<-widedt %>% 
  dplyr::select(id, salienceW7, salienceW8, salienceW10) %>%
  gather(key = "mediasalience", value = "mediasalience_v", -id)
mediasalience_long <- create_new_id(mediasalience_long, "id", "mediasalience", "")
mediasalience_long$id_new <- str_replace(mediasalience_long$id_new, "salience", "_") 
mediasalience_long<-mediasalience_long[c(3,4)]



####  merge them #### 
longdt<-merge(merge(merge(long_timedt, wave_long, by="id_new"), mip_long, by="id_new"), 
              imm_att_long, by='id_new')
longdt<-merge(merge(longdt, satDemUK_long, by="id_new"), satDemEU_long, by="id_new")
longdt<-merge(merge(longdt, mediasalience_long, by="id_new", all.x = T), p_ethnicity_long, all.x=T, by="id_new")


write.csv(longdt, "SI_longdt.csv")




# analysis #### 

library(cquad)
library(plm)
library(MASS)
library(stringr)

longdt$wave_n<-str_replace(longdt$wave, "wave", "")
longdt$wave_n<-as.numeric(longdt$wave_n)
longdt<-subset(longdt, p_ethnicity_v==1)

idcols <- c("id", "wave_n")
cols <- c(idcols, names(longdt)[-which(names(longdt) %in% idcols)]) 
longdt <- longdt[cols]  

model1 = cquad(MIP_v ~ mediasalience_v + satDemUK_v + satDemEU_v, data = longdt)
summary(model1)
exp(coef(model1))
exp(cbind(OR = coef(model1), confint(model1)))

p_longdt <- pdata.frame(longdt, index=c("id", "wave_n"))
fixed_UK<-plm(imm_att_v ~ mediasalience_v + satDemUK_v + satDemEU_v, data=p_longdt, 
              index=c("id_new", "wave_n"),
              model="within")
options(scipen=999)
summary(fixed_UK)

