library(haven)
library(tidyverse)
library(BBmisc)
library(dplyr)
library(tidyr)

#### data #### 
# load and prepare data
BES<-read_dta('BES2019_W21_Panel_v21.0.dta')
 


#### Main variables #### 
# wave 
# wave variable 
wavedt<-BES[c(1:4)]

# interview date
timedt<-BES[c(1, 24:26)]
timedt[-c(1)]<-lapply(timedt[-c(1)], as.Date)
names(timedt) = gsub(pattern = "start", replacement = "", x = names(timedt))


# MIP/MII  
mipdt <- BES[, c(1, grep("^mii_catW", names(BES)))]
mipfunction <- function (x) {
  ifelse(x == 9 | x == 12 | x ==13, 1, ifelse(is.na(x)==FALSE, 0, NA))
}  
mipdt[-c(1)]<-lapply(mipdt[-c(1)], mipfunction)
names(mipdt) = gsub(pattern = "mii_cat", replacement = "MIP", x = names(mipdt))
mipdt<-subset(mipdt, select = c("id", "MIPW1", "MIPW2", "MIPW3"))


# anti-immigration attitudes  
immatt<-BES[c("id", "immigEconW1", "immigEconW2",
             "immigEconW3", 
             "immigCulturalW1", "immigCulturalW2",
             "immigCulturalW3")] 

myf9999 <- function (x) {
  ifelse(x == 9999, NA, x)
} 
myf1to7 <- function (x) {
  mapValues(x, from=c(1:7), to=c(7:1))
}  

immatt[-c(1)]<-as.data.frame(lapply(immatt[-c(1)], myf9999))
immatt[-c(1)]<-as.data.frame(lapply(immatt[-c(1)], myf1to7))

immatt <- immatt %>%
  mutate(imm_att_W1 = rowMeans(select(., immigCulturalW1, immigEconW1), na.rm = TRUE), 
         imm_att_W2 = rowMeans(select(., immigCulturalW2, immigEconW2), na.rm = TRUE),
         imm_att_W3 = rowMeans(select(., immigCulturalW3, immigEconW3), na.rm = TRUE))

maindt <- merge(merge(merge(timedt, wavedt, by='id'), mipdt, by='id'), immatt, by='id')


####  control variables #### 
# satisfaction with democracy UK/ EU
satDemUK<-BES %>%
  dplyr::select(id, contains("satDemUK"))
satDemUK[-c(1)]<-lapply(satDemUK[-c(1)], myf9999)
satDemUK<-satDemUK[c(1:4)]

satDemEU<-BES %>%
  dplyr::select(id, contains("satDemEU"))
satDemEU[-c(1)]<-lapply(satDemEU[-c(1)], myf9999)
satDemEU<-satDemEU[c(1:4)]

# past vote 
p_past_vote<-BES %>% 
  dplyr::select(id, contains("p_past_vote"))   
p_past_vote[-c(1)]<-lapply(p_past_vote[-c(1)], myf9999)   

# ethnicity 
p_ethnicitydt<-BES %>%
  select(id, contains("p_ethnicity"))  

mydummy <- function (x) {
  ifelse(x == 1, 1, 0)
}

p_ethnicitydt[-c(1)]<-lapply(p_ethnicitydt[-c(1)], mydummy)
p_ethnicitydt[-c(1)]<-lapply(p_ethnicitydt[-c(1)], as.factor)
p_ethnicitydt<-p_ethnicitydt[c(1:4)]

contdt <- merge(merge(merge(satDemUK, p_past_vote, by='id'), p_ethnicitydt, by='id'), satDemEU, by='id')
widedt<-merge(maindt, contdt, by='id')



# media salience data #### 
uk_salience<-read.csv("uk_salience.csv")
salience_W1<-uk_salience[c(2,3)]
names(salience_W1)<-c("timeW1", "salienceW1")
salience_W2<-uk_salience[c(2,3)]
names(salience_W2)<-c("timeW2", "salienceW2")
salience_W3<-uk_salience[c(2,3)]
names(salience_W3)<-c("timeW3", "salienceW3")

widedt<-merge(merge(merge(widedt, salience_W1, by="timeW1", all.x = T), 
                  salience_W2, by="timeW2", all.x = T), salience_W3, by="timeW3", all.x = T)

write.csv(widedt, "widedt.csv")




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
  dplyr::select(id, salienceW1, salienceW2, salienceW3) %>%
  gather(key = "mediasalience", value = "mediasalience_v", -id)
mediasalience_long <- create_new_id(mediasalience_long, "id", "mediasalience", "")
mediasalience_long$id_new <- str_replace(mediasalience_long$id_new, "salience", "_") 
mediasalience_long<-mediasalience_long[c(3,4)]



####  merge them #### 
longdt<-merge(merge(merge(long_timedt, wave_long, by="id_new"), mip_long, by="id_new"), 
              imm_att_long, by='id_new')
longdt<-merge(merge(longdt, satDemUK_long, by="id_new"), satDemEU_long, by="id_new")
longdt<-merge(merge(longdt, mediasalience_long, by="id_new", all.x = T), p_ethnicity_long, all.x=T, by="id_new")


write.csv(longdt, "longdt.csv")
