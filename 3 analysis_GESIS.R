library(cquad)
library(plm)
library(MASS)

long_de<-read.csv("gesis_longdt.csv")
long_de<-long_de %>% mutate(wave_n = case_when(wave=="wave_ga" ~ 1, 
                                               wave=="wave_gb" ~ 2, 
                                               wave=="wave_gc" ~ 3))

# only natives 
long_de<-subset(long_de, migration_v==1)

#### Table 3 #### 
idcols <- c("id", "wave_n")
cols <- c(idcols, names(long_de)[-which(names(long_de) %in% idcols)]) 
long_de <- long_de[cols]

out_de = cquad(MIP_b_v ~ salience_v + satisfe_v + satisEU_v, data = long_de)
summary(out_de)
exp(coef(out_de))
exp(cbind(OR = coef(out_de), confint(out_de)))


#### Table 4 #### 
p_long_de <- pdata.frame(long_de, index=c("id", "wave_n"))
fixed_de<-plm(imm_att_v ~ salience_v + satisfe_v + satisEU_v,  
               data=p_long_de, index=c("id", "wave_n"),
               model="within")
summary(fixed_de)
