library(cquad)
library(plm)
library(MASS)
library(stringr)

longdt<-read.csv("longdt.csv")
longdt$wave_n<-str_replace(longdt$wave, "wave", "")
longdt$wave_n<-as.numeric(longdt$wave_n)
longdt<-subset(longdt, p_ethnicity_v==1)


# Table 4 #### 
idcols <- c("id", "wave_n")
cols <- c(idcols, names(longdt)[-which(names(longdt) %in% idcols)]) 
longdt <- longdt[cols]  

model1 = cquad(MIP_v ~ mediasalience_v + satDemUK_v + satDemEU_v, data = longdt)
summary(model1)
exp(coef(model1))
exp(cbind(OR = coef(model1), confint(model1)))


#  Table 5 #### 
p_longdt <- pdata.frame(longdt, index=c("id", "wave_n"))
fixed_UK<-plm(imm_att_v ~ mediasalience_v + satDemUK_v + satDemEU_v, data=p_longdt, 
                index=c("id_new", "wave_n"),
                model="within")
summary(fixed_UK)
