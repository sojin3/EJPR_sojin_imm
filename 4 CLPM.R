## This R-Code is based on the article:

## Mund, M. & Nestler, S. (2017), Beyond the cross-lagged panel model:
## Next-generation statistical tools for analyzing interdependencies
## across the life courx Manuscript submitted for publication.


## load package
library(lavaan)



#### The UK #### 
# Figure 5
widedt<-read.csv("widedt.csv")

ndt_wave3<-subset(widedt, p_ethnicityW1==1 |  p_ethnicityW2==1 |  p_ethnicityW3==1)
ndt_wave3<-subset(ndt_wave3, select = c(id, MIPW1, MIPW2, MIPW3,
                                     imm_att_W1,imm_att_W2, imm_att_W3))
names(ndt_wave3) <- c("id","x1","x2", "x3", 
                      "y1", "y2", "y3")



riclpm <- '
# Define intercept factors
ix =~ 1*x1+1*x2+1*x3
iy =~ 1*y1+1*y2+1*y3


# Define phantom latent variables
etax1 =~ 1*x1
etax2 =~ 1*x2
etax3 =~ 1*x3


etay1 =~ 1*y1
etay2 =~ 1*y2
etay3 =~ 1*y3

# Autoregressive effects
etax2 ~ a1*etax1
etax3 ~ a1*etax2

etay2 ~ a2*etay1
etay3 ~ a2*etay2


# Crosslagged effects
etay2 ~ c1*etax1 
etay3 ~ c1*etax2 


etax2 ~ c2*etay1 
etax3 ~ c2*etay2 


x1 ~~ 0*x1
x2 ~~ 0*x2
x3 ~~ 0*x3


y1 ~~ 0*y1
y2 ~~ 0*y2
y3 ~~ 0*y3



etax1 ~~ varx1*etax1
etax2 ~~ varx2*etax2
etax3 ~~ varx3*etax3


etay1 ~~ vary1*etay1
etay2 ~~ vary2*etay2
etay3 ~~ vary3*etay3


ix ~~ varix*ix
iy ~~ variy*iy
ix ~~ covi*iy

etax1 ~~ 0*ix
etay1 ~~ 0*ix
etax1 ~~ 0*iy
etay1 ~~ 0*iy


etax1 ~~ cov1*etay1
etax2 ~~ e1*etay2
etax3 ~~ e1*etay3



x1 ~ 0*1
x2 ~ 0*1
x3 ~ 0*1

y1 ~ 0*1
y2 ~ 0*1
y3 ~ 0*1


etax1 ~ 0*1
etax2 ~ 0*1
etax3 ~ 0*1


etay1 ~ 0*1
etay2 ~ 0*1
etay3 ~ 0*1


ix ~ 1
iy ~ 1


cori := covi / (sqrt(varix) * sqrt(variy))
cor1 := cov1 / (sqrt(varx1) * sqrt(vary1))
cort2 := e1 / (sqrt(varx2) * sqrt(vary2))
cort3 := e1 / (sqrt(varx3) * sqrt(vary3))
'

fit_UK <- lavaan::sem(riclpm, data = ndt_wave3)
lavaan::fitMeasures(fit_UK)
fit.parameter_UK<-lavaan::parameterestimates(fit_UK, output ="text", standardized =T,rsquare=T)
fit.parameter_UK


#### Germany ####
# Figure 6
wide_de<-read.csv("gesis_widedt.csv")

de_wave3<-subset(wide_de, select = c(z000001a, MIP_ga_b, MIP_gb_b, MIP_gc_b,
                                      imm_att_ga,imm_att_gb, imm_att_gc))
names(de_wave3) <- c("id","x1","x2", "x3", 
                      "y1", "y2", "y3")
fit_de <- lavaan::sem(riclpm3, data = de_wave3)
lavaan::fitMeasures(fit_de)
fit.parameter_de<-lavaan::parameterestimates(fit_de, output ="text", standardized =T,rsquare=T)
fit.parameter_de
