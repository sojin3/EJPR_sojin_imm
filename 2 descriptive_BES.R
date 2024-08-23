library(gplots)
library(broom)
library(dplyr)
library(ggpubr)
library(stringr)
library(tidyr)


widedt<-read.csv("widedt.csv")
longdt<-read.csv("longdt.csv")
longdt$wave_n<-str_replace(longdt$wave, "wave", "")
longdt$wave_n<-as.numeric(longdt$wave_n)

# only natives 
longdt<-subset(longdt, p_ethnicity_v==1)

# Figure 1 #### 
# daily frequency data 
media_freq<-read.csv("uk_frequency.csv")

# calculate the percentage and mean from longdata
longdt_graph<-longdt %>% 
  group_by(wave_n) %>% 
  summarise(N=n(), 
            MIPsum = sum(MIP_v, na.rm=TRUE), 
            MIP_v = MIPsum/N, 
            imm_att_v = mean(imm_att_v, na.rm = T))



par(mfrow=c(2,2))
plotmeans(Freq ~ wave, main = "Media salience",
          xlab="Wave", ylab="Mean number of articles per day", 
          n.label=FALSE, data=media_freq) 
plot_MIP<-plotmeans(MIP_v ~ wave_n, main="Public issue salience", 
                    xlab="Wave", ylab="Percentage", n.label=FALSE, data=longdt_graph, 
                    ylim = c(0.0, 0.4)) 
plot_imm<-plotmeans(imm_att_v ~ wave_n, main="Anti-immigrant attitudes",
                    xlab="Wave", ylab="Mean", ylim = c(4, 5), n.label=FALSE, data=longdt_graph) 


# Table 2 #### 
longdt %>% 
  summarise(N_W1 = sum(wave_n==1, na.rm = T), 
            N_W2 = sum(wave_n==2, na.rm = T), 
            N_W3 = sum(wave_n==3, na.rm = T), 
            imm_mean_W1_1 = mean(imm_att_v[.$wave_n==1 & .$MIP_v==1], na.rm=TRUE), 
            imm_mean_W1_0 = mean(imm_att_v[.$wave_n==1 & .$MIP_v==0], na.rm=TRUE), 
            imm_mean_W2_1 = mean(imm_att_v[.$wave_n==2 & .$MIP_v==1], na.rm=TRUE), 
            imm_mean_W2_0 = mean(imm_att_v[.$wave_n==2 &.$MIP_v==0], na.rm=TRUE), 
            imm_mean_W3_1 = mean(imm_att_v[.$wave_n==3 &.$MIP_v==1], na.rm=TRUE), 
            imm_mean_W3_0 = mean(imm_att_v[.$wave_n==3 & .$MIP_v==0], na.rm=TRUE), 
            cor_W1 = tidy(cor.test(imm_att_v[.$wave_n==1], MIP_v[.$wave_n==1], na.rm = TRUE))$estimate, 
            cor_W2 = tidy(cor.test(imm_att_v[.$wave_n==2], MIP_v[.$wave_n==2], na.rm = TRUE))$estimate,
            cor_W3 = tidy(cor.test(imm_att_v[.$wave_n==3], MIP_v[.$wave_n==3], na.rm = TRUE))$estimate)


# Figure 3 #### 
widedt<-widedt %>% mutate(vote_2010 = case_when(p_past_vote_2010==1 ~ "Con", 
                                                p_past_vote_2010==2 ~ "Lab", 
                                                p_past_vote_2010==6 ~"UKIP", 
                                                p_past_vote_2010 %in% c(3,4,5,7,8,9,9999) ~ NA))


widedt$vote_2010<-as.factor(widedt$vote_2010)
graph<-widedt %>% 
  group_by(vote_2010) %>% 
  filter(is.na(vote_2010)==F) %>% 
  summarise(W1_1 = mean(imm_att_W1[p_ethnicityW1==1 & MIPW1==1], na.rm=T),
            W1_0 = mean(imm_att_W1[p_ethnicityW1==1 & MIPW1==0], na.rm=T),
            W2_1 = mean(imm_att_W2[p_ethnicityW2==1 & MIPW2==1], na.rm=TRUE), 
            W2_0 = mean(imm_att_W2[p_ethnicityW2==1 & MIPW2==0], na.rm=TRUE),
            W3_1 = mean(imm_att_W3[p_ethnicityW3==1 & MIPW3==1], na.rm=TRUE), 
            W3_0 = mean(imm_att_W3[p_ethnicityW3==1 & MIPW3==0], na.rm=TRUE), 
            cor_W1 = tidy(cor.test(MIPW1[p_ethnicityW1==1], imm_att_W1[p_ethnicityW1==1], na.rm = TRUE))$estimate, 
            cor_W2 = tidy(cor.test(MIPW2[p_ethnicityW2==1], imm_att_W2[p_ethnicityW2==1], na.rm = TRUE))$estimate,
            cor_W3 = tidy(cor.test(MIPW3[p_ethnicityW3==1], imm_att_W3[p_ethnicityW3==1], na.rm = TRUE))$estimate)

graph<-graph %>% 
  pivot_longer(cols = starts_with("W"), 
               names_to = c("wave"))
graph$MIP <- rep(c(1, 0), length.out = nrow(graph))
graph[c(2,3,4)]<-round(graph[c(2,3,4)], 2)
graph$cor_W1[c(FALSE, TRUE)] <- NA
graph$cor_W2[c(FALSE, TRUE)] <- NA
graph$cor_W3[c(FALSE, TRUE)] <- NA


# wave 1 
w1<-graph %>% filter(wave=="W1_1" | wave=="W1_0") %>% 
  ggplot(aes(value, vote_2010, fill = as.factor(MIP))) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip() +
  labs(title = "20 Feb ~ 9 Mar 2014 (wave 1)", 
       x ="Anti-immigrant attitudes", y = "") + 
  geom_text(aes(label=cor_W1), size = 6) + 
  scale_fill_brewer(palette="Oranges") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 15),
        axis.text.y=element_text(size=9), 
        axis.title=element_text(size=9), 
        axis.text.x=element_text(size=20),
        legend.position = "none")

# wave 2 
w2<-graph %>% filter(wave=="W2_1" | wave=="W2_0") %>% 
  ggplot(aes(value, vote_2010, fill = as.factor(MIP))) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip() +
  geom_text(aes(label=cor_W2), size = 6) + 
  labs(title = "22 May~ 25 Jun 2014 (wave 2)", 
       x ="Anti-immigrant attitudes", y = "") + 
  scale_fill_brewer(palette="Oranges") + theme_minimal() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 15),
        axis.text.y=element_text(size=9), 
        axis.title=element_text(size=9), 
        axis.text.x=element_text(size=20),
        legend.position = "none")


# wave 3 
w3<-graph %>% filter(wave=="W3_1" | wave=="W3_0") %>% 
  ggplot(aes(value, vote_2010, fill = as.factor(MIP))) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip() +
  geom_text(aes(label=cor_W3), size = 6) + 
  labs(title = "19 Sep~ 17 Oct 2014 (wave 3)", 
       x ="Anti-immigrant attitudes", y = "") + 
  guides(fill=guide_legend(title="Immigration most important")) +
  scale_fill_brewer(palette="Oranges", labels = c("No", "Yes")) + theme_minimal() +
  theme(plot.title = element_text(size = 15), 
        axis.text.y=element_text(size=9), 
        axis.title=element_text(size=9), 
        axis.text.x=element_text(size=20),
        legend.position="none")



graph$forlegends<-1000


legends<-graph %>% 
  ggplot(aes(forlegends, vote_2010, fill = as.factor(MIP))) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip() +
  guides(fill=guide_legend(title="Immigration most important")) +
  scale_fill_brewer(palette="Oranges", labels = c("No", "Yes")) + 
  scale_x_continuous(limits = c(0,0)) +
  theme_void() +
  theme(axis.title= element_blank(),
        axis.text.y = element_blank(),        
        axis.text.x = element_blank(),        
        axis.title.y = element_blank(),      
        axis.title.x = element_blank(), 
        legend.position=c(0.5, 0.5),
        legend.title = element_text(size=25, family = "serif"),
        legend.text=element_text(size=25, family = "serif"))





ggarrange(w1, w2, w3, legends,
          ncol = 2, nrow = 2)
