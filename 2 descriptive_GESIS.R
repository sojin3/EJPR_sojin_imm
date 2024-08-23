library(ggplot2)
library(gplots)
library(broom)
library(ggpubr)
library(stringr)
library(tidyr)
library(dplyr)

wide_de<-read.csv("gesis_widedt.csv")
long_de<-read.csv("gesis_longdt.csv")
long_de<-long_de %>% mutate(wave_n = case_when(wave=="wave_ga" ~ 1, 
                                               wave=="wave_gb" ~ 2, 
                                               wave=="wave_gc" ~ 3))


#### Figure 2 ####
media_freq<-read.csv("de_frequency.csv")
media_freq$Date<-as.Date(media_freq$Date)
media_freq$wave<-ifelse(media_freq$Date > "2019-04-16" & media_freq$Date <= "2019-06-11", "gb", 
                               ifelse(media_freq$Date >= "2019-06-12", "gc", "ga"))

plot_MIP<-long_de %>% group_by(wave) %>% filter(wave_v ==1) %>% 
  summarise(N = n(), 
            MIP = sum(MIP_b_v, na.rm=TRUE), 
            perc = MIP/N)




par(mfrow=c(2,2))
custom_labels <- c("ga", "gb", "gc")
plotmeans(all ~ wave, main="Media salience",
          xlab="wave", ylab="Mean number of articles per day", n.label=FALSE, data=media_freq) 
plotmeans(perc ~ wave, main="Public issue salience",
          xlab="wave", ylab="Percentage",  ylim = c(0.4, 0.7),
          n.label=FALSE, data=plot_MIP, xaxt="n") 
axis(side = 1, at = 1:3, labels = custom_labels)
plotmeans(imm_att_v ~ wave, main="Anti-immigrant attitudes",
          xlab="wave", ylab="Mean", ylim = c(4, 5.5),
          n.label=FALSE, data=long_de, xaxt="n") 
axis(side = 1, at = 1:3, labels = custom_labels)




#### Table 2 #### 
# only natives 
long_de<-subset(long_de, migration_v==1)

long_de %>% 
  summarise(N_ga = sum(wave_n==1 & wave_v==1, na.rm = T), 
            N_gb = sum(wave_n==2 & wave_v==1, na.rm = T), 
            N_gc = sum(wave_n==3 & wave_v==1, na.rm = T), 
            imm_ga_1 = mean(imm_att_v[.$wave_n==1 & .$MIP_b_v==1], na.rm=TRUE), 
            imm_ga_0 = mean(imm_att_v[.$wave_n==1 & .$MIP_b_v==0], na.rm=TRUE), 
            imm_gb_1 = mean(imm_att_v[.$wave_n==2 & .$MIP_b_v==1], na.rm=TRUE), 
            imm_gb_0 = mean(imm_att_v[.$wave_n==2 &.$MIP_b_v==0], na.rm=TRUE), 
            imm_gc_1 = mean(imm_att_v[.$wave_n==3 &.$MIP_b_v==1], na.rm=TRUE), 
            imm_gc_0 = mean(imm_att_v[.$wave_n==3 & .$MIP_b_v==0], na.rm=TRUE), 
            cor_W1 = tidy(cor.test(imm_att_v[.$wave_n==1], MIP_v[.$wave_n==1], na.rm = TRUE))$estimate, 
            cor_W2 = tidy(cor.test(imm_att_v[.$wave_n==2], MIP_v[.$wave_n==2], na.rm = TRUE))$estimate,
            cor_W3 = tidy(cor.test(imm_att_v[.$wave_n==3], MIP_v[.$wave_n==3], na.rm = TRUE))$estimate)




#### Figure 4 #### 
wide_de<-subset(wide_de, migrationstatus==1)
table(wide_de$BTW_2017)
wide_de$BTW_2017<-ifelse(wide_de$BTW_2017=="AFD", "AfD", wide_de$BTW_2017)

graph<-wide_de %>% 
  group_by(BTW_2017) %>% 
  filter(BTW_2017!="others") %>% 
  summarise(W1_1 = mean(imm_att_ga[MIP_ga_b==1], na.rm=T),
            W1_0 = mean(imm_att_ga[MIP_ga_b==0], na.rm=T),
            W2_1 = mean(imm_att_gb[MIP_gb_b==1], na.rm=TRUE), 
            W2_0 = mean(imm_att_gb[MIP_gb_b==0], na.rm=TRUE),
            W3_1 = mean(imm_att_gc[MIP_gc_b==1], na.rm=TRUE), 
            W3_0 = mean(imm_att_gc[MIP_gc_b==0], na.rm=TRUE), 
            cor_W1 = tidy(cor.test(MIP_ga_b, imm_att_ga, na.rm = TRUE))$estimate, 
            cor_W2 = tidy(cor.test(MIP_gb_b, imm_att_gb, na.rm = TRUE))$estimate,
            cor_W3 = tidy(cor.test(MIP_gc_b, imm_att_gc, na.rm = TRUE))$estimate)

graph<-graph %>% 
  pivot_longer(cols = starts_with("W"), 
               names_to = c("wave"))
graph$MIP <- rep(c(1, 0), length.out = nrow(graph))
graph[c(2,3,4)]<-round(graph[c(2,3,4)], 2)
graph$cor_W1[c(FALSE, TRUE)] <- NA
graph$cor_W2[c(FALSE, TRUE)] <- NA
graph$cor_W3[c(FALSE, TRUE)] <- NA


# wave ga
graph$party <- factor(graph$BTW_2017, levels = c("CDU", "SPD", "AfD"))


ga<-graph %>% filter(wave=="W1_1" | wave=="W1_0") %>% 
  ggplot(aes(value, party, fill = as.factor(MIP))) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip() +
  labs(title = "12 Feb ~ 16 April 2019 (wave ga)", 
       x ="Anti-immigrant attitudes", y = "") + 
  geom_text(aes(label=cor_W1), size = 6) + 
  scale_fill_brewer(palette="Greens") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 15),
        axis.text.y=element_text(size=9), 
        axis.title=element_text(size=9), 
        axis.text.x=element_text(size=20),
        legend.position = "none")


# wave gb 
gb<-graph %>% filter(wave=="W2_1" | wave=="W2_0") %>% 
  ggplot(aes(value, party, fill = as.factor(MIP))) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip() +
  geom_text(aes(label=cor_W2), size = 6) + 
  labs(title = "17 April ~ 10 June 2019 (wave gb)", 
       x ="Anti-immigrant attitudes", y = "") + 
  scale_fill_brewer(palette="Greens") + theme_minimal() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 15),
        axis.text.y=element_text(size=9), 
        axis.title=element_text(size=9), 
        axis.text.x=element_text(size=20),
        legend.position = "none")

# wave gc 
gc<-graph %>% filter(wave=="W3_1" | wave=="W3_0") %>% 
  ggplot(aes(value, party, fill = as.factor(MIP))) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip() +
  geom_text(aes(label=cor_W3), size = 6) + 
  labs(title = "12 June ~ 13 August 2019 (wave gc)", 
       x ="Anti-immigrant attitudes", y = "") + 
  guides(fill=guide_legend(title="Immigration most important")) +
  scale_fill_brewer(palette="Greens", labels = c("No", "Yes")) + theme_minimal() +
  theme(plot.title = element_text(size = 15), 
        axis.text.y=element_text(size=9), 
        axis.title=element_text(size=9), 
        axis.text.x=element_text(size=20),
        legend.position="none")


graph$forlegends<-1000


legends<-graph %>% 
  ggplot(aes(forlegends, party, fill = as.factor(MIP))) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip() +
  guides(fill=guide_legend(title="Immigration most important")) +
  scale_fill_brewer(palette="Greens", labels = c("No", "Yes")) + 
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



ggarrange(ga, gb, gc, legends,
          ncol = 2, nrow = 2)
