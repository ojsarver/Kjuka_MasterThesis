pacman::p_load(tidyverse,
               patchwork,
               here,
               car,
               emmeans,
               nlme)

#create data frames

df_bmass<-read_csv(here("su25biomass1.csv"))

df_bmassc<-df_bmass%>%
  group_by(Treatment,Beetle) %>%
  summarize(mu_AGB = mean(Total_AGB,na.rm=T),
            SE_AGB=sd(Total_AGB,na.rm=T)/sqrt(length(Total_AGB)),
            mu_pod = mean(Pod_num,na.rm=T),
            SE_pod=sd(Pod_num,na.rm=T)/sqrt(length(Pod_num)),
            mu_seednum = mean(Seed_num,na.rm=T),
            SE_seednum=sd(Seed_num,na.rm=T)/sqrt(length(Seed_num)),
            mu_seedwei = mean(Seed_weight,na.rm=T),
            SE_seedwei=sd(Seed_weight,na.rm=T)/sqrt(length(Seed_weight)),
            mu_BGB = mean(Total_BGB,na.rm=T),
            SE_BGB=sd(Total_BGB,na.rm=T)/sqrt(length(Total_BGB)),
            mu_nod = mean(Nodule_num,na.rm=T),
            SE_nod=sd(Nodule_num,na.rm=T)/sqrt(length(Nodule_num)))

df_bmassc$B_T<-paste(df_bmassc$Treatment,df_bmassc$Beetle)

#plots

mu_AGB<-df_bmassc %>%
  ggplot(aes(x=B_T,
             y=mu_AGB,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_AGB-SE_AGB, ymax=mu_AGB+SE_AGB),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 41, label = '(a)', size = 8)+
  annotate('text', x = 2, y = 41, label = '(b)', size = 8)+
  annotate('text', x = 3, y = 41, label = '(a)', size = 8)+
  annotate('text', x = 4, y = 41, label = '(a)', size = 8)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "",
       y = "Average Aboveground Biomass")

mu_pod<-df_bmassc %>%
  ggplot(aes(x=B_T,
             y=mu_pod,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_pod-SE_pod, ymax=mu_pod+SE_pod),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 61, label = '(a)', size = 8)+
  annotate('text', x = 2, y = 61, label = '(a)', size = 8)+
  annotate('text', x = 3, y = 61, label = '(a)', size = 8)+
  annotate('text', x = 4, y = 61, label = '(a)', size = 8)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "",
       y = "Average Number of Pods")


mu_seednum<-df_bmassc %>%
  ggplot(aes(x=B_T,
             y=mu_seednum,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_seednum-SE_seednum, ymax=mu_seednum+SE_seednum),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 115, label = '(ab)', size = 8)+
  annotate('text', x = 2, y = 115, label = '(ab)', size = 8)+
  annotate('text', x = 3, y = 115, label = '(a)', size = 8)+
  annotate('text', x = 4, y = 115, label = '(b)', size = 8)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "",
       y = "Average Number of Seeds")

mu_seedwei<-df_bmassc %>%
  ggplot(aes(x=B_T,
             y=mu_seedwei,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_seedwei-SE_seedwei, ymax=mu_seedwei+SE_seedwei),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 14, label = '(ab)', size = 8)+
  annotate('text', x = 2, y = 14, label = '(a)', size = 8)+
  annotate('text', x = 3, y = 14, label = '(b)', size = 8)+
  annotate('text', x = 4, y = 14, label = '(a)', size = 8)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "",
       y = "Average Weight of Seeds")

mu_BGB<-df_bmassc %>%
  ggplot(aes(x=B_T,
             y=mu_BGB,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_BGB-SE_BGB, ymax=mu_BGB+SE_BGB),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 9, label = '(ab)', size = 8)+
  annotate('text', x = 2, y = 9, label = '(a)', size = 8)+
  annotate('text', x = 3, y = 9, label = '(ab)', size = 8)+
  annotate('text', x = 4, y = 9, label = '(b)', size = 8)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "",
       y = "Average Belowground Biomass")

pushViewport(viewport(layout=grid.layout(3,2)))
print(mu_AGB,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(mu_BGB,vp=viewport(layout.pos.row=1,layout.pos.col = 2))
print(mu_pod,vp=viewport(layout.pos.row=2,layout.pos.col = 1))
print(mu_seedwei,vp=viewport(layout.pos.row=2,layout.pos.col = 2))
print(mu_seednum,vp=viewport(layout.pos.row=3,layout.pos.col = 1))

#stats --- run everything for all variables but nodule

agbaov<-aov(Total_AGB~Treatment*Beetle,
          data=df_bmass)

Anova(agbaov,type=3)

emmeans(agbaov,~Treatment*Beetle)
#

podaov<-aov(Pod_num~Treatment*Beetle,
            data=df_bmass)

Anova(podaov,type=3)

emmeans(podaov,~Treatment*Beetle)
#

snaov<-aov(Seed_num~Treatment*Beetle,
            data=df_bmass)

Anova(snaov,type=3)

emmeans(snaov,~Treatment*Beetle)
#

swbaov<-aov(Seed_weight~Treatment*Beetle,
            data=df_bmass)

Anova(swbaov,type=3)

emmeans(swbaov,~Treatment*Beetle)
#

bgbaov<-aov(Total_BGB~Treatment*Beetle,
            data=df_bmass)

Anova(bgbaov,type=3)

emmeans(bgbaov,~Treatment*Beetle)