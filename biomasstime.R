pacman::p_load(tidyverse,
               patchwork,
               here,
               car,
               emmeans,
               nlme,
               grid)

#create data frames

df_bmasssu<-read_csv(here("su25biomass1.csv"))
df_bmassfa<-read_csv(here("fa25biomass1.csv"))

df_bmasssuc<-df_bmasssu%>%
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

df_bmasssuc$B_T<-paste(df_bmasssuc$Treatment,df_bmasssuc$Beetle)

df_bmassfac<-df_bmassfa%>%
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
            SE_BGB=sd(Total_BGB,na.rm=T)/sqrt(length(Total_BGB)))

df_bmassfac$B_T<-paste(df_bmassfac$Treatment,df_bmassfac$Beetle)

#plots
#fix all letters

mu_AGBsu<-df_bmasssuc %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_AGB,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_AGB-SE_AGB, ymax=mu_AGB+SE_AGB),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 41, label = 'a', size = 6)+
  annotate('text', x = 2, y = 41, label = 'b', size = 6)+
  annotate('text', x = 3, y = 41, label = 'ab', size = 6)+
  annotate('text', x = 4, y = 41, label = 'a', size = 6)+
  annotate('text', x = .75, y = 45, label = '(A)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "Average Aboveground Biomass")

mu_AGBfa<-df_bmassfac %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_AGB,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_AGB-SE_AGB, ymax=mu_AGB+SE_AGB),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 41, label = 'a', size = 6)+
  annotate('text', x = 2, y = 41, label = 'b', size = 6)+
  annotate('text', x = 3, y = 41, label = 'ab', size = 6)+
  annotate('text', x = 4, y = 41, label = 'a', size = 6)+
  annotate('text', x = .75, y = 45, label = '(B)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "")

mu_podsu<-df_bmasssuc %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_pod,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_pod-SE_pod, ymax=mu_pod+SE_pod),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 61, label = 'a', size = 6)+
  annotate('text', x = 2, y = 61, label = 'a', size = 6)+
  annotate('text', x = 3, y = 61, label = 'a', size = 6)+
  annotate('text', x = 4, y = 61, label = 'a', size = 6)+
  annotate('text', x = .75, y = 65, label = '(C)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "Average Number of Pods")

mu_podfa<-df_bmassfac %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_pod,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_pod-SE_pod, ymax=mu_pod+SE_pod),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 61, label = 'a', size = 6)+
  annotate('text', x = 2, y = 61, label = 'a', size = 6)+
  annotate('text', x = 3, y = 61, label = 'a', size = 6)+
  annotate('text', x = 4, y = 61, label = 'a', size = 6)+
  annotate('text', x = .75, y = 65, label = '(D)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "")

mu_seednumsu<-df_bmasssuc %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_seednum,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_seednum-SE_seednum, ymax=mu_seednum+SE_seednum),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 115, label = 'a', size = 6)+
  annotate('text', x = 2, y = 115, label = 'a', size = 6)+
  annotate('text', x = 3, y = 115, label = 'a', size = 6)+
  annotate('text', x = 4, y = 115, label = 'a', size = 6)+
  annotate('text', x = .75, y = 119, label = '(E)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "Average Number of Seeds")

mu_seednumfa<-df_bmassfac %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_seednum,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_seednum-SE_seednum, ymax=mu_seednum+SE_seednum),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 115, label = 'a', size = 6)+
  annotate('text', x = 2, y = 115, label = 'a', size = 6)+
  annotate('text', x = 3, y = 115, label = 'a', size = 6)+
  annotate('text', x = 4, y = 115, label = 'a', size = 6)+
  annotate('text', x = .75, y = 119, label = '(F)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "")

mu_seedweisu<-df_bmasssuc %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_seedwei,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_seedwei-SE_seedwei, ymax=mu_seedwei+SE_seedwei),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 14, label = 'ab', size = 6)+
  annotate('text', x = 2, y = 14, label = 'a', size = 6)+
  annotate('text', x = 3, y = 14, label = 'b', size = 6)+
  annotate('text', x = 4, y = 14, label = 'b', size = 6)+
  annotate('text', x = .75, y = 18, label = '(G)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "Average Weight of Seeds")

mu_seedweifa<-df_bmassfac %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_seedwei,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_seedwei-SE_seedwei, ymax=mu_seedwei+SE_seedwei),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 14, label = 'ab', size = 6)+
  annotate('text', x = 2, y = 14, label = 'a', size = 6)+
  annotate('text', x = 3, y = 14, label = 'b', size = 6)+
  annotate('text', x = 4, y = 14, label = 'b', size = 6)+
  annotate('text', x = .75, y = 18, label = '(H)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "")

mu_BGBsu<-df_bmasssuc %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_BGB,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_BGB-SE_BGB, ymax=mu_BGB+SE_BGB),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 9, label = 'ab', size = 6)+
  annotate('text', x = 2, y = 9, label = 'ab', size = 6)+
  annotate('text', x = 3, y = 9, label = 'a', size = 6)+
  annotate('text', x = 4, y = 9, label = 'b', size = 6)+
  annotate('text', x = .75, y = 13, label = '(I)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "Average Belowground Biomass")

mu_BGBfa<-df_bmassfac %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_BGB,
             fill=B_T))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin=mu_BGB-SE_BGB, ymax=mu_BGB+SE_BGB),
                width=.2,
                position = position_dodge(.9))+
  annotate('text', x = 1, y = 9, label = 'ab', size = 6)+
  annotate('text', x = 2, y = 9, label = 'ab', size = 6)+
  annotate('text', x = 3, y = 9, label = 'a', size = 6)+
  annotate('text', x = 4, y = 9, label = 'b', size = 6)+
  annotate('text', x = .75, y = 13, label = '(J)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "")

pushViewport(viewport(layout=grid.layout(5,2)))
print(mu_AGBsu,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(mu_AGBfa,vp=viewport(layout.pos.row=1,layout.pos.col = 2))
print(mu_BGBsu,vp=viewport(layout.pos.row=2,layout.pos.col = 1))
print(mu_BGBfa,vp=viewport(layout.pos.row=2,layout.pos.col = 2))
print(mu_podsu,vp=viewport(layout.pos.row=3,layout.pos.col = 1))
print(mu_podfa,vp=viewport(layout.pos.row=3,layout.pos.col = 2))
print(mu_seedweisu,vp=viewport(layout.pos.row=4,layout.pos.col = 1))
print(mu_seedweifa,vp=viewport(layout.pos.row=4,layout.pos.col = 2))
print(mu_seednumsu,vp=viewport(layout.pos.row=5,layout.pos.col = 1))
print(mu_seednumfa,vp=viewport(layout.pos.row=5,layout.pos.col = 2))

#stats --- run everything for all variables but nodule

agbaov<-aov(Total_AGB~Treatment*Beetle,
          data=df_bmasssu)

Anova(agbaov,type=3)

emmeans(agbaov,~Treatment*Beetle)
#

podaov<-aov(Pod_num~Treatment*Beetle,
            data=df_bmasssu)

Anova(podaov,type=3)

emmeans(podaov,~Treatment*Beetle)
#

snaov<-aov(Seed_num~Treatment*Beetle,
            data=df_bmasssu)

Anova(snaov,type=3)

emmeans(snaov,~Treatment*Beetle)
#

swbaov<-aov(Seed_weight~Treatment*Beetle,
            data=df_bmasssu)

Anova(swbaov,type=3)

emmeans(swbaov,~Treatment*Beetle)
#

bgbaov<-aov(Total_BGB~Treatment*Beetle,
            data=df_bmasssu)

Anova(bgbaov,type=3)

emmeans(bgbaov,~Treatment*Beetle)
