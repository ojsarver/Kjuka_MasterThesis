pacman::p_load(tidyverse,
               patchwork,
               here)

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



#plots - do we wanna include nodules despite small sample size?

mu_AGB<-df_bmassc %>%
  ggplot(aes(x=Treatment,
             y=mu_AGB,
             fill=Beetle))+
  geom_bar(stat="identity",position = "dodge")


mu_pod<-df_bmassc %>%
  ggplot(aes(x=Treatment,
             y=mu_pod,
             fill=Beetle))+
  geom_bar(stat="identity",position = "dodge")


mu_seednum<-df_bmassc %>%
  ggplot(aes(x=Treatment,
             y=mu_seednum,
             fill=Beetle))+
  geom_bar(stat="identity",position = "dodge")

mu_seedwei<-df_bmassc %>%
  ggplot(aes(x=Treatment,
             y=mu_seedwei,
             fill=Beetle))+
  geom_bar(stat="identity",position = "dodge")

mu_BGB<-df_bmassc %>%
  ggplot(aes(x=Treatment,
             y=mu_BGB,
             fill=Beetle))+
  geom_bar(stat="identity",position = "dodge")

mu_nod<-df_bmassc %>%
  ggplot(aes(x=Treatment,
             y=mu_nod,
             fill=Beetle))+
  geom_bar(stat="identity",position = "dodge")

pushViewport(viewport(layout=grid.layout(3,2)))
print(mu_AGB,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(mu_BGB,vp=viewport(layout.pos.row=1,layout.pos.col = 2))
print(mu_nod,vp=viewport(layout.pos.row=2,layout.pos.col = 1))
print(mu_pod,vp=viewport(layout.pos.row=2,layout.pos.col = 2))
print(mu_seedwei,vp=viewport(layout.pos.row=3,layout.pos.col = 1))
print(mu_seednum,vp=viewport(layout.pos.row=3,layout.pos.col = 2))


#geom_errorbar(aes(ymin=mu_AGB-SE_AGB,ymax=mu_AGB+SE_AGB),width=0.2)
#cant get it to work but leaving code in case I need it


#p-value in excel like.16

#do the multiple plots and split screen from egg time