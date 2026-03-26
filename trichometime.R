pacman::p_load(tidyverse,
               patchwork,
               here,
               car,
               emmeans,
               nlme,
               grid)

df_tri<-read_csv(here("trichomes.csv"))


# clean data --------------------------------------------------------------


df_tric<-df_tri%>%
  group_by(Treatment,Beetle) %>%
  summarize(mu_bh = mean(Beforeharvest,na.rm=T),
            SE_bh=sd(Beforeharvest,na.rm=T)/sqrt(length(Beforeharvest)),
            mu_ah = mean(Atharvest,na.rm=T),
            SE_ah=sd(Atharvest,na.rm=T)/sqrt(length(Atharvest)))

df_tric$B_T<-paste(df_tric$Treatment,df_tric$Beetle)

# plot --------------------------------------------------------------------

tribh<-df_tric %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_bh,
             fill=B_T))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_bh-SE_bh,ymax=mu_bh+SE_bh),
                width=0.2)+
  annotate('text', x = 1, y = 129, label = 'a', size = 6)+
  annotate('text', x = 2, y = 129, label = 'a', size = 6)+
  annotate('text', x = 3, y = 129, label = 'a', size = 6)+
  annotate('text', x = 4, y = 129, label = 'a', size = 6)+
  scale_fill_manual(values = c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "Week 15",
       y = "Average Trichome Number")

triah<-df_tric %>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_ah,
             fill=B_T))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_ah-SE_ah,ymax=mu_ah+SE_ah),
                width=0.2)+
  annotate('text', x = 1, y = 122, label = 'a', size = 6)+
  annotate('text', x = 2, y = 122, label = 'a', size = 6)+
  annotate('text', x = 3, y = 122, label = 'a', size = 6)+
  annotate('text', x = 4, y = 122, label = 'a', size = 6)+
  scale_fill_manual(values = c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(x = "At Harvest",
       y = "")

pushViewport(viewport(layout=grid.layout(1,2)))
print(tribh,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(triah,vp=viewport(layout.pos.row=1,layout.pos.col = 2))

# stats -------------------------------------------------------------------

#before harvest
bhaov<-aov(Beforeharvest~Treatment*Beetle,
            data=df_tri)

Anova(bhaov,type=3)

emmeans(bhaov,~Treatment*Beetle)

#at harvest

ahaov<-aov(Atharvest~Treatment*Beetle,
           data=df_tri)

Anova(ahaov,type=3)

emmeans(ahaov,~Treatment*Beetle)
