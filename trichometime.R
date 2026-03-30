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
  group_by(Treatment,Beetle,Harvest) %>%
  summarize(mu_bh = mean(Trichome_count,na.rm=T),
            SE_bh=sd(Trichome_count,na.rm=T)/sqrt(length(Trichome_count)))

df_tric$B_T<-paste(df_tric$Treatment,df_tric$Beetle)

df_tribh<-df_tric %>%
  filter(Harvest=="Week 15")

df_triah<-df_tric %>%
  filter(Harvest!="Week 15")

# plot --------------------------------------------------------------------

tribh<-df_tric %>%
  filter(Harvest=="Week 15")%>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_bh,
             fill=B_T))+
  geom_bar(stat="identity", position = position_dodge())+
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
  filter(Harvest=="At Harvest")%>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                   "Control, Beetle",
                                   "Microplastic, No Beetle",
                                   "Microplastic, Beetle")),
             y=mu_bh,
             fill=B_T))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_bh-SE_bh,ymax=mu_bh+SE_bh),
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

#repeated measures anova

summary(rptri <- lme(Trichome_count~Harvest*Treatment*Beetle,
                    data=df_tri, 
                    random=~1|Plant_ID,
                    correlation=corAR1(form=~1|Plant_ID),
                    control=lmeControl(returnObject=T)))

Anova(rptri, type=3)

lsmeans(rptri, pairwise~Treatment*Beetle, adjust='tukey')
