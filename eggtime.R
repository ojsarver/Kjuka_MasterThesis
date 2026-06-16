pacman::p_load(tidyverse,
               patchwork,
               here,
               grid)

#create data frames

df_egg<-read_csv(here("su25beetleegg1.csv"))
df_egglar<-read_csv(here("su25EGGLARVAE.csv"))


# subset data] ------------------------------------------------------------

df_prop<-df_egglar[c("Treatment","Plant_ID","prop_hat","prop_adult")]

df_prop<-na.omit(df_prop)

# calculations ------------------------------------------------------------


df_egglarc<-df_egglar%>%
group_by(Treatment) %>%
  summarize(mu_egg = mean(Num_Eggs),
            SE_egg=sd(Num_Eggs,na.rm=T)/sqrt(length(Num_Eggs)),
            
            total_egg=sum(Num_Eggs),
            
            mu_mass=mean(Egg_mass),
            SE_mass=sd(Egg_mass,na.rm=T)/sqrt(length(Egg_mass)))

df_propc<-df_prop%>%
  group_by(Treatment)%>%
  summarize(mu_hat=mean(prop_hat),
            SE_hat=sd(prop_hat)/sqrt(length(prop_hat)),
            mu_adult=mean(prop_adult),
            SE_adult=sd(prop_adult)/sqrt(length(prop_adult)))

            
            #mu_hat = mean(Eggs_hatched),
            #SE_hat=sd(Eggs_hatched,na.rm=T)/sqrt(length(Eggs_hatched)),
            
            ##mu_prop_hat=mean(prop_hat),
            #SE_prop_hat=sd(prop_hat,na.rm=T)/sqrt(length(prop_hat)),
            #prop_hatched0=(sum(Eggs_hatched))/(sum(Egg_mass)),
            
            #mu_adult=mean(Adults),
            #SE_adult=sd(Adults,na.rm=T)/sqrt(length(Adults)),
            
            #prop_adult=(sum(Adults))/(sum(Adults)))

# plots -------------------------------------------------------------------


#avg eggs in each mass
mu_egg<-df_egglarc %>%
  ggplot(aes(x=Treatment,
             y=mu_egg,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_egg-SE_egg,ymax=mu_egg+SE_egg),
                width=0.2)+
  annotate('text', x = 1, y = 58, label = 'a', size = 6)+
  annotate('text', x = 2, y = 58, label = 'b', size = 6)+
  annotate('text', x = .65, y = 59, label = '(A)', size = 7)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        plot.subtitle=element_text(size=15,hjust=.5))+
  labs(x = "",
       subtitle="N = 11 N = 4",
       y = "Average Number of Eggs in Each Mass")

#avg # of egg masses
mu_mass<-df_egglarc %>%
  ggplot(aes(x=Treatment,
             y=mu_mass,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_mass-SE_mass,ymax=mu_mass+SE_mass),
                width=0.2)+
  annotate('text', x = 1, y = 2, label = 'a', size = 6)+
  annotate('text', x = 2, y = 2, label = 'b', size = 6)+
  annotate('text', x = .65, y = 2, label = '(B)', size = 7)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=19),
        axis.text=element_text(size=15),
        plot.subtitle=element_text(size=15,hjust=.5))+
  labs(x = "",
       subtitle = "N = 11 N = 4",
       y = "Average Number of Egg Masses Laid")

#avg adults
mu_adults<-df_propc%>%
  ggplot(aes(x=Treatment,
             y=mu_adult,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_adult-SE_adult,ymax=mu_adult+SE_adult),
                width=0.2)+
  annotate('text', x = 1, y = 1, label = 'a', size = 6)+
  annotate('text', x = 2, y = 1, label = 'a', size = 6)+
  annotate('text', x = .65, y = 1.5, label = '(D)', size = 7)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=19),
        axis.text=element_text(size=15),
        plot.subtitle=element_text(size=15,hjust=.5))+
  labs(x = "",
       subtitle = "N = 47 N = 7",
       y = "Average Proportion of Adults")

#avg hatched

mu_hat<-df_propc%>%
  ggplot(aes(x=Treatment,
             y=mu_hat,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_hat-SE_hat,ymax=mu_hat+SE_hat),
                width=0.2)+
  annotate('text', x = 1, y = 1, label = 'a', size = 6)+
  annotate('text', x = 2, y = 1, label = 'a', size = 6)+
  annotate('text', x = .65, y = 1.5, label = '(C)', size = 7)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=19),
        axis.text=element_text(size=15),
        plot.subtitle=element_text(size=15,hjust=.5))+
  labs(x = "",
       subtitle = "N = 319                                    N = 50",
       y = "Average Proportion of Eggs that Hatched")

pushViewport(viewport(layout=grid.layout(2,2)))
print(mu_egg,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(mu_mass,vp=viewport(layout.pos.row=1,layout.pos.col = 2))
print(mu_hat,vp=viewport(layout.pos.row=2,layout.pos.col = 1))
print(mu_adults,vp=viewport(layout.pos.row=2,layout.pos.col = 2))

#stats for eggs and larvae

#separate linear mixed models for total mass and total egg

numegglme<-lme(Num_Eggs~Treatment,
           data=df_egglar,
           random=~1|Plant_ID)

anova(numegglme)

eggmasslme<-lme(Egg_mass~Treatment,
                data=df_egglar,
                random=~1|Plant_ID)

anova(eggmasslme)

prophatlme<-lme(prop_hat~Treatment,
                data=df_prop,
                random=~1|Plant_ID)

anova(prophatlme)

propadultlme<-lme(prop_adult~Treatment,
                data=df_prop,
                random=~1|Plant_ID)

anova(propadultlme)

#Original wilcoxon tests

wilcox.test(Num_Eggs~Treatment,
            data=df_egglar)

wilcox.test(Eggs_hatched~Treatment,
            data=df_egglar)

wilcox.test(Egg_mass~Treatment,
            data=df_egglar)

wilcox.test(Adults~Treatment,
       data=df_egglar)

wilcox.test(prop_hat~Treatment,
            data=subset(df_egglar,!is.na(df_egglar$prop_hat)))

wilcox.test(prop_adult~Treatment,
            data=subset(df_egglar,!is.na(df_egglar$prop_adult)))

#No need for tables, just put in pvalue, df=1, test stat (w value)
