pacman::p_load(tidyverse,
               patchwork,
               here,
               car,
               emmeans,
               nlme,
               grid)

df_sufungpa<-read_csv(here("fung_presenceabsence.csv"))
df_fafungpa<-read_csv(here("fa25fungpa.csv"))

# tidy data ---------------------------------------------------------------

#summer
df_sufpa<-df_sufungpa%>%
  pivot_longer(cols=Week_1:Week_11,
               names_to="Week",
               values_to="value")%>%
  separate(col=Week,
           into=c("remove","Week"),
           sep="_")%>% 
  select(-remove)

df_sufpa<-df_sufpa%>%
  mutate(Week=as.numeric(Week))

df_sufpa$B_T<-paste(df_sufpa$Treatment,df_sufpa$Beetle)

df_sufpavg<-df_sufpa%>%
  group_by(B_T,Week)%>%
  summarize(mu_f=mean(value),
            SE_f=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

su8<-subset(df_sufpavg,Week==8)
su8$B_T <- factor(su8$B_T)

#fall

df_fafungpa$B_T<-paste(df_fafungpa$Treatment,df_fafungpa$Beetle)

df_fafpa<-df_fafungpa%>%
  pivot_longer(cols=Week_1:Week_14,
               names_to="Week", 
               values_to="value")%>% 
  separate(col=Week,
           into=c("remove","Week"),
           sep="_")%>% 
  select(-remove)

df_fafpa<-df_fafpa%>%
  mutate(Week=as.numeric(Week))

df_fafpa$B_T<-paste(df_fafpa$Treatment,df_fafpa$Beetle)

df_fafpavg<-df_fafpa%>%
  group_by(Treatment,Beetle,Week)%>%
  summarize(mu_f=mean(value),
            SE_f=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

df_fafpavg$B_T<-paste(df_fafpavg$Treatment,df_fafpavg$Beetle)

fa10<-subset(df_fafpavg, Week==8)

fa10$B_T <- factor(fa10$B_T)

# plots -------------------------------------------------------------------
#summer

sufp<-su8%>%
  ggplot(aes(x=factor(B_T,levels=c("Control, No Beetle",
                                    "Control, Beetle",
                                    "Microplastic, No Beetle",
                                    "Microplastic, Beetle")),
             y=mu_f,
             fill=B_T))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_f-SE_f, ymax=mu_f+SE_f,
                width=0.2))+
  annotate('text', x = 1, y = 1.1, label = 'a', size = 8)+
  annotate('text', x = 2, y = 1.1, label = 'b', size = 8)+
  annotate('text', x = 3, y = 1.1, label = 'a', size = 8)+
  annotate('text', x = 4, y = 1.1, label = 'b', size = 8)+
  annotate('text', x = .75, y = 1.4, label = '(A)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=14),
        legend.position = "none",
        plot.title = element_text(size=29,hjust=.5),
        plot.subtitle=element_text(size=25,hjust=.5))+
  labs(title="Trophic Interaction Week 8",
       subtitle="N = 21 N = 8 N = 21 N = 8",
       x="",
       y="Proportion of Plants with Fungal Damage")


#fall

fafp<-fa10%>%
  ggplot(aes(x=factor(B_T, levels=c("Control, No Beetle",
                                    "Control, Beetle",
                                    "Microplastic, No Beetle",
                                    "Microplastic, Beetle")),
             y=mu_f,
             fill=B_T))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_f-SE_f, ymax=mu_f+SE_f,
                    width=0.2))+
  annotate('text', x = 1, y = .68, label = 'a', size = 8)+
  annotate('text', x = 2, y = .68, label = 'a', size = 8)+
  annotate('text', x = 3, y = .68, label = 'a', size = 8)+
  annotate('text', x = 4, y = .68, label = 'a', size = 8)+
  annotate('text', x = .75, y = .8, label = '(B)', size = 9)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=14),
         legend.position = "none",
        plot.title = element_text(size=29,hjust=.5),
        plot.subtitle=element_text(size=25,hjust=.5))+
  labs(title="Plant Response Week 10",
       subtitle="N = 13 N = 8 N = 13 N = 8",
       x="",
       y="")

pushViewport(viewport(layout=grid.layout(1,2)))
print(sufp,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(fafp,vp=viewport(layout.pos.row=1,layout.pos.col = 2))


# stats' ------------------------------------------------------------------


#summer presence/absence
fglmmsu<-glm(Week_8~Beetle*Treatment,
           data=df_sufungpa,
           family=binomial)

Anova(fglmmsu, type=3)

lsmeans(fglmmsu, pairwise~Treatment*Beetle, adjust='tukey')

#fall presence/absence

fglmmfa<-glm(Week_8~Beetle*Treatment,
             data=df_fafungpa,
             family=binomial)

Anova(fglmmfa, type=3)

lsmeans(fglmmfa, pairwise~Treatment*Beetle, adjust='tukey')

#Fall avg percentage including zeroes - too low sample size to do

haov<-aov(value~Treatment*Beetle,
          data=subset(df_fafpa,Week==10))

Anova(haov,type=3)

emmeans(haov,~Treatment*Beetle)

#fall avg percentage w/o zeroes - no anova cause unequal observations
#linear mixed effects model

falme<-lme(value~Treatment*Beetle,
           data=subset(df_fa0,Week==10),
           random=~1|Plant_ID)

anova(falme)