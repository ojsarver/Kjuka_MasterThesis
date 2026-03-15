pacman::p_load(tidyverse,
               patchwork,
               here,
               grid)

df_sufungpa<-read_csv(here("fung_presenceabsence.csv"))
df_fafungpa<-read_csv(here("fa25fung.csv"))


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

fa10<-subset(df_fafpavg, Week==10)

# plots -------------------------------------------------------------------
#summer

sufp<-su8%>%
  ggplot(aes(x=B_T,
             y=mu_f,
             fill=B_T))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_f-SE_f, ymax=mu_f+SE_f,
                width=0.2))+
  annotate('text', x = 1, y = 1.1, label = '(a)', size = 8)+
  annotate('text', x = 2, y = 1.1, label = '(b)', size = 8)+
  annotate('text', x = 3, y = 1.1, label = '(a)', size = 8)+
  annotate('text', x = 4, y = 1.1, label = '(b)', size = 8)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15,margin=margin(10,0,0,0)),
        axis.title.y = element_text(size = 15,margin=margin(0,10,0,0)),
        legend.position = "none")+
  labs(x="Summer Week 8",
       y="Proportion of Plants with Fungal Damage")


#fall

fafp<-fa10%>%
  ggplot(aes(x=B_T,
             y=mu_f,
             fill=B_T))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_f-SE_f, ymax=mu_f+SE_f,
                    width=0.2))+
  annotate('text', x = 1, y = .0225, label = '(a)', size = 8)+
  annotate('text', x = 2, y = .0225, label = '(a)', size = 8)+
  annotate('text', x = 3, y = .0225, label = '(a)', size = 8)+
annotate('text', x = 4, y = .0225, label = '(b)', size = 8)+
  scale_fill_manual(values=c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15,margin=margin(10,0,0,0)),
         axis.title.y = element_text(size = 15,margin=margin(0,10,0,0)),
         legend.position = "none")+
  labs(x="Fall Week 10",
       y="Average Percentage of Fungal Damage")

pushViewport(viewport(layout=grid.layout(1,2)))
print(sufp,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(fafp,vp=viewport(layout.pos.row=1,layout.pos.col = 2))


# stats' ------------------------------------------------------------------

fglmmsu<-glm(Week_8~Beetle*Treatment,
           data=df_sufungpa,
           family=binomial)

Anova(fglmmsu, type=3)

lsmeans(fglmmsu, pairwise~Treatment*Beetle, adjust='tukey')

#beetle plants have less fungal presence even after adjusting for observations 

#stole from biomass data idk if it is right

haov<-aov(value~Treatment*Beetle,
          data=subset(df_fafpa,Week==10))

Anova(haov,type=3)

emmeans(haov,~Treatment*Beetle)
