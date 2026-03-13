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

pushViewport(viewport(layout=grid.layout(2,2)))
print(mu_egg,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(total_egg,vp=viewport(layout.pos.row=1,layout.pos.col = 2))

# plots -------------------------------------------------------------------
#summer

sufp<-ggplot()+
  geom_line(data=df_sufpavg,aes(x=Week,
                                 y=mu_f, color=B_T))+
  geom_point(data=df_sufpavg,aes(x=Week,
                                y=mu_f, color=B_T))+
  geom_errorbar(data=df_sufpavg, aes(x=Week,ymin=mu_f-SE_f,ymax=mu_f+SE_f),width=.2)+
  scale_color_manual(values = c("indianred1",
                                "skyblue1",
                                "orangered4",
                                "royalblue1"))+
  theme_bw()+
  labs(x="Week",
       y="Proportion of Plants with Fungal Damage")+
  theme(legend.position = "none")

#fall

fafp<-ggplot()+
  geom_line(data=df_fafpavg,aes(x=Week,
                                y=mu_f, color=B_T))+
  geom_point(data=df_fafpavg,aes(x=Week,
                                 y=mu_f, color=B_T))+
  geom_errorbar(data=df_fafpavg, aes(x=Week,ymin=mu_f-SE_f,ymax=mu_f+SE_f),
                width=.2)+
  scale_color_manual(values = c("indianred1",
                                "skyblue1",
                                "orangered4",
                                "royalblue1"))+
  theme_bw()+
  labs(x="Week",
       y="Average Percent of Fungal Damage",
       color="Legend")

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
