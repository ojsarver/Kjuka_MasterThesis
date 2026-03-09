pacman::p_load(tidyverse,
               patchwork,
               here,
               car,
               emmeans,
               nlme)

#create data frames

df_repr<-read_csv(here("su25growr1.csv"))

df_vege<-read_csv(here("su25Vgrow1.csv"))
#tidy data frames

df_repr2<-df_repr%>%
  pivot_longer(cols=R_1:R_6,
               names_to="Reproductive_Stage",
               values_to="value")%>%
  separate(col=Reproductive_Stage,
           into=c("remove","Reproductive_stage"),
           sep="_")%>% 
  select(-remove) 

df_repr2<-df_repr2%>%
  mutate(Reproductive_stage=as.numeric(Reproductive_stage))

df_repr2$B_T<-paste(df_repr2$Treatment,df_repr2$Beetle)

df_repr3<-df_repr2%>%
  mutate(Reproductive_stage=as.numeric(Reproductive_stage))%>%
  group_by(B_T,Reproductive_stage)%>%
  summarize(mu_R=mean(value),
            SE_R=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

df_vege2<-df_vege%>%
  pivot_longer(cols=V_1:V_17,
               names_to="Vegetative_Stage",
               values_to="value")%>%
  separate(col=Vegetative_Stage,
           into=c("remove","Vegetative_stage"),
           sep="_")%>% 
  select(-remove)

df_vege2<-df_vege2%>%
  mutate(Vegetative_stage=as.numeric(Vegetative_stage))

df_vege2$B_T<-paste(df_vege2$Treatment,df_vege2$Beetle)

df_vege3<-df_vege2%>%
  group_by(B_T,Vegetative_stage)%>%
  summarize(mu_V=mean(value,na.rm=T),
            SE_V=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

#plots

ggplot()+
  geom_line(data=df_repr3,aes(x=Reproductive_stage,
                              y=mu_R,
                              color=B_T))+
  geom_point(data=df_repr3,aes(x=Reproductive_stage,
                               y=mu_R,
                               color=B_T))+
  geom_point(data=df_repr2,aes(x=Reproductive_stage,
                               y=value,
                               color=B_T))+
  geom_errorbar(data=df_repr3, aes(x=Reproductive_stage,
                                   ymin=mu_R-SE_R,
                                   ymax=mu_R+SE_R),width=.2)+
  scale_color_manual(values = c("#648FFF",
                                "#785EF0",
                                "#DC267F",
                                "#FE6100"))+
  labs(x="Reproductive Stage",
       y="Weeks To",
       color="Legend")

ggplot()+
  geom_line(data=df_vege3,aes(x=Vegetative_stage,
                              y=mu_V,
                              color=B_T))+
  geom_point(data=df_vege3,aes(x=Vegetative_stage,
                               y=mu_V,
                               color=B_T))+
  geom_point(data=df_vege2,aes(x=Vegetative_stage,
                               y=value,
                               color=B_T))+
  geom_errorbar(data=df_vege3, aes(x=Vegetative_stage,
                                   ymin=mu_V-SE_V,
                                   ymax=mu_V+SE_V),width=.2)+
  scale_color_manual(values = c("#648FFF",
                                         "#785EF0",
                                         "#DC267F",
                                         "#FE6100"))+
  labs(x="Vegetative Stage",
       y="Weeks To",
       color="Legend")



#idk how to change the order of the growth stages :(

#Stats

#repeated measures ANOVA ****go with this model 
#reproduction:

summary(rpaovr <- lme(value~Reproductive_stage*Treatment*Beetle,
                     data=df_repr2, 
                           random=~1|Plant_ID,
                           correlation=corAR1(form=~Reproductive_stage|Plant_ID), #autoregressive structure
                           control=lmeControl(returnObject=T)))


Anova(rpaovr, type=3)
lsmeans(rpaovr, pairwise~Treatment*Beetle, adjust='tukey')

#make table of ANOVA results, bold significant lines

#vegetative

summary(rpaovv <- lme(value~Vegetative_stage*Treatment*Beetle,
                     data=subset(df_vege2,!is.na(df_vege2$value)), 
                     random=~1|Plant_ID,
                     correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                     control=lmeControl(returnObject=T)))

Anova(rpaovv, type=3)
lsmeans(rpaovv, pairwise~Treatment*Beetle, adjust='tukey')
