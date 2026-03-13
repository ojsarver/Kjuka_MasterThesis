pacman::p_load(tidyverse,
               patchwork,
               here,
               car,
               emmeans,
               nlme)

#create data frames

df_surepr<-read_csv(here("su25growr1.csv"))
df_suvege<-read_csv(here("su25Vgrow1.csv"))
df_farepr<-read_csv(here("fa25repr1.csv"))
df_favege<-read_csv(here("fa25veggrow1.csv"))

# Tidy data frames --------------------------------------------------------

#summer vegetative data frame

df_suvege2<-df_suvege%>%
  pivot_longer(cols=V_1:V_17,
               names_to="Vegetative_Stage",
               values_to="value")%>%
  separate(col=Vegetative_Stage,
           into=c("remove","Vegetative_stage"),
           sep="_")%>% 
  select(-remove)

df_suvege2<-df_suvege2%>%
  mutate(Vegetative_stage=as.numeric(Vegetative_stage))

df_suvege2$B_T<-paste(df_suvege2$Treatment,df_suvege2$Beetle)

df_suvege3<-df_suvege2%>%
  group_by(B_T,Vegetative_stage)%>%
  summarize(mu_V=mean(value,na.rm=T),
            SE_V=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

#fall vegetative data frame

df_favege2<-df_favege%>%
  pivot_longer(cols=V_1:V_18,
               names_to="Vegetative_Stage",
               values_to="value")%>%
  separate(col=Vegetative_Stage,
           into=c("remove","Vegetative_stage"),
           sep="_")%>% 
  select(-remove)

df_favege2<-df_favege2%>%
  mutate(Vegetative_stage=as.numeric(Vegetative_stage))

df_favege2$B_T<-paste(df_favege2$Treatment,df_favege2$Beetle)

df_favege3<-df_favege2%>%
  group_by(B_T,Vegetative_stage)%>%
  summarize(mu_V=mean(value,na.rm=T),
            SE_V=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

#summer reproductive data frame

df_surepr2<-df_surepr%>%
  pivot_longer(cols=R_1:R_6,
               names_to="Reproductive_Stage",
               values_to="value")%>%
  separate(col=Reproductive_Stage,
           into=c("remove","Reproductive_stage"),
           sep="_")%>% 
  select(-remove) 

df_surepr2<-df_surepr2%>%
  mutate(Reproductive_stage=as.numeric(Reproductive_stage))

df_surepr2$B_T<-paste(df_surepr2$Treatment,df_surepr2$Beetle)

df_surepr3<-df_surepr2%>%
  mutate(Reproductive_stage=as.numeric(Reproductive_stage))%>%
  group_by(B_T,Reproductive_stage)%>%
  summarize(mu_R=mean(value),
            SE_R=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

#fall reproductive data frame

df_farepr2<-df_farepr%>%
  pivot_longer(cols=R_1:R_6,
               names_to="Reproductive_Stage",
               values_to="value")%>%
  separate(col=Reproductive_Stage,
           into=c("remove","Reproductive_stage"),
           sep="_")%>% 
  select(-remove) 

df_farepr2<-df_farepr2%>%
  mutate(Reproductive_stage=as.numeric(Reproductive_stage))

df_farepr2$B_T<-paste(df_farepr2$Treatment,df_farepr2$Beetle)

df_farepr3<-df_farepr2%>%
  mutate(Reproductive_stage=as.numeric(Reproductive_stage))%>%
  group_by(B_T,Reproductive_stage)%>%
  summarize(mu_R=mean(value),
            SE_R=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()


# plots -------------------------------------------------------------------

#vegetative summer
ggplot()+
  geom_line(data=df_suvege3,aes(x=Vegetative_stage,
                              y=mu_V,
                              color=B_T))+
  geom_point(data=df_suvege3,aes(x=Vegetative_stage,
                               y=mu_V,
                               color=B_T))+
  geom_point(data=df_suvege2,aes(x=Vegetative_stage,
                               y=value,
                               color=B_T),alpha=.5)+
  geom_errorbar(data=df_suvege3, aes(x=Vegetative_stage,
                                   ymin=mu_V-SE_V,
                                   ymax=mu_V+SE_V),width=.2)+
  scale_color_manual(values = c("indianred1",
                                "skyblue1",
                                "orangered4",
                                "royalblue1"))+
  theme_bw()+
  labs(x="Vegetative Stage",
       y="Weeks To",
       color="Legend")

#vegetative fall

ggplot()+
  geom_line(data=df_favege3,aes(x=Vegetative_stage,
                                y=mu_V,
                                color=B_T))+
  geom_point(data=df_favege3,aes(x=Vegetative_stage,
                                 y=mu_V,
                                 color=B_T))+
  geom_point(data=df_favege2,aes(x=Vegetative_stage,
                                 y=value,
                                 color=B_T),alpha=.5)+
  geom_errorbar(data=df_favege3, aes(x=Vegetative_stage,
                                     ymin=mu_V-SE_V,
                                     ymax=mu_V+SE_V),width=.2)+
  scale_color_manual(values = c("indianred1",
                                "skyblue1",
                                "orangered4",
                                "royalblue1"))+
  theme_bw()+
  labs(x="Vegetative Stage",
       y="Days To",
       color="Legend")

#reproductive summer 

ggplot()+
  geom_line(data=df_surepr3,aes(x=Reproductive_stage,
                              y=mu_R,
                              color=B_T))+
  geom_point(data=df_surepr3,aes(x=Reproductive_stage,
                               y=mu_R,
                               color=B_T))+
  geom_point(data=df_surepr2,aes(x=Reproductive_stage,
                               y=value,
                               color=B_T),alpha=.5)+
  geom_errorbar(data=df_surepr3, aes(x=Reproductive_stage,
                                   ymin=mu_R-SE_R,
                                   ymax=mu_R+SE_R),width=.2)+
  scale_color_manual(values = c("indianred1",
                                "skyblue1",
                                "orangered4",
                                "royalblue1"))+
  labs(x="Reproductive Stage",
       y="Weeks To",
       color="Legend")

#reproductive fall

ggplot()+
  geom_line(data=df_farepr3,aes(x=Reproductive_stage,
                                y=mu_R,
                                color=B_T))+
  geom_point(data=df_farepr3,aes(x=Reproductive_stage,
                                 y=mu_R,
                                 color=B_T))+
  geom_point(data=df_farepr2,aes(x=Reproductive_stage,
                                 y=value,
                                 color=B_T),alpha=.5)+
  geom_errorbar(data=df_farepr3, aes(x=Reproductive_stage,
                                     ymin=mu_R-SE_R,
                                     ymax=mu_R+SE_R),width=.2)+
  scale_color_manual(values = c("indianred1",
                                "skyblue1",
                                "orangered4",
                                "royalblue1"))+
  labs(x="Reproductive Stage",
       y="Days To",
       color="Legend")

#idk how to change the order of the growth stages :(


# stats -------------------------------------------------------------------

#repeated measures ANOVA 
#reproduction summer

summary(rpaovr <- lme(value~Reproductive_stage*Treatment*Beetle,
                     data=df_surepr2, 
                           random=~1|Plant_ID,
                           correlation=corAR1(form=~Reproductive_stage|Plant_ID), #autoregressive structure
                           control=lmeControl(returnObject=T)))


Anova(rpaovr, type=3)
lsmeans(rpaovr, pairwise~Treatment*Beetle, adjust='tukey')

#reproduction fall-mad about NA's that need filled in

summary(rpaovrf <- lme(value~Reproductive_stage*Treatment*Beetle,
                      data=df_farepr2, 
                      random=~1|Plant_ID,
                      correlation=corAR1(form=~Reproductive_stage|Plant_ID),
                      control=lmeControl(returnObject=T)))


Anova(rpaovrf, type=3)
lsmeans(rpaovrf, pairwise~Treatment*Beetle, adjust='tukey')

#vegetative summer

summary(rpaovv <- lme(value~Vegetative_stage*Treatment*Beetle,
                     data=subset(df_suvege2,!is.na(df_suvege2$value)), 
                     random=~1|Plant_ID,
                     correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                     control=lmeControl(returnObject=T)))

Anova(rpaovv, type=3)
lsmeans(rpaovv, pairwise~Treatment*Beetle, adjust='tukey')

#vegetative fall

summary(rpaovvf <- lme(value~Vegetative_stage*Treatment*Beetle,
                      data=subset(df_favege2,!is.na(df_favege2$value)), 
                      random=~1|Plant_ID,
                      correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                      control=lmeControl(returnObject=T)))

Anova(rpaovvf, type=3)
lsmeans(rpaovvf, pairwise~Treatment*Beetle, adjust='tukey')
