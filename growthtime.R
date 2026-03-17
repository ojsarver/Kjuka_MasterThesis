pacman::p_load(tidyverse,
               patchwork,
               here,
               car,
               emmeans,
               nlme,
               grid,
               ggplot2,
               ggbreak)

df_surepr<-read_csv(here("su25growr1.csv"))
df_suvege<-read_csv(here("su25Vgrow1.csv"))
df_farepr<-read_csv(here("fa25repr1.csv"))
df_favege<-read_csv(here("fa25veggrow2.csv"))
# Tidy data frames --------------------------------------------------------

#summer vegetative - prep

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

df_suveAVG<-df_suvege2%>%
  group_by(B_T,Vegetative_stage)%>%
  summarize(mu_V=mean(value,na.rm=T),
            SE_V=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

#Split into before beetle addition

df_subbp<-subset(df_suvege2,Vegetative_stage <=10)
df_subbl<-subset(df_suveAVG,Vegetative_stage <=10)

#split into after beetle addition
df_suabp<-subset(df_suvege2,Vegetative_stage >10)
df_suabl<-subset(df_suveAVG,Vegetative_stage >10)

#summer combo vegetative and reproductive after beetle addition

df_surepr22 <- rename(df_surepr2, Vegetative_stage = Reproductive_stage)
bindeer <- rbind(df_suabp,df_surepr22) %>%
  mutate(groups = ifelse(Vegetative_stage < 7, "Reproductive", "Vegetative"))

df_surepr33 <- rename(df_surepr3, Vegetative_stage = Reproductive_stage,
                      mu_V = mu_R,
                      SE_V= SE_R)
bindeer2 <- rbind(df_suabl,df_surepr33) %>% 
  mutate(groups = ifelse(Vegetative_stage < 7, "Reproductive", "Vegetative"))

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

df_favege2$B_T<-paste(df_favege2$Treatment,df_favege2$Beetles)

df_faveAVG<-df_favege2%>%
  group_by(B_T,Vegetative_stage)%>%
  summarize(mu_V=mean(value,na.rm=T),
            SE_V=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

#Split into before beetle addition

df_fabbpv<-subset(df_favege2,Vegetative_stage <=14)
df_fabblv<-subset(df_faveAVG,Vegetative_stage <=14)

#split into after beetle addition
df_faabpv<-subset(df_favege2,Vegetative_stage >14)
df_faablv<-subset(df_faveAVG,Vegetative_stage >14)

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

df_fabbp<-subset(df_favege2,Vegetative_stage <=14)
df_fabbl<-subset(df_faveAVG,Vegetative_stage <=14)

#split into after beetle addition
df_faabp<-subset(df_favege2,Vegetative_stage >14)
df_faabl<-subset(df_faveAVG,Vegetative_stage >14)
# plots -------------------------------------------------------------------

#summer - bb
  scale_color_manual(values = c("indianred1",
                                "skyblue1",
                                "orangered4",
                                "royalblue1"))

subb<-df_subbl%>%
  ggplot(aes(x=Vegetative_stage,
             y=mu_V,
             color=B_T))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mu_V-SE_V,
                    ymax=mu_V+SE_V),width=.2)+
  geom_point(data=df_subbp,aes(x=Vegetative_stage,
                               y=value,
                               color=B_T),alpha=.5)+
  theme_bw()+
  labs(x="Summer Before Beetle Addition",
       y="Weeks To To Reach Growth Stage",
       color="Legend")

#summer ab

suab<-bindeer2 %>% 
  ggplot(aes(x=Vegetative_stage, y=mu_V, color = B_T))+
  geom_line()+
  geom_point(data = bindeer,aes(y=value))+
  geom_errorbar(data = bindeer2, aes(ymin=mu_V-SE_V,
                                     ymax=mu_V+SE_V),width=.2)+
  facet_wrap(~groups, scales = "free_x")+
  scale_color_manual(values = c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  labs(x="Summer After Beetle Addition",
       y="Weeks To Reach Growth Stage",
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

#reproduction fall

summary(rpaovrf <- lme(value~Reproductive_stage*Treatment*Beetle,
                       data=df_farepr2, 
                       random=~1|Plant_ID,
                       correlation=corAR1(form=~Reproductive_stage|Plant_ID),
                       control=lmeControl(returnObject=T)))


Anova(rpaovrf, type=3)
lsmeans(rpaovrf, pairwise~Treatment*Beetle, adjust='tukey')

#vegetative summer - before beetle

summary(rpaovv1 <- lme(value~Vegetative_stage*Treatment,
                       data=subset(df_subb,!is.na(df_subb$value)), 
                       random=~1|Plant_ID,
                       correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                       control=lmeControl(returnObject=T)))

anova(rpaovv1)
lsmeans(rpaovv1, pairwise~Treatment, adjust='tukey')

#vegetative summer after beetle

summary(rpaovv2 <- lme(value~Vegetative_stage*Treatment*Beetle,
                       data=subset(df_suab,!is.na(df_suab$value)), 
                       random=~1|Plant_ID,
                       correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                       control=lmeControl(returnObject=T)))

Anova(rpaovv2, type=3)
lsmeans(rpaovv2, pairwise~Treatment*Beetle, adjust='tukey')

#vegetative fall bb

summary(rpaovvf1 <- lme(value~Vegetative_stage*Treatment,
                        data=subset(df_fabb,!is.na(df_fabb$value)), 
                        random=~1|Plant_ID,
                        correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                        control=lmeControl(returnObject=T)))

Anova(rpaovvf1, type=3)
lsmeans(rpaovvf1, pairwise~Treatment, adjust='tukey')

#vegetative fall ab

summary(rpaovvf2 <- lme(value~Vegetative_stage*Treatment*Beetle,
                        data=subset(df_favege2,!is.na(df_favege2$value)), 
                        random=~1|Plant_ID,
                        correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                        control=lmeControl(returnObject=T)))

Anova(rpaovvf2, type=3)
lsmeans(rpaovvf2, pairwise~Treatment*Beetle, adjust='tukey')