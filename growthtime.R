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
df_farepr<-read_csv(here("fa25repr2.csv"))
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

df_suveAVGab<-df_suvege2%>%
  group_by(B_T,Vegetative_stage)%>%
  summarize(mu_V=mean(value,na.rm=T),
            SE_V=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

df_suveAVGbb<-df_suvege2%>%
  group_by(Treatment,Vegetative_stage)%>%
  summarize(mu_V=mean(value,na.rm=T),
            SE_V=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

#Split into before beetle addition

df_subbp<-subset(df_suvege2,Vegetative_stage <=10)
df_subbl<-subset(df_suveAVGbb,Vegetative_stage <=10)

#split into after beetle addition
df_suabp<-subset(df_suvege2,Vegetative_stage >10)
df_suabl<-subset(df_suveAVGab,Vegetative_stage >10)

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

df_faveAVGab<-df_favege2%>%
  group_by(B_T,Vegetative_stage)%>%
  summarize(mu_V=mean(value,na.rm=T),
            SE_V=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

df_faveAVGbb<-df_favege2%>%
  group_by(Treatment,Vegetative_stage)%>%
  summarize(mu_V=mean(value,na.rm=T),
            SE_V=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

#Split into before beetle addition

df_fabbpv<-subset(df_favege2,Vegetative_stage <=14)
df_fabblv<-subset(df_faveAVGbb,Vegetative_stage <=14)

#split into after beetle addition
df_faabpv<-subset(df_favege2,Vegetative_stage >14)
df_faablv<-subset(df_faveAVGab,Vegetative_stage >14)

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

df_farepr3ab<-df_farepr2%>%
  mutate(Reproductive_stage=as.numeric(Reproductive_stage))%>%
  group_by(B_T,Reproductive_stage)%>%
  summarize(mu_R=mean(value),
            SE_R=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

df_farepr3bb<-df_farepr2%>%
  mutate(Reproductive_stage=as.numeric(Reproductive_stage))%>%
  group_by(Treatment,Reproductive_stage)%>%
  summarize(mu_R=mean(value),
            SE_R=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

df_fabbpr<-subset(df_farepr2,Reproductive_stage <=3)
df_fabblr<-subset(df_farepr3bb,Reproductive_stage <=3)

#split into after beetle addition
df_faabpr<-subset(df_farepr2,Reproductive_stage >3)
df_faablr<-subset(df_farepr3ab,Reproductive_stage >3)

#summer combo vegetative and reproductive after beetle addition

df_surepr22 <- rename(df_surepr2, Vegetative_stage = Reproductive_stage)%>%
  mutate(Life_stage="Reproductive",
         Timing="Post Beetle")
df_suabp22<- df_suabp%>%
  mutate(Life_stage="Vegetative",
         Timing="Post Beetle")
df_subbp22<-df_subbp %>% 
  mutate(Life_stage="Vegetative",
         Timing="Pre Beetle")
bindeer <- rbind(df_suabp22,df_surepr22)


df_surepr33 <- rename(df_surepr3, Vegetative_stage = Reproductive_stage,
                      mu_V=mu_R,
                      SE_V=SE_R)%>%
  mutate(Life_stage="Reproductive",
         Timing="Post Beetle")

df_suabl33<- df_suabl%>%
  mutate(Life_stage="Vegetative",
         Timing="Post Beetle")
df_subbl33<-df_subbl %>% 
  mutate(Life_stage="Vegetative",
         Timing="Pre Beetle")
bindeer2 <- rbind(df_suabl33,df_surepr33)

#fall combo before beetle addition


df_fabbpr22<-rename(df_fabbpr, Vegetative_stage = Reproductive_stage) %>% 
  mutate(Life_stage="Reproductive",
         Timing="Pre Beetle")
df_fabbpv22<-df_fabbpv %>% 
  mutate(Life_stage="Vegetative",
         Timing="Pre Beetle")

bindeerbbp <- rbind(df_fabbpr22,df_fabbpv22)

df_fabblr22<-rename(df_fabblr, Vegetative_stage = Reproductive_stage,
                    mu_V=mu_R,
                    SE_V=SE_R) %>% 
  mutate(Life_stage="Reproductive",
         Timing="Pre Beetle") 
df_fabblv33<-df_fabblv %>% 
  mutate(Life_stage="Vegetative",
         Timing="Pre Beetle")

bindeerbbl <- rbind(df_fabblr22,df_fabblv33)

#fall combo after beetle addition

df_faabpr22<- rename(df_faabpr, Vegetative_stage = Reproductive_stage)%>%
  mutate(Life_stage="Reproductive",
         Timing="Post Beetle")
df_faabpv22<- df_faabpv%>%
  mutate(Life_stage="Vegetative",
         Timing="Post Beetle")

bindeerabp<-rbind(df_faabpr22,df_faabpv22)

df_faablv33<- df_faablv%>%
  mutate(Life_stage="Vegetative",
         Timing="Post Beetle")
df_faablr22<- rename(df_faablr, Vegetative_stage = Reproductive_stage,
                     mu_V=mu_R,
                     SE_V=SE_R)%>%
  mutate(Life_stage="Reproductive",
         Timing="Post Beetle")

bindeerabl <- rbind(df_faablr22,df_faablv33)

# plots -------------------------------------------------------------------

#summer - bb

subeforeb<-df_subbl33 %>% 
  ggplot(aes(x=Vegetative_stage, y=mu_V, color = Treatment))+
  geom_line(linewidth =.75)+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mu_V-SE_V,
                    ymax=mu_V+SE_V),,width=.2)+
  facet_wrap(~Life_stage, scales = "free_x")+
  annotate('text', x = .75, y = 8.2, label = '(A)', size = 8)+
  scale_color_manual(values = c("#6ABFD6", "#43527A"))+
  theme_bw()+
  theme(axis.title=element_text(size=13),
        legend.position ="none")+
  labs(x="Trophic Interaction Before Beetle Addition",
       y="Weeks To Reach Growth Stage")

#summer ab

suafterb<-bindeer2 %>% 
  ggplot(aes(x=Vegetative_stage, y=mu_V, color = B_T))+
  geom_line(linewidth =.75)+
  geom_point(size=2.5)+
  geom_errorbar(data = bindeer2, aes(ymin=mu_V-SE_V,
                                     ymax=mu_V+SE_V),width=.2)+
  facet_wrap(~Life_stage, scales = "free_x")+
  scale_color_manual(values = c("#B3967D","#6ABFD6","#6E4D3E", "#43527A"))+
  theme_bw()+
  theme(legend.key.height = unit(.5, 'cm'),
        legend.position ="inside",
        legend.position.inside = c(.17,.81),
        axis.title=element_text(size=13))+
  labs(x="Trophic Interaction After Beetle Addition",
       y="Weeks To Reach Growth Stage",
       color="Treatment")

#fall bb

fabb<-bindeerbbl %>% 
  ggplot(aes(x=Vegetative_stage, y=mu_V, color = Treatment))+
  geom_line(linewidth =.75)+
  geom_point(size=2.5)+
  geom_errorbar(data = bindeerbbl, aes(ymin=mu_V-SE_V,
                                     ymax=mu_V+SE_V),width=.2)+
  facet_wrap(~Life_stage, scales = "free_x")+
  scale_color_manual(values = c("#6ABFD6", "#43527A"))+
  theme_bw()+
  theme(legend.position ="none",
        axis.title=element_text(size=13))+
  labs(x="Plant Response Before Beetle Addition",
       y="")

#fall ab 

faab<-bindeerabl %>% 
  ggplot(aes(x=Vegetative_stage, y=mu_V, color = B_T))+
  geom_line(linewidth =.75)+
  geom_point(size=2.5)+
  geom_errorbar(data = bindeerabl, aes(ymin=mu_V-SE_V,
                                       ymax=mu_V+SE_V),width=.2)+
  facet_wrap(~Life_stage, scales = "free_x")+
  scale_color_manual(values = c("#B3967D","#6ABFD6","#6E4D3E", "#43527A"))+
  theme_bw()+
  theme(legend.position ="none",
        axis.title=element_text(size=13))+
  labs(x="Plant Response After Beetle Addition",
       y="",
       color="Treatment")

pushViewport(viewport(layout=grid.layout(2,2)))
print(subeforeb,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(suafterb,vp=viewport(layout.pos.row=2,layout.pos.col = 1))
print(fabb,vp=viewport(layout.pos.row=1,layout.pos.col = 2))
print(faab,vp=viewport(layout.pos.row=2,layout.pos.col = 2))


# stats -------------------------------------------------------------------

#repeated measures ANOVA 


#vegetative summer - before beetle

summary(vsbb <- lme(value~Vegetative_stage*Treatment,
                       data=subset(df_subb,!is.na(df_subb$value)), 
                       random=~1|Plant_ID,
                       correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                       control=lmeControl(returnObject=T)))

anova(vsbb)
lsmeans(vsbb, pairwise~Treatment, adjust='tukey')

#vegetative summer after beetle

summary(vsab <- lme(value~Vegetative_stage*Treatment*Beetle,
                       data=subset(df_suab,!is.na(df_suab$value)), 
                       random=~1|Plant_ID,
                       correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                       control=lmeControl(returnObject=T)))

Anova(vsab, type=3)
lsmeans(vsab, pairwise~Treatment*Beetle, adjust='tukey')

#vegetative fall bb

summary(vfbb <- lme(value~Vegetative_stage*Treatment,
                        data=subset(df_fabbpv,!is.na(df_fabbpv$value)), 
                        random=~1|Plant_ID,
                        correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                        control=lmeControl(returnObject=T)))

anova(vfbb)
lsmeans(vfbb, pairwise~Treatment, adjust='tukey')

#vegetative fall ab

summary(vfab <- lme(value~Vegetative_stage*Treatment*Beetle,
                        data=subset(df_favege2,!is.na(df_favege2$value)), 
                        random=~1|Plant_ID,
                        correlation=corAR1(form=~Vegetative_stage|Plant_ID),
                        control=lmeControl(returnObject=T)))

Anova(vfab, type=3)
lsmeans(vfab, pairwise~Treatment*Beetle, adjust='tukey')

#reproduction summer (ab)

summary(rsab <- lme(value~Reproductive_stage*Treatment*Beetle,
                    data=df_surepr2, 
                    random=~1|Plant_ID,
                    correlation=corAR1(form=~Reproductive_stage|Plant_ID), #autoregressive structure
                    control=lmeControl(returnObject=T)))
Anova(rsab, type=3)
lsmeans(rsab, pairwise~Treatment*Beetle, adjust='tukey')

#reproduction fall ab

summary(rfab <- lme(value~Reproductive_stage*Treatment*Beetle,
                    data=df_faabpr,
                    random=~1|Plant_ID,
                    correlation=corAR1(form=~Reproductive_stage|Plant_ID),
                    control=lmeControl(returnObject=T)))
Anova(rfab, type=3)
lsmeans(rfab, pairwise~Treatment*Beetle, adjust='tukey')

#reproduction fall bb

summary(rfbb <- lme(value~Reproductive_stage*Treatment,
                    data=df_fabbpr, 
                    random=~1|Plant_ID,
                    correlation=corAR1(form=~Reproductive_stage|Plant_ID),
                    control=lmeControl(returnObject=T)))

anova(rfbb)
lsmeans(rpaovrf, pairwise~Treatment*Beetle, adjust='tukey')
