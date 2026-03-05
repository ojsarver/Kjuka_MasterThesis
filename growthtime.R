pacman::p_load(tidyverse,
               patchwork,
               here)

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

df_repr3<-df_repr2%>%
  mutate(Reproductive_stage=as.numeric(Reproductive_stage))%>%
  group_by(Treatment,Reproductive_stage)%>%
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

df_vege3<-df_vege2%>%
  group_by(Treatment,Vegetative_stage)%>%
  summarize(mu_V=mean(value,na.rm=T),
            SE_V=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()


#plots

df_repr2%>%
  ggplot(aes(x=Reproductive_stage,
             y=value,
             color=Treatment))+
  geom_point()

ggplot()+
  geom_line(data=df_repr3,aes(x=Reproductive_stage,
                              y=mu_R,
                              color=Treatment))+
  geom_point(data=df_repr3,aes(x=Reproductive_stage,
                               y=mu_R,
                               color=Treatment))+
  geom_point(data=df_repr2,aes(x=Reproductive_stage,
                               y=value,
                               color=Treatment))+
  geom_errorbar(data=df_repr3, aes(x=Reproductive_stage,
                                   ymin=mu_R-SE_R,
                                   ymax=mu_R+SE_R),width=.2)

ggplot()+
  geom_line(data=df_vege3,aes(x=Vegetative_stage,
                              y=mu_V,
                              color=Treatment))+
  geom_point(data=df_vege3,aes(x=Vegetative_stage,
                               y=mu_V,
                               color=Treatment))+
  geom_point(data=df_vege2,aes(x=Vegetative_stage,
                               y=value,
                               color=Treatment))+
  geom_errorbar(data=df_vege3, aes(x=Vegetative_stage,
                                   ymin=mu_V-SE_V,
                                   ymax=mu_V+SE_V),width=.2)

#idk how to change the order of the growth stages :(, also idk how it is 
#interpretting na in the graph

df_vege2%>%
  ggplot(aes(x=Vegetative_stage,
             y=value,
             color=Treatment))+
  geom_point()+
  geom_smooth()

#need to combine these?
