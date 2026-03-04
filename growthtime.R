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

df_vege2<-df_vege%>%
  pivot_longer(cols=V_E:V_15,
               names_to="Vegetative_Stage",
               values_to="value")%>%
  separate(col=Vegetative_Stage,
           into=c("remove","Vegetative_stage"),
           sep="_")%>% 
  select(-remove) 

df_repr2<-df_repr2%>%
  mutate(Reproductive_stage=as.numeric(Reproductive_stage))

#plots

df_repr2%>%
  ggplot(aes(x=Reproductive_stage,
             y=value,
             color=Treatment))+
  geom_point()+
  geom_smooth()

#idk how to change the order of the growth stages :(

df_vege2%>%
  ggplot(aes(x=Vegetative_stage,
             y=value,
             color=Treatment))+
  geom_point()+
  geom_smooth()
