#summer

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

#fall
df_surepr22 <- rename(df_surepr2, Vegetative_stage = Reproductive_stage)%>%
  mutate(Life_stage="Reproductive",
         Timing="Post Beetle")
df_suabp22<- df_suabp%>%
  mutate(Life_stage="Vegetative",
         Timing="Post Beetle")
df_subbp22<-df_subbp %>% 
  mutate(Life_stage="Vegetative",
         Timing="Pre Beetle")
bindeer3 <- rbind(df_suabp22,df_surepr22)


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
bindeer4 <- rbind(df_suabl33,df_surepr33)


#plots

suafterb<-bindeer2 %>% 
  ggplot(aes(x=Vegetative_stage, y=mu_V, color = B_T))+
  geom_line()+
  geom_point()+
  geom_point(data = bindeer,aes(y=value))+
  geom_errorbar(data = bindeer2, aes(ymin=mu_V-SE_V,
                                   ymax=mu_V+SE_V),width=.2)+
  facet_wrap(~Life_stage, scales = "free_x")+
  scale_color_manual(values = c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  labs(x="Summer After Beetle Addition",
       y="Weeks To Reach Growth Stage",
       color="Legend")

subeforeb<-df_subbl33 %>% 
  ggplot(aes(x=Vegetative_stage, y=mu_V, color = B_T))+
  geom_line()+
  geom_point()+
  geom_point(data = df_subbp,aes(y=value))+
  geom_errorbar(aes(ymin=mu_V-SE_V,
                                     ymax=mu_V+SE_V),width=.2)+
  facet_wrap(~Life_stage, scales = "free_x")+
  scale_color_manual(values = c("#BFA89E","lightcyan1","#8B786D", "lightblue3"))+
  theme_bw()+
  labs(x="Summer Before Beetle Addition",
       y="Weeks To Reach Growth Stage",
       color="Legend")


pushViewport(viewport(layout=grid.layout(2,1)))
print(subeforeb,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(suafterb,vp=viewport(layout.pos.row=2,layout.pos.col = 1))
