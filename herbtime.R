pacman::p_load(tidyverse,
               patchwork,
               here,
               grid)

#create data frames

df_suherbc<-read_csv(here("Herbivory25v1.csv"))
df_faherb<-read_csv(here("fall25herb.csv"))


# tidy data frames --------------------------------------------------------

#Summer

df_suherbcl<-df_suherbc%>%
  pivot_longer(cols=Week_1:Week_9, #converts the named col to row
               names_to="Week", #name the new col to put the new row data into
               values_to="value")%>% #specifying data in new col should be a value
  separate(col=Week,
           into=c("remove","Week"),
           sep="_")%>% 
  select(-remove) #removes the Week_ in front of all data in the Week col

df_suherbcl$Treatment[df_suherbcl$Treatment == "C"] = "Control"
df_suherbcl$Treatment[df_suherbcl$Treatment == "MP"] = "Microplastic"

df_suherbclse<-df_suherbcl%>%
  mutate(Week=as.numeric(Week))%>% #column was character, changed to numeric
  group_by(Treatment,Week)%>%
  summarize(mu=mean(value,na.rm=T),
    SE=sd(value,na.rm=T)/sqrt(length(value)),
    N=length(value),
    sd=sd(value))%>%
  ungroup()

df_suherbcl<-df_suherbcl%>%
  mutate(Week=as.numeric(Week))

su3<-subset(df_suherbclse,Week==3)

su3$Treatment[su3$Treatment == "C"] = "Control"
su3$Treatment[su3$Treatment == "MP"] = "Microplastic"

#fall

df_faherb<-df_faherb%>%
  pivot_longer(cols=Week_1:Week_7,
               names_to="Week",
               values_to="value")%>%
  separate(col=Week,
           into=c("remove","Week"),
           sep="_")%>% 
  select(-remove)

df_faherb<-df_faherb%>%
  mutate(Week=as.numeric(Week))

#length function not ignoring na

df_fase<-df_faherb%>%
  mutate(Week=as.numeric(Week))%>%
  group_by(Week,Treatment)%>%
  summarize(mu=mean(value,na.rm=T),
            SE=sd(value,na.rm=T)/sqrt(length(value)),
            N=length(value),
            sd=sd(value,na.rm=T))%>%
  ungroup()

fa3<-subset(df_fase,Week==3)

# plots -------------------------------------------------------------------

su<-su3%>%
  ggplot(aes(x=Treatment,
             y=mu,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu-(sd/sqrt(N)), 
                    ymax=mu+(sd/sqrt(N))),
                width=0.2)+
  annotate('text', x = 1, y = 30, label = 'a', size = 8)+
  annotate('text', x = 2, y = 30, label = 'b', size = 8)+
  annotate('text', x = .5, y = 31, label = '(A)', size = 10)+
  scale_fill_manual(values=c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(axis.title=element_text(size=25),
        axis.text=element_text(size=20),
        legend.position = "none")+
  labs(x="Trophic Interaction Week 3",
       y="Average Percent Herbivory")

fa<-fa3%>%
  ggplot(aes(x=Treatment,
             y=mu,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu-(sd/sqrt(N)), 
                    ymax=mu+(sd/sqrt(N))),
                width=0.2)+
  annotate('text', x = 1, y = 32, label = 'a', size = 8)+
  annotate('text', x = 2, y = 32, label = 'b', size = 8)+
  annotate('text', x = .5, y = 33, label = '(B)', size = 10)+
  scale_fill_manual(values=c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(axis.title=element_text(size=25),
        axis.text=element_text(size=20),
        legend.position = "none")+
  labs(x="Plant Response Week 3",
       y="")

pushViewport(viewport(layout=grid.layout(1,2)))
print(su,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(fa,vp=viewport(layout.pos.row=1,layout.pos.col = 2))

# stats -------------------------------------------------------------------

#summer
haov<-aov(value~Treatment,
          data=subset(df_suherbcl,Week==3))

anova(haov)

emmeans(haov,~Treatment)

#fall
haov<-aov(value~Treatment,
          data=subset(df_faherb,Week==3))

anova(haov)

emmeans(haov,~Treatment)

