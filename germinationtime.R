library(pacman)
pacman::p_load(tidyverse,
               patchwork,
               here,
               ggplot2,
               grid)

df_spmgt <- read_csv(here("Spring25germination.csv"))
df_sumgt <- read_csv(here("Summer25germ.csv"))
df_famgt <- read_csv(here("fall25germ.csv"))

# t.test  -----------------------------------------------------------------

x1 <- df_spmgt %>%
  filter(Treatment == "Control") %>%
  pull(DTG)

x2 <- df_sumgt %>%
  filter(Treatment == "Control") %>%
  pull(DTG)

x3 <- df_famgt %>%
  filter(Treatment == "Control") %>%
  pull(DTG)

y1 <- df_spmgt %>%
  filter(Treatment =="Microplastic") %>%
  pull(DTG)

y2 <- df_sumgt %>%
  filter(Treatment == "Microplastic") %>%
  pull(DTG)

y3 <- df_famgt %>%
  filter(Treatment == "Microplastic") %>%
  pull(DTG)

t.test(x1, y1, var.equal = FALSE)

t.test(x2, y2, var.equal = FALSE)

t.test(x3, y3, var.equal = FALSE)

# Column Chart ------------------------------------------------------------

df_spmu <- df_spmgt %>% 
  group_by(Treatment) %>%
  summarize(mu_l = mean(DTG),
            sd_l = sd(DTG),
            N_l = length(DTG))

df_sumu <- df_sumgt %>% 
  group_by(Treatment) %>%
  summarize(mu_l = mean(DTG),
            sd_l = sd(DTG),
            N_l = length(DTG))

df_famu <- df_famgt %>% 
  group_by(Treatment) %>%
  summarize(mu_l = mean(DTG),
            sd_l = sd(DTG),
            N_l = length(DTG))

vp1<-df_spmu %>%
  ggplot(aes(x=Treatment,
             y=mu_l, fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_l-(sd_l/sqrt(N_l)), 
                    ymax=mu_l+(sd_l/sqrt(N_l)), 
                    width=0.2))+
  annotate('text', x = 1, y = 21, label = '(a)', size = 8)+
  annotate('text', x = 2, y = 21, label = '(b)', size = 8)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Spring 2025",
       y = "MGT in Days")

vp2<-df_sumu %>%
  ggplot(aes(x=Treatment,
             y=mu_l, fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_l-(sd_l/sqrt(N_l)), 
                    ymax=mu_l+(sd_l/sqrt(N_l)), 
                    width=0.2))+
  annotate('text', x = 1, y = 4, label = '(a)', size = 8)+
  annotate('text', x = 2, y = 4, label = '(a)', size = 8)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Summer 2025",
       y = "")

vp3<-df_famu %>%
  ggplot(aes(x=Treatment,
             y=mu_l, fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_l-(sd_l/sqrt(N_l)), 
                    ymax=mu_l+(sd_l/sqrt(N_l)), 
                    width=0.2))+
  annotate('text', x = 1, y = 5, label = '(a)', size = 8)+
  annotate('text', x = 2, y = 5, label = '(b)', size = 8)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Fall 2025",
       y = "")

pushViewport(viewport(layout=grid.layout(1,3)))
print(vp1,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(vp2,vp=viewport(layout.pos.row=1,layout.pos.col = 2))
print(vp3,vp=viewport(layout.pos.row=1,layout.pos.col = 3))
  