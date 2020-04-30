## Career Projections Data
## by State https://www.bls.gov/oes/tables.htm
df<- read.csv("state_M2019_dl.csv")

#str(df)
#summary(df)

##Slide 1
##Cleaning
df<- df%>%
      select(area,area_title,occ_title,tot_emp, h_mean, a_mean)
df$tot_emp <- as.numeric(gsub(",","",df$tot_emp))
df$h_mean <- as.numeric(gsub(",","",df$h_mean))
df$a_mean <- as.numeric(gsub(",","",df$a_mean))
## Subset
va<- df%>%filter(area=="51")

##Slide 2
va%>%filter(occ_title=="All Occupations")%>%select(tot_emp)
## Lets look at the occupatiosn with the most employed
va%>%
  filter(tot_emp<1000000)%>%
  top_n(5,wt=tot_emp)%>%
  ggplot()+
  geom_col(aes(occ_title,tot_emp))+
  coord_flip(ylim=c(250000,500000))+
  labs(title="Total VA Employment",
       subtitle="Top 5 Occupations",
       y= "Total Employment",
       x=NULL)+scale_y_continuous(label=comma)

head(va)
#Slide 3

##Next lets filter to the relevant Data Professional job titles we are looking for
va%>%filter(grepl("data|analyst",occ_title, ignore.case = TRUE))%>%select(occ_title)%>%filter(occ_title!="Data Entry Keyers")

df%>%filter(grepl("data|analyst",occ_title, ignore.case = TRUE))%>%
  select(occ_title)%>%
  filter(occ_title!="Data Entry Keyers")%>%unique()%>%knitr::kable()

#Slide 4
va%>%
  filter(grepl("data|analyst",occ_title, ignore.case = TRUE))%>%
  filter(occ_title!="Data Entry Keyers")%>%
  ggplot(aes(x=reorder(str_wrap(occ_title),-tot_emp),tot_emp,label=tot_emp, fill=occ_title))+
  geom_col()+
  geom_text(nudge_y=5000)+
  guides(fill=FALSE, color=FALSE)+
  coord_flip(ylim=c())+
  labs(title="Virginian Data Professionals",
       y= "Total Employment",
       x=NULL)+scale_y_continuous(label=comma)

va%>%
  filter(grepl("data|analyst",occ_title, ignore.case = TRUE))%>%
  filter(occ_title!="Data Entry Keyers")%>%summarise(sum=sum(tot_emp))

#Slide 4 salary Hourly
#Smaller Boxplot
va%>%
  filter(grepl("data|analyst",occ_title, ignore.case = TRUE))%>%
  filter(occ_title!="Data Entry Keyers")%>%
  ggplot(aes(x=area_title, y=h_mean))+
  geom_boxplot(varwidth = TRUE)+
  geom_jitter(width=0.05)+
  scale_fill_manual(values=c("#69b3a2", "grey")) +
  scale_alpha_manual(values=c(1,0.1))+
  theme(legend.position = "none") +
  xlab("")+
  guides(fill=FALSE, color=FALSE)+
  labs(title="Virginian Data Professionals",
       y= "Hourly Pay($)",
       x=NULL)+scale_y_continuous(label=comma)+
  theme_ipsum()


library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
##Larger Ridge plot
df%>%
  filter(grepl("data|analyst",occ_title, ignore.case = TRUE))%>%
  filter(occ_title!="Data Entry Keyers")%>%
  mutate(type=ifelse(area=="51","Highlighted","Normal")) %>%
  ggplot(aes(x =h_mean, y=area_title, fill = type)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = 'Hourly Wages of Data Professionals($): Broken Down by State', 
       y= "State",
       x=NULL) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.line=NULL,
    panel.grid.major=element_blank()
    
  )



#Slide 5 salary Annual

va%>%
  filter(grepl("data|analyst",occ_title, ignore.case = TRUE))%>%
  filter(occ_title!="Data Entry Keyers")%>%
  ggplot(aes(x=reorder(str_wrap(occ_title),-a_mean),a_mean,label=a_mean, fill=occ_title))+
  geom_col()+
  geom_text(nudge_y=5000)+
  guides(fill=FALSE, color=FALSE)+
  coord_flip(ylim=c())+
  labs(title="Virginian Data Professionals",
       subtitle="Annual Salary",
       y= "$",
       x=NULL)+scale_y_continuous(label=comma)

va%>%
  filter(grepl("data|analyst",occ_title, ignore.case = TRUE))%>%
  filter(occ_title!="Data Entry Keyers")%>%summarise(average=weighted.mean(x=a_mean, w=tot_emp, na.rm=TRUE))

