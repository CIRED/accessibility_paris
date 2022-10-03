### CHARTS

install.packages('devtools')
devtools::install_github('bbc/bbplot')

library(tidyverse)
library(readxl)
library(readr)
library(shadowtext)
library(ggrepel)
library('bbplot')
library(waterfalls)
library(directlabels)
library(forcats)

# PATH

path_data = "C:/Users/charl/OneDrive/Bureau/GPE2/Data/"
path_outputs = "C:/Users/charl/OneDrive/Bureau/GPE2/Outputs/"

SAVE_ici = TRUE

result<-read.csv(paste(path_outputs, "match_40_multiplicative.csv", sep = "")) %>% 
  mutate('match'="match_40")

for (namei in c("match_20","match_60","no_match_40", "exponential_decay")){
  tmp<-read.csv(paste0(path_outputs,namei,'_multiplicative.csv'),sep = ",") %>% 
    mutate('match'=namei)
  result<-rbind(result, tmp)
}

result<-result %>% 
  pivot_longer(2:5,names_to="group")%>% 
  mutate(group=case_when(
    group=='group2' ~ "Group 1\n(high income)",
    group=='group3' ~ "Group 2\n(middle income)",
    group=='group4' ~ "Group 3\n(low income)",
    group=='group5' ~ "Group 4\n(low income)"
  ))

result2<-result%>% 
  mutate(year=str_wrap(year,width=6))

result <- result %>% 
  select(-X) %>% 
  pivot_wider(names_from = name,values_from = value) %>% 
  mutate(`Real accessibility gains (%)`= 100*(1+`Job growth (%)`/100)*(1+`Speed (%)`/100)*(1+`Proximity (%)`/100)-100) %>% 
  rename('Proximity - households (%)'='Proximity - hh (%)') %>% 
  pivot_longer(-c(year,match,group)) 

### FIGURE A1 - weighted accessibility, absolute value

weighted_accessibility<-read.csv(paste(path_outputs, "match_40_weighted_accessibility.csv", sep = "")) %>% 
  rename('year'='X') %>% 
  pivot_longer(cols=-1,names_to = 'group') %>% 
  mutate(group=case_when(
    group=='group2' ~ "Group 1\n(high income)",
    group=='group3' ~ "Group 2\n(middle income)",
    group=='group4' ~ "Group 3\n(low income)",
    group=='group5' ~ "Group 4\n(low income)"
  ))

p<-ggplot(weighted_accessibility,aes(x=year,y=value,group=group))+
  geom_line(aes(color=group),show.legend = TRUE,size=1)+
  bbc_style()+
  #facet_wrap('name',scales='free')+
  theme(legend.position = "top", 
        #legend.justification = "left",
        axis.ticks.x = element_line(colour = "#333333"), 
        axis.ticks.length =  unit(0.26, "cm")) +
  theme(
    #legend.title = element_blank(),
    # panel.border = element_blank(),
    # panel.background = element_blank(),
    # strip.background = element_blank(),
  )+
  geom_hline(yintercept = 0000000, size = 1, colour="#333333") +
  #geom_dl(aes(label = group,color=group), method = list("last.qp",cex=1.2,hjust = +2))+
  #xlim(1968,2017)+
  labs(title = "Weighted accessibility to jobs",subtitle = "Historical evolution in Île de France region")
#direct.label(p,method=list("angled.boxes",cex=1))

if (SAVE_ici) {
  ggsave(paste(path_outputs, "Figure_A1.png", sep = ""),plot=p,
         width = 2666/300,
         height= 1875/300,
         unit="in")
}

### FIGURE 5 - weighted accessibility, relative value

tmp<-weighted_accessibility %>% 
  filter(year==1968)%>% 
  select(-year) %>% 
  rename(valeur_initiale=value)

p<-weighted_accessibility %>% 
  left_join(.,tmp) %>% 
  mutate(value=value/valeur_initiale) %>% 
  select(-valeur_initiale) %>% 
  ggplot(aes(x=year,y=value,group=group))+
  geom_line(aes(color=group),show.legend = TRUE,size=1)+
  bbc_style()+
  #facet_wrap('name',scales='free')+
  theme(legend.position = "top", 
        legend.justification = "left",
        axis.ticks.x = element_line(colour = "#333333"), 
        axis.ticks.length =  unit(0.26, "cm")) +
  theme(
    legend.title = element_blank(),
    # panel.border = element_blank(),
    # panel.background = element_blank(),
    # strip.background = element_blank(),
  )+
  geom_hline(yintercept = 0000000, size = 1, colour="#333333") +
  labs(title = "Weighted accessibility to jobs (index)",subtitle = "Evolution relative to 1968")
#direct.label(p,method=list("angled.boxes",cex=1))

if (SAVE_ici) {
  ggsave(paste(path_outputs, "Figure_5a.png", sep = ""),plot=p,
         width = 2666/300,
         height= 1875/300,
         unit="in")
}

# FIGURE 7: DECOMPOSITION BY DRIVER

for (nami in c('Job growth (%)','Real accessibility gains (%)','Speed (%)','Proximity (%)',"Proximity - households (%)","Proximity - jobs (%)")){
  
  nami2=case_when(
    nami=='Speed (%)' ~ 'Public Transport improvements',
    nami=='Proximity (%)' ~ 'Changes in spatial proximity to jobs',
    nami=='Job growth (%)' ~ 'Changes in total number of matching jobs',
    nami=="Proximity - households (%)" ~ 'Changes in spatial proximity to jobs: impact\nof the locations of households',
    nami=="Proximity - jobs (%)" ~ 'Changes in spatial proximity to jobs: impact\nof the locations of jobs',
    TRUE ~ nami
  )
  nami2
  nami
  p<-result %>% 
    filter(match=='match_40',
           name==nami
    )  %>% 
    ggplot(aes(x=year,y=value))+
    geom_bar(stat="identity", position=position_dodge(), aes(fill=group),show.legend = TRUE,width=0.6)+
    bbc_style()+
    #facet_wrap('name',scales='free')+
    theme(legend.position = "top", 
          legend.justification = "left",
          #axis.ticks.x = element_line(colour = "#333333"), 
          #axis.ticks.length =  unit(0.26, "cm")
    ) +
    theme(
      legend.title = element_blank(),
      # panel.border = element_blank(),
      # panel.background = element_blank(),
      # strip.background = element_blank(),
    )+
    geom_hline(yintercept = 0000000, size = 1, colour="#333333") +
    labs(title = nami2,subtitle = 'Impact on accessibility (variation in %)')
  
  if (SAVE_ici) {
    ggsave(paste(path_outputs, "Figure_7_",substr(nami,1,nchar(nami)-3),".png", sep = ""),plot=p,
           width = 2666/300,
           height= 1875/300,
           unit="in")
  }
  
  plot(p)
}

# FIGURE 4

nami='Speed (%)'
p<-result %>% 
  filter(match=='match_40',
         name==nami
  ) %>% 
  ggplot(aes(x=year,y=value))+
  geom_bar(stat="identity", position=position_dodge(), aes(fill=group),show.legend = TRUE,size=1)+
  bbc_style()+
  #facet_wrap('name',scales='free')+
  theme(legend.position = "top", 
        legend.justification = "left",
        axis.ticks.x = element_line(colour = "#333333"), 
        axis.ticks.length =  unit(0.26, "cm")) +
  theme(
    legend.title = element_blank(),
    # panel.border = element_blank(),
    # panel.background = element_blank(),
    # strip.background = element_blank(),
  )+
  geom_hline(yintercept = 0000000, size = 1, colour="#333333") +
  labs(subtitle = "Accessibility gains per labor group (%)",title="Impact of public transport improvements")

if (SAVE_ici) {
  ggsave(paste(path_outputs, "Figure_4.png", sep = ""),plot=p,
         width = 2666/300,
         height= 1875/300,
         unit="in")
}

plot(p)

### FIGURE 5 - accessibility variations

nami='Real accessibility gains (%)'
p<-result %>% 
  filter(match=='match_40',
         name==nami
  ) %>% 
  ggplot(aes(x=year,y=value))+
  geom_bar(stat="identity", position=position_dodge(), aes(fill=group),show.legend = TRUE,width=0.6)+
  bbc_style()+
  #facet_wrap('name',scales='free')+
  theme(legend.position = "top", 
        legend.justification = "left",
        axis.ticks.x = element_line(colour = "#333333"), 
        axis.ticks.length =  unit(0.26, "cm")) +
  theme(
    legend.title = element_blank(),
    # panel.border = element_blank(),
    # panel.background = element_blank(),
    # strip.background = element_blank(),
  )+
  geom_hline(yintercept = 0000000, size = 1, colour="#333333") +
  labs(subtitle = "Difference in accessibility in each time period (%)",title="Accessibility variations")

if (SAVE_ici) {
  ggsave(paste(path_outputs, "Figure_5b.png", sep = ""),plot=p,
         width = 2666/300,
         height= 1875/300,
         unit="in")
}

plot(p)

# FIGURE 6

for (groupi in unique(result$group)){
  
  nami<-c('Real accessibility gains (%)',"Proximity (%)")#"Proximity - households (%)","Proximity - jobs (%)",
  #groupi<-"group3"
  
  result4 <- result %>% 
    filter(match=='match_40',
           #transport=='TC',
           !(name %in% nami),
           group==groupi
    ) %>% 
    mutate(name2=name,
           name=case_when(
             name=='Speed (%)' ~ 'Public Transport\nimprovements',
             name=="Proximity - jobs (%)" ~ 'Locations\nof jobs',
             name=="Proximity - households (%)" ~ 'Locations of\nhouseholds',
             name=='Job growth (%)' ~ 'Changes in\ntotal number\nof matching jobs',)) %>% 
    #mutate(name=fct_reorder(name,name2)) %>% 
    #mutate(name=factor(name,levels=name2)) %>% 
    #select(-name2) %>% 
    #mutate(name=as.character( name )) %>% 
    group_by(name,name2,group) %>% 
    summarise(value=100*(prod(1+value/100)-1)) %>% 
    arrange(name2)%>% 
    mutate(value2=100*log(1+value/100)) 
  
  kk<-function(y) 100*log(1+y/100)
  kj<-function(y) 100*(exp(y/100)-1)
  
  # tn <- trans_new(name="log_custom",
  #                 transform=function(y) log(1+y/100),
  #                 inverse=function(y) 100*(exp(y)-1),
  #                 domain=c(-100, Inf))
  
  p<-waterfall(labels = result4$name,values = result4$value2,
               rect_text_labels=sprintf("%+d %%",round(result4$value)),
               rect_text_size=3,
               put_rect_text_outside_when_value_below=15,
               calc_total=TRUE,
               #total_axis_text=nami,
               total_rect_text=ifelse(abs(round(sum(result4$value2)))<10,'',sprintf("%+d %%",round(100*(prod(1+result4$value/100)-1)))))+
    bbc_style()+
    #facet_wrap('group')+
    theme(legend.position = "top", 
          legend.justification = "left",
          #axis.ticks.x = element_line(colour = "#333333"), 
          #axis.ticks.length =  unit(0.26, "cm"),
          axis.text.x = element_text(angle=0,size=18),
          #axis.text.y = element_blank(),
          #panel.grid.major.y = element_blank(),
    ) +
    theme(
      legend.title = element_blank(),
      # panel.border = element_blank(),
      # panel.background = element_blank(),
      # strip.background = element_blank(),
    )+
    # geom_hline(yintercept = 0000000, size = 1, colour="#333333") +
    # scale_y_continuous(trans=tn,breaks =seq(-100,100,10) )+
    scale_y_continuous(breaks =kk(c(seq(-75,0,25),seq(50,200,50))),labels = kj,limits=c(-85,105))+
    #ylim(-100,105)+
    labs(subtitle = 'Accessibility variations 1968-2010 (%)',title=gsub("[\r\n]", "  ", groupi))
  
  
  if (SAVE_ici) {
    ggsave(paste0(path_outputs, "Figure_6_",substr(groupi, 1, 7),".png"),plot=p,
           width = 2666/300,
           height= 1875/300,
           unit="in")
  }
  plot(p)
}


# FIGURE A8

nami='Speed (%)'
p<-result %>% 
  filter(match=='match_40',
         name==nami
  ) %>% 
  group_by(group) %>% 
  summarise(value=(prod(1+value/100))*100-100) %>% 
  ggplot(aes(x=group,y=value))+
  geom_bar(stat="identity", position=position_dodge(), aes(fill=group),show.legend = TRUE,width=0.6)+
  bbc_style()+
  #facet_wrap('name',scales='free')+
  theme(legend.position = "top", 
        legend.justification = "left",
        axis.ticks.x = element_line(colour = "#333333"), 
        axis.ticks.length =  unit(0.26, "cm")) +
  theme(
    legend.title = element_blank(),
    # panel.border = element_blank(),
    # panel.background = element_blank(),
    # strip.background = element_blank(),
  )+
  geom_hline(yintercept = 0000000, size = 1, colour="#333333") +
  labs(subtitle = "Accessibility gains per labor group, 1968-2010 (%)",title="Ex ante impact of public transport improvements")

if (SAVE_ici) {
  ggsave(paste0(path_outputs, "Figure_A8a.png"),plot=p,
         width = 2666/300,
         height= 1875/300,
         unit="in")
}

plot(p)

nami='Real accessibility gains (%)'
p<-result %>% 
  filter(match=='match_40',
         name==nami
  ) %>% 
  group_by(group) %>% 
  summarise(value=(prod(1+value/100))*100-100) %>% 
  ggplot(aes(x=group,y=value))+
  geom_bar(stat="identity", position=position_dodge(), aes(fill=group),show.legend = TRUE,width=0.6)+
  bbc_style()+
  #facet_wrap('name',scales='free')+
  theme(legend.position = "top", 
        legend.justification = "left",
        axis.ticks.x = element_line(colour = "#333333"), 
        axis.ticks.length =  unit(0.26, "cm")) +
  theme(
    legend.title = element_blank(),
    # panel.border = element_blank(),
    # panel.background = element_blank(),
    # strip.background = element_blank(),
  )+
  geom_hline(yintercept = 0000000, size = 1, colour="#333333") +
  labs(subtitle = "Accessibility gains per labor group, 1968-2010 (%)",title="Actual (total) accessibility variations")

if (SAVE_ici) {
  ggsave(paste0(path_outputs, "Figure_A8b.png"),plot=p,
         width = 2666/300,
         height= 1875/300,
         unit="in")
}

plot(p)

for (nami in c('Job growth (%)','Real accessibility gains (%)','Speed (%)','Proximity (%)',"Proximity - households (%)","Proximity - jobs (%)")){
  
  nami2=case_when(
    nami=='Speed (%)' ~ 'Public Transport improvements',
    nami=='Proximity (%)' ~ 'Changes in spatial proximity to jobs',
    nami=='Job growth (%)' ~ 'Changes in total number of matching jobs',
    nami=="Proximity - households (%)" ~ 'Changes in spatial proximity to jobs: impact\nof the locations of households',
    nami=="Proximity - jobs (%)" ~ 'Changes in spatial proximity to jobs: impact\nof the locations of jobs',
    TRUE ~ nami
  )
  
  p<-result %>% 
    filter(match=='match_40',
           name==nami
    ) %>% 
    group_by(group) %>% 
    summarise(value=(prod(1+value/100))*100-100) %>% 
    ggplot(aes(x=group,y=value))+
    geom_bar(stat="identity", position=position_dodge(), aes(fill=group),show.legend = TRUE,width=0.6)+
    bbc_style()+
    #facet_wrap('name',scales='free')+
    theme(legend.position = "top", 
          legend.justification = "left",
          axis.ticks.x = element_line(colour = "#333333"), 
          axis.ticks.length =  unit(0.26, "cm")) +
    theme(
      legend.title = element_blank(),
      # panel.border = element_blank(),
      # panel.background = element_blank(),
      # strip.background = element_blank(),
    )+
    geom_hline(yintercept = 0000000, size = 1, colour="#333333") +
    labs(subtitle = "Accessibility gains per labor group, 1968-2010 (%)",title=nami2)
  
  if (SAVE_ici) {
    ggsave(paste0(path_outputs, "Figure_A8c",substr(nami,1,nchar(nami)-3),".png"),plot=p,
           width = 2666/300,
           height= 1875/300,
           unit="in")
  }
  
  plot(p)
}
