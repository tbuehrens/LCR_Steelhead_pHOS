#things to clean up
  # are population things correct
  #pivot by year and pop and get a total with all or by live vs carc
  #look at JAGS code


#clean r work data
rm(list=ls(all=TRUE))
#-------------------------------------------------------------------------------
# load packages
#-------------------------------------------------------------------------------
library("dataRetrieval")
library("scales")
library("RColorBrewer")
library('viridis')
library('ggsci')
library('tidyr')
library('dplyr')
library('readr')
library('ggplot2')
library('lubridate')
#-------------------------------------------------------------------------------
# Call in data and reorganize
#-------------------------------------------------------------------------------

#Step 1 - Call in DF from csv
DF<-read.csv("STH_PHOS_TWS_12.09.22.csv")%>%
     as_tibble()%>%
     dplyr::filter(Mark!="UNK")%>%
     dplyr::filter(Mark!="")%>%
     dplyr::filter(!is.na(Mark))%>%
     dplyr::filter(Population!="Cedar",
                   Population!="Delameter_Above Weir",
                   Population!="Salmon Creek",
                   Population!="White Salmon",
                   Population!="Olequa_Above Weir",
                   Population!="Ostrander_Above Weir",
                   Population!="Mainstem Columbia",
                   Population!="Lower Cowlitz",
                   Population!="")%>%
     dplyr::mutate(Date=mdy(Date),
                   Year=year(Date),
                   Date1= as.Date(format(Date,'2022-%m-%d')),
                   Spawn_Year= ifelse(Date1>='2022-01-01'&Date1<='2022-06-30',Year,
                               ifelse(Date1>='2022-12-01'&Date1<='2022-12-31',Year+1,
                               "9999")))%>%
     dplyr::filter(Spawn_Year!="9999")%>% #exclude data from 6/30 -12/1 each year
     dplyr::select(-Date1,-Year,-Stream_Reach)%>%
     dplyr::filter(Spawn_Year!="2023")%>% #not a complete year yet
     # dplyr::filter(Spawn_Year=="2017"|
     #               Spawn_Year=="2018"|
     #               Spawn_Year=="2019"|
     #               Spawn_Year=="2020"|
     #               Spawn_Year=="2021"|
     #               Spawn_Year=="2022")%>% 
     dplyr::group_by(Spawn_Year,Population,Mark)%>%
     dplyr::summarise(Count = sum(Count))
print(DF)

DF1<-DF%>%
    group_by(Spawn_Year,Population)%>%
    pivot_wider(names_from = Mark, values_from = Count)%>%
    replace(is.na(.), 0) %>%
    mutate(HOR = rowSums(across(c(AD, LV))))%>%
    mutate(Total = rowSums(across(c(HOR,UM))))%>%
    dplyr::mutate(pHOS = HOR/Total)%>%
    dplyr::select(-UM,-AD,-LV)
print(DF1)

write.csv(DF1,"LCR_STH_PHOS_DF.csv")
#------------------------------------------------------------------------------
p1<-(ggplot(DF1,aes(x=Spawn_Year,y=pHOS))+
          geom_line(size=0.6)+
          geom_point(size=2)+
          facet_wrap(~Population,ncol=2)+
          ylab("Proportion of Hatchery-Origin Spawners")+
          xlab("Spawn Year")+
          theme_bw()+
          theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))

#save for plots as .PNG 
ggsave(p1, file="LCR_STH_pHOS_1.png", width=11,height=8.5,
       units="in", dpi=300)
