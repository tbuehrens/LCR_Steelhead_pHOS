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
library('tidyverse')
library('dplyr')
library('readr')
library('ggplot2')
library('lubridate')
library("mgcv")
library("modelr")
library("brms")
#-------------------------------------------------------------------------------
# Call in data and reorganize
#-------------------------------------------------------------------------------
#1 all spawning ground survey data (spring and fall & summer)
#2 includes lives & deads
#2 spawner vs. holder (left in holders)
#3 below filtered out cowlitz (already estimates, filtered out non spawning months)
#4 Maybe filter pre-2014 ish when we started making live fish calls???? (no, some carc data)


add_predictions <- function (data, model, var = "pred", ...) {
     fit <- as_tibble(stats::predict(model, data, ...))
     names(fit)[1] <- var  
     bind_cols(data, fit)
}

ilogit<-function(x){exp(x)/(1+exp(x))}

#Step 1 - Call in DF from csv
DF<-read_csv("STH_PHOS_TWS_12.09.22.csv")%>%
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
     mutate(Population = str_replace(Population, "Kalama","Kalama_below_KFH"),
            Population = str_replace(Population, "Green","NF_Toutle_Green_only")
     )%>%
     dplyr::mutate(Date=mdy(Date),
                   Year=year(Date),
                   month= month(Date),
                   Spawn_Year= ifelse(month < 12, Year, Year + 1)
     )%>%
     dplyr::filter(month >11 | month <7)%>% #exclude data from 6/30 -12/1 each year
     dplyr::filter(Spawn_Year!="2023")%>% #not a complete year yet
     mutate(Mark = ifelse(Mark %in% c("AD","LV"),"HOS","NOS"))%>%
     dplyr::group_by(Spawn_Year,Population,Mark)%>%
     dplyr::summarise(Count = sum(Count))%>%
     pivot_wider(names_from = Mark, values_from = Count,values_fill = 0)%>%
     mutate(pHOS_mom = HOS/(HOS+NOS),Population=factor(Population))
     #filter(Population=="NF_Toutle_Green_only")
     
print(DF)

m1<-gam(cbind(HOS,NOS) ~ s(Spawn_Year,by=Population), family = "binomial",data=DF)

#m1<-brm(cbind(HOS,NOS)~ + as.factor(Population) + s(Spawn_Year,by=Population,bs="TS",m=2,k=10), family = "binomial",data=DF)


DF<-expand.grid("Population" = unique(DF$Population),
                    "Spawn_Year" = unique(DF$Spawn_Year)
                    )%>%
     as_tibble()%>%
     add_predictions(m1,type="link", var ="pHOS_gam",se.fit=T)%>%
     mutate(pHOS_gam_lower =ilogit(pHOS_gam - 1.96 * se.fit),
            pHOS_gam_upper =ilogit(pHOS_gam + 1.96 * se.fit),
            pHOS_gam=ilogit(pHOS_gam)
            )%>%
     left_join(DF,by=c("Spawn_Year","Population"))%>%
     ungroup()%>%
     arrange(Population, Spawn_Year)

p1<-ggplot(DF,aes(x=Spawn_Year,y=pHOS_gam,group=Population))+
     geom_ribbon(aes(ymin=pHOS_gam_lower,ymax=pHOS_gam_upper),color=NA,fill="black",alpha=0.5)+
     geom_line(size=0.6)+
     geom_point(aes(y=pHOS_mom,size=HOS+NOS))+
     scale_size(name   = "Sample Size",
                breaks = c(2,4,8,16,32,64),
                labels = c(2,4,8,16,32,64)
                )+
     facet_wrap(~Population,ncol=2)+
     ylab("Proportion of Hatchery-Origin Spawners")+
     xlab("Spawn Year")+
     theme_bw()+
     theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

print(p1)
print(DF)
#save for plots as .PNG 
ggsave(p1, file="LCR_STH_pHOS_1.png", width=11,height=8.5,
       units="in", dpi=300)


write.csv(DF,"LCR_STH_PHOS_DF.csv")

DF%>%
     filter(Spawn_Year>2017)%>%
     group_by(Population)%>%
     summarise(mean_pHOS = scales::percent(mean(pHOS_gam),accuracy = 0.1),
               min= scales::percent(min(pHOS_gam),accuracy = 0.1),
               max=scales::percent(max(pHOS_gam),accuracy = 0.1)
               )

