


pacman::p_load(DBI,tidyverse)
#function
select_table_PostgreSQL<-function(database_args,SQL){
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = database_args$Driver,
    Server = database_args$Server,
    Database = database_args$Database,
    Port = database_args$Port,
    UID = database_args$UID,
    PWD = database_args$PWD,
    Trusted_Connection = database_args$Trusted_Connection
  )
  dat<- data.frame(DBI::dbGetQuery(con, SQL))
  DBI::dbDisconnect(con)
  return(dat)
}


database_args<-list(
  Driver = "PostgreSQL Unicode",
  Server = Sys.getenv("POSTGRES_TWS_IP"),
  Database = "FISH",
  Port = 5433, #main server; 5432 is exact mirror..but is read only
  UID = Sys.getenv("POSTGRES_TWS_UN"),
  PWD = Sys.getenv("POSTGRES_TWS_PW"),
  Trusted_Connection = "True"
)

SQL3<-
  "SELECT *
 FROM
  tws.age_lut;
"

age_lut<-select_table_PostgreSQL(database_args=database_args,SQL=SQL3)%>%
  as_tibble()%>%
  rename(scale_age_code=age_lut_id)

dat<-read_csv(here::here("data/Wind_Steelhead_Scales.csv"))%>%
  filter(run==2)%>%
  mutate(age_code=paste0(fresh_age,".",salt_age))%>%
  right_join(age_lut,by=join_by(age_code))%>%
  filter(!is.na(sy_total_age) & !is.na(spawn_year))%>%
  group_by(spawn_year,sy_total_age)%>%
  summarize(n=n())%>%
  pivot_wider(names_from = sy_total_age,values_from = n,values_fill = 0)%>%
  select(spawn_year, order(as.numeric(names(.)[-1])) + 1)%>%
  write_csv(here::here("results/Wind_Summer_Steelhead_Age_Comp.csv"))
