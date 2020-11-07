
###0.Environment
rm(list=ls());gc()
setwd("D:\\YingjieLi")

library(readr)
library(dplyr)

pop_flow<-read_csv("D:\\YingjieLi\\pop_flow.csv")
#Province cross, 930 rows
pop_flow%>%rename(No=X1)%>%
  left_join(pop_flow,by=c("from"="to","to"="from"))%>%
  select(-X1)%>%
  rename(Outflow=num.x,Inflow=num.y)->Pop_flow_Out_in

#By province, 31 rows
pop_flow%>%rename(No=X1)->pop_flow
pop_flow<-data.frame(pop_flow)
pop_flow<-pop_flow[!is.na(pop_flow$num),]

attach(pop_flow)
Outflow_Net<-tapply(num,from,sum)
#Outflow_Net%>%as_tibble()%>%tibble::rownames_to_column()
Outflow_Net<-data.frame(Outflow_Net)
Outflow_Net$Province<-row.names(Outflow_Net)
Inflow_Net<-tapply(num,to,sum)
Inflow_Net<-data.frame(Inflow_Net)
Inflow_Net$Province<-row.names(Inflow_Net)
Outflow_inflow_net<-merge(Outflow_Net,Inflow_Net,by="Province")

###Save theresults
library(openxlsx)
library(writexl)
write_xlsx(list(ProvinceCross=Pop_flow_Out_in,
                ProvinceBy=Outflow_inflow_net),
           path="D:\\YingjieLi\\Pop_Flow.xlsx")

rm(list=ls());gc()
