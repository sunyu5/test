# first project 
# Boston
#1. summary each year total crime number, crime number of each categorical, crime number in each district, 
#   the most frequency top 15 crime in boston, the top 3 district with highest crime-happening in boston 
#2. find top 3 district with highest crime-happening in each year and check whether top3 district in each year 
#   are the same as the total top3 district
#2. compare the number of top 15 crime happening each year in boston

crime<-read.csv("/Users/fudanvip/IE7280/project1/test/dataset/bostoncrime.csv",sep = ",",stringsAsFactors = FALSE)
library(ggplot2)
library(dplyr)
year_crime<-summarise(group_by(crime,year),total_number_of_crime_each_year=n())
year_crime
ggplot(crime, aes(x = year)) +geom_histogram(binwidth = 1, color = "grey30", fill = "grey")+theme_bw()+ylab("total crime number")
any(crime$district=="")
crime_district<-filter(crime, district != "")
district<-summarise(group_by(crime_district,district),total_number_of_crime=n())
district
top_district_total<-arrange(district,desc(total_number_of_crime))[1:3,]
top_district_total
top_district_total$district
library(magrittr)
district_year<-crime_district %>%
  group_by(year,district)%>%
  summarise(total_crime=n()) %>%
  arrange(year,desc(total_crime)) %>%
  mutate(rank=row_number())
district_year
top3<-filter(district_year, rank <=3)
top3
## the top3 high crime district in each year are the same as the top3 total high crime district from 2015-2019
ggplot(top3,aes(x=year,y=total_crime,fill=factor(district)))+
  geom_bar(stat="identity",position="dodge")+ylab("crime number")+
  xlab("year")
year2015<-filter(crime,year=2015)
unique(year2015$month)
# so the data for year 2015 is not complete. and the data for 2019 also is not completed. this the reason why 
# the total number of crime for 2015 and 2019 are much lower than other three year.

offense<-summarise(group_by(crime,offense_description),total_number_of_crime=n())
offense
top_offense<-arrange(offense,desc(total_number_of_crime))[1:15,]
top_offense
