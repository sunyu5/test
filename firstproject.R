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
year2015<-filter(crime,year==2015)
unique(year2015$month)
# so the data for year 2015 is not complete. and the data for 2019 also is not completed. this the reason why 
# the total number of crime for 2015 and 2019 are much lower than other three year.


crime_district$period[crime_district$hour >=8 & crime_district$hour<=16 ]<-"Day" 
crime_district$period[crime_district$hour >16 & crime_district$hour<=22 ]<-"Night"
crime_district$period[crime_district$hour >22 | crime_district$hour <8 ]<-"LateNight"
Time<-summarise(group_by(crime_district,period),total_crime=n())
p1<-ggplot( Time, aes(x=period, y=total_crime,group=1))+geom_line(color="blue")+theme_bw()+xlab("boston period") 

top_district<-filter(crime_district,district %in% top_district_total$district)
Time2<-summarise(group_by(top_district,period),total_crime=n())
p2<-ggplot( Time2, aes(x=period, y=total_crime,group=1))+geom_line(color="red")+theme_bw()+xlab("top3 period")
library(gridExtra)
grid.arrange(p1, p2,ncol = 2)
# take sample which is year == 18 and sample size = 5000. 

library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")
library("dplyr")
library("ggplot2")
crime<-read.csv("/Users/sunyu/Desktop/IE 6600/bostoncrime.csv",sep = ",",stringsAsFactors = FALSE)
crime<- filter(crime,crime$long>-72)
crime<-filter(crime,crime$lat>41)
crime_s<- filter(crime,year==2018)
set.seed(1)
samp <- sample_n(crime_s, 5000)
setnames(samp, tolower(colnames(samp)))
samp<-as.data.table(samp, keep.rownames=FALSE, sorted=TRUE, value.name="value", na.rm=TRUE)
samp <- samp[!is.na(long)]
samp<- samp[!is.na(lat)]
kde <- bkde2D(samp[ , list(long,lat)],
              bandwidth=c(.0045, .0068), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)
# plot desity
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])
#plot circle on it
leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(lng = samp$long, lat = samp$lat,
             radius = .01, opacity = .2, col = "blue")