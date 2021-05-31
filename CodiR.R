####EJERCICIO PRACTICA 2 02/06/21####

#### Instalación y carga de los paquetes ####

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}

if (!require("readr")) {
  install.packages("readr")
  library(readr)
}

#### Importación del conjunto de datos ####

#setwd("C:/1r MEI/BIA/Practica02/netflix_titles.csv")
#Session -> Set working directory -> To source file location

NETFLIX <- read_csv("netflix_titles.csv")
#head(NETFLIX)
#str(NETFLIX)

#setwd("C:/1r MEI/BIA/Practica02/EDA-Netflix-2020-in-R-master/EDA-Netflix-2020-in-R-master")
#Session -> Set working directory -> To source file location

#NETFLIX2 <- read_csv("netflix_titles.csv")
#head(NETFLIX)
#str(NETFLIX)
netflix<-NETFLIX
netflix=distinct(netflix,title,country,type,release_year, .keep_all = TRUE)

netflix$rating <- as.factor(netflix$rating)
netflix$date_added <- mdy(netflix$date_added)

netflix$year_added<-format(netflix$date_added, "%Y")

table(netflix$type)
table(netflix$year_added)
table(netflix$rating)

k <- strsplit(netflix$country, split = ", ")
NetflixCountry<- data.frame(show_id=rep(netflix$show_id, sapply(k, length)),
                      type = rep(netflix$type, sapply(k, length)),
                      title = rep(netflix$title, sapply(k, length)),
                      director = rep(netflix$director, sapply(k, length)),
                      cast = rep(netflix$cast, sapply(k, length)),
                      date_added = rep(netflix$date_added, sapply(k, length)),
                      release_year = rep(netflix$release_year, sapply(k, length)),
                      rating = rep(netflix$rating, sapply(k, length)),
                      duration = rep(netflix$duration, sapply(k, length)),
                      listed_in = rep(netflix$listed_in, sapply(k, length)),
                      description = rep(netflix$description, sapply(k, length)),
                      year_added = rep(netflix$year_added, sapply(k, length)),
                      country = unlist(k))
rm(k)
NetflixCountry$country<-gsub(",", "", NetflixCountry$country, fixed=TRUE)
table(NetflixCountry$country, useNA="ifany")

k <- strsplit(netflix$listed_in, split = ", ")
netflixCat<- data.frame(show_id=rep(netflix$show_id, sapply(k, length)),
                      type = rep(netflix$type, sapply(k, length)),
                      title = rep(netflix$title, sapply(k, length)),
                      director = rep(netflix$director, sapply(k, length)),
                      cast = rep(netflix$cast, sapply(k, length)),
                      country = rep(netflix$country, sapply(k, length)),
                      date_added = rep(netflix$date_added, sapply(k, length)),
                      release_year = rep(netflix$release_year, sapply(k, length)),
                      rating = rep(netflix$rating, sapply(k, length)),
                      duration = rep(netflix$duration, sapply(k, length)),
                      description = rep(netflix$description, sapply(k, length)),
                      year_added = rep(netflix$year_added, sapply(k, length)),
                      listed_in = unlist(k))
rm(k)
table(netflixCat$listed_in, useNA="ifany")

####Number of Movies and TV Shows####

netflixMovies<-netflix[netflix$type=="Movie",]
netflixMovies$duration<-as.numeric(gsub(" min", "", netflixMovies$duration, fixed=TRUE))
summary(netflixMovies$duration)

netflixSeries<-netflix[netflix$type=="TV Show",]
netflixSeries$duration<-gsub(" Season", "", netflixSeries$duration, fixed=TRUE)
netflixSeries$duration<-as.numeric(gsub("s", "", netflixSeries$duration, fixed=TRUE))
summary(netflixSeries$duration)

ggplot(netflix,aes(x=type))+geom_bar(col="black",fill="red")+  
  ggtitle(label = "Cantidad de películas y series") + xlab("")+
  ylab(label="Cantidad total")+theme_classic()+
  scale_y_continuous(breaks=seq(0,6000,1000))

####Content by Country####

netflixByCountryArr<-NetflixCountry %>% group_by(country, type) %>%
  summarise(count=n())
netflixByCountryArr<-reshape(data=data.frame(netflixByCountryArr), idvar="country",
           v.names="count", timevar="type", direction="wide")
colnames(netflixByCountryArr)<-c("country", "NumberOfMovies", "NumberOfTVShows")
netflixByCountryArr[is.na(netflixByCountryArr)]<-0
netflixByCountryArr<-netflixByCountryArr[-nrow(netflixByCountryArr),]
netflixByCountryArr$total<-netflixByCountryArr$NumberOfMovies+netflixByCountryArr$NumberOfTVShows
netflixByCountryArr<-netflixByCountryArr %>% arrange(desc(total))
netflixByCountryArr<-netflixByCountryArr[netflixByCountryArr$total>=netflixByCountryArr$total[10],]
ggplot(netflixByCountryArr, aes(NumberOfMovies, NumberOfTVShows, colour=country))+ 
  geom_point(size=5)+
  xlab("Number of Movies") + ylab("Number of TV Shows")+
  ggtitle("Amount of Netflix Content in Top 10 countries")+theme_classic()+
  labs(col="Country")
rm(netflixByCountryArr)

####Content by Date####
netflix$show_id<-tibble(netflix$show_id)
netflixByDate<-netflix$show_id %>% group_by(netflix$year_added,netflix$type) %>% 
  summarize(count=n()) %>% na.omit()
colnames(netflixByDate)<-c("YearAdded", "Type", "count")

ggplot(netflixByDate, aes(YearAdded, count, group=Type))+
  geom_line(aes(colour = Type), size = 2)+ 
  geom_point() + 
  xlab("Year") + 
  ylab("Number of Content")+
  ggtitle("Amount of Netflix Content By Time")+
  theme_classic()

####Content by Rating####

