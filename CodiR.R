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

setwd("C:/1r MEI/BIA/Practica02/netflix_titles.csv")
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
netflix$year_added<-as.numeric(substr(netflix$date_added, nchar(netflix$date_added)-4, nchar(netflix$date_added)))

table(netflix$type)
table(netflix$year_added)
table(netflix$rating)

k <- strsplit(netflix$country, split = ", ")
netflixP<- data.frame(show_id=rep(netflix$show_id, sapply(k, length)),
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
netflixP$country<-gsub(",", "", netflixP$country, fixed=TRUE)
table(netflixP$country, useNA="ifany")

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

netflixMovies<-netflix[netflix$type=="Movie",]
netflixMovies$duration<-as.numeric(gsub(" min", "", netflixMovies$duration, fixed=TRUE))
summary(netflixMovies$duration)

netflixSeries<-netflix[netflix$type=="TV Show",]
netflixSeries$duration<-as.numeric(gsub(c(" Season"~" Seasons"), "", netflixSeries$duration, fixed=TRUE))
summary(netflixSeries$duration)


#guyfygklhh