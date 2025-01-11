library(jsonlite)
library(RCurl)


# Function for downloading commuting times
googleurl <- function(key="ENTER YOUR KEY HERE", return.call = "json", origin="50.74021,7.12322", destination="50.70624 7.13778", mode="transit") {
  root <- "https://maps.googleapis.com/maps/api/distancematrix/"
  u <- paste(root, return.call, "?origins=", origin, "&destinations=", destination,"&units=metric" ,"&mode=",mode,"&key=", key, sep = "","&departure_time=1687154400")
  return(URLencode(u))
}

query<-jsonlite::fromJSON(url(googleurl()))

# Already prepared dataset
load("./prepared_distances.Rdata")
distances$pop_time <- 0
distances$cen_time <- 0


for(i in 1:10)
#for(i in 1:nrow(distances))
{
  origins <- paste(st_coordinates(distances$pop_centroid)[i,2:1], collapse=",")
  destinations <-  paste(st_coordinates(distances$geometry.y)[i,2:1], collapse=",")
  print("")
  print(googleurl(origin=origins, destination = destinations))
  query<-jsonlite::fromJSON(url(googleurl(origin=origins, destination = destinations)))
  if( query$rows$elements[[1]]$status == "ZERO_RESULTS" )
  {
      minutes <- Inf
  }
  else
  {
   
    minutes <- query$rows$elements[[1]]$duration$value/60
  } 
  distances$pop_time[i] <- minutes
  print(i)
}

for(i in 1:nrow(distances))
{
  origins <- paste(st_coordinates(distances$centroid)[i,2:1], collapse=",")
  destinations <-  paste(st_coordinates(distances$geometry.y)[i,2:1], collapse=",")
  print("")
  print(googleurl(origin=origins, destination = destinations))
  query<-jsonlite::fromJSON(url(googleurl(origin=origins, destination = destinations)))
  if( query$rows$elements[[1]]$status == "ZERO_RESULTS" )
  {
    minutes <- Inf
  }
  else
  {
    
    minutes <- query$rows$elements[[1]]$duration$value/60
  } 
  distances$cen_time[i] <- minutes
  print(i)
}


save(data, file="distances_around_universities_with_times.Rdata")
