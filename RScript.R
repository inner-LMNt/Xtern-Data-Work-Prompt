install.packages("httr")
install.packages("jsonlite")
install.packages("ggplot2")
install.packages("leaflet")
install.packages("dplyr")

library(httr)
library(jsonlite)
library(ggplot2)
library(leaflet)
library(dplyr)

api_key <- "<MyKey>"
endpoint <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
num_results <- 20
results_count <- 0

food_trucks <- data.frame(
  Name = character(0),
  Address = character(0),
  Rating = numeric(0),
  Website = character(0),
  OpenHours = character(0),
  CuisineType = character(0),
  Latitude = numeric(0),
  Longitude = numeric(0)
)

parameters <- list(
  location = "39.7684,-86.1581", # Indianapolis
  radius = 10000,
  keyword = "food truck",
  key = api_key
)

num_requests <- ceiling(num_results / 20)  # 20 is the default limit per request

for (i in 1:num_requests) {
  response <- GET(url = endpoint, query = parameters)
  
  data <- content(response, as = "text")
  data <- fromJSON(data)
  
  results <- data$results

  for (j in 1:length(results$name)) {
    food_trucks <- rbind(food_trucks, data.frame(
      Name = results$name[j],
      Address = results$vicinity[j],
      Rating = results$rating[j],
      Website = results$website[j],
      OpenHours = results$hours[j],
      CuisineType = results$cuisine[j],
      Latitude = results$geometry$location$lat[j],
      Longitude = results$geometry$location$lng[j]
    ))
    
    results_count <- results_count + 1

    if (results_count >= num_results) {
      break
    }
  }

  next_page_token <- data$next_page_token

  if (is.na(next_page_token)) {
    break
  }

  parameters <- c(parameters, list(pagetoken = next_page_token))
}

write.csv(food_trucks, "food_trucks_data.csv", row.names = FALSE)


food_trucks_map <- leaflet(data = food_trucks) %>%
  addTiles() %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~Name)

food_trucks_map  # Display the map


food_trucks <- food_trucks[order(food_trucks$Rating, decreasing = TRUE), ]

breakfast_data <- food_trucks[1:6, ]
lunch_data <- food_trucks[7:12, ]
dinner_data <- food_trucks[13:18, ]

top_food_trucks <- rbind(breakfast_data, lunch_data, dinner_data)

print(top_food_trucks)


top_6_food_trucks <- head(top_food_trucks, 6)

top_6_food_trucks_map <- leaflet(data = top_6_food_trucks) %>%
  addTiles() %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~Name)

top_6_food_trucks_map  # Display the map
