rm(list = ls())

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

url <- "https://api.metatft.com/tft-pbe-comps/comps"
res <- GET(url)
data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

comps_stat = data[["comps"]][["stats"]]
comps_units = data.frame("comps" = data[["comps"]]$name)

nrow(comps_units[1,])

top_comp = comps_units[1, "comps"]
length(top_comp)

compnames_cvt = function(comp){
  for (i in 1:nrow(comp){
    
  })
}

comps_combined_stats = mutate(comps_units, comps_stat)
comps_units
