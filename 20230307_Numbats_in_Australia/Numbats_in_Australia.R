library(tidyverse)
library(ggtext)
library(extrafont)
library(sf)
library(ozmaps) 
library(ggthemr)
library(ggmap)

theme_flat_dark <- ggthemr("flat dark")
swatch <- c("#ecf0f1", "#3498db", "#2ecc71", "#f1c40f", "#e74c3c", 
            "#9b59b6", "#1abc9c", "#f39c12", "#d35400")

# Load Data ---------------------
numbats <- readr::read_csv('20220307_Numbats_in_Australia/numbats.csv') 

# EDA ---------------------------
summary(as.factor(numbats$dataResourceName))
summary(as.factor(numbats$scientificName))
summary(as.factor(numbats$taxonConceptID))
summary(numbats$day)
summary(numbats$dryandra)

View(filter(numbats, is.na(day)))

numbats %>%
  filter(is.na(decimalLatitude)) %>%
  pull(dataResourceName) %>% 
  as.factor() %>%
  summary

# Figures ----------------------
bbox <- c(left = 112.9211, bottom = -44.5, right = 153.6299, top = -10.5)
map <- get_stamenmap(bbox, zoom = 6, maptype = "terrain-background", color = "bw", force = TRUE)


ggmap(map) +
  geom_point(data = subset(numbats, !is.na(decimalLongitude)), 
             aes(x = decimalLongitude, y = decimalLatitude),
             color = "#d35400") +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.1, 0.2),
        legend.background = element_blank())

ggsave("20220307_Numbats_in_Australia/numbats_map.png")


# sf_oz <- ozmap("states") %>% filter(NAME != "Other Territories")
# 
# sf_oz %>%
#   ggplot() + geom_sf(fill = NA, color = "white") +
#   geom_sf_text(aes(label = NAME)) +
#   geom_point(data = subset(numbats, !is.na(decimalLongitude)), 
#              aes(x = decimalLongitude, y = decimalLatitude, color = scientificName)) +
#   labs(x = NULL, y = NULL) +
#   theme(axis.text = element_blank(),
#         axis.line = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank(),
#         legend.position = c(0.1, 0.2),
#         legend.background = element_blank())

