library(tidyverse)
library(lubridate)
library(extrafont)
library(sf)
library(ozmaps) 
library(ggthemr)
library(ggmap)
library(ggtext)
library(ggrepel)
library(grid)
library(gridExtra)
library(cowplot)
library(RColorBrewer)
library(png)

theme_flat_dark <- ggthemr("flat dark")
swatch <- c("#ecf0f1", "#3498db", "#2ecc71", "#f1c40f", "#e74c3c", 
            "#9b59b6", "#1abc9c", "#f39c12", "#d35400")
bkground_col <- theme_flat_dark$palette$background
line_col <- theme_flat_dark$palette$gridline

# Load Data ---------------------
numbats <- readr::read_csv('20230307_Numbats_in_Australia/numbats.csv') %>%
  mutate(decade = case_when(
    is.na(year) ~ "Unknown",
    year == 1856 ~ "1856",
    between(year, 1900, 1909) ~ "1900s",
    between(year, 1950, 1969) ~ "1950s-60s",
    between(year, 1980, 1989) ~ "1980s",
    between(year, 2000, 2009) ~ "2000s",
    between(year, 2010, 2019) ~ "2010s",
    between(year, 2020, 2029) ~ "2020s")) %>%
  mutate(decade = as.factor(decade)) %>%
  mutate(eventDate = ymd_hms(eventDate)) %>%
  mutate(wday = factor(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>%
  mutate(month = month(eventDate, label = TRUE)) %>%
  mutate(month = fct_relevel(month, levels(.$month)[c(12, 1:11)])) %>%
  mutate(hms = hms::as_hms(eventDate)) %>%
  mutate (season = case_when(
    month %in% c("Dec", "Jan", "Feb") ~ "SUMMER",
    month %in% c("Mar", "Apr", "May") ~ "AUTUMN",
    month %in% c("Jun", "Jul", "Aug") ~ "WINTER",
    month %in% c("Sep", "Oct", "Nov") ~ "SPRING"))%>%
  mutate(season = factor(season, toupper(c("Spring", "Summer", "Autumn", "Winter")))) 
  

# EDA ---------------------------
summary(as.factor(subset(numbats, !is.na(eventDate))$dataResourceName))
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

subset(numbats, !is.na(eventDate)) %>%
  group_by(year, dataResourceName) %>%
  summarise(n = n()) %>%
  View



# Figures ----------------------


## Distribution Map -----------------------
sf_oz <- ozmap("states") %>% filter(NAME != "Other Territories")
coord_reserves <- data.frame(lat = -c(32.7848, 34.2718, 34.5071, 33.2162, 34.2122, 29.724), 
                             long = c(116.9670, 116.6318, 139.4759, 141.1671, 142.6246, 117.1705),
                       label = c("Dryandra\nWoodland", "Perup Nature\nReserve",
                                 "Yookamurra\nSanctuary", "Scotia\nSanctuary", 
                                 "Mallee Cliffs\nNational Park", "Mount Gibson\nSanctuary"))
map_xlim1 <- c(114.8, 117.5); map_ylim1 <- c(-35.2, -32.4)
map_xlim2 <- c(138.2, 143); map_ylim2 <- c(-34.8, -32.9)
map_col1 <- "#fa9c07"; map_col2 <- "#1abc9c"
numbats_summ <- numbats %>%
  group_by(decimalLongitude, decimalLatitude, year, decade) %>%
  summarise(n = n())

theme_custom <- theme(  
  text = element_text(color = swatch[1], family = "Dubai"),
  plot.title = element_markdown(size = 13, face = "bold"),
  plot.background = element_rect(fill = bkground_col, color = NA),
  panel.background = element_rect(fill = bkground_col, color = NA),
  legend.background = element_rect(fill = bkground_col, color = NA),
  legend.key = element_blank(),
  legend.title = element_text(face = "bold", size = 10),
  axis.text = element_text(color = swatch[1], size = 11),
  axis.ticks = element_line(color = swatch[1]),
  axis.line = element_line(color = swatch[1]),
  panel.spacing.x = unit(0.5, "mm"),
  panel.grid.major.x = element_blank(),
  strip.text = element_text(margin = margin(t = 4, b = 4))
)

theme_map <- theme_custom + theme(
  plot.title = element_markdown(size = 14, margin = margin(b = -4, l = 5), face = "bold"),
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
)

span <- function(text, color) {
  paste0("<span style='color:", color, "'>", text, "</span>")
}

map_subtitle <- paste0(
  "<p>Numbats were formerly widely distributed across southern Australia. However, their range has<br/>",
  "significantly decreased since the arrival of Europeans. Today, the species has survived only in<br/>", 
  span("Dryandra Woodland", map_col1), " and ", span("Perup Nature Reserve", map_col1), ".</p>",
  "<p>The species has been successfully reintroduced into three fenced, feral predator-proof reserves:<br/>", 
  span("Yookamurra Sanctuary", map_col2), ", ", span("Scotia Sanctuary", map_col2), ", and ", 
  span("Mount Gibson Sanctuary", map_col2), ". Reintroduction to ", span("Mallee ", map_col2), "<br/>", 
  span("Cliffs National Park", map_col2), " began in December 2020.</p>"
)

map_caption <- paste0("**SOURCE:** ", span("**Atlas of Living Australia**", "#f16648"),
                      ". Data as of March 7, 2023. Dataset prepared by Di Cook.")
map_base <- ggplot(sf_oz) + 
  geom_sf(fill = swatch[1], color = bkground_col) +
  geom_point(data = subset(numbats_summ, !is.na(decimalLongitude) & decade == "Unknown"),
             aes(x = decimalLongitude, y = decimalLatitude, color = decade, size = n),
             shape = 16, alpha = 0.6) +
  geom_point(data = subset(numbats_summ, !is.na(decimalLongitude) & decade != "Unknown") %>% arrange(desc(decade)),
             aes(x = decimalLongitude, y = decimalLatitude, color = decade, size = n), 
             shape = 16, alpha = 0.8) +
  scale_color_manual(values = c(viridis::plasma(11)[seq(11, 1, -2)], "grey65"), name = "YEAR") 
  
map_main <- map_base +
  geom_rect(data = NULL, aes(xmin = map_xlim1[1] - 0.2, xmax = map_xlim1[2] + 0.2,
                             ymin = map_ylim1[1] - 0.4, ymax = map_ylim1[2] + 0.5),
            color = map_col1, fill = NA) +
  geom_rect(data = NULL, aes(xmin = map_xlim2[1], xmax = map_xlim2[2] + 1,
                             ymin = map_ylim2[1] - 0.8, ymax = map_ylim2[2] + 0.4),
            color = map_col2, fill = NA) +
  geom_text(data = coord_reserves[6, ], aes(x = long, y = lat, label = "\u2605"), color = map_col2) +
  geom_text(data = coord_reserves[6, ], aes(x = long + 1, y = lat + 0.8, label = label),
            color = map_col2, lineheight = 0.8, family = "Dubai", size = 3.5,
            vjust = 0, hjust = 0.5, fontface = "bold") +
  scale_radius(name = "# SIGHTINGS", range = c(2, 9), breaks = c(1, 5 * (1:3))) +
  labs(x = NULL, y = NULL, title = "NUMBAT DISTRIBUTION IN AUSTRALIA", subtitle = map_subtitle) +
  coord_sf(expand = FALSE, clip = "off") +
  guides(color = guide_legend(keywidth = 1.5, keyheight = 1, override.aes = list(size = 2), order = 1),
         size = guide_legend(override.aes = list(color = "#ccd3d5"), order = 2)) +
  theme_map +
  theme(legend.position = "right", 
        legend.direction = "vertical",
        plot.margin = margin(t = -25, l = 5, r = 5),
        plot.subtitle = element_markdown(size = 9.5, margin = margin(t = 6, b = -4, l = 5, r = 5), 
                                         color = "#ccd3d5", family = "Franklin Gothic Book", lineheight = 0.8))

map_inset1 <- map_base +
  labs(x = NULL, y = NULL, title =  "NATURAL HABITAT") +
  geom_text(data = coord_reserves[1:2, ], aes(x = long, y = lat, label = "\u2605"), color = map_col1) +
  geom_text_repel(data = coord_reserves[1:2, ], aes(x = long + c(-0.7, 0), y = lat + c(0.05, -0.05), label = label),
                  color = map_col1, lineheight = 0.8, family = "Dubai", size = 3.5,
                  vjust = c(0, 0.2), hjust = c(1, 0.6), seed = 42, fontface = "bold") +
  scale_radius(name = "# Sightings", range = c(2, 10), breaks = c(1, 5 * (1:3))) +
  coord_sf(expand = FALSE, xlim = map_xlim1, ylim = map_ylim1) +
  theme_map +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA, color = map_col1, linewidth = 2),
        plot.margin = margin(t = -30, l = -10, b = 10),
        plot.title = element_markdown(color = map_col1, size = 11, margin = margin()))

map_inset2 <- map_base +
  labs(x = NULL, y = NULL, caption = map_caption, title = "REINTRODUCED<span> </span>AREAS") +
  geom_text(data = coord_reserves[3:5, ], aes(x = long, y = lat, label = "\u2605"), color = map_col2) +
  geom_text_repel(data = coord_reserves[3:5, ], aes(x = long + c(0, 0, -0.1), y = lat + c(0.1, -0.1, 0), label = label),
                  color = map_col2, lineheight = 0.8, family = "Dubai", size = 3.5, fontface = "bold",
                  vjust = c(1, 0.5, 0), hjust = 0.4, seed = 42, min.segment.length = 1) +
  coord_sf(expand = FALSE, xlim = map_xlim2, ylim = map_ylim2) +
  scale_radius(name = "# Sightings", range = c(2, 10), breaks = c(1, 5 * (1:3))) +
  theme_map +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA, color = map_col2, linewidth = 2),
        plot.margin = margin(t = -15, b = 0, r = 5, l = -5),
        plot.title = element_markdown(color = map_col2, size = 11, margin = margin()),
        plot.caption = element_markdown(size = 9.5, hjust = 1.1, family = "Dubai", margin = margin(t = 3)))

numbat_img <- readPNG("20230307_Numbats_in_Australia/numbat_rm_bkground.png") %>%
  rasterGrob(interpolate = TRUE, vjust = 0.9)
  
map_g <- arrangeGrob(
  map_main, map_inset1, map_inset2, numbat_img,
  layout_matrix = matrix(
    c(rep(1, 9 * 1), 
      4, 4, 1, 1, 1, 1, 1, 1, 1,
      rep(1, 9 * 4), 
      2, 2, 2, 2, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 3, 3, 3, 3, 3,
      2, 2, 2, 2, 3, 3, 3, 3, 3), 
    nrow = 9, byrow = TRUE),
  top = textGrob("@akela@mstdn.social", x = 0.97, y = 0,
                 vjust = 0.5, hjust = 1, gp = gpar(fontsize = 10, fontfamily = "Dubai", col = swatch[1]))) 

map_g <- cowplot::ggdraw(map_g)+
  theme(plot.background = element_rect(fill = bkground_col, color = NA),
        plot.margin = margin(r = 0, b = 5))

ggsave("20230307_Numbats_in_Australia/numbats_map.png", map_g,
       dpi = 600, width = 5.7, height = 7)

## Sightings by month -----------------
p_month <- numbats %>%
  filter(!is.na(wday)) %>%
  group_by(year, season, month) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = month, y = as.factor(year), fill = n)) +
  geom_tile() +
  geom_hline(yintercept = 0:30 + 0.5, color = line_col, linewidth = 0.4) +
  geom_vline(xintercept = 1:2 + 0.5, color = line_col, linewidth = 0.4) +
  scale_y_discrete(limits = rev)+
  scale_fill_stepsn(breaks = sort(c(1, 5, seq(10, 60, 10))), 
                    colors = c(viridis::inferno(15)[c(2, seq(5, 14, 2))], "#fff3b1"), 
                    name = "# SIGHTINGS", limits = c(1, 60)) +
  coord_cartesian(expand = FALSE) +
  guides(fill = guide_coloursteps(barheight = 0.5, barwidth = 12, title.vjust = 1)) +
  labs(x = NULL, y = NULL, title = toupper("No. of Sightings by Month")) +
  facet_grid( ~ season, scales = "free", switch = "y") +
  theme_custom +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.4,
                                    color = line_col),
        panel.grid.major.y = element_blank(),
        plot.title = element_markdown(hjust = -0.35),
        panel.spacing.y = unit(0.1, "mm"),
        legend.position = "bottom", 
        legend.title = element_text(margin = margin(1, 5, 5, 5)))


## Sightings by hours --------------------
## Data resources except for iNaturalist have fixed reporting time(s)
subset(numbats, !is.na(hms)) %>%
  group_by(dataResourceName) %>%
  filter(n() > 2) %>%
  group_by(dataResourceName, hms) %>%
  summarise(n = n()) 

p_time <- numbats %>%
  filter(!is.na(eventDate)) %>%
  filter(dataResourceName == "iNaturalist Australia") %>%
  mutate(dryandra = case_when(is.na(dryandra) ~ "Unknown", 
                              dryandra == TRUE ~ "Dryandra Woodland",
                              dryandra == FALSE ~ "Other Locations")) %>%
  ggplot(aes(x = month, y = -hms)) +
  geom_rect(data = data.frame(xmin = -Inf, ymin = -hms::as.hms("18:00:00"),
                             xmax = Inf,  ymax = -hms::as.hms("06:00:00")),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = swatch[1], alpha = 0.1, inherit.aes = FALSE) +
  geom_point(aes(color = dryandra)) +
  labs(x = NULL, y = NULL, title = paste(toupper("Sighting Time Reported on"), span("iNATURALIST", "#74ac00"))) +
  scale_y_time(labels = function(x) {str_remove(as.character(-x), ":00$")}, 
               limits = -hms::as.hms(c("24:00:00", "00:00:00")),
               breaks = -hms::as.hms(paste0(seq(0, 24, 6), ":00:00")),
               expand = expansion(mult = c(0, 0.01))) +
  scale_color_manual(values = swatch[c(2, 4)]) +
  facet_grid(~ season, scales = "free_x") +
  theme_custom +
  theme(plot.subtitle = element_text(margin = margin(b = 5, t = -3)),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 11))


## Sightings in Dryandra Woodland -------------------
dummy <- data.frame(eventDate = as.POSIXct(
  paste0(c("2007-09-01", "2008-01-31", "2014-09-01",
           "2019-12-24", "2019-12-25", "2023-03-10"), " 00:00:00")),
  decade = c("2000s", "2000s", "2010s", "2010s", "2020s", "2020s"))

p_prcp_temp <- numbats %>%
  filter(dryandra) %>%
  filter(!is.na(prcp)) %>% 
  ggplot(aes(x = eventDate)) +
  geom_errorbar(aes(ymin = tmin, ymax = tmax), linewidth = 0.5) +
  geom_blank(data = dummy) +
  geom_point(aes(y = prcp * 40 / 12), color = swatch[4], shape = 16, size = 1.5) +
  scale_x_datetime(expand = expansion(mult = c(0, 0)), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(name = NULL, labels = function(x) {paste0(x, "\u2103")},
                     sec.axis = dup_axis(trans = ~ ./40 * 12, name = NULL, labels = function(x) {paste0(x, "mm")})) +
  labs(title = paste0(toupper("Dryandra Woodland Sightings: "), span(toupper("Temperature"), color = swatch[2]), 
                      " & ", span(toupper("Precipitation"), swatch[4])),
       x = NULL, caption = map_caption) +
  facet_grid(~decade, scale = "free_x", space = "free_x") +
  guides(color = guide_legend(keywidth = 0.8, keyheight = 0.8)) +
  theme_custom +
  theme(plot.title = element_markdown(hjust = -0.25),
        plot.caption = element_markdown(hjust = -0.8, size = 13),
        plot.margin = margin(l = 10, r = 0, b = 5, t = 10))



g_sighting <- arrangeGrob(
  p_month, p_time, p_prcp_temp,
  layout_matrix = matrix(c(rep(c(1, 1, 1, 2, 2, 2), 4), rep(3, 6 * 2)),  
                         byrow = TRUE, ncol = 6),
  top = textGrob(label = c("NUMBAT SIGHTINGS IN AUSTRALIA", "@akela@mstdn.social"), 
                 x = c(0.012, 0.99), y = c(0, 0.05), vjust = 0, hjust = c(0, 1), 
                 gp = gpar(fontsize = c(17, 12.5), fontfamily = "Dubai", col = swatch[1], fontface = c("bold", "plain"))))

g_sighting <- cowplot::ggdraw(g_sighting)+
  theme(plot.background = element_rect(fill = bkground_col, color = NA),
        plot.margin = margin(-20, 5, 3, 5))

ggsave("20230307_Numbats_in_Australia/numbat_sighting.png", g_sighting,
       width = 8.5, height = 7.5)
  
