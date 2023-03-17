library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(extrafont)
library(ggtext)
library(ggrepel)
library(grid)
library(geosphere)
library(ggmap)

theme_flat <- ggthemr::ggthemr("flat")
swatch <- theme_flat$palette$swatch
scales::show_col(swatch)
theme_custom <- theme(
  text = element_text(family = "Dubai"),
  panel.grid.major.x = element_line(linetype = 3, linewidth = 0.4),
  panel.grid.major.y = element_line(linetype = 3, linewidth = 0.4),
  plot.title.position = "plot",
  plot.title = element_text(size = 12, family = "Dubai"),
  plot.caption.position = "plot",
  plot.caption = element_text(hjust = 0)
)


cats_uk <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')

cats_uk_cleaned <- cats_uk %>%
  filter(!algorithm_marked_outlier & !manually_marked_outlier) %>%
  arrange(timestamp) %>%
  group_by(tag_id) %>%
  mutate(location_lat_lag = ifelse(row_number() == 1, location_lat, lag(location_lat)), 
         location_long_lag = ifelse(row_number() == 1, location_long, lag(location_long)))%>%
  split(f = .$tag_id) %>%
  lapply(function(df) {
    df %>% rowwise() %>%
      mutate(distance = distHaversine(c(location_long, location_lat), 
                                      c(location_long_lag, location_lat_lag)))
  }) %>%
  do.call(rbind, .) %>%
  select(-location_lat_lag, -location_long_lag) %>%
  left_join(cats_uk_reference, by = "tag_id") %>%
  ungroup()
  # group_by(tag_id) %>%
  # mutate(distance = ifelse(n() == 1 | row_number() == 1, 0, distm(
  #   c(location_long, location_lat), 
  #   c(lag(location_long), lag(location_lat)), fun = "distHaversine")))

n_events_summ <- cats_uk_cleaned %>%
  group_by(tag_id) %>%
  summarise(n = n()) %>%
  left_join(cats_uk_reference, by = "tag_id")

n_events_summ %>%
  ggplot(aes(y = forcats::fct_reorder(animal_id, n), x = n)) +
  geom_col(aes(fill = animal_sex), alpha = 0.7, width = 0.8) +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Set1") +
  theme_custom

n_events_summ %>%
  ggplot(aes(x = n)) +
  geom_histogram(alpha = 0.7, bins = 25, color = "white") +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_custom

n_events_summ %>%
  ggplot(aes(n, animal_sex, fill = animal_sex, color = animal_sex)) +
  geom_violin(alpha = 0.5) +
  geom_point(shape = 16, size = 1.5) +
  labs(x = NULL, y = NULL) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_custom

cats_uk_cleaned %>%
  ggplot(aes(x = ground_speed, y = forcats::fct_reorder(animal_id, ground_speed))) +
  geom_boxplot(outlier.alpha = 0) +
  geom_point(shape = 16, size = 1) +
  labs(x = NULL, y = NULL) +
  theme_custom

library(ggmap)



top_active_cats <- cats_uk_cleaned %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(tag_id, animal_id, date, animal_sex,
           animal_reproductive_condition, hrs_indoors, age_years) %>%
  summarise(daily_dist = sum(distance)) %>%
  group_by(tag_id) %>%
  arrange(date) %>%
  mutate(day_number = row_number()) %>%
  mutate(total_dist = sum(daily_dist))

p_dist_speed <- cats_uk_cleaned %>%
  ggplot(aes(x = distance/1E3, y = ground_speed/1E3, color = animal_sex)) +
  geom_point(aes(shape = animal_sex), shape = 16, size = 1.5) +
  geom_text_repel(data = subset(cats_uk_cleaned, distance > 500 | ground_speed > 2.7E4),
                  aes(label = animal_id), family = "Dubai", size = 3, box.padding = 0.15, 
                  max.overlaps = Inf, show.legend = FALSE) +
  scale_color_brewer(palette = "Set1", name = "Sex", labels = c("Female", "Male")) +
  scale_shape_discrete(name = "Sex", labels = c("Female", "Male")) +
  facet_wrap(~ paste0(str_pad(hour(timestamp), 2, "left", "0"), ":00"), ncol = 4) +
  labs(x = "Distance Since Last Timestamp (km)", y = "Estimated Ground Speed (km/s)",
       title = "PET CATS U.K. STUDY: DISTANCE VS. ESTIMATED GROUND SPEED",
       caption = "Source: Movebank for Animal Tracking Data via Data is Plural") +
  theme_custom +
  theme(legend.text = element_text(color = "grey15", size = 11),
        legend.title = element_text(color = "grey15", size = 11.5, face = "bold"),
        strip.text = element_text(margin = margin(t = 2, b = 2), size = 10))
ggsave("20230131_Pet_Cats_UK/cat_dist_speed_by_hour.png", width = 6.5, height = 8)

p_daily_dist <- top_active_cats %>%
  mutate(animal_sex = ifelse(animal_sex == "f", "Female", "Male")) %>%
  ggplot(aes(x = day_number, y = forcats::fct_reorder(animal_id, total_dist))) +
  geom_tile(aes(fill = daily_dist/1E3), color = "white") +
  scale_x_continuous(limits = c(0.5, 11.5), labels = function(x) {paste("day", x)}) +
  coord_cartesian(expand = FALSE) +
  scale_fill_stepsn(colors = c("grey35", rev(brewer.pal(11, "Spectral")), "#551100"),
                    name = "Daily travel\nDistance (km)",
                    breaks = seq(0, 8, 1), limits = c(0, 8)) +
  ggforce::facet_row( ~ animal_sex, scales = "free", space = "free") +
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 8, label.vjust = 0.9)) +
  labs(x = NULL, y = NULL, title = "PET CATS U.K. STUDY: DAILY TRAVEL DISTANCE", 
       caption = "Source: Movebank for Animal Tracking Data via Data is Plural")+
  theme_custom +
  theme(legend.text = element_markdown(),
        legend.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        plot.title = element_text(margin = margin(b = 0))) 
ggsave("20230131_Pet_Cats_UK/cat_daily_dist.png", width = 6.5, height = 8)




height <- max(cats_uk_cleaned$location_lat) - min(cats_uk_cleaned$location_lat)
width <- max(cats_uk_cleaned$location_long) - min(cats_uk_cleaned$location_long)
borders <- c(bottom  = min(cats_uk_cleaned$location_lat)  - 0.05 * height, 
             top     = max(cats_uk_cleaned$location_lat)  + 0.05 * height,
             left    = min(cats_uk_cleaned$location_long) - 0.05 * width,
             right   = max(cats_uk_cleaned$location_long) + 0.05 * width)
map <- get_stamenmap(borders, zoom = 9, maptype = "toner-lite")

p_map <- ggmap(map) +
  geom_point(data = cats_uk_cleaned %>%
               group_by(tag_id, animal_sex) %>%
               summarise(location_lat = median(location_lat),
                         location_long = median(location_long)),
             aes(x = location_long, y = location_lat, color = animal_sex, shape = animal_sex),
             size = 1.5) +
  scale_color_brewer(palette = "Set1", name = "Sex", labels = c("Female", "Male")) +
  scale_shape_discrete(name = "Sex", labels = c("Female", "Male")) +
  theme_custom +
  labs(title = "PET CATS U.K. STUDY: CAT LOCATIONS", caption = "Source: Movebank for Animal Tracking Data via Data is Plural")+
  theme(legend.position = c(0.1, 0.9),
        legend.background = element_blank(),
        legend.text = element_text(color = "grey15", size = 11),
        legend.title = element_text(color = "grey15", size = 11.5, face = "bold"),
        plot.title.position = "panel",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

ggsave("20230131_Pet_Cats_UK/cat_map.png", width = 5, height = 5, dpi = 500)
