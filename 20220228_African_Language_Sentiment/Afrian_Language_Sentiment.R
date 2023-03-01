library(tidyverse)
library(ggtext)
library(extrafont)
library(ggrepel)
library(maptools)
library(grid)

fresh_palette <- c("#65ADC2", "#E84646", "lightsteelblue4", "#C29365", "#168E7F")
theme_light <- ggthemr::ggthemr("light")
theme_custom <- theme(
  text = element_text(family = "Corbel", color = "#373634"),
  plot.title = element_text(hjust = 0.5, size = 10.5, face = "bold"),
  legend.title = element_text(size = 10.5, face = "bold"),
  strip.clip = "off",
  axis.text.x = element_text(angle = 40, hjust = 1, size = 9),
  panel.spacing.y = unit(1.1, "mm"),
  panel.border = element_rect(color = "#ababab", fill = NA, linewidth = 0.3),
  axis.line = element_blank()
)

# Load Data --------------------------
# tuesdata <- tidytuesdayR::tt_load('2023-02-28')

afrisenti <- tuesdata$afrisenti
languages <- tuesdata$languages
language_scripts <- tuesdata$language_scripts
language_countries <- tuesdata$language_countries
country_regions <- tuesdata$country_regions

# Data Wrangling ------------------------
## Add language family from Figure 2 of the paper (https://arxiv.org/pdf/2302.08956.pdf)
afro_asiatic <- c("Amharic", "Oromo", "Tigrinya", "Algerian Arabic/Darja",
                  "Moroccan Arabic/Darija", "Hausa")
niger_congo <- c("Yorùbá", "Igbo", "Twi", "Swahili", "Xitsonga", "Kinyarwanda")


afrisenti_summ <- afrisenti %>%
  group_by(language_iso_code, label) %>%
  summarise(n = n()) %>%
  group_by(language_iso_code) %>% 
  mutate(percent = n/sum(n))  

languages <- languages %>%
  mutate(language_family = case_when(
    language %in%  afro_asiatic ~ "Afro-Asiatic",
    language == "Nigerian Pidgin" ~ "English Creole",
    language == "Mozambican Portuguese" ~ "Indo-European",
    language %in% niger_congo ~ "Niger-Congo")) %>%
  left_join(subset(afrisenti_summ, label == "positive") %>%
              select(language_iso_code, percent), by = "language_iso_code") %>%
  mutate(language = forcats::fct_reorder(language, percent, .desc = TRUE)) %>%
  select(-percent)

languages_summ <- languages %>%
  left_join(language_scripts, by = "language_iso_code", multiple = "all") %>%
  left_join(language_countries, by = "language_iso_code", multiple = "all") %>%
  left_join(country_regions, by = "country", multiple = "all") %>%
  mutate(region = ifelse(country == "Mozambique", "Southeast", region)) %>%
  mutate(region = str_remove(region, " Africa")) %>%
  mutate(region = ifelse(!str_detect(region, "ern$"), paste0(region, "ern"), region)) %>%
  mutate(region = factor(region, paste0(c("North", "West", "South", "Southeast", "East"), "ern"))) %>%
  arrange(language_family, region, country) %>%
  mutate(country = factor(country, unique(.$country)))


# Plots ------------------------------
## Stacked % bar chart of Twitter sentiment by language, grouped by language family -------------------
p_language <- afrisenti_summ %>% 
  left_join(languages, by = "language_iso_code") %>%
  ggplot(aes(x = n, y = language, fill = forcats::fct_rev(label))) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity", 
           width = 0.85, color = "#ababab", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_fill(reverse = TRUE, vjust = 0.5), 
            size = 3, family = "Corbel", color = "#373634") +
  facet_grid(language_family ~ ., scale = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(labels = function(x) {paste0(x * 100, "%")}, breaks = seq(0, 1, 0.2)) +
  scale_y_discrete(limits = rev, labels = function(x) {str_replace(x, " ", "\n")}) +
  scale_fill_manual(name = "SENTIMENT", labels = function(x) {str_to_title(x)},
                    values = c("#ffb84d", "grey85", "#62bba5")) +
  coord_cartesian(expand = FALSE) +
  guides(fill = guide_legend(keywidth = 0.8, keyheight = 0.8)) +
  labs(x = NULL, y = NULL) +
  theme_custom +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y.left = element_blank(),
        legend.position = "top",
        legend.box.margin = margin(b = -4))

## Language by country/region spoken in ---------------
p_region <- languages_summ %>%
  ggplot(aes(x = country, y = language, fill = region)) +
  geom_tile(color = "#f6f1eb", linewidth = 0.6) +
  geom_hline(yintercept = 0:6 + 0.5, color = "#ababab", linewidth = 0.4, linetype = 3) +
  geom_vline(xintercept = 0:13 + 0.5, color = "#ababab", linewidth = 0.4, linetype = 3) +
  geom_tile(color = "#f6f1eb", linewidth = 0.6) +
  scale_y_discrete(limits = rev, labels = function(x) {str_replace(x, " ", "\n")}) +
  facet_grid(language_family ~ ., scale = "free", space = "free", switch = "y") +
  coord_cartesian(expand = FALSE) +
  scale_fill_manual(values = fresh_palette) +
  labs(x = NULL, y = NULL, title = toupper("Countries spoken in")) +
  theme_custom + 
  theme(strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.spacing.x = unit(0, "mm"))

## No. of tweets by language ----------------------
p_n_tweets <- afrisenti %>%
  group_by(language_iso_code) %>%
  summarise(n = n()) %>%
  left_join(languages, by = "language_iso_code") %>%
  mutate(language = factor(language, levels(languages$language))) %>%
  ggplot(aes(y = language, x = n)) +
  geom_col(width = 0.85, fill = "#353525") +
  facet_grid(str_replace(language_family, "\\-", "-\n") %>% str_replace(" ", "\n") ~ ., 
             scale = "free_y", space = "free_y", switch = "y") +
  scale_x_reverse(labels = function(x) {
                  x = paste0(x/1E3, "K"); x[x == '0K'] <- 0; x })+
  scale_y_discrete(limits = rev, labels = function(x) {str_replace(x, " ", "\n")})+
  labs(x = NULL, y = NULL, title = toupper("Language    Family  No. of tweets"),
       caption = "@akela@mstdn.social") +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_custom + 
  theme(plot.title = element_text(hjust = 1.12),
        plot.caption = element_text(size = 10.5, hjust = 4.5, vjust = 1,
                                    face = "bold", margin = margin(t = 5, l = 0)),
        panel.border = element_rect(color = "#ababab", fill = NA, linewidth = 0.3),
        strip.text.y.left = element_text(angle = 0, margin = margin(l = 2, r = 2), size = 9),
        strip.background = element_rect(fill = "#ddd8d3", color = "#ababab", linewidth = 0.3),
        panel.grid.major.x = element_line(color = "#ababab", linewidth = 0.4, linetype = 3),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(5, 5, 5, 10),
        axis.text.y = element_text(face = "bold")
        )


## Map of countries involved in the study ---------------------
data(wrld_simpl)
afr <- wrld_simpl[wrld_simpl$REGION==2,]
afr_tidy <- broom::tidy(afr) %>%
  left_join(afr@data, by = c("id" = "ISO3")) %>%
  mutate(NAME = case_when(
    NAME == "United Republic of Tanzania" ~ "Tanzania", 
    NAME == "Swaziland" ~ "Eswatini",
    NAME == "Western Sahara" ~ "Morocco",
    id == "LSO" ~ "Lesotho",
    TRUE ~ as.character(NAME))) %>% 
  left_join(languages_summ %>% select(country, region) %>% distinct(), 
            by = c("NAME" = "country")) 


afr_tidy_summ <- afr_tidy %>% 
  filter(!is.na(region)) %>%
  select(NAME, region, long = LON, lat = LAT) %>%
  distinct() %>%
  group_by(NAME, region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  mutate(text_col = ifelse(region %in% c("Eastern", "Western", "Southern"), "white", "black"))

data_source <- paste(
  c('**Source**: Muhammad, Shamsuddeen Hassan, et al. "AfriSenti: A',
    'Twitter Sentiment Analysis Benchmark for African Languages."',
    'arXiv preprint arXiv:2302.08956 (2023).'), 
  collapse = "<br/>")
data_descript <- paste0(
  "Sentiment analysis dataset covering 110,000+ annotated\n",
  "tweets in 14 African languages")

p_map <- afr_tidy %>%
  ggplot(aes(x = long, y = lat, fill = region)) + 
  geom_polygon(aes(group = group), color = "#ababab", linewidth = 0.3, fill = "#f6f1eb") +
  geom_polygon(data = subset(afr_tidy, !is.na(region)), aes(group = group), 
               color = "#373634", linewidth = 0.4) +
  geom_polygon(data = subset(afr_tidy, id == "LSO"), aes(group = group), color = "#373634", 
               linewidth = 0.4, fill = "#f6f1eb") +
  geom_label_repel(data = afr_tidy_summ, aes(label = NAME, color = text_col),
                   box.padding = 0.45, hjust = 0.5, vjust = 0.5, max.overlaps = Inf,
                   label.padding = 0.18, family = "Corbel", size = 3, show.legend = FALSE,
                   label.size = 0.3, segment.size = 0.3) +
  geom_text(data = data.frame(x = -25, y = 47.5, label = data_descript), 
            aes(x = x, y = y, label = label), vjust = 1, hjust = 0, size = 4, 
            family = "Corbel", color = "#373634", 
            lineheight = 1, inherit.aes = FALSE) +
  scale_color_identity() +
  scale_fill_manual(values = fresh_palette, na.translate = FALSE, name = "REGION") +
  scale_y_continuous(limits = c(-50, 48)) +
  guides(fill = guide_legend(keywidth = 0.8, keyheight = 0.8)) +
  labs(title = "AfriSenti", caption = data_source) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_custom + 
  theme(legend.position = c(0.2, 0.17),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.background = element_blank(), 
        plot.background = element_rect(fill = "#373634"),
        plot.title = element_text(size = 18, hjust = 0, margin = margin(b = 0), 
                                  face = "bold", family = "Corbel"),
        plot.caption = element_markdown(size = 10, hjust = 0, vjust = 1, 
                                        lineheight = 1.1, margin = margin(t = -3)))



## Arrange panels ----------------------------
g <- egg::ggarrange(p_n_tweets, p_language, p_region, p_map, nrow = 1, 
                    widths = c(0.25, 1, 0.65, 0.8))
ggsave("20220228_African_Language_Sentiment/african_language.png",
       g, height = 6, width = 15, dpi = 600)


