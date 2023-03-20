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
  plot.caption = element_text(hjust = 0, size = 9.5),
  plot.subtitle = element_text(margin = margin(-19, 5, 5, 5), hjust = 1),
  legend.title = element_text(size = 11),
  legend.text = element_text(size = 10),
  strip.text = element_text(margin = margin(t = 2, b = 5), size = 11)
)

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv') %>%
  mutate(book_long = ifelse(book == "Gardner", "Gardner: Art Through the Ages", "Janson: History of Art"))

source_caption <- "Source: Lemus S, Stam H (2022). arthistory: Art History Textbook Data. https://github.com/saralemus7/arthistory,\nhttps://saralemus7.github.io/arthistory/."

# Gender -------------------------
artists_summ_gender <- artists %>%
  group_by(book_long, edition_number, year, artist_gender) %>%
  summarise(n = n()) %>%
  group_by(book_long, edition_number, year) %>%
  mutate(perc = n/sum(n)) %>%
  ungroup %>%
  arrange(desc(artist_gender))

p_gender_perc_bar <- artists_summ_gender %>%
  ggplot(aes(x = factor(edition_number), y = n, fill = artist_gender)) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.8) +
  facet_grid( ~ book_long, space = "free", scales = "free") +
  scale_y_continuous(labels = function(x) {paste0(x * 100, "%")}, expand = c(0, 0)) +
  scale_fill_manual(values = c(swatch[5], swatch[2], "grey"), name = "Gender") +
  labs(x = NULL, y = "Percentage", subtitle = "@akela@mstdn.social",
       title = toupper("Female artists are under-represented in art history textbooks")) +
  guides(fill = guide_legend(keywidth = 0.8, keyheight = 0.8)) +
  theme_custom +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(l = 5, r = -3)))

p_gender_n <- artists_summ_gender %>%
  ggplot(aes(x = factor(edition_number), y = n, color = artist_gender)) +
  geom_line(aes(group = artist_gender)) +
  geom_point(aes(shape = artist_gender)) +
  facet_grid( ~ book_long, space = "free", scales = "free") +
  scale_color_manual(values = c(swatch[5], swatch[2], "grey"), name = "Gender") +
  scale_shape_discrete(name = "Gender") +
  guides(color = guide_legend(keywidth = 0.8, keyheight = 0.8),
         shape = guide_legend(keywidth = 0.8, keyheight = 0.8)) +
  labs(x = NULL, y = "# Artists") +
  theme_custom +
  theme(strip.text = element_blank(),
        plot.margin = margin(0, 5, 5, 5),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(l = 5, r = -3)))

p_gender_page_ratio <- artists %>%
  filter(artist_gender != "N/A") %>%
  arrange(artist_gender) %>%
  mutate(artist_gender = factor(artist_gender)) %>%
  ggplot(aes(x = factor(edition_number), y = space_ratio_per_page_total, color = artist_gender))+
  geom_boxplot(aes(fill = artist_gender), alpha = 0.7, width = 0.6, linewidth = 0.4,
               outlier.shape = 16, outlier.size = 0.5, outlier.alpha = 0.5, 
               position = position_dodge(width = 0.65, preserve = "single")) +
  facet_grid( ~ book_long, space = "free", scales = "free") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), trans = "log1p") +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "grey30"), name = "Gender") +
  scale_fill_manual(values = c(swatch[5], swatch[2], "grey"), name = "Gender") +
  guides(color = guide_legend(keywidth = 0.8, keyheight = 0.8),
         fill = guide_legend(keywidth = 0.8, keyheight = 0.8)) +
  labs(x = "Edition", y = "Total Space\nRatio Per Page", 
       caption = str_replace(source_caption, "\n", " ")) +
  theme_custom +
  theme(strip.text = element_blank(),
        plot.margin = margin(0, 5, 5, 5),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(margin = margin(l = 5, r = -3)))

plot_grid(p_gender_perc_bar, p_gender_n, p_gender_page_ratio, ncol = 1, align = "v",
          rel_heights = c(1, 0.4, 0.45))
ggsave("20230117_Art_History/artist_gender_book.png", width = 8.5, height = 5.5)



# Race/Ethnicity ---------------------
artists_summ_race <- artists %>%
  group_by(book_long, edition_number, year, artist_race, artist_ethnicity) %>%
  summarise(n = n()) %>%
  group_by(book_long, edition_number, year) %>%
  mutate(perc = n/sum(n)) %>%
  ungroup %>%
  mutate(artist_race = str_replace(artist_race, " or ", "/")) %>%
  mutate(artist_race = str_replace(artist_race, "Other", "\nOther")) %>%
  mutate(artist_race = ifelse(!is.na(artist_ethnicity) & artist_ethnicity == "Hispanic or Latino origin",
                              paste0(artist_race, ",\nHispanic/Latino"), artist_race)) %>%
  mutate(artist_race = factor(artist_race, c("White", "White,\nHispanic/Latino", 
                                             "Black/African American", "Black/African American,\nHispanic/Latino", 
                                             "Asian", "Native Hawaiian/\nOther Pacific Islander", 
                                             "American Indian/Alaska Native", "N/A")))

p_race_perc_bar <- artists_summ_race %>%
  ggplot(aes(x = factor(edition_number), y = n, fill = artist_race)) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.8) +
  facet_grid( ~ book_long, space = "free", scales = "free") +
  scale_y_continuous(labels = function(x) {paste0(x * 100, "%")}, expand = c(0, 0)) +
  labs(x = NULL, y = "Percentage", subtitle = "@akela@mstdn.social",
       title = toupper("POC artists have been largely overlooked by art history textbooks")) +
  scale_fill_manual(values = c(brewer.pal(10, "Paired")[1:6], "grey", "grey35"), name = NULL) +
  theme_custom +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.subtitle = element_text(margin = margin(-4, 5, 0, 5), hjust = 0, size = 10),
        legend.position = "none")

p_race_n <- artists_summ_race %>%
  ggplot(aes(x = factor(edition_number), y = n, group = artist_race, color = artist_race)) +
  geom_line(show.legend = FALSE) +
  geom_point() +
  facet_grid( ~ book_long, space = "free", scales = "free") +
  scale_fill_manual(values = c(brewer.pal(10, "Paired")[1:6], "grey", "grey35"), name = NULL) +
  scale_color_manual(values = c(brewer.pal(10, "Paired")[1:6], "grey", "grey35"), name = NULL) +
  guides(color = guide_legend(nrow = 2, keyheight = 1, keywidth = 0.5,
                              override.aes = list(alpha = 1, size = 3, shape = 15))) +
  labs(x = NULL, y = "# Artists", caption = source_caption) +
  theme_custom +
  theme(strip.text = element_blank(),
        plot.margin = margin(0, 5, 5, 5),
        legend.position = "bottom",
        legend.key = element_rect(fill = NA),
        legend.box.margin = margin(b = 5, l = -45),
        legend.text = element_text(size = 9.5, lineheight = 0.8, vjust = 0.6))

plot_grid(p_race_perc_bar, p_race_n, ncol = 1, align = "v", rel_heights = c(1, 0.85))
ggsave("20230117_Art_History/artist_race_book.png", width = 6.3, height = 5)


# Museum Exhibition --------------
p_exhibit_gender <- artists %>%
  select(year, artist_name, artist_unique_id, artist_gender, 
         moma_count_to_year, whitney_count_to_year) %>%
  distinct() %>%
  filter(artist_gender != "N/A") %>%
  group_by(artist_unique_id) %>%
  filter(max(whitney_count_to_year) >= 1 & max(moma_count_to_year) >= 1) %>%
  arrange(year) %>%
  pivot_wider(names_from = c("year"), values_from = c("moma_count_to_year", "whitney_count_to_year")) %>%
  pivot_longer(4:ncol(.), values_to = "count", names_to = "group") %>%
  mutate(year = str_extract(group, "[0-9]+"),
         museum = str_extract(group, "moma|whitney")) %>%
  mutate(museum = ifelse(museum == "moma", "Museum of Modern Art (MoMA)", "The Whitney")) %>%
  ggplot(aes(x = factor(year), y = forcats::fct_reorder(artist_name, count, max, .na_rm = TRUE), 
             fill = count)) +
  geom_tile(color = "white") +
  labs(x = NULL, y = NULL) +
  facet_grid(artist_gender ~ museum, space = "free", scales = "free", switch = "y") +
  coord_cartesian(expand = FALSE) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 10)) +
  scale_fill_stepsn(colors = rev(brewer.pal(11, "Spectral")), breaks = seq(5, 60, 5),
                    name = "Total #\nExhibit.", right = TRUE, na.value = "#ecf0f1") +
  theme_custom +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8.5),
        strip.placement = "outside",
        strip.text.x = element_text(face = "bold", size = 11),
        strip.text.y = element_text(face = "bold", size = 11))


artist_summ_museum <- artists %>%
  select(year, artist_name, artist_unique_id, artist_gender, artist_race, artist_ethnicity,
         moma_count_to_year, whitney_count_to_year) %>%
  distinct() %>% ungroup() %>%
  filter(artist_gender != "N/A") %>%
  group_by(artist_unique_id) %>%
  arrange(year) %>%
  slice(n()) %>% 
  pivot_longer(cols = c("moma_count_to_year", "whitney_count_to_year"),
               values_to = "count", names_to = "museum") %>%
  mutate(museum = ifelse(museum == "moma_count_to_year", "Museum of Modern\n Art (MoMA)", "The Whitney"))  %>%
  mutate(label_y = ifelse((artist_gender == "Male" & count > 40) | (artist_gender == "Female" & count > 20), count, NA))

p_boxplot_gender <- artist_summ_museum %>%
  ggplot(aes(x = museum, y = count, fill = artist_gender, color = artist_gender)) +
  geom_violin(scale = "width", width = 0.8, alpha = 0.5) +
  geom_point(shape = 16, size = 1, position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2, seed = 42)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8"), name = "Gender") +
  geom_text_repel(aes(y = label_y, label = artist_name), #str_replace(artist_name, " ", "\n")),
                  hjust = 0, size = 3, family = "Dubai", lineheight = 0.8, box.padding = 0.1, show.legend = FALSE,
                  position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2, seed = 42)) +
  labs(x = NULL, y = NULL, title = toupper("total # Exhibitations by 2020")) +
  scale_fill_manual(values = c(swatch[5], swatch[2]), name = "Gender") +
  theme_custom +
  theme(plot.margin = margin(5, 5, 5, 25),
        plot.background = element_blank())

p_boxplot_race <- artist_summ_museum %>%
  filter(artist_race != "N/A") %>%
  mutate(artist_race = str_replace(artist_race, " or ", " or\n")) %>%
  ggplot(aes(x = museum, y = count, fill = artist_race, color = artist_race)) +
  geom_violin(scale = "width", width = 0.8, alpha = 0.5) +
  geom_point(shape = 16, size = 1, position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2, seed = 42)) +
  labs(x = NULL, y = NULL, title = toupper("total # Exhibitations by 2020")) +
  theme_custom +
  theme(plot.margin = margin(5, 5, 5, 25),
        plot.background = element_blank())

plot_grid(p_boxplot_gender, p_boxplot_race, ncol = 1, align = "v") %>%
  plot_grid(p_exhibit_gender + theme(plot.margin = margin(5, -20, 5, 5)), ., nrow = 1) +
  theme(plot.background = element_rect(fill = "white"))
ggsave("20230117_Art_History/exhibition.png", width = 15, height = 9)


