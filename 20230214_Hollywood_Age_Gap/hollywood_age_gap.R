library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(extrafont)
library(grid)

theme_flat <- ggthemr::ggthemr("flat")
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')
raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')

age_gaps_cleaned <- age_gaps %>%
  left_join(subset(raw_bechdel, !is.na(imdb_id)), 
            c("movie_name" = "title", "release_year" = "year")) %>%
  mutate(group = case_when(character_1_gender == character_2_gender &  character_1_gender == "man" ~ "M / M",
                           character_1_gender == character_2_gender ~ "F / F",
                           character_1_gender == "man" ~ "M > F",
                           character_1_gender == "woman" ~ "F > M")) %>%
  mutate(group = factor(group, c("M > F", "F > M", "M / M", "F / F"))) %>%
  mutate_at(vars(ends_with("gender")), str_to_title) %>%
  mutate(movie_name = str_replace(movie_name, "Indiana Jones and the ", "Indiana Jones...")) %>%
  mutate(movie_name = str_replace(movie_name, "and", "&")) %>%
  mutate(movie_name = str_remove(movie_name, " of the Spotless Mind")) #%>%
  #mutate(rating = as.character(rating)) %>%
 # mutate(rating = replace_na(rating, "Unscored"))

theme_custom <- theme(
  text = element_text(family = "Franklin Gothic Medium Cond"),
  plot.background = element_rect(fill = theme_flat$palette$background),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(linetype = 3, linewidth = 0.4)
)

age_gaps_over_25 <- age_gaps_cleaned %>%
  filter(age_difference >= 25) %>% 
  arrange(desc(age_difference)) %>%
  group_by(movie_name) %>%
  slice(1) %>%
  arrange(desc(age_difference)) %>%
  mutate(movie_name = factor(movie_name, unique(.$movie_name))) 

p1 <- age_gaps_over_25 %>%
  ggplot(aes(y = movie_name))+
  geom_point(aes(x = actor_1_age, color = character_1_gender)) +
  geom_point(aes(x = actor_2_age, color = character_2_gender)) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_color_manual(values = c("#3498db", "#e74c3c"), name = "Gender") +
  scale_y_discrete(limits = rev) +
  labs(x = toupper("Actor Ages"), y = NULL) + 
  guides(fill = guide_legend(keywidth = 0.8, keyheight = 0.8)) +
  theme_custom +
  theme(strip.text.y = element_blank(),
        legend.position = "none")
  

p2 <- age_gaps_over_25 %>%
  ggplot(aes(x = age_difference, y = movie_name))+
  geom_col(aes(fill = factor(rating)), width = 0.8) +
  geom_point(aes(color = character_1_gender), alpha = 0) +
  geom_vline(xintercept = 25, color = "black", linetype = "31", linewidth = 0.4) +
  coord_cartesian(expand = FALSE, clip = "off") +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_y_discrete(limits = rev) +
  labs(x = "AGE GAP", y = NULL) +
  scale_color_manual(values = c("#3498db", "#e74c3c"), name = "Gender") +
  scale_fill_manual(values = c("#C6DBEF", "#75b9e7", "#217dbb", "#34495e"),
                    name = "Bechdel Test", na.value = "grey") +
  guides(fill = guide_legend(keywidth = 0.8, keyheight = 0.8),
         color = guide_legend(keywidth = 0.8, keyheight = 0.8,
                              override.aes = list(alpha = 1, size = 2, fill = NA))) +
  theme_custom +
  theme(plot.margin = margin(5, 5, 5, 0),
        strip.text.y.right = element_text(angle = 0, hjust = 0, face = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom", 
        legend.box.margin = margin(l = -200, t = -2, b = 5),
        legend.text = element_text(size = 11)) 

g <- egg::ggarrange(
  p1, p2, widths = c(1, 0.6),
  top = textGrob(c("MOVIES WITH LOVE INTERESTS' AGE GAP > 25", "@akela@mstdn.social"),
                 x = c(0.01, 0.99), hjust = c(0, 1), vjust = 0.6,
                 gp = gpar(fontfamily = "Franklin Gothic Medium Cond", fontsize = c(15, 12))),
  bottom = textGrob(toupper("Source: Hollywood Age Gap via Data Is Plural"), x = 0.01, hjust = 0, vjust = 0.1,
                    gp = gpar(fontfamily = "Franklin Gothic Medium Cond", fontsize = 12))
)

ggsave("20230214_Hollywood_Age_Gap/hollywood_age_gap.png", 
       g, width = 6, height = 11, dpi = 500)

