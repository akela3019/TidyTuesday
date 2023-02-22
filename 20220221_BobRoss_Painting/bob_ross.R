library(tidyverse)
library(cowplot)
library(ggthemr)
library(ggtext)
library(extrafont)

tuesdata <- tidytuesdayR::tt_load('2023-02-21')

bob_ross <- tuesdata$bob_ross
bob_ross_cleaned <- bob_ross %>%
  select(painting_index, painting_title, season, episode, colors, color_hex) %>%
  mutate(color_hex = str_remove_all(color_hex, "\\]|\\[|\\'"),
         colors = str_remove_all(colors, "\\]|\\[|\\'")) %>%
  mutate(color_hex = str_split(color_hex, ", "),
         colors = str_split(colors, ", ")) %>%
  unnest(colors, color_hex) %>%
  mutate(colors = str_remove_all(colors, "\\r\\n")) 

ggthemr("grape")
bob_ross_cleaned %>%
  group_by(season, color_hex) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = season, y = n, fill = color_hex)) +
  geom_col(color = "grey15", width = 1, position = "fill", alpha = 0.9) +
  scale_fill_identity() +
  theme_cowplot() +
  coord_cartesian(expand = FALSE)

bob_ross_cleaned %>%
  mutate(colors = str_extract(colors, "[a-zA-Z ]+")) %>%
  mutate(colors = paste0(colors, " (", color_hex, ")")) %>%
  ggplot(aes(x = season, y = episode, fill = color_hex)) +
  geom_tile(color = "grey70", linewidth = 0.3) +
  scale_fill_identity() +
  coord_cartesian(expand = FALSE) +
  labs(x = "Season", y = "Episode", title = "BOB ROSS' <i>THE JOY OF PAINTING</i>",
       subtitle = toupper("Color Usage by Season & Episode"), 
       tag = "@akela@mstdn.social") +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  facet_wrap(~ fct_reorder(colors, as.numeric(as.factor(color_hex)), .desc = TRUE), ncol = 3) +
  theme(text = element_text(family = "Corbel"),
        plot.title = element_markdown(family = "Franklin Gothic Medium"),
        plot.subtitle = element_markdown(margin = margin(t = -3, b = 3), family = "Franklin Gothic Medium"),
        plot.tag.position = "topright",
        plot.tag = element_text(size = 10, margin = margin(5, 0, -5, -100)),
        plot.margin = margin(5, 15, 5, 5),
        panel.grid = element_blank(),
        panel.spacing.x = unit(3, "mm"),
        panel.spacing.y = unit(0, "mm"),
        panel.border = element_rect(color = "grey70", linewidth = 1, fill = NA),
        axis.line = element_blank(),
        axis.title = element_text(face = "bold"))

ggsave("bob_ross_paintings.png", dpi = 600, width = 8, height = 8)

