library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(extrafont)
library(ggtext)
library(ggrepel)
library(grid)

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
  plot.caption = element_text(hjust = 0, size = 11),
  plot.subtitle = element_text(margin = margin(-18, 5, 5, 5), hjust = 1),
  legend.title = element_text(size = 11),
  legend.text = element_text(size = 10),
  strip.text = element_markdown(size = 11, vjust = 1)
)


# Read Data ----------------------
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')
source_caption <- "Source: Programming Language DataBase (PLDB.com)"
folder <- "20230321_Programming_Languages/"



  
# Language Rank vs. Github Repo Metrics --------------------
octicon <- data.frame(metric = c("Forks", "Issues", "Stars", "Subscribers"),
                      octicon = paste0("<img src='", folder, "octicon/", 
                                       c("git-branch", "issue-opened", "star", "eye"), 
                                       ".png' width='10'/>"))

languages_summ_rank_github <- languages %>%
  select(pldb_id, title, github_language_type, language_rank, github_repo_stars, github_repo_forks, 
         github_repo_subscribers, github_repo_issues) %>%
  pivot_longer(cols = starts_with("github_repo_"),
               names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(metric = str_remove(metric, "github_repo_"),
         metric = str_to_title(metric)) %>%
  left_join(octicon, by = "metric") %>%
  mutate(metric = paste0(octicon, "<span> ", metric, "</span>")) %>%
  mutate(metric = factor(metric)) %>%
  mutate(metric = factor(metric, levels(metric)[c(2, 4, 1, 3)]))

rank_github_label_df <- subset(languages_summ_rank_github, 
                   (str_detect(metric, "Forks")  & value > 6000) | 
                   (str_detect(metric, "Issues") & value > 2500) | 
                   (str_detect(metric, "Stars")  & value > 35000) | 
                   (str_detect(metric, "Subscribers") & 
                    (value > 850 | (language_rank > 1500 & value > 100)))) %>%
  mutate(nudge_x = ifelse(title == toupper(title), 2, 0))

languages_summ_rank_github %>%
  ggplot(aes(x = language_rank, y = value)) +
  geom_point(na.rm = TRUE, shape = 16, size = 1.2) +
  geom_text_repel(data = rank_github_label_df, aes(label = title), seed = 42,
                  size = 3, family = "Dubai", box.padding = 0.2,
                  max.overlaps = Inf, nudge_x = 100) +
  facet_wrap(~ metric, scales = 'free') +
  labs(x = "Rank", y = NULL, 
       title = toupper("Programming Languages: Rank vs. GitHub Repo Metrics"),
       caption = source_caption, subtitle = "@akela@mstdn.social") +
  theme_custom

ggsave(paste0(folder, "language_rank_github.png"),
       width = 6.5, height = 6.5)

# Language Rank vs. # Books/#Papers --------------------
languages_summ_rank_pub <- languages %>%
  select(pldb_id, title, language_rank, isbndb, semantic_scholar) %>%
  pivot_longer(cols = c("isbndb", "semantic_scholar"),
               names_to = "pub", values_to = "value") %>%
  mutate(pub = ifelse(pub == "isbndb", "Books", "Papers"))

rank_pub_label_df <- subset(languages_summ_rank_pub, 
  title %in% c("Python", "R") |
  (pub == "Books"  & (value > 200 | (language_rank > 500 & value > 50))) |
  (pub == "Papers" & (value > 40  | (language_rank > 800 & value > 15))) ) %>%
  mutate(color = ifelse(title %in% c("Python", "R"), swatch[5], swatch[1]))  %>%
  mutate(face = ifelse(title %in% c("Python", "R"), 2, 1)) %>%
  mutate(nudge_x = case_when(title == "R" & pub == "Papers" ~ 500, 
                             title == "R" & pub == "Books" ~ 300, 
                             pub == "Papers" ~ 100,
                             TRUE ~ 0),
         nudge_y = ifelse(title == "R", 1, 0))

languages_summ_rank_pub %>%
  ggplot(aes(x = language_rank, y = value)) +
  geom_point(na.rm = TRUE, shape = 16, size = 1.2) +
  geom_point(data = subset(languages_summ_rank_pub, title %in% c("R", "Python")), 
             color = "#d35400", shape = 16, size = 1.4) +
  facet_wrap(~ pub, scales = 'free_y') +
  geom_text_repel(data = rank_pub_label_df, aes(label = title, color = color, fontface = face),
                  nudge_x = rank_pub_label_df$nudge_x, nudge_y = rank_pub_label_df$nudge_y, 
                  size = 3, family = "Dubai", box.padding = 0.2, seed = 42) +
  scale_color_identity() +
  labs(x = "Rank", y = NULL, 
       title = toupper("Programming Languages: Rank vs. # Publications"),
       caption = source_caption, subtitle = "@akela@mstdn.social") +
  theme_custom
ggsave(paste0(folder, "language_rank_publications.png"),
       width = 7, height = 4.5)


# features ------------------
languages %>%
  filter(!is.na(line_comment_token)) %>%
  group_by(line_comment_token) %>%
  filter(n() > 2) %>%
  ggplot(aes(x = language_rank, color = line_comment_token, fill = line_comment_token,
             y = forcats::fct_reorder(line_comment_token, language_rank))) +
  geom_violin(scale = "width", width = 0.8, alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = NA) +
  geom_point(position = position_jitter(height = 0.2), shape = 16, size = 1.2) +
  labs(x = "Rank", y = NULL) +
  theme(legend.position = "none")


df <- languages %>%
  filter(!is.na(line_comment_token)) %>%
  group_by(line_comment_token) %>%
  mutate(line_comment_token = ifelse(n() <= 5, "Other", line_comment_token)) %>%
  mutate(n = n()) %>% ungroup %>%
  mutate(line_comment_token = forcats::fct_reorder(line_comment_token, n, .desc = TRUE)) %>%
  mutate(line_comment_token = factor(line_comment_token, c(setdiff(levels(line_comment_token), "Other"), "Other")))

df %>% ggplot(aes(x = appeared, y = language_rank)) +
  geom_point(aes(color = line_comment_token))  +
  geom_text_repel(data = subset(df, !is.na(line_comment_token) & 
                                (language_rank > 2200 | appeared < 1963 | 
                                 (appeared > 2015 & language_rank < 150) |
                                 (appeared < 1975 & language_rank > 500)) ),
                  aes(label = title), size = 3, family = "Dubai", nudge_x = 1, max.overlaps = Inf) +
  scale_color_manual(values = c("#3498db", "#A6D854", "gold", "orange", "brown2", 
                                "#9b59b6", "#1abc9c", "#34495e"),
                     name = "Comment\nToken") +
  scale_y_reverse() +
  labs(x = "Year Released/Announced", y = "Rank", 
       title = toupper("Programming Languages with Line Comment Tokens"),
       caption = source_caption, subtitle = "@akela@mstdn.social") +
  theme_custom

df %>% 
  count(appeared) %>%
  ggplot(aes(x = factor(appeared), y = n)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_custom +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



# Year Released vs Updated ---------------------
df <- languages %>%
  filter(type == "pl") %>%
  filter(appeared < last_activity) 

df %>%
  ggplot(aes(x = appeared, y = last_activity)) +
  geom_point(aes(color = -language_rank), shape = 16, size = 1.2) +
  geom_text_repel(data = subset(df, (appeared < 1960 & last_activity > 2010) | last_activity < 2000),
                  aes(label = title), size = 3, family = "Dubai",
                  max.overlaps = Inf, nudge_x = 1) +
  coord_equal(xlim = range(c(df$appeared, df$last_activity)),
              ylim = range(c(df$appeared, df$last_activity))) +
  scale_color_stepsn(colors = viridis::plasma(10), limits = c(-3000, 0),
                     name = "Rank", labels = function(x) {-x},
                     breaks = sort(c(-100, -seq(0, 4000, 500)))) +
  labs(x = "Year Released / Announced", y = "Last Activity", 
       title = toupper("Programming Languages: Released vs. Last Activity"),
       caption = source_caption, subtitle = "@akela@mstdn.social") +
  theme_custom +
  theme(legend.key.height = unit(10, "mm"),
        legend.key.width = unit(2, "mm"),
        plot.subtitle = element_text(size = 10))

ggsave(paste0(folder, "language_released_updated.png"), width = 6, height = 5.5)
