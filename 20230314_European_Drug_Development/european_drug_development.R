library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(extrafont)
library(ggtext)
library(ggrepel)
library(grid)

drugs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')
View(drugs)
theme_flat <- ggthemr::ggthemr("flat")
theme_custom <- theme(
  text = element_text(family = "Franklin Gothic Medium Cond"),
  panel.grid.major.x = element_line(linetype = 3, linewidth = 0.4),
  panel.grid.major.y = element_line(linetype = 3, linewidth = 0.4),
  plot.title.position = "plot",
  plot.title = element_text(size = 12, family = "Dubai"),
  plot.background = element_blank()
)


drugs_human <- subset(drugs, category == "human") %>%
  mutate(authorisation_status = str_to_title(authorisation_status)) %>%
  mutate(authorisation_status = factor(authorisation_status, c("Authorised", "Withdrawn", "Refused"))) %>%
  rename("company_name" = "marketing_authorisation_holder_company_name") %>%
  mutate(company_name = str_squish(company_name)) %>%
  mutate(company_name = str_replace(company_name, "& Co\\. KG", "Co. KG")) %>%
  mutate(company_name = str_replace(company_name, "ehf\\.", "ehf")) %>%
  mutate(company_name = str_replace(company_name, "([Ss]\\.[Pp]\\.[Aa](\\.|))|SpA$", "s.p.a.")) %>%
  mutate(company_name = str_replace(company_name, "(Ltd|Limited)$", "Ltd.")) %>%
  mutate(company_name = str_replace(company_name, "(B.( |)V(\\.|)|BV)$", "B.V.")) %>%
  mutate(company_name = str_replace(company_name, "([Ss].( |)[Aa](\\.|)|SA)$", "S.A.")) %>%
  mutate(company_name = str_replace(company_name, "S.A.S(\\.|)$", "S.A.S.")) %>%
  mutate(company_name = str_replace(company_name, "Bristol Myers", "Bristol-Myers")) %>%
  mutate(company_name = str_replace(company_name, "Merck Sharp.+Dohme", "Merck Sharp & Dohme")) %>%
  mutate(company_name = str_replace(company_name, "Grunenthal", "Grünenthal")) %>%
  mutate(company_name = str_replace(company_name, "Teva Pharma", "Teva")) %>%
  mutate(company_name = str_replace(company_name, "Krka", "KRKA")) %>%
  mutate(company_name = str_replace(company_name, "and Company", "Co.")) %>%
  mutate(company_name = str_replace(company_name, "Janssen Cilag", "Janssen-Cilag")) %>%
  mutate(company_name = str_replace(company_name, "NV$", "N.V.")) %>%
  mutate(company_name = str_replace(company_name, "Instituto Grifols S.A.", "Instituto Grifols, S.A.")) %>%
  mutate(company_name = str_remove(company_name, ",")) %>%
  mutate(company_name = str_remove(company_name, " Branch$")) %>%
  mutate(company_name = case_when(str_detect(company_name, "Portela") ~ "Bial - Portela Ca, S.A.",
                                  company_name == "Zentiva, k.s." ~ "Zentiva k.s.",
                                  TRUE ~ company_name)) 


# Top 25 Most Popular Therapeutic Areas -------------
top_therapeutic_areas <- drugs_human %>%
  select(medicine_name, product_number, common_name, therapeutic_area, 
         authorisation_status, marketing_authorisation_date) %>%
  mutate(year_authorized = year(marketing_authorisation_date)) %>%
  mutate(therapeutic_area = str_split(therapeutic_area, ";  ")) %>%
  unnest(therapeutic_area) %>%
  mutate(therapeutic_area = str_remove(therapeutic_area, ", .+")) %>%
  count(therapeutic_area, authorisation_status) %>% 
  group_by(therapeutic_area) %>%
  mutate(n_total = sum(n)) %>%
  ungroup() %>%
  select(therapeutic_area, n, n_total, authorisation_status) %>%
  arrange(desc(n_total)) %>%
  mutate(therapeutic_area = factor(therapeutic_area, unique(.$therapeutic_area)))  %>%
  mutate(Authorised_rate = n/n_total) 

p_top25_therap_areas_ndrugs <- top_therapeutic_areas %>%
  filter(therapeutic_area %in% levels(.$therapeutic_area)[1:25]) %>%
  mutate(nudge_x = ifelse(authorisation_status == "Refused", 3, 0)) %>%
  ggplot(aes(x = n, y = therapeutic_area)) +
  geom_col(aes(fill = authorisation_status), 
           position = position_stack(reverse = TRUE), width = 0.8) +
  geom_text(aes(x = n + nudge_x, label = n, color = authorisation_status), 
            position = position_stack(vjust = 0.5, reverse = TRUE), 
            family = "Dubai", size = 3, fontface = "bold") +
  coord_cartesian(expand = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 0))) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("white", "#34495e", "#34495e"), 
                     name = "Authorization Status") +
  scale_fill_discrete(name = "Authorization Status") +
  labs(x = "TOTAL # DRUGS", y = NULL, 
       title = toupper("Top 25 Popular Therapeutic Areas")) +
  theme_custom +
  theme(legend.background = element_blank(),
        legend.position = c(0.8, 0.1),
        legend.key.size = unit(4.5, "mm"))





p_top25_therap_areas_approve_rate <- top_therapeutic_areas %>%
  filter(therapeutic_area %in% levels(.$therapeutic_area)[1:25]) %>%
  filter(authorisation_status == "Authorised") %>%
  ggplot(aes(x = Authorised_rate, y = therapeutic_area)) +
  geom_col(aes(fill = Authorised_rate), width = 0.7, alpha = 0.9) +
  coord_cartesian(expand = FALSE) +
  scale_fill_stepsn(colors = c("lightyellow", "gold", "tomato2", "#641A80", "#140B35"), limits = c(0.375, 1),
                    breaks = seq(0.375, 1, 0.125), labels= function(x) {paste0(x * 100, "%")}) +
  scale_x_continuous(labels= function(x) {paste0(x * 100, "%")}, limits = c(0, 1)) +
  scale_y_discrete(limits = rev) +
  guides(fill = guide_colorbar(barwidth = 8, barheight = 0.5)) +
  labs(x = toupper("Approval Rate"), y = NULL) +
  theme_custom +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank())

p_scatter_therapeutic_areas <- top_therapeutic_areas %>%
  filter(authorisation_status == "Authorised") %>% 
  filter(!is.na(therapeutic_area)) %>% 
  ggplot(aes(x = n, y = Authorised_rate)) +
  geom_point(position = position_jitter(width = 0.2, seed = 42), shape = 16, size = 1) +
  geom_text_repel(data = subset(top_therapeutic_areas, authorisation_status == "Authorised" &
                                !is.na(therapeutic_area) & (n_total >= 50 | therapeutic_area == "COVID-19 virus infection" | (Authorised_rate < 0.4 & n_total >= 4))),
                  aes(label = str_remove(therapeutic_area, "(virus infection| Infections)$") %>% str_replace(" ", "\n") ), 
                  family = "Franklin Gothic Medium Cond", size = 3.5, color = "saddlebrown", 
                  position = position_jitter(width = 0.2, seed = 42), 
                  min.segment.length = 0.2, max.overlaps = Inf, 
                  box.padding = 0.2, lineheight = 0.8, seed = 42) +
  scale_x_continuous(expand = expansion(add = c(1, 1)), limits = c(0, NA)) +
  scale_y_continuous(labels = function(x) {paste0(x * 100, "%")}, 
                     expand = expansion(c(0.02, 0.02)), limits = c(0.2, NA)) +
  labs(x = "# APPROVED DRUGS", y = "APPROVAL RATE", 
       title = "APPROVAL RATE VS. # APPROVED DRUGS") +
  theme_custom +
  theme(plot.title.position = "panel")


therap_area_title <- textGrob(
  c("EUROPEAN DRUG DEVELOPMENT: THERAPEUTIC AREAS", "@akela@mstdn.social"), 
  x = c(0.007, 0.993), hjust = c(0, 1),
  gp = gpar(fontfamily = "Dubai", fontface = c(2, 1), 
            fontsize = c(14, 11), col = "#19232d"))
source_caption <- textGrob(
  "Source: European Medicines Agency", x = 0.007, hjust = 0,
  gp = gpar(fontfamily = "Franklin Gothic Medium Cond", fontsize = 11, col = "#19232d"))


plot_grid(p_top25_therap_areas_ndrugs, 
          p_top25_therap_areas_approve_rate,
          p_scatter_therapeutic_areas,
          nrow = 1, rel_widths = c(1, 0.25, 1), align = "h") %>%
  plot_grid(therap_area_title, ., source_caption, ncol = 1, rel_heights = c(0.03, 1, 0.015)) +
  theme(plot.background = element_rect(color = NA, fill = theme_flat$palette$background),
        plot.margin = margin(t = 5, b = 5))


ggsave("20230314_European_Drug_Development/therapeautic_areas.png", 
       width = 13, height = 5.5)

# Top 20  Marketing Authorisation Holders -------------
top_company <- drugs_human %>%
  mutate(therapeutic_area = str_split(therapeutic_area, ";  ")) %>%
  unnest(therapeutic_area) %>%
  mutate(therapeutic_area = str_remove(therapeutic_area, ", .+")) %>%
  group_by(company_name) %>%
  mutate(n_therapeutic_area = length(unique(therapeutic_area))) %>%
  select(company_name, n_therapeutic_area, medicine_name, authorisation_status) %>%
  distinct() %>%
  count(company_name, authorisation_status, n_therapeutic_area) %>%
  group_by(company_name) %>%
  mutate(n_total = sum(n)) %>%
  arrange(desc(n_total)) %>%
  mutate(company_name = factor(company_name, unique(.$company_name))) %>%
  mutate(Authorised_rate = n/n_total)

p_top20_company_ndrugs <- top_company %>%
  filter(company_name %in% levels(company_name)[1:20]) %>%
  ggplot(aes(y = company_name, x = n)) +
  geom_col(aes(fill = authorisation_status), width = 0.8, 
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(x = n, label = n, color = authorisation_status), 
            position = position_stack(vjust = 0.5, reverse = TRUE), 
            family = "Dubai", size = 3, fontface = "bold") +
  coord_cartesian(expand = FALSE) +
  scale_y_discrete(limits = rev, labels = function(x) {str_replace(x, "International", "Intl.")}) +
  scale_color_manual(values = c("white", "#34495e", "#34495e"), 
                     name = "Authorization Status") +
  scale_fill_discrete(name = "Authorization Status") +
  guides(color = guide_legend(override.aes = list(size = 0))) +
  labs(x = "TOTAL # DRUGS", y = NULL, title = toupper("Top 20 Marketing Authorisation Holders")) +
  theme_custom +
  theme(legend.background = element_blank(),
        legend.position = c(0.8, 0.12),
        legend.key.size = unit(4.5, "mm"))



p_top20_company_approve_rate <- top_company %>%
  filter(company_name %in% levels(company_name)[1:20]) %>%
  filter(authorisation_status == "Authorised") %>%
  ggplot(aes(x = Authorised_rate, y = company_name)) +
  geom_col(aes(fill = Authorised_rate), width = 0.7, alpha = 0.9) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(labels= function(x) {paste0(x * 100, "%")}) +
  scale_y_discrete(limits = rev) +
  scale_fill_stepsn(colors = c("lightyellow", "gold", "tomato2", "#641A80", "#140B35"), limits = c(0.375, 1),
                    breaks = seq(0.375, 1, 0.125), labels= function(x) {paste0(x * 100, "%")}) +
  guides(fill = guide_colorbar(barwidth = 8, barheight = 0.5)) +
  labs(x = toupper("Approval Rate"), y = NULL) +
  theme_custom +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank())





p_scatter_company <- top_company %>%
  filter(authorisation_status == "Authorised") %>% 
  filter(!is.na(company_name)) %>% 
  arrange(desc(n_therapeutic_area)) %>%
  ggplot(aes(x = n, y = Authorised_rate, color = n_therapeutic_area)) +
  geom_point(position = position_jitter(width = 0.2, seed = 42), 
             shape = 16, size = 1.5) +
  geom_text_repel(data = subset(top_company, authorisation_status == "Authorised" &
                                !is.na(company_name) & (n_total >= 45 | (Authorised_rate < 0.5 & n_total >= 4))),
                  aes(label = str_replace(company_name, "(?=(International|Biologicals|Dohme|MA) )", "\n")),
                  family = "Franklin Gothic Medium Cond", size = 3.5,# color = "saddlebrown",
                  position = position_jitter(width = 0.2, seed = 42), vjust = 0.8,
                  max.overlaps = Inf, box.padding = 0.15, seed = 42, hjust = 0.7, lineheight = 0.8) +
  scale_color_stepsn(colors = c("#000009", "#5E4FA2", "#3498db", "#97cc2e", 
                                "gold", "#ff9900", "brown1", "#9E0142", "#551100"), 
                     breaks = seq(0, 70, 10), limits = c(0, 70), name = "# THERAP.\nAREAS") +
  scale_x_continuous(expand = expansion(add = c(1, 3))) +
  scale_y_continuous(labels = function(x) {paste0(x * 100, "%")}, expand = expansion(add = c(0.02, 0.02))) +
  guides(color = guide_colorbar(barwidth = 0.4, barheight = 10, label.vjust = 1, title.position = "top")) +
  labs(x = "# APPROVED DRUGS", y = "APPROVAL RATE", 
       title = "APPROVAL RATE VS. # AUTHORIZED DRUGS") +
  theme_custom +
  theme(legend.position = "right",
        legend.background = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9.5),
        plot.title.position = "panel")


company_title <- textGrob(
  c("EUROPEAN DRUG DEVELOPMENT: PHARMACEUTICAL COMPANIES", "@akela@mstdn.social"), 
  x = c(0.007, 0.993), hjust = c(0, 1),
  gp = gpar(fontfamily = "Dubai", fontface = c(2, 1), 
            fontsize = c(14, 11), col = "#19232d"))

plot_grid(p_top20_company_ndrugs, p_top20_company_approve_rate, p_scatter_company, 
          nrow = 1, rel_widths = c(1, 0.25, 1), align = "h") %>%
  plot_grid(company_title, ., source_caption, ncol = 1, rel_heights = c(0.03, 1, 0.015)) +
  theme(plot.background = element_rect(color = NA, fill = theme_flat$palette$background),
        plot.margin = margin(t = 5, b = 5))

ggsave("20230314_European_Drug_Development/pharma_company.png", 
       width = 13, height = 5.5)





