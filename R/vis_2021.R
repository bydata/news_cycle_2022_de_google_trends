library(tidyverse)
library(lubridate)
library(glue)
library(ggtext)

trends_combined <- read_rds(file.path("data", "trends_2021_combined.rds"))

# keywords
keywords <- count(trends_combined, keyword) %>% pull(keyword)
# exclude some keywords manually
keywords <- keywords[!keywords %in% c("WhatsApp", "Biontech", "Curevac", "2G+",
                                      "Markus Söder", "PCR-Test", "Farbfilm",
                                      "Kuba-Syndrom", "Bärbel Bas", "Gil Ofarim",
                                      "Christian Eriksen", "Karl Lauterbach",
                                      "Wetten Dass", "Alec Baldwin")]
# sort keywords alphabetically
keywords <- keywords[order(keywords)]

trends_combined <- trends_combined %>% 
  group_by(keyword) %>% 
  mutate(hits_rescaled_7dmean = zoo::rollmean(hits_rescaled, align = "center", k = 7, fill = NA),
         hits_rescaled_7dmean = hits_rescaled_7dmean * 100 / max(hits_rescaled_7dmean, na.rm = TRUE)) %>% 
  ungroup()


# Google palette
google_colors <- c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")
google_colors_desaturated <- colorspace::desaturate(google_colors, 0.2)

# identify "local" peak hits date for each keyword
keywords_by_peak_date <- trends_combined %>% 
  filter(hits_rescaled >= 90) %>% 
  filter(keyword %in% keywords) %>% 
  group_by(keyword) %>% 
  summarize(peak_date = min(date)) %>%
  arrange(desc(peak_date)) %>% 
  mutate(row_color = google_colors_desaturated[(row_number() %% length(google_colors) + 1)],
         label = glue::glue("<span style='color: {row_color}'>{keyword}</span>")) %>% 
  select(-row_color)

# set a smaller value for a bigger overlap (>100 = no overlap)
added_height <- 72
# control level of smoothing
loess_bw <- 45/366

# Annotations
plot_titles <- list(
  title = glue("Google Trends <span style='color:{google_colors[1]}'>2021</span>"),
  # subtitle = "Die Höhe gibt das relative Suchaufkommen <i>je Begriff</i> wieder,<br> auf den Wertebereich
  #      von 0 (kein Interesse) bis 100 (maximales Interesse) skaliert.",
  # subtitle = "Die Höhe gibt das relative Suchaufkommen <i>je Begriff</i> wieder",
  subtitle = "Tägliches relatives Suchaufkommen ausgewählter Suchbegriffe Deutschland",
  # caption = glue::glue("Tägliches Suchaufkommen geglättet mit Loess 
  # (bw = {round(loess_bw, 2)}).
  #      Aufgrund der Glättung können Begriffe mit sehr kurzfristigen Peaks im Suchinteresse 
  #      einen Höchstwert unter 100 haben.<br>
  #      Daten: **Google Trends** (Websuche Deutschland) | Visualisierung: **Ansgar Wolsing**")
  caption = glue::glue("Tägliches Suchaufkommen skaliert auf einen Wertbereich von 
  0 bis 100 (maximales Suchinteresse je Suchbegriff).
  <br>
  Daten: **Google Trends** (Websuche Deutschland) | Visualisierung: **Ansgar Wolsing**"))


df_plot <- trends_combined %>% 
  mutate(hits_rescaled = hits_rescaled_7dmean) %>% 
  mutate(date = as.POSIXct(date)) %>% 
  inner_join(keywords_by_peak_date, by = "keyword") %>% 
  arrange(desc(peak_date), keyword) %>% 
  nest(data = -keyword) %>% 
  mutate(row = row_number()) %>% 
  unnest(data) %>% 
  # add missing dates per keyword
  # complete(keyword, date, fill = list(hits_rescaled = 0)) %>% 
  # calculate position on y-axis
  mutate(added = added_height * row,
         y = hits_rescaled + added)

# set locale for month abbreviations in plot
Sys.setlocale(category = "LC_ALL", locale = "de_DE.UTF-8")

g <- df_plot %>% 
  ggplot(aes(date, y, group = keyword)) +
  # stat_smooth(geom = "line", method = "loess", span = loess_bw, col = "white", alpha = 0.7) +
  geom_line(aes(fill = factor(row %% length(google_colors))), # for colouring the areas later 
            col = "white", size = 0.2) + 
  scale_x_datetime(
    expand = c(0.001, 0), 
    date_breaks = "1 month", date_labels = "%b", position = "top") +
  scale_y_continuous(breaks = seq(added_height, added_height * length(keywords), added_height),
                     labels = keywords_by_peak_date$keyword, expand = c(0, 0)
                     ) +
  scale_fill_manual(values = google_colors) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption,
       x = NULL, y = NULL) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    text = element_text(color = "grey85"),
    plot.title = element_markdown(family = "Roboto Condensed", face = "bold",
                                      margin = margin(t = 6, l = 6, r = 6, b = 8),
                                      size = 22, color = "white"),
        plot.subtitle = element_textbox_simple(margin = margin(l = 6, b = 18), 
                                               lineheight = 1.2, size = 10),
        plot.caption = element_textbox_simple(size = 8, hjust = 0,
                                        margin = margin(t = 16, b = 6),
                                        lineheight = 1.25, color = "grey80"),
    plot.title.position = "plot",
        plot.margin = margin(t = 4, l = 16, r = 16, b = 4),
        axis.text.y = element_markdown(hjust = 1, vjust = 0.5, color = "grey90",
                                   family = "Roboto Condensed", size = 8,
                                 margin = margin(l = 6, r = 4)),
        axis.text.x = element_text(hjust = 0, color = "grey80"),
        plot.background = element_rect(color = NA, fill = "grey8"),
    panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "grey94"),
        panel.grid.minor = element_blank())

# build the ggplot2 to access the smoothed values for the ribbon geom
gg <- ggplot_build(g)
# access the ggplot object to obtain the smoothed values
gg_df <- data.frame(x = gg$data[[1]]$x,
                    ymax = gg$data[[1]]$y,
                    group = keywords[gg$data[[1]]$group],  # IMPORTANT: keywords must be ordered alphabetically
                    group_id = gg$data[[1]]$group,
                    fill = gg$data[[1]]$fill,
                    y = NA_real_) %>% 
  # add the keyword based on group id
  inner_join(arrange(distinct(gg$plot$data, keyword, added), desc(added)), 
             by = c("group" = "keyword"))

# add the ribbon with smoothed values as ymax and keyword height as ymin
p2 <- g + geom_ribbon(data = gg_df,
                aes(x = as_datetime(x), ymin = added, ymax = ymax, group = group, fill = fill), 
                inherit.aes = FALSE,
                alpha = 0.7) +
  scale_fill_identity()

ggsave(file.path("plots", "gtrends_de_2021_dark.png"), device = ragg::agg_png,
       dpi = 300, width = 5.25, height = 8)

p2 + theme(
  plot.background = element_rect(color = NA, fill = "white"),
  text = element_text(color = "grey35"),
  axis.text.x = element_markdown(color = "grey30"),
  axis.text.y = element_markdown(color = "grey2", family = "Roboto"),
  # panel.grid.major.x = element_line(color = "grey89"),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "grey67"),
  plot.title = element_markdown(color = "black", family = "Roboto", face = "bold")
)

ggsave(file.path("plots", "gtrends_de_2021_light.png"), 
       dpi = 300, width = 6.5, height = 9.5)

# 
# ## Use geom_area instead
# p3 <- df_plot %>% 
#   ggplot(aes(date, y, group = keyword)) +
#   geom_ribbon(aes(ymin = added, ymax = y, fill = factor(row %% length(google_colors))), # for colouring the areas later 
#             col = "white", size = 0.2, alpha = 0.5, show.legend = FALSE) + 
#   scale_x_datetime(expand = c(0, 0), date_breaks = "1 month", date_labels = "%b", position = "top") +
#   scale_y_continuous(breaks = seq(added_height, added_height * length(keywords), added_height),
#                      labels = keywords_by_peak_date$keyword, expand = c(0, 0)) +
#   scale_fill_manual(values = google_colors) +
#   labs(title = plot_titles$title,
#        subtitle = plot_titles$subtitle,
#        caption = plot_titles$caption,
#        x = NULL, y = NULL) +
#   theme_minimal(base_family = "Raleway") +
#   theme(plot.title = element_markdown(family = "Source Sans Pro SemiBold",
#                                       margin = margin(t = 6, l = 6, r = 6, b = 10),
#                                       size = 18,
#                                       color = "white"),
#         plot.subtitle = element_textbox_simple(margin = margin(l = 6, b = 12), color = "grey95",
#                                                lineheight = 1.2),
#         plot.caption = element_textbox_simple(color = "grey95", size = 8, hjust = 0,
#                                               margin = margin(t = 16, b = 6),
#                                               lineheight = 1.1),
#         plot.title.position = "plot",
#         plot.margin = margin(t = 4, l = 12, r = 12, b = 4),
#         axis.text.y = element_markdown(hjust = 1, vjust = 0.5, color = "grey95",
#                                    margin = margin(l = 6, r = 4)),
#         axis.text.x = element_text(hjust = 0, color = "grey95"),
#         plot.background = element_rect(color = NA, fill = "grey8"),
#         panel.grid.major = element_line(size = 0.1, color = "grey40"),
#         panel.grid.minor = element_blank())
# 
# ggsave(file.path("plots", "gtrends_de_2021_dark-2.png"), type = "cairo", 
#        dpi = 300, width = 5, height = 5 * 10/6)




library(gganimate)

p2 + transition_reveal(date)
