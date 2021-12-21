library(tidyverse)
library(lubridate)
# library(ggridges)
library(ggtext)

trends_combined <- read_rds(file.path("data", "trends_2021_combined.rds"))

# define Coronavirus-related keywords
count(trends_combined, keyword) %>% pull(keyword)
keywords_corona <- c("Pandemie", "Desinfektionsmittel", "Lockdown", "Klopapier", "Wuhan", "Lieferdienst",
                     "Kurzarbeit", "Home Office", "Geisterspiele", "Schnelltest", "PCR-Test",
                     "Christian Drosten", "Hendrik Streeck", "Brot backen", "Mund-Nasen-Schutz", "Maske",
                     "Biontech", "Curevac", "Impfstoff", "Zoom",
                     "Querdenken")

# sort keywords alphabetically
keywords_corona <- keywords_corona[order(keywords_corona)]

# identify "local" peak hits date for each keyword
keywords_corona_by_peak_date <- trends_combined %>% 
  filter(hits_rescaled >= 90) %>% 
  filter(keyword %in% keywords_corona) %>% 
  group_by(keyword) %>% 
  summarize(peak_date = min(date)) %>%
  arrange(desc(peak_date))

# set a smaller value for a bigger overlap (>100 = no overlap)
added_height <- 50
# control level of smoothing
loess_bw <- 45/366

g <- trends_combined %>% 
  filter(keyword %in% keywords_corona) %>% 
  inner_join(keywords_corona_by_peak_date, by = "keyword") %>% 
  arrange(desc(peak_date), keyword) %>% 
  nest(data = -keyword) %>% 
  mutate(row = row_number()) %>% 
  unnest(data) %>% 
  mutate(added = added_height * row,
         y = hits_rescaled + added) %>% 
  ggplot(aes(date, y, group = keyword)) +
  stat_smooth(geom = "line", method = "loess", span = loess_bw, col = "white", alpha = 0.7) +
  scale_x_datetime(expand = c(0, 0), date_breaks = "1 month", date_labels = "%b", position = "top") +
  scale_y_continuous(breaks = seq(added_height, added_height * length(keywords_corona), added_height), 
                     labels = keywords_corona_by_peak_date$keyword, expand = c(0, 0)) +
  labs(title = "Coronavirus-Suchbegriffe in den Google Trends 2020",
       subtitle = "Die Höhe gibt das relative Suchaufkommen <i>je Begriff</i> wieder,<br> auf den Wertebereich
       von 0 (kein Interesse) bis 100 (maximales Interesse) skaliert.",
       caption = glue::glue("Tägliches Suchaufkommen geglättet mit Loess (bw = {round(loess_bw, 2)}).
       Aufgrund der Glättung können<br>Begriffe mit sehr kurzfristigen Peaks im Suchinteresse einen Höchstwert unter 100 haben.<br>
       @4nsgarW. Daten: Google Trends (Websuche Deutschland)"),
       x = NULL, y = NULL) +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(plot.title = element_markdown(family = "Source Sans Pro SemiBold",
                                      margin = margin(t = 6, l = 6, r = 6, b = 10),
                                      color = "white"),
        plot.subtitle = element_markdown(margin = margin(l = 6, b = 12), color = "grey95",
                                         lineheight = 1.2),
        plot.caption = element_markdown(color = "grey95", size = 8, hjust = 0,
                                        margin = margin(t = 8, b = 6),
                                        lineheight = 1.1),
        plot.title.position = "plot",
        axis.text.y = element_text(hjust = 1, vjust = 0.5, color = "grey95",
                                 margin = margin(l = 6, r = 4)),
        axis.text.x = element_text(hjust = 0, color = "grey95"),
        plot.background = element_rect(color = NA, fill = "#353535"),
        panel.grid.major = element_line(size = 0.1, color = "grey40"),
        panel.grid.minor = element_blank())

plot(g)

# build the ggplot2 to access the smoothed values for the ribbon geom
gg <- ggplot_build(g)

# access the ggplot object to obtain the smoothed values
gg_df <- data.frame(x = gg$data[[1]]$x,
                    ymax = gg$data[[1]]$y,
                    group = keywords_corona[gg$data[[1]]$group],  # IMPORTANT: keywords_corona must be ordered alphabetically
                    group_id = gg$data[[1]]$group,
                    y = NA_real_) %>% 
  # add the keyword based on group id
  inner_join(arrange(distinct(gg$plot$data, keyword, added), desc(added)), 
             by = c("group" = "keyword"))

# gg_df <- filter(gg_df, group == "Desinfektionsmittel")

# add the ribbon with smoothed values as ymax and keyword height as ymin
g + geom_ribbon(data = gg_df,
                aes(x = as_datetime(x), ymin = added, ymax = ymax, group = group), 
                inherit.aes = FALSE,
                fill = "#BB86FC", alpha = 0.5)

ggsave(file.path("plots", "corona_gtrends_de_2021_dark.png"), type = "cairo", dpi = 200, width = 6, height = 8)

