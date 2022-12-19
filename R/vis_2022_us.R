library(tidyverse)
library(lubridate)
library(glue)
library(ggtext)

# set locale for month abbreviations in plot
Sys.setlocale(category = "LC_ALL", locale = "de_DE.UTF-8")

# Google palette
google_colors <- c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")
google_colors_desaturated <- colorspace::desaturate(google_colors, 0.2)


## DATA PREP ===================================================================

trends_combined <- read_rds(file.path("data", "us", "trends_2022_combined.rds"))

# keywords
keywords <- unique(trends_combined$keyword)
# exclude some keywords manually
keywords <- keywords[!keywords %in% c(
  "Homer", "Rupee", "Cacao"
)]

# sort keywords alphabetically
keywords <- keywords[order(keywords)]

trends_combined <- trends_combined %>% 
  group_by(keyword) %>% 
  mutate(hits_rescaled_7dmean = zoo::rollmean(hits_rescaled, align = "center", k = 7, fill = NA),
         hits_rescaled_7dmean = hits_rescaled_7dmean * 100 / max(hits_rescaled_7dmean, na.rm = TRUE),
         hits_rescaled_5dmean = zoo::rollmean(hits_rescaled, align = "center", k = 5, fill = NA),
         hits_rescaled_5dmean = hits_rescaled_5dmean * 100 / max(hits_rescaled_5dmean, na.rm = TRUE)) %>% 
  ungroup()


# identify "local" peak hits date for each keyword - sort search topics in plot
keywords_by_peak_date <- trends_combined %>%
  filter(hits_rescaled >= 90) %>%
  filter(keyword %in% keywords) %>%
  group_by(keyword) %>%
  summarize(peak_date = min(date)) %>%
  arrange(desc(peak_date)) %>%
  mutate(
    row_color = google_colors_desaturated[(row_number() %% length(google_colors) + 1)],
    # add a colored dot as a guidance in the y-axis labels
    label = glue::glue(
      # "{keyword}
      # <span style='color: {row_color};font-family:Arial;'>\u2022</span>"
      "<span style='color: {row_color}'>{keyword}</span>"
      )
  ) %>%
  select(-row_color)

# Define the overlap of ridgelines 
# Choose a smaller value for a bigger overlap (>100 = no overlap)
added_height <- 75

# Prepare data for plot
df_plot <- trends_combined %>% 
  mutate(hits_rescaled = hits_rescaled_7dmean,
         # set all values below 5 to 5
         hits_rescaled = ifelse(hits_rescaled < 3, 3, hits_rescaled),
         date = as.POSIXct(date)) %>% 
  inner_join(keywords_by_peak_date, by = "keyword") %>% 
  arrange(desc(peak_date), keyword) %>% 
  nest(data = -keyword) %>% 
  mutate(row = row_number()) %>% 
  unnest(data) %>% 
  # calculate position on y-axis
  mutate(added = added_height * row,
         y = hits_rescaled + added)


## PLOT ========================================================================

# Annotations
plot_titles <- list(
  title = glue("Google Trends US <span style='color:{google_colors[1]}'>2022</span>"),
  subtitle = "Daily relative search interest in top 5 Google search terms in 
  different categories in the US",
  caption = glue::glue("Rolling average (7 days, centered) 
  of the daily search interest scaled to a range of 0 to 100 (maximum search interest
  per search term).
  Source: **Google Trends** (Web search United States) | Visualisation: **Ansgar Wolsing**"))

g <- df_plot %>% 
  ggplot(aes(date, y, group = keyword)) +
  geom_line(aes(fill = factor(row %% length(google_colors))), # for colouring the areas later 
            col = "white", 
            linewidth = 0.2) + 
  scale_x_datetime(
    expand = c(0.0005, 0), 
    date_breaks = "1 month", date_labels = "%b", position = "top") +
  scale_y_continuous(breaks = seq(added_height, added_height * length(keywords), added_height),
                     labels = keywords_by_peak_date$label, expand = c(0, 0)
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
    plot.subtitle = element_textbox_simple(margin = margin(l = 6, t = 6, b = 18), 
                                           lineheight = 1.2, size = 10),
    plot.caption = element_textbox_simple(size = 8, hjust = 0,
                                          margin = margin(t = 16, b = 6),
                                          lineheight = 1.25, color = "grey80"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(t = 4, l = 16, r = 16, b = 4),
    axis.text.y = element_markdown(hjust = 1, vjust = 0, color = "grey90",
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
                    # IMPORTANT: keywords must be ordered alphabetically
                    group = keywords[gg$data[[1]]$group],  
                    group_id = gg$data[[1]]$group,
                    fill = gg$data[[1]]$fill,
                    y = NA_real_) %>% 
  # add the keyword based on group id
  inner_join(arrange(distinct(gg$plot$data, keyword, added), desc(added)), 
             by = c("group" = "keyword"))

# add the ribbon with smoothed values as ymax and keyword height as ymin
p2 <- g + geom_ribbon(data = gg_df,
                aes(x = as_datetime(x), ymin = added, ymax = ymax, group = group, fill = fill), 
                inherit.aes = FALSE, alpha = 0.7, col = "white", size = 0.2) +
  scale_fill_identity()

ggsave(file.path("plots", "gtrends_us_2022_dark-smoothed-7d.png"), device = ragg::agg_png,
       dpi = 400, width = 6, height = 8)



## Light version ---------------------------------------------

p2 + theme(
  plot.background = element_rect(color = NA, fill = "white"),
  text = element_text(color = "grey35"),
  axis.text.x = element_markdown(color = "grey35"),
  axis.text.y = element_markdown(color = "grey24", family = "Roboto Condensed", 
                                 size = 9, face = "bold"),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "grey70"),
  panel.ontop = TRUE,
  plot.title = element_markdown(color = "black", family = "Roboto", face = "bold"),
  plot.caption = element_textbox_simple(color = "grey45",
                                        margin = margin(t = 16, b = 6))
)

ggsave(file.path("plots", "gtrends_us_2022_light-smoothed-7d.png"), device = ragg::agg_png,
       dpi = 400, width = 6, height = 7)
