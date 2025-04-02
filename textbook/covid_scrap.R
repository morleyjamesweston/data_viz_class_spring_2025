
install.packages("COVID19")

library(COVID19)
library(zoo)
covid_data = covid19()

covid_spiral <- covid_data |> 
  as_tibble() |> 
  filter(administrative_area_level_1 == "Switzerland") |> 
  select(date, confirmed)
covid_spiral

covid_spiral |> 
  ggplot() +
  geom_line(aes(x = date, y = confirmed))


covid_spiral <- covid_spiral |>
  fill(confirmed, .direction = "down") |> 
  mutate(confirmed = replace_na(confirmed, 0))
covid_spiral

covid_spiral |> 
  ggplot() +
  geom_line(aes(x = date, y = confirmed))

covid_spiral <- covid_spiral |> 
  mutate(yesterday_cases = lag(confirmed, 1)) |> 
  mutate(daily_cases = confirmed - yesterday_cases)
covid_spiral

covid_spiral |> 
  ggplot() +
  geom_line(aes(x = date, y = daily_cases))


covid_spiral <- covid_spiral |> 
  mutate(year_day = yday(date)) |> 
  mutate(daily_cases = rollmean(daily_cases, k=14, fill=0)) 
covid_spiral

covid_spiral <- covid_spiral |> 
  mutate(daily_cases = replace_na(daily_cases, 0))

covid_spiral |> 
  ggplot() +
  geom_line(aes(x = date, y = daily_cases))


covid_spiral |>
  ggplot() +
  geom_ribbon(
    aes(
      x = date,
      ymin = -daily_cases,
      ymax = daily_cases
    ),
    fill = "#eeaaaa",
    color = "#662222"
  )

covid_spiral |>
  ggplot() +
  geom_ribbon(
    aes(
      x = date,
      ymin = -daily_cases,
      ymax = daily_cases
    ),
    fill = "#eeaaaa",
    color = "#662222"
  ) + 
  coord_polar()

spiral_plot_data <- tibble(
  year_day = rep(1:365, 5),
  year = rep(2020:2024, each=365)
  ) |> 
  mutate(year_num = year - 2020) |>
  mutate(spiral = year_num * 365 + year_day)


circle_plot <- spiral_plot_data |>
  ggplot() +
  geom_line(aes(x = year_day, y = year, group=year_num, color = year_day), size = 5) +
  theme_minimal() +
  scale_color_gradientn(colors = c("#ff3322", "#cccc33", "#33cccc")) +
  theme(legend.position = "none") +
  labs(title = "Circle plot", x = "Day of the year", y = "Year")

circle_plot
circle_plot + coord_polar()

spiral_plot <- spiral_plot_data |>
  ggplot() +
  geom_line(aes(x = year_day, y = spiral, group=year_num, color = spiral), size = 5) +
  theme_minimal() +
  scale_color_gradientn(colors = c("#ff3322", "#cccc33", "#33cccc")) +
  theme(legend.position = "none") +
  labs(title = "Spiral plot", x = "Day of the year", y = "Days since 2020")
  
spiral_plot
spiral_plot + coord_polar()


covid_spiral <- covid_spiral |> 
  mutate(year_num = year(date) - 2020) |> 
  filter(year_day <= 365) |> 
  mutate(days_since_2020 = year_num * 365 + year_day)

covid_spiral

CASES_MULTIPLIER = 300

covid_spiral |> 
  ggplot() +
  geom_line(aes(x = year_day, y = days_since_2020, group=year_num))

covid_spiral <- covid_spiral |> 
  mutate(rescaled_daily_cases = (daily_cases / max(daily_cases)) * CASES_MULTIPLIER)

covid_spiral |> 
  ggplot() +
  geom_ribbon(
    aes(
      x = year_day,
      ymin = days_since_2020-rescaled_daily_cases,
      ymax = days_since_2020+rescaled_daily_cases,
      group = year_num
    ), fill = "#eeaaaa", color = "#662222") +
  geom_line(aes(x = year_day, y = days_since_2020, group=year_num))
  

covid_spiral |> 
  ggplot() +
  geom_ribbon(
    aes(
      x = year_day,
      ymin = days_since_2020-rescaled_daily_cases,
      ymax = days_since_2020+rescaled_daily_cases,
      group = year_num
    ), fill = "#eeaaaa", color = "#662222") +
  geom_line(aes(x = year_day, y = days_since_2020, group=year_num)) +
  coord_polar()


covid_spiral |> 
  ggplot() +
  geom_ribbon(
    aes(
      x = year_day,
      ymin = days_since_2020-rescaled_daily_cases,
      ymax = days_since_2020+rescaled_daily_cases,
      group = year_num
    ), fill = "#eeaaaa", color = "#662222") +
  geom_line(aes(x = year_day, y = days_since_2020, group=year_num), alpha =0.2) +
  coord_polar() +
  lims(y = c(-300, 1300)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 350, 90), labels = c("Jan", "Apr", "Jul", "Oct")) +
  theme(legend.position = "none", 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
)

covid_spiral |>
  mutate(year_label = ifelse(year_day == 1, as.character(year(date)), NA)) |>
  ggplot() +
  geom_ribbon(
    aes(
      x = year_day,
      ymin = days_since_2020 - rescaled_daily_cases,
      ymax = days_since_2020 + rescaled_daily_cases,
      group = year_num
    ),
    fill = "#eeaaaa",
    color = "#662222"
  ) +
  geom_line(aes(x = year_day, y = days_since_2020, group = year_num), alpha =
              0.2) +
  coord_polar() +
  lims(y = c(-300, 1300)) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_label(aes(x = 1, y = days_since_2020, label = year_label), size = 3)



covid_spiral |>
  mutate(year_label = ifelse(year_day == 1, paste0(as.character(year(
    date
  )), "→"), NA)) |>
  ggplot() +
  geom_ribbon(
    aes(
      x = year_day,
      ymin = days_since_2020 - rescaled_daily_cases,
      ymax = days_since_2020 + rescaled_daily_cases,
      group = year_num
    ),
    fill = "#eeaaaa",
    color = "#662222"
  ) +
  geom_line(aes(x = year_day, y = days_since_2020, group = year_num), alpha =
              0.2) +
  coord_polar() +
  lims(y = c(-300, 1300)) +
  theme_minimal() +
  scale_x_continuous(
    breaks = c(1, 90, 182, 274),
    minor_breaks = c(1, 29, 60, 90, 121, 151, 182, 213, 243, 274, 304, 335),
    labels = c("Jan", "Apr", "Jul", "Oct")
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank()
  ) +
  geom_text(
    aes(x = 1, y = days_since_2020, label = year_label),
    hjust = 0,
    vjust = -0.8,
    size = 3
  )


ggsave("images/aesthetics/covid_spiral.png", width = 6, height = 6, dpi = 300)













```{r}
ch_covid_data |> 
  mutate(year_label = ifelse(year_day == "1970-12-31", as.character(year(date)), NA)) |> 
  ggplot() +
  geom_ribbon(
    aes(
      x = year_day,
      ymin = days_since_2020 - rescaled_daily_cases * ribbon_multiplier,
      ymax = days_since_2020 + rescaled_daily_cases * ribbon_multiplier,
      group = year_num
    ), fill = "#eeaaaa", color = "#662222") +
  geom_line(aes(x = year_day, y = days_since_2020, group=year_num), alpha =0.2) +
  coord_polar() +
  theme_minimal() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") + 
  theme(legend.position = "none", 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank()
  ) +
  geom_text(
    aes(x = as_date("1970-12-31"), y = days_since_2020, label = year_label),
    hjust = 0,
    vjust = 0,
    size = 3
  )
```



lims(y = c(-300, 1300)) +
  
  covid_spiral |>
  mutate(year_label = ifelse(year_day == 1, as.character(year(date)), NA)) |>
  ggplot() +
  geom_ribbon(
    aes(
      x = year_day,
      ymin = days_since_2020 - rescaled_daily_cases,
      ymax = days_since_2020 + rescaled_daily_cases,
      group = year_num
    ),
    fill = "#eeaaaa",
    color = "#662222"
  ) +
  geom_line(aes(x = year_day, y = days_since_2020, group = year_num), alpha =
              0.2) +
  coord_polar() +
  lims(y = c(-300, 1300)) +
  theme_void() +
  # scale_x_continuous(breaks = seq(1, 350, 90), labels = c("Jan", "Apr", "Jul", "Oct")) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_label(aes(x = 1, y = days_since_2020, label = year_label), size = 3)

```{r}
ch_covid_data |> 
  mutate(year_label = ifelse(year_day == "1970-12-31", as.character(year(date)), NA)) |> 
  ggplot() +
  geom_ribbon(
    aes(
      x = year_day,
      ymin = days_since_2020 - rescaled_daily_cases * ribbon_multiplier,
      ymax = days_since_2020 + rescaled_daily_cases * ribbon_multiplier,
      group = year_num
    ), fill = "#eeaaaa", color = "#662222") +
  geom_line(aes(x = year_day, y = days_since_2020, group=year_num), alpha =0.2) +
  coord_polar() +
  ```


```{r}
ch_covid_data |>
  ggplot() +
  geom_ribbon(
    aes(
      x = year_day,
      ymin = days_since_2020 - rescaled_daily_cases,
      ymax = days_since_2020 + rescaled_daily_cases,
      group = year_num
    ),
    fill = "#eeaaaa",
    color = "#662222"
  ) +
  geom_line(aes(x = year_day, y = days_since_2020, group = year_num), alpha =
              0.2) +
  coord_polar() +
  lims(y = c(-300, 1300)) +
  theme_minimal() +
  # scale_x_continuous(
  #   breaks = c(1, 90, 182, 274),
  #   minor_breaks = c(1, 29, 60, 90, 121, 151, 182, 213, 243, 274, 304, 335),
  #   labels = c("Jan", "Apr", "Jul", "Oct")
  # ) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank()
  ) 
```




ggsave("images/aesthetics/covid_spiral.png", width = 6, height = 6, dpi = 300)
