# Depends
pacman::p_load(tidyverse)

# Data
df <- read_rds("./processed_data/new_build_prices_clean.RDS") |> 
  ungroup() |>
  mutate(
    Month = factor(
      Month, levels = c(
        months(
          seq.Date(
            from = date("2024-01-01"), date("2024-12-01"), by = "month"
          ), abbreviate = FALSE
        )
      )
    )
  )



df |>
  ggplot(aes(factor(Year), price_mean)) +
  facet_wrap(vars(Month), scales = "fixed") +
  stat_summary(
    fun.data = "mean_cl_boot", aes(col = developer),
    position = position_dodge(.5)
    ) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£"))
