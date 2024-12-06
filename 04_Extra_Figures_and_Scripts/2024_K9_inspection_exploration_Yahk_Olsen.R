library(tidyverse)
library(plotly)

# These numbers came from a query on metabase.
d = tibble(
  count = c(3505, 274, 19, 6042, 1, 100, 4),
  k9_on_shift = c(F,T,T,F,F,T,T),
  k9_inspection = c(F,F,T,F,T,F,T),
  station = c(rep("Olsen (Hwy 3)",3),
              rep("Yahk", 4)),
) |> 
  dplyr::mutate(k9_on_shift = ifelse(k9_on_shift,"K9 On Shift","K9 Not On Shift")) |> 
  dplyr::mutate(k9_inspection = ifelse(k9_inspection,"K9 Inspection","Not K9 Inspection")) |> 
  dplyr::mutate(k9_summary = paste0(k9_on_shift,", ",k9_inspection)) |> 
  dplyr::mutate(k9_summary = factor(k9_summary,levels = c(
    "K9 Not On Shift, Not K9 Inspection",
    "K9 Not On Shift, K9 Inspection",
    "K9 On Shift, K9 Inspection",
    "K9 On Shift, Not K9 Inspection"
  )))
  
d |> 
  ggplot(aes(fill = station)) + 
  geom_col(aes(x = k9_summary, y = count)) +
  geom_text(aes(x = k9_summary, y = count + 0.05*max(count), label = count)) +
  labs(fill = "Station", 
       y = "Number of Inspections", 
       x = "K9 Data Fields", title = "K9 Inspection Details") +
  facet_wrap( ~ station, nrow = 2, scales = 'free_y')

ggplotly(k9_on_shift_plot)
