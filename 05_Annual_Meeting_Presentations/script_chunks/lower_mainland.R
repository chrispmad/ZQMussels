

## Lower Mainland - Percent High-risk by Year (2016 - 2025)

```{r}
# Look across all years, where are all inspections coming from, and % of high risk for each of those.

# Checked - there is only one Lower Mainland Roving inspection in 2024
lower_mainland <- all_dat %>%
  filter(str_detect(
    Station,
    regex("(?i)(Lower Mainland|Fraser Valley)( Roving)?")
  ))

# fraser<-all_dat %>%
#   filter(str_detect(
#     Station,
#     regex("(?i)(Fraser)")
#   ))

make_bar_with_lbls(
  data = lower_mainland |> dplyr::mutate(Year = factor(Year, levels = c(2015:2025))),
  y_var_label = 'Total Inspections',
  fill_var = "Year",
  x_var = "Year",
  reorder_x_by_y = F,
  station_filter = 'none',
  ggplotify = F,
  year_filter = 2016:this_year,
  label_cutoff = 10,
  highlight_field = "High_Risk_AIS_Ind",
  add_sep_lines = T
)
```

## Lower Mainland - Percent High-risk by Month (2016 - 2025)

```{r}
total_dat = lower_mainland |> 
  dplyr::mutate(Month = lubridate::month(TimeOfInspection,label=T,abbr=F)) |> 
  dplyr::mutate(Month = factor(Month, levels = month.name)) |> 
  dplyr::count(Month)

highlight_dat = lower_mainland |> 
  dplyr::filter(High_Risk_AIS_Ind) |> 
  dplyr::mutate(Month = lubridate::month(TimeOfInspection,label=T,abbr=F)) |> 
  dplyr::mutate(Month = factor(Month, levels = month.name)) |> 
  dplyr::count(Month, name = 'HR')

ggplot_fig = total_dat |> 
  dplyr::left_join(highlight_dat) |> 
  dplyr::mutate(HR = tidyr::replace_na(HR,0)) |> 
  ggplot(aes(x = Month, group = Month)) + 
  geom_col(aes(y=n), fill = 'red', position='dodge') + 
  geom_col(aes(y=n-HR, fill = Month), position='dodge') + 
  geom_col(aes(y=n-HR), fill = 'white', alpha = 0.6, position='dodge') + 
  geom_text(position=position_dodge(width = .9),
            aes(y = n + 0.05*max(n), group = Month, label = ifelse(
              HR/n == 1,
              paste0(100*round(HR/n,3),"% (",Month,")"),
              paste0(100*round(HR/n,3),"%")))
  ) + 
  scale_fill_brewer(palette = 'Set3') +
  big_text +
  labs(y = 'Total Inspections', x = '') +
  guides(fill = guide_legend(override.aes = list(alpha = 0.6))) + 
  theme(axis.text.x = element_text(angle = 90))


plotly::ggplotly(
  ggplot_fig,
  width = 1000,
  height = 600) |> 
  plotly::config(displayModeBar = F, staticPlot = T)
```



## Lower Mainland - Percent High-risk by Day of Week (2016 - 2025)

```{r}
total_dat = lower_mainland |> 
  dplyr::mutate(DOW = lubridate::wday(TimeOfInspection,label=T)) |> 
  dplyr::mutate(DOW = factor(DOW, levels = lubridate::wday(c(1:7), label = T))) |> 
  dplyr::count(DOW)

highlight_dat = lower_mainland |> 
  dplyr::filter(High_Risk_AIS_Ind) |> 
  dplyr::mutate(DOW = lubridate::wday(TimeOfInspection,label=T)) |> 
  dplyr::mutate(DOW = factor(DOW, levels = lubridate::wday(1:7, label = T))) |> 
  dplyr::count(DOW, name = 'HR')

ggplot_fig = total_dat |> 
  dplyr::left_join(highlight_dat) |> 
  dplyr::mutate(HR = tidyr::replace_na(HR,0)) |> 
  ggplot(aes(x = DOW, group = DOW)) + 
  geom_col(aes(y=n), fill = 'red', position='dodge') + 
  geom_col(aes(y=n-HR, fill = DOW), position='dodge') + 
  geom_col(aes(y=n-HR), fill = 'white', alpha = 0.6, position='dodge') + 
  geom_text(position=position_dodge(width = .9),
            aes(y = n + 0.05*max(n), group = DOW, label = ifelse(
              HR/n == 1,
              paste0(100*round(HR/n,3),"% (",DOW,")"),
              paste0(100*round(HR/n,3),"%")))
  ) + 
  scale_fill_brewer(palette = 'Set3') +
  big_text +
  labs(y = 'Total Inspections', x = '', fill = 'Day') +
  guides(fill = guide_legend(override.aes = list(alpha = 0.6))) + 
  theme(axis.text.x = element_text(angle = 90))


plotly::ggplotly(
  ggplot_fig,
  width = 1000,
  height = 600) |> 
  plotly::config(displayModeBar = F, staticPlot = T)
```

## Lower Mainland - Source Jurisdiction (2016 - 2025)

```{r}
lower_mainland |> 
  dplyr::rename(srce = Previous_Waterbody_1_Province_Or_State) |> 
  dplyr::mutate(srce = as.factor(srce)) |> 
  dplyr::group_by(Year) |> 
  dplyr::mutate(srce = forcats::fct_lump(srce, n = 4)) |> 
  dplyr::ungroup() |> 
  make_bar_with_lbls(
    year_filter = 2016:this_year,
    y_var_label = 'Total Inspections',
    x_var = 'srce',
    fill_var = 'Year',
    station_filter = 'none',
    label_cutoff = 15,
    ggplotify = F,
    add_sep_lines = T
  )
```

## Lower Mainland - Source Jurisdiction HR (2016 - 2025)

```{r source_high_risk}


lower_mainland |> 
  dplyr::filter(High_Risk_AIS_Ind) |> 
  dplyr::rename(srce = Previous_Waterbody_1_Province_Or_State) |> 
  dplyr::mutate(srce = as.factor(srce)) |> 
  dplyr::group_by(Year) |> 
  dplyr::mutate(srce = forcats::fct_lump(srce, n = 4)) |> 
  dplyr::ungroup() |> 
  make_bar_with_lbls(
    year_filter = 2016:this_year,
    y_var_label = 'High Risk Inspections',
    x_var = 'srce',
    fill_var = 'Year',
    station_filter = 'none',
    label_cutoff = 15,
    ggplotify = F,
    add_sep_lines = T
  )
```
