library(ggrepel)
library(scales)

p9.lowrisk = dat |> 
  filter(High_Risk_AIS_Ind != TRUE) |> 
  mutate(the.hour = hour(TimeOfInspection)) |>
  count(the.hour, name = "Number_Insp") |> 
  ggplot(aes(x = the.hour, y = Number_Insp)) + 
  
  geom_col(
    fill = "#5FC76D",   # softer grey-blue
    width = 0.6
  ) +
  
  geom_text(
    aes(label = comma(Number_Insp)),
    vjust = -0.4,
    size = 3.2,
    color = "#2b2b2b"
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 23, 1),
    labels = function(x) sprintf("%02d", x)
  ) +
  
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  labs(
    x = NULL,
    y = "Watercraft Encounters",
    title = "Watercraft Inspections by Hour (Low Risk)"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#e6e6e6"),
    
    axis.text = element_text(color = "#2b2b2b"),
    axis.title.y = element_text(color = "#2b2b2b"),
    
    plot.title = element_text(
      face = "bold",
      size = 14,
      color = "#2b2b2b",
      margin = margin(b = 10)
    ),
    
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave("C://Users/JPHELAN/Pictures/NumberInspections_hour_of_day_lr.jpg",p9.lowrisk)

p9.2 = dat |> 
  filter(High_Risk_AIS_Ind == T) |> 
  mutate(the.hour = hour(TimeOfInspection)) |>
  count(the.hour, name = "Number_Insp") |> 
  ggplot(aes(x = the.hour, y = Number_Insp)) + 
  
  geom_col(
    fill = "#5B5FC9",   # softer grey-blue
    width = 0.6
  ) +
  
  geom_text(
    aes(label = comma(Number_Insp)),
    vjust = -0.4,
    size = 3.2,
    color = "#2b2b2b"
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 23, 1),
    labels = function(x) sprintf("%02d", x)
  ) +
  
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  labs(
    x = NULL,
    y = "Watercraft Encounters",
    title = "Watercraft Inspections by Hour (High Risk)"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#e6e6e6"),
    
    axis.text = element_text(color = "#2b2b2b"),
    axis.title.y = element_text(color = "#2b2b2b"),
    
    plot.title = element_text(
      face = "bold",
      size = 14,
      color = "#2b2b2b",
      margin = margin(b = 10)
    ),
    
    plot.margin = margin(10, 15, 10, 10)
  )
ggsave("C://Users/JPHELAN/Pictures/NumberInspections_hour_of_day_hr.jpg",p9.2)
