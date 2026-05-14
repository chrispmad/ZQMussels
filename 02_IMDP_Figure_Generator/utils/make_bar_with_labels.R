make_bar_with_lbls = function(data,y_var_label,
                              station_filter,year_filter,
                              x_var = 'Station',
                              reorder_x_by_y = T,
                              fill_var = 'Year',
                              label_cutoff,
                              highlight_field = NA,
                              ggplotify = TRUE,
                              add_sep_lines = F){
  
  if(length(station_filter) > 1 | station_filter[1] != 'none'){
    plot_dat = data |> 
      filter(Station %in% station_filter)
  } else {
    plot_dat = data
  }
  plot_dat_sum = plot_dat |> 
    filter(Year %in% year_filter) |> 
    mutate(Year = as.character(Year)) |> 
    mutate(Year = factor(Year, levels = year_filter)) |> 
    dplyr::mutate(Station = str_replace_all(Station," (?=\\()","\n")) |> 
    count(Year,!!rlang::sym(x_var)) |> 
    group_by(!!rlang::sym(x_var)) |> 
    mutate(station_total = sum(n)) |> 
    ungroup()
  
  if(reorder_x_by_y){
    plot_dat_sum = plot_dat_sum |> 
      arrange(desc(station_total)) |> 
      mutate(!!rlang::sym(x_var) := as.factor(!!rlang::sym(x_var))) |> 
      mutate(!!rlang::sym(x_var) := forcats::fct_inorder(!!rlang::sym(x_var)))
  }
  
  text_labels = plot_dat_sum |> 
    dplyr::mutate(text_label = ifelse(n <= label_cutoff, n, NA))
  
  if(is.na(highlight_field)){
    ggplot_fig = plot_dat_sum |> 
      ggplot(aes(x = !!rlang::sym(x_var), y = n, 
                 group = Year, fill = !!rlang::sym(fill_var))) + 
      geom_col(position='dodge') + 
      geom_text(position=position_dodge(width = .9),
                aes(y = n + 0.05*max(plot_dat_sum$n), 
                    group = Year, label = text_label),
                data = text_labels) + 
      scale_fill_brewer(palette = 'Set3') +
      big_text +
      labs(y = y_var_label, x = '')
  }
  
  if(!is.na(highlight_field)){
    highlight_dat = plot_dat |> 
      dplyr::filter(!!rlang::sym(highlight_field)) |> 
      dplyr::mutate(Station = str_replace_all(Station," (?=\\()","\n")) |> 
      filter(Year %in% year_filter) |> 
      mutate(Year = as.character(Year)) |> 
      mutate(Year = factor(Year, levels = year_filter)) |> 
      count(Year,!!rlang::sym(x_var),name = "highlight_field") |> 
      group_by(!!rlang::sym(x_var)) |> 
      mutate(hl_station_total = sum(highlight_field)) |> 
      ungroup()
    
    if(reorder_x_by_y){
      highlight_dat = highlight_dat |> 
        arrange(desc(hl_station_total)) |> 
        mutate(!!rlang::sym(x_var) := as.factor(!!rlang::sym(x_var))) |> 
        mutate(!!rlang::sym(x_var) := forcats::fct_inorder(!!rlang::sym(x_var)))
    }
    
    ggplot_fig = plot_dat_sum |> 
      dplyr::left_join(highlight_dat) |> 
      dplyr::mutate(highlight_field = tidyr::replace_na(highlight_field,0)) |> 
      ggplot(aes(x = !!rlang::sym(x_var), group = Year)) + 
      geom_col(aes(y=n), fill = 'red', position='dodge') + 
      geom_col(aes(y=n-highlight_field, fill = !!rlang::sym(fill_var)), position='dodge') + 
      geom_col(aes(y=n-highlight_field), fill = 'white', alpha = 0.6, position='dodge') + 
      geom_text(position=position_dodge(width = .9),
                aes(y = n + 0.05*max(plot_dat_sum$n), 
                    group = Year, label = ifelse(
                      highlight_field/n == 1,
                      paste0(100*round(highlight_field/n,3),"% (",Year,")"),
                      paste0(100*round(highlight_field/n,3),"%")))
      ) + 
      scale_fill_brewer(palette = 'Set3') +
      big_text +
      labs(y = y_var_label, x = '') +
      guides(fill = guide_legend(override.aes = list(alpha = 0.6)))
    
  }
  
  if(add_sep_lines){
    ggplot_fig = ggplot_fig + 
      facet_wrap(as.formula(paste("~", x_var)), nrow = 1, scales = 'free_x', strip.position = "bottom") + 
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_rect(color = 'grey', fill = 'transparent'),
            strip.text = element_text(size = 14, angle = 45, margin = margin(r = 15)),
            strip.clip = "off")
  }
  
  if(!ggplotify){
    return(ggplot_fig)
  }
  if(ggplotify){
    return(
      ggplotly(
        ggplot_fig,
        width = 1000,
        height = 600
      )
    )
  }
}