# Read in sampling protocol word documents; find priority waterbody list.

get_wb_list_from_protocol_doc = function(dat){
  
  table_cells = officer::docx_summary(dat) |> 
    dplyr::filter(content_type == "table cell") |> 
    tidyr::as_tibble()
  
  # Find which index our table is at.
  table_index = table_cells |> dplyr::filter(text == 'Region') |> dplyr::pull(doc_index)
  
  table_cells = table_cells |> 
    dplyr::filter(doc_index == table_index)
  
  table_data <- table_cells |> dplyr::filter(!is_header) |> dplyr::select(row_id, cell_id, text)
  
  # split data into individual columns
  splits <- split(table_data, table_data$cell_id)
  splits <- lapply(splits, function(x) x$text)
  
  # combine columns back together in wide format
  table_result <- dplyr::bind_cols(splits)
  
  # get table headers
  # Are there headers in the table?
  if(sum(table_cells$is_header) > 0){
  cols <- table_cells |> dplyr::filter(is_header)
  names(table_result) <- cols$text
  } else {
  # If that didn't work because the table headers weren't classified as headers
  # in the word document, just shift everything up by one.
  names(table_result) = table_result[1,]
  table_result = table_result[-1,]
  }
  return(table_result)
}
