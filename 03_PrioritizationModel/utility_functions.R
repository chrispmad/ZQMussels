get_wb_list_from_protocol_doc <- function(file_path){
  
  # Read the Word document
  doc <- docxtractr::read_docx(file_path)
  
  # Extract all tables
  all_tables <- docx_extract_all_tbls(doc)
  
  # Find the table that has a column named "Region"
  table_index <- which(sapply(all_tables, function(tbl) "Region" %in% colnames(tbl)))
  
  if(length(table_index) == 0){
    stop("No table with a column named 'Region' found in the document.")
  }
  
  # Select the correct table
  table <- all_tables[[table_index]]
  
  # Convert to tibble for easier handling
  table_result <- as_tibble(table)
  
  # Remove completely empty rows, if any
  table_result <- table_result %>% filter(if_any(everything(), ~ !is.na(.) & . != ""))
  
  return(table_result)
}
