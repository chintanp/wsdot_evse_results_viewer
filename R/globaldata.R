GlobalModule <- function(input, output, session) {
  stash = reactiveValues()
  #####################
  # Database connection
  #####################
  
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("MAIN_DB"),
    host = Sys.getenv("MAIN_HOST"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD"),
    port = Sys.getenv("MAIN_PORT")
  )
  
  stash$pool <- pool
  
  bevses_db <- pool %>% dplyr::tbl("built_evse")
  
  bevse_dcfc <-
    bevses_db %>% 
    dplyr::select(dcfc_count,
                  latitude,
                  longitude,
                  bevse_id,
                  connector_code) %>% 
    dplyr::filter(dcfc_count > 0) %>% 
    dplyr::mutate(evse_id = paste0("b", bevse_id)) %>% 
    dplyr::select(-c(bevse_id)) %>% 
    dplyr::collect()
  
  stash$bevse_dcfc <- bevse_dcfc
  
  return (list(stash = stash))
  
}