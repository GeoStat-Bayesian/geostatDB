## viewInfo ##

#'view information on wwhypda database, such as list of rock types,
#'parameters, and sites.
#'
#'\code{viewInfo} returns information on wwhypda database
#'@return list of information on wwhypda database
#'@examples
#'info <- viewInfo()
#'@import DBI
#'@export
viewInfo <- function()
{
  print('RUNNING viewInfo')

  # connect to wwhypda sqlite
  # ===========================================================================
  db_loc <- system.file("extdata", "wwhypda.sqlite", package="geostatDB")
  con = RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_loc)

  # get info from wwhypda sqlite
  # ===========================================================================
  all_rocks <- DBI::dbGetQuery(con, "select distinct rt_name from rock_type;")
  all_sites <- DBI::dbGetQuery(con, "select distinct site_name, region from site_info;")
  all_params <- DBI::dbGetQuery(con, "select distinct param_name from parameter;")

  colnames(all_rocks) <- NULL; colnames(all_sites) <- NULL; colnames(all_params) <- NULL
  all_sites <- stats::na.omit(all_sites)

  # disconnect from wwhypda sqlite
  # ===========================================================================
  RSQLite::dbDisconnect(con) # close connection

  return(
    list("parameters" = all_params,
         "rockTypes" = all_rocks,
         "sites" = all_sites)
  )

}
