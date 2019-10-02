## getData ##
#'extract data from wwhypda database
#'
#'\code{getData} queries the wwhypda database according to specifications
#'of site name, rock type, and parameter of interest and returns data as a dataframe.
#'
#'@param rockType a character indicating rock type. if left blank, data from all rock types returned.
#'@param param a character indicating parameter. if left blank, data from all parameters returned.
#'@param site the name of the site from which data are available. if left blank, data from all site returned.
#'@param viewInfo logical; if TRUE, calls viewInfor(), returns list of rock types, parameters, and sites
#'@return data queried from the wwhypda database as a dataframe
#'@examples
#'my_data <- getData(rockType = "Sandstone, channel", param = "porosity")
#'@export
getData <- function(rockType=NULL,
                    param=NULL,
                    site=NULL,
                    viewInfo = FALSE)
{
  # make sure RSQLite is loaded
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # connect to wwhypda sqlite
  # ===========================================================================
  db_loc <- system.file("extdata", "wwhypda.sqlite", package="geostatDB")
  con = RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_loc)

  # sanity checks: ensure that rock type, parameter, and site are valid
  # ===========================================================================
  info <- geostatDB::viewInfo()
  print(info)

  if (!(is.null(rockType)) && !(grepl(rockType, info$rockTypes)))
    stop (paste(rockType, "not in database. use viewInfo()=TRUE to see available rock types!"))

  if (!(is.null(site)) && !(grepl(site, info$sites)))
    stop (paste(site, "not in database. use viewInfo()=TRUE to see available sites!"))

  if (!(is.null(param)) && !(grepl(param, info$parameters)))
    stop (paste(parameter, "not in database. use viewInfo()=TRUE to see available parameters!"))

  # extract data
  # ===========================================================================
  basic_query <- "select id_Measure, msr_value, id_smpl, id_coh, id_ex_ty,
  id_int_mtd, id_qlt, quality_level, id_Parameter, 'code', param_name,
  units, 'MaxValue', MinValue, key_Fract, key_rt, rt_name, rt_description, rt_left, rt_right,
  rt_id_parent, key_Scale, id_src, id_rew, id_env,
  env_name, env_description, env_id_parent,
  id_pnt,
  site_id, site_name, region, ISO_code, country_name,
  key_Mgroup, mg_date, mg_comment, sample_comment

  from measure
  join parameter as p on measure.id_par_msr = p.id_Parameter
  join quality as q on measure.id_qlt = q.id_Quality
  join sample as s on s.id_Sample = measure.id_smpl
  join rock_type as r on r.rt_id = s.key_rt
  join measure_group as mg on mg.id_Measure_group = s.key_Mgroup
  join environment as ev on ev.env_id = mg.id_env
  join site_info as si on si.site_id = mg.id_pnt
  join country as co on co.ISO_code = si.iso_country
  order by id_Measure;"

  basic_data <- RSQLite::dbGetQuery(con, basic_query)

  # # all rock types
  # rocktypes <- dbGetQuery(con,
  #                         "select distinct rt_name from rockType;")
  # # all params
  # params <- dbGetQuery(con,
  #                      "select distinct param_name from parameter;")
  #
  # # all sites
  # sites <- dbGetQuery(con,
  #                     "select distinct site_name from site_info;")
  #
  # #sites <- unique(basic_data$site_name, na.rm=T)
  #

  # specifying rock type, param, site
  # ===========================================================================
  if ( !(is.null(rockType)) ){basic_data <- basic_data[which(basic_data$rt_name %in% rockType),]}
  if ( !(is.null(param)) ){basic_data <- basic_data[which(basic_data$param_name %in% param),]}
  if ( !(is.null(site)) ){basic_data <- basic_data[which(basic_data$site_name %in% site),]}

  #
  # ===========================================================================

  # modify type of site_id to characters
  basic_data$site_id <- as.character(basic_data$site_id)

  # modify name msr_value to val
  names(basic_data)[which(names(basic_data)=="msr_value")] <- "val"

  # add notation for parameters: K for conductivity, n for porosity etc
  basic_data$param_not <- basic_data$param_name
  basic_data$param_not[which(basic_data$param_not == "hydraulic conductivity")] <- "K"
  basic_data$param_not[which(basic_data$param_not == "porosity")] <- "n"
  basic_data$param_not[which(basic_data$param_not == "effective porosity")] <- "ne"

  # close connection
  # ===========================================================================
  RSQLite::dbDisconnect(con) # close connection

  if(viewInfo)
  {
    return(list(
      "data" = basic_data,
      "info" = info
    ))
  }
  else
  {
    return(basic_data)
  }

}
