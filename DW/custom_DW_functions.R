# custom DataWrappR functions

# Update and Re-publish DW chart

dw_update_and_republish <- function(dw_id, # may be a vector, if the same data should be inserted into several charts
                                    data_for_dw,
                                    nourls = T) {
  
  
  for(id in dw_id) {
    
    dw_data_to_chart(data_for_dw, chart_id = id, api_key = Sys.getenv("DW_API_KEY"))
    
    Sys.sleep(3)
    
  }
  
  
  for(id in dw_id) {
    
    dw_publish_chart(chart_id = id,
                     api_key = Sys.getenv("DW_API_KEY"),
                     return_urls = !nourls)
    
    cat(paste0("*** Chart ", id, " updated @", format(Sys.time(), format = "%Y-%m-%d %H:%M"), "\n"))
    
    Sys.sleep(3)
    
  }
  
}

# dependecies for dw_update_and_republish
# 

dw_data_to_chart <- function(x, chart_id, parse_dates = TRUE, api_key = "environment") {
  
  # if (api_key == "environment") {
  #   api_key <- dw_get_api_key()
  # }
  
  # try conversion - to avoid problems with tibbles
  x <- as.data.frame(x)
  
  # test class of input dataframe
  try(if (class(x) != "data.frame") stop("Data is not of class data.frame!"))
  
  # find indices that contain vectors of type Data or POSIXt (which includes both POSIXct and POSIXlt)
  if (parse_dates == TRUE) {
    idx <- sapply(x, function(df_x) inherits(df_x, "Date") || inherits(df_x, "POSIXt"))
    x[idx] <- lapply(x[idx], as.character)
  }
  
  # collapse the data in the dataframe as a string
  data_body <- readr::format_csv(x)
  
  url <- paste0("https://api.datawrapper.de/v3/charts/", chart_id, "/data")
  
  r <- httr::PUT(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                 body = data_body)
  
  if (httr::status_code(r) %in% c(200, 201, 202, 204)) {
    cat(paste0("Data in ", chart_id, " successfully updated.", "\n"))
  } else {
    stop(paste0("There has been an error in the upload process. Statuscode of the response: ", httr::status_code(r)), immediate. = TRUE)
  }
  
}

dw_publish_chart <- function(chart_id, api_key = "environment", return_urls = TRUE, return_object = FALSE) {
  
  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }
  
  # chart_id <- dw_check_chart_id(chart_id)
  
  url <- paste0("https://api.datawrapper.de/charts/", chart_id, "/publish")
  
  r <- httr::POST(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")))
  
  parsed <- dw_handle_errors(r)
  
  httr::handle_reset("https://api.datawrapper.de/")
  
  parsed
  
  if (httr::status_code(r) %in% c(200, 201, 202, 203, 204)) {
    cat(paste0("Chart ", chart_id, " published!"))
    
    if (isTRUE(return_urls)) {
      
      iframe_code <- parsed$data[[1]]$metadata$publish$`embed-codes`$`embed-method-responsive`
      chart_url <- parsed$data[[1]]$publicUrl
      
      writeLines(paste0("### Responsive iFrame-code: ###\n", iframe_code, "\n\n", "### Chart-URL:###\n", chart_url))
      
    }
    
    if (isTRUE(return_object)) {
      
      structure(
        list(
          content = parsed,
          path = "https://api.datawrapper.de/v3/charts",
          id = parsed[["id"]],
          publicUrl = parsed$data[[1]]$publicUrl,
          iframeCode = parsed$data[[1]]$metadata$publish$`embed-codes`$`embed-method-responsive`
        ),
        class = "dw_chart"
      )
      
    }
    
  } else {
    stop(paste0("There has been an error in the publication process. Status code of the response: ", httr::status_code(r)), immediate. = TRUE)
  }
}

#' @export

print.dw_chart <- function(x, ...) {
  cat("<Datawrapper ", x$path, ">\n", sep = "")
  cat("Chart-ID: ", x$id, "\n", sep = "")
  str(x$content)
  invisible(x)
}

dw_handle_errors <- function(r) {
  # error handling
  if (httr::http_type(r) != "application/json" & httr::http_type(r) != "application/octet-stream") {
    stop("API did not return json", call. = FALSE)
  }
  
  if (httr::http_error(r)) {
    stop(
      sprintf(
        "Datawrapper API request failed [%s]\n%s",
        httr::status_code(r),
        httr::content(r, "parsed")$message
      ),
      call. = FALSE
    )
  }
  
  # separate check for type application/json;
  # to avoid raising an error when exporting a chart which returns application/octet-stream
  
  if (httr::http_type(r) == "application/json") {
    parsed <- jsonlite::fromJSON(httr::content(r, "text"), simplifyVector = FALSE)
    
    if (length(parsed[["data"]]) > 1) {
      parsed[["data"]] <- list(parsed[["data"]])
    }
    
    return(parsed)
  }
  
  # end of error handling
  
}

dw_edit_chart <- function(chart_id, api_key = "environment", title = "", intro = "", annotate = "", byline = "",
                          type = "", source_name = "", source_url = "", folderId = "", axes = list(), data = list(), visualize = list(),
                          describe = list(), publish = list(), ...) {
  
  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }
  
  # create empty body for API-call
  call_body <- list(metadata = list())
  
  # change only specified parts of existing data
  if (title != "") {call_body <- rlist::list.append(call_body, title = title)}
  if (type != "") {call_body <- rlist::list.append(call_body, type = type)}
  if (folderId != "") {call_body <- rlist::list.append(call_body, folderId = folderId)}
  
  if (intro != "") {call_body$metadata$describe$intro <- intro}
  if (annotate != "") {call_body$metadata$annotate$notes <- annotate}
  
  if (byline != "") {call_body$metadata$describe$byline <- byline}
  if (source_name != "") {call_body$metadata$describe$`source-name` <- source_name}
  if (source_url != "") {
    
    if (grepl("^http", source_url) == FALSE) {  # include simple test, if url is starting with http(s)
      source_url <- paste0("http://", source_url)
    }
    
    call_body$metadata$describe$`source-url` <- source_url
  }
  
  # work in additional arguments, if specified
  if (length(data) > 0) {
    if (!is.list(call_body$metadata$data)) {
      call_body$metadata$data <- list()
    }
    call_body$metadata$data <- utils::modifyList(call_body$metadata$data, data)
  }
  
  if (length(visualize) > 0) {
    if (!is.list(call_body$metadata$visualize)) {
      call_body$metadata$visualize <- list()
    }
    
    call_body$metadata$visualize <- utils::modifyList(call_body$metadata$visualize, visualize)
  }
  
  if (length(describe) > 0) {
    if (!is.list(call_body$metadata$describe)) {
      call_body$metadata$describe <- list()
    }
    
    call_body$metadata$describe <- utils::modifyList(call_body$metadata$describe, describe)
  }
  
  if (length(publish) > 0) {
    if (!is.list(call_body$metadata$publish)) {
      call_body$metadata$publish <- list()
    }
    
    call_body$metadata$publish <- utils::modifyList(call_body$metadata$publish, publish)
  }
  
  if (length(axes) > 0) {
    if (!is.list(call_body$metadata$axes)) {
      call_body$metadata$axes <- list()
    }
    call_body$metadata$axes <- utils::modifyList(call_body$metadata$axes, axes)
  }
  
  additional_arguments <- list(...)
  
  if (length(additional_arguments) > 0) {
    call_body <- rlist::list.append(call_body, additional_arguments)
  }
  
  # send call to API
  # upload modified data
  # solution for API v1:
  # url_upload <- paste0("https://api.datawrapper.de/charts/", chart_id)
  #
  # r <- httr::PUT(url_upload, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
  #                       body = call_body, encode = "json", .DATAWRAPPR_UA)
  
  url_upload <- paste0("https://api.datawrapper.de/v3/charts/", chart_id)
  r <- httr::PATCH(url_upload, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                   body = call_body, encode = "json")
  
  parsed <- dw_handle_errors(r)
  
  chart_id_response <- parsed["id"][[1]] #for v3: parsed["id"][[1]], for v1: parsed[["data"]][[1]][["id"]]
  
  try(if (chart_id != chart_id_response) stop(paste0("The chart_ids between call (",  chart_id ,") and response (",  chart_id_response ,") do not match. Try again and check API.")))
  
  if (chart_id == chart_id_response & httr::status_code(r) %in% c(200, 201, 202, 204)) {
    
    cat(paste0("Chart ", chart_id_response, " succesfully updated.", "\n"))
    
  } else {
    stop(paste0("There has been an error in the upload process. Status code of the response: ", httr::status_code(r)), immediate. = TRUE)
  }
  
  httr::handle_reset(url_upload)
  
}

# ch_maps <- dw_basemaps %>% 
#   filter(str_detect(id, "switzerland"))
# 
# 
# dw_gde_2021 <- tibble(Gemeinde_Name = fromJSON("https://api.datawrapper.de/plugin/basemaps/switzerland-municipalities-2021/GMDNAME")$data$values,
#                       Gemeinde_Nr = fromJSON("https://api.datawrapper.de/plugin/basemaps/switzerland-municipalities-2021/GMDNR")$data$values)

# GMDNAME, GMDNR

# add extra line for map tooltip (link to Gde-Sites)
# tt_w_gdesite <- "<div style='display:block;'>Artikel auf <a href='https://www.tagblatt.ch'>Gemeindeseite</a><br><br></div><div style='display: {{ eidgnoresults == 1 ? 'block' : 'none' }};'>Noch keine Resultate.</div><div style='display: {{ eidgnoresults == 1 ? 'none' : 'block' }};'><div style='border:1px dashed #a8a8a8; padding: 5px 5px 8px; border-radius:3px; background-color:#a8a8a820; font-weight:bold; width:95%'><p style='display: {{ eidgausgezaehlt == 1 ? 'none' : 'inline-block' }}; color:red; background-color:yellow; font-weight:normal;'><em>Zwischenresultat</em></p>
# <b>Verhüllungsverbot</b>
# <br>
# <div style='color: {{ eidgvorl_1_ja_in_prozent < 50 ? '#cc3a88' : eidgvorl_1_ja_in_prozent > 50 ? '#4d9221' : 'black' }}; display:inline-block'> <div style='display:inline-block; background-color: {{ eidgvorl_1_ja_in_prozent < 50 ? '#cc3a88' : eidgvorl_1_ja_in_prozent > 50 ? '#4d9221' : 'black' }}; border-radius:3px; color:white; padding:1px 4px'>{{ eidgvorl_1_winning_formula }}</div></div><br>
# 
# <b>E-ID-Gesetz</b>
# <br>
# <div style='color:green; display:inline-block'> <div style='display:inline-block; background-color:{{ eidgvorl_2_ja_in_prozent < 50 ? '#cc3a88' : eidgvorl_2_ja_in_prozent > 50 ? '#4d9221' : 'black' }}; border-radius:3px; color:white; padding:1px 4px'>{{ eidgvorl_2_winning_formula }}</div></div><br>
# 
# <b>Handelsabkommen<br>Indonesien</b>
# <br>
# <div style='color:green; display:inline-block'> <div style='display:inline-block; background-color:{{ eidgvorl_3_ja_in_prozent < 50 ? '#cc3a88' : eidgvorl_3_ja_in_prozent > 50 ? '#4d9221' : 'black' }}; border-radius:3px; color:white; padding:1px 4px'>{{ eidgvorl_3_winning_formula }}</div></div>
# <p><b>Stimmbeteiligung</b><br>{{ ROUND((eidgvorl_1_stimmbeteiligung + eidgvorl_2_stimmbeteiligung + eidgvorl_3_stimmbeteiligung) / 3, 1)}} Prozent</p>
# <div style='opacity:0.5; font-weight:normal; padding:0px 0px; text-align: right;'>national</div></div>
# <br>
# <div style='display: {{ kanton_n_vorlagen == 0 ? 'none' : 'inline-block' }}; border:1px dashed #a8a8a8; padding: 5px 5px 8px; border-radius:3px; background-color:#a8a8a820; font-weight:bold; width:95%'><p style='display: {{ ktausgezaehlt == 1 ? 'none' : ktnoresults == 1 ? 'none' : 'inline-block' }}; color:red; background-color:yellow; font-weight:normal;'><em>Zwischenresultat</em></p>
# <div style='display: {{ kanton_n_vorlagen >= 1 ? 'block' : 'none' }}'><b>{{ ktvorl_1_name }}</b><br></div>
# <div style='color:red; display: {{ kanton_n_vorlagen >= 1 ? 'inline-block' : 'none' }};'> <div style='display:inline-block; background-color: {{ ktvorl_1_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_1_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; border-radius:3px; color:white; padding:1px 4px'>{{ ktvorl_1_winning_formula }}<br></div><br><br></div>
# <div style='display: {{ kanton_n_vorlagen >= 2 ? 'block' : 'none' }}'><b>{{ ktvorl_2_name }}</b><br></div>
# <div style='color:red; display: {{ kanton_n_vorlagen >= 2 ? 'inline-block' : 'none' }};'> <div style='display:inline-block; background-color: {{ ktvorl_2_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_2_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; border-radius:3px; color:white; padding:1px 4px'>{{ ktvorl_2_winning_formula }}<br></div><br><br></div>
# <div style='display: {{ kanton_n_vorlagen >= 3 ? 'block' : 'none' }}'><b>{{ ktvorl_3_name }}</b><br></div>
# <div style='color:red; display: {{ kanton_n_vorlagen >= 3 ? 'inline-block' : 'none' }};'> <div style='display:inline-block; background-color: {{ ktvorl_3_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_3_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; border-radius:3px; color:white; padding:1px 4px'>{{ ktvorl_3_winning_formula }}<br></div><br><br></div>
# <div style='display: {{ kanton_n_vorlagen >= 4 ? 'block' : 'none' }}'><b>{{ ktvorl_4_name }}</b><br></div>
# <div style='color: {{ ktvorl_4_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_4_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; display: {{ kanton_n_vorlagen >= 4 ? 'inline-block' : 'none' }};'> <div style='display:inline-block; background-color:{{ ktvorl_4_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_3_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; border-radius:3px; color:white; padding:1px 4px'>{{ ktvorl_4_winning_formula }}<br></div><br></div> <div style='opacity:0.5; font-weight:normal; padding:0px 0px 5px; text-align:right'>kantonal</div>
# </div>
# </div>"

# tt_w_5vorl <- "<div style='display: {{ eidgnoresults == 1 ? 'block' : 'none' }};'>Noch keine Resultate.</div><div style='display: {{ eidgnoresults == 1 ? 'none' : 'block' }};'><div style='border:1px dashed #a8a8a8; padding: 5px 5px 8px; border-radius:3px; background-color:#a8a8a820; font-weight:bold; width:95%'><p style='display: {{ eidgausgezaehlt == 1 ? 'none' : 'inline-block' }}; color:red; background-color:yellow; font-weight:normal;'><em>Zwischenresultat</em></p>
# <b><br>Verhüllungsverbot</b>
# <br>
# <div style='color: {{ eidgvorl_1_ja_in_prozent < 50 ? '#cc3a88' : eidgvorl_1_ja_in_prozent > 50 ? '#4d9221' : 'black' }}; display:inline-block'> <div style='display:inline-block; background-color: {{ eidgvorl_1_ja_in_prozent < 50 ? '#cc3a88' : eidgvorl_1_ja_in_prozent > 50 ? '#4d9221' : 'black' }}; border-radius:3px; color:white; padding:1px 4px'>{{ eidgvorl_1_winning_formula }}</div></div><br>
# 
# <b>E-ID-Gesetz</b>
# <br>
# <div style='color:green; display:inline-block'> <div style='display:inline-block; background-color:{{ eidgvorl_2_ja_in_prozent < 50 ? '#cc3a88' : eidgvorl_2_ja_in_prozent > 50 ? '#4d9221' : 'black' }}; border-radius:3px; color:white; padding:1px 4px'>{{ eidgvorl_2_winning_formula }}</div></div><br>
# 
# <b>Handelsabkommen<br>Indonesien</b>
# <br>
# <div style='color:green; display:inline-block'> <div style='display:inline-block; background-color:{{ eidgvorl_3_ja_in_prozent < 50 ? '#cc3a88' : eidgvorl_3_ja_in_prozent > 50 ? '#4d9221' : 'black' }}; border-radius:3px; color:white; padding:1px 4px'>{{ eidgvorl_3_winning_formula }}</div></div>
# <p><b>Stimmbeteiligung</b><br>{{ ROUND((eidgvorl_1_stimmbeteiligung + eidgvorl_2_stimmbeteiligung + eidgvorl_3_stimmbeteiligung) / 3, 1)}} Prozent</p>
# <div style='opacity:0.5; font-weight:normal; padding:0px 0px; text-align: right;'>national</div></div>
# <br>
# <div style='display: {{ kanton_n_vorlagen == 0 ? 'none' : ktnoresults == 1 ? 'none' : 'inline-block' }}; border:1px dashed #a8a8a8; padding: 5px 5px 8px; border-radius:3px; background-color:#a8a8a820; font-weight:bold; width:95%'><p style='display: {{ ktausgezaehlt == 1 ? 'none' : 'inline-block' }}; color:red; background-color:yellow; font-weight:normal;'><em>Zwischenresultat</em></p>
# <div style='display: {{ kanton_n_vorlagen >= 1 ? 'block' : 'none' }}'><b>{{ ktvorl_1_name }}</b><br></div>
# <div style='color:red; display: {{ kanton_n_vorlagen >= 1 ? 'inline-block' : 'none' }};'> <div style='display:inline-block; background-color: {{ ktvorl_1_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_1_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; border-radius:3px; color:white; padding:1px 4px'>{{ ktvorl_1_winning_formula }}<br></div><br><br></div>
# <div style='display: {{ kanton_n_vorlagen >= 2 ? 'block' : 'none' }}'><b>{{ ktvorl_2_name }}</b><br></div>
# <div style='color:red; display: {{ kanton_n_vorlagen >= 2 ? 'inline-block' : 'none' }};'> <div style='display:inline-block; background-color: {{ ktvorl_2_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_2_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; border-radius:3px; color:white; padding:1px 4px'>{{ ktvorl_2_winning_formula }}<br></div><br><br></div>
# <div style='display: {{ kanton_n_vorlagen >= 3 ? 'block' : 'none' }}'><b>{{ ktvorl_3_name }}</b><br></div>
# <div style='color:red; display: {{ kanton_n_vorlagen >= 3 ? 'inline-block' : 'none' }};'> <div style='display:inline-block; background-color: {{ ktvorl_3_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_3_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; border-radius:3px; color:white; padding:1px 4px'>{{ ktvorl_3_winning_formula }}<br></div><br><br></div>
# <div style='display: {{ kanton_n_vorlagen >= 4 ? 'block' : 'none' }}'><b>{{ ktvorl_4_name }}</b><br></div>
# <div style='color: {{ ktvorl_4_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_4_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; display: {{ kanton_n_vorlagen >= 4 ? 'inline-block' : 'none' }};'> <div style='display:inline-block; background-color:{{ ktvorl_4_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_4_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; border-radius:3px; color:white; padding:1px 4px'>{{ ktvorl_4_winning_formula }}<br></div><br></div>
# <div style='display: {{ kanton_n_vorlagen >= 5 ? 'block' : 'none' }}'><b>{{ ktvorl_5_name }}</b><br></div>
# <div style='color: {{ ktvorl_5_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_5_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; display: {{ kanton_n_vorlagen >= 5 ? 'inline-block' : 'none' }};'> <div style='display:inline-block; background-color:{{ ktvorl_5_ja_in_prozent < 50 ? '#cc3a88' : ktvorl_5_ja_in_prozent > 50 ? '#4d9221' : 'black'}}; border-radius:3px; color:white; padding:1px 4px'>{{ ktvorl_5_winning_formula }}<br></div><br></div> <div style='opacity:0.5; font-weight:normal; padding:0px 0px 5px; text-align:right'>kantonal</div>
# </div>
# </div>"
# 
# 
# 
# # edit Sandpit chart 
# # THIS WORKS!
# # DatawRappr::dw_edit_chart(chart_id = "ebWqo",
# #               visualize = list(tooltip = list(body = tt_w_5vorl)))
# #               VHV: hVRH9
# #               EID: kxIBC
# #               FIN: ENNdK
# #               
# dw_edit_chart(chart_id = "hVRH9",
#               visualize = list(tooltip = list(body = tt_w_5vorl)),
#               api_key = Sys.getenv("DW_API_KEY"))
# Sys.sleep(5)
# dw_edit_chart(chart_id = "kxIBC",
#                           visualize = list(tooltip = list(body = tt_w_5vorl)),
#               api_key = Sys.getenv("DW_API_KEY"))
# Sys.sleep(5)
# dw_edit_chart(chart_id = "ENNdK",
#                           visualize = list(tooltip = list(body = tt_w_5vorl)),
#               api_key = Sys.getenv("DW_API_KEY"))
# 
# for(id in c("hVRH9", "kxIBC", "ENNdK")) {
#   
#   dw_publish_chart(chart_id = id,
#                    api_key = Sys.getenv("DW_API_KEY"))
#   
#   Sys.sleep(5)
#   
# }
# 
# # 
# # gde_map_test5 <- dta_4_gde_maps(dta_eidg = dta_eidg_mar, dta_kant = dta_kant_mar)
# # 
# # dw_update_and_republish(dw_id = "ebWqo", data_for_dw = gde_map_test5)
# 
# # dw_publish_chart(chart_id = "ebWqo")
#  