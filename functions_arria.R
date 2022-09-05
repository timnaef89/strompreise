# functions needed for text generation and text import to LD

# helpers
# 
load_arria_config <- function(config_path = "config_arria_connector.json") {
  
  config_arria_connector <<- jsonlite::read_json(config_path)
  
  cat("Loaded config_file into variable 'config_arria_connector'.\n")
  
}

init_function <- function() {

  empty_export_journal(do_save = T)
  empty_error_log(do_save = T)
  
  if(!dir.exists(config_arria_connector$plot_image_folder)) {
    
    dir.create(config_arria_connector$plot_image_folder)
    
  } else {
    
    cat(paste0("Photo folder ('", config_arria_connector$plot_image_folder, "') already exists.\n"))
    
  }
  
}

# function to have Swiss time, always (needs lubridate)
systime_ch <- function(as_char = F,
                       no_spaces = F) {
  
  if(as_char) {
    
    if(no_spaces) {
      
      t1 <- as.character(with_tz(Sys.time(), tz = "Europe/Zurich"))
      
      t2 <- str_replace_all(t1, ":", "") 
      
      t3 <- str_replace_all(t2, "-", "")
      
      t4 <- str_replace_all(t3, " ", "")
      
      return(t4)
      
    } else {
      
      return(as.character(with_tz(Sys.time(), tz = "Europe/Zurich")))
      
    }
    
    
  } else {
    
    return(with_tz(Sys.time(), tz = "Europe/Zurich"))
    
  }
  
}



# create empty export_journal table

empty_export_journal <- function(required_cols = c("Gemeinde_Nr",
                                                   "Gemeinde_Name",
                                                   "Newsml_Headline",
                                                   "Newsml_Lead",
                                                   "Newsml_alg_path",
                                                   "Newsml_pha_path"),
                                 do_save = F) {
  
  out <- tibble()
  
  for(i in require_cols) {
    
    out[[i]] <- character()
    
  }
  
  if(do_save) {
    
    if(!file.exists(config_arria_connector$export_journal_path)) {
      
      write_csv(out, config_arria_connector$export_journal_path)      
      
    } else {
      
      cat(paste0("Export journal already exists in ", config_arria_connector$export_journal_path, "\n"))
      
    }
    
  }
  
  return(out)

}


# function that sends data to Arria and returns the response (as is) 
get_text_from_arria <- function(dta = NULL, # this should be a data.frame with one line
                                output_format = "TEXT",
                                return_text_as_vector = T,
                                arria_project_url = getOption("arria_project_url"),
                                arria_token = getOption("arria_token")) {
  
  res <- POST(arria_project_url,
              add_headers(authorization = paste("Bearer", arria_token, sep = " ")),
              body = list(data = tibble(id = "Primary",
                                        type = "json",
                                        jsonData = dta %>% list()),
                          # jsonData = tibble(all_games = dta %>% list())),
                          options = jsonlite::unbox(tibble(nullValueBehaviour = "SHOW_IDENTIFIER",
                                                           contentOutputFormat = output_format))),
              encode = "json",
              content_type_json())
  
  return(res)
  
}


# function to display important ARRIA stuff (from response)
arria_diagnostics <- function(res) {
  
  cat(paste0("Word count     : ", content(res)[[1]]$wordCount, "\n"))
  cat(paste0("Error          : ", content(res)[[1]]$errorMessage, "\n"))
  cat(paste0("No of warnings : ", length(content(res)[[1]]$warnings), "\n"))
 
  if(length(content(res)[[1]]$warnings) > 0) {
    
    for(i in 1:length(content(res)[[1]]$warnings)) {
      
      cat(paste0(content(res)[[1]]$warnings[[i]]))
      
    }  
    
  }

}

# wrapper to get texts
arria_get_texts <- function(dta_4_arria = dta_json) {
  
  cat("Starting process of text generation via Arria.\n")
  
  tryCatch({
    
    res_arria <- get_text_from_arria(dta = dta_4_arria,
                                     arria_project_url = Sys.getenv("ARRIA_PROJECT_URL"),
                                     arria_token = Sys.getenv("ARRIA_TOKEN"))
    
    arria_diagnostics(res_arria)
    
    out <- list(content = content(res_arria)[[1]],
                text = content(res_arria)[[1]]$result)
    
  },
  error = function(e) {
    
    cat("ERROR while getting texts from Arria.\n")
    
    add_entry_to_error_log(new_gde_nr = NA_real_,
                           error_msg = e,
                           error_location = "arria_get_texts")
    
  })
  
  if(!exists("out")) {

    out <- NULL

  }
  
  return(out)
  
}

# function to create the tibble that will eventually serve for the importing loop
create_gde_dta <- function(dta_4_arria = dta_json, ## This is used for environment variables!!!
                           # arria_object = arria_get_texts(dta_4_arria = dta_json), # <- arria_get_texts(dta_4_arria = dta_json)
                           save_as_csv = NULL) {
  
  arria_object <- arria_get_texts(dta_4_arria = dta_4_arria)
  
  out <- tibble(Gemeinde_Nr = dta_4_arria$all_gde$Gemeinde_Nr,
                   Gemeinde_Name = dta_4_arria$all_gde$Gemeinde_Name) %>% 
    mutate(text_arria = str_split(arria_object$text, config_arria_connector$string_split_arria_texts)[[1]][1:nrow(dta_4_arria$all_gde)]) %>% 
    mutate(text_arria = str_replace(text_arria, "\\*", ""))
  
  
  # NOT WORKING:
  # out <- tibble(Gemeinde_Nr = dta_4_arria$all_gdes$Gemeinde_Nr,
  #               Gemeinde_Name = dta_4_arria$all_gdes$Gemeinde_Name) %>%
  #   mutate(text_arria = map_chr(arria_object, ~ str_split(.x, "\\*\\*\\*\\*\\*\\*\r\n\r\n|\\*\\*\\*\\*\\*\\*"))) %>% 
  #   # OLD mutate(text_arria =  str_split(str_sub(arria_object$text, end = str_length(arria_object$text)-5), "\\*\\*\\*\\*\\*\\*\r\n\r\n|\\*\\*\\*\\*\\*\\*")[[1]]) %>% 
  #   mutate(text_arria = str_replace(text_arria, "\\*", ""))
  
  if(!is.null(save_as_csv)) {
    
    write_csv(out, save_as_csv)
    
  }
  
  return(out)
  
}

## USAGE
## 
# get_texts_return <- get_texts(dta_4_arria = dta_json)
# 
# gde_dta <- create_gde_dta(dta_4_arria = dta_json,
#                           arria_object = get_texts_return,
#                           save_as_csv = "az_final_texts.csv")

# fake_gde_dta <- read_csv("az_final_texts.csv")


# function to replace PLACEHOLDERS from one text

arria_replace_placeholders <- function(text, # must be a string
                                       replace_tbl_path = config_arria_connector$replace_placeholders,
                                       col_find = "find",
                                       col_replace = "replace") {
  
  if(!file.exists(replace_tbl_path)) {
    
    cat(paste0("No table with replacements found at ", replace_tbl_path, ".\n"))
    
    return(text)
    
  }
    
  r <- suppressMessages(read_csv(replace_tbl_path))
  
  if(nrow(r) > 0) {
  
    out <- str_replace_all(string = text, pattern = r[[col_find]][1], replacement = r[[col_replace]][1])
    
    if(nrow(r) > 1) {
      
      for(i in 2:nrow(r)) {
        
        out <- str_replace_all(string = out, pattern = r[[col_find]][i], replacement = r[[col_replace]][i])
        
      }
      
    }
   
    return(out)
     
  } else {

    # return unchanged text if there is nothing to replace
    return(text)
    
  }
  
}

# function to split ONE text 
arria_split_one_txt <- function(t) {
  
  out <- str_split(t, "\r\n\r\n\r\n|\r\n\r\n") %>% unlist()
  
  # Delete last element, if empty
  if(last(out) == "") {out <- out[1:length(out)-1]}
  
  return(out)
  
} 


# function to create list objects to insert into combine_article (via arria_export_to_ld)
# Input: result of create_gde_dta (CHECK?)
arria_create_text_objects <- function(gdedta,
                                      update_newsml = F) {
  
  # from rss_scraper: out <- pmap(new_lot_tbl %>% select(a = Gemeinde_Nr, b = RSS_URL, c= Newsml_Headline), function(a, b, c) list(gde_nr = a, rss_url = b, item_desc = c))
  # target output: list(gde_nr =, gde_name =, text_arria = )
  # 
  if(update_newsml) {
    
    out <- pmap(gdedta %>% select(a = Gemeinde_Nr, b = Gemeinde_Name, cc = text_arria),
                function(a, b, cc) list(gde_nr = a, 
                                        gde_name = b, 
                                        text_arria = cc, 
                                        newsml_alg_path = get_newsml_path_by_gde_nr(a))) # this functions finds the old newsml-path
    
  } else {
    
    out <- pmap(gdedta %>% select(a = Gemeinde_Nr, b = Gemeinde_Name, cc = text_arria),
                function(a, b, cc) list(gde_nr = a, 
                                        gde_name = b, 
                                        text_arria = cc))
    
  }
  
  
  return(out)
  
}


# function to EXPORT one text to LD - this is an ADAPTION to ensure compatibility with footomat
arria_export_to_ld <- function(text_obj, # list(gde_nr =, gde_name =, text_arria = )
                               sleep_time = config_arria_connector$upload_sleep_time,
                               catchword_index = config_arria_connector$catchword_index,
                               headline_index = config_arria_connector$headline_index,
                               lead_index = config_arria_connector$lead_index,
                               photo_path = NULL, # paste0(config_arria_connector$photo_path_prefix, text_obj$gde_nr , ".jpg"),
                               default_photo = NULL,
                               photo_caption_index = config_arria_connector$photo_caption_index,
                               department, # get_department_by_bfs_nr(text_obj$gde_nr, type_text = config_arria_connector$department_type_text),
                               city = NULL) { # get_city_id_by_bfs_nr(text_obj$gde_nr)
  
  tryCatch({
    
    text_split <- arria_split_one_txt(text_obj$text_arria)
    
    text <- arria_replace_placeholders(text_split)
    
    if(is.null(photo_path) & is.null(default_photo)){stop("ERROR: If no `photo_path` supplied, `default_photo` cannot also be NULL.\n")}
    
    photo_path_def <- ifelse(!is.null(photo_path),
                             photo_path,
                             default_photo)
    
    art <- combine_article(article_catchword = text[catchword_index],  
                           article_headline = text[headline_index], 
                           article_text = c(text[lead_index:(length(text))]),
                           photo_paths = paste0(photo_path_def),
                           photo_captions = text[photo_caption_index],
                           department = department,
                           cities = city)
    
    upload_status <- quick_upload(art)
    
    # print("Upload Status")
    # print(upload_status)
    
    # print("textobj")
    # print(text_obj)
    # 
    # print("art")
    # print(art)
    
    #  what does upload_status look like
    # saved_upload_status <<- upload_status
    # #  If the upload to LD failed, we create an entry to the error log!
    if(sum(unlist(upload_status) != 0) > 0) {
      
      add_entry_to_error_log(new_gde_nr = text_obj$gde_nr,
                             error_msg = paste0("UPLOAD to LD failed (ftp_status != 0); ftp_stati: ", paste(upload_status %>% unlist(), collapse = " - ")),
                             error_location = "arria_export_to_ld")
      
    }
    
    add_entry_to_export_journal(new_Gemeinde_Nr = text_obj$gde_nr,
                                new_Gemeinde_Name = text_obj$gde_name,
                                new_Headline = text[config_arria_connector$headline_index],
                                new_Lead = text[config_arria_connector$lead_index],
                                new_alg_path = art$text,
                                new_pha_path = art$photo,
                                new_Photo_path = art$jpg)
    
    
  },
  error = function(e) {
    
    cat(paste0("ERROR with arria_export_to_ld: ", e))
    
    add_entry_to_error_log(new_gde_nr = text_obj$gde_nr,
                           error_msg = e,
                           error_location = "arria_export_to_ld")
    
    
    
  }
  
  )
  
  if(sleep_time > 0) {
    
    cat(paste0("Upload process going to sleep for ", sleep_time, " seconds.\n"))
    
  }
  
  Sys.sleep(sleep_time)
  
  return(upload_status)
  
}
 



# print details 

# wrapper function to EXPORT all texts to LD
arria_export_batch <- function(text_objs) { # result of arria_create_text_objects
  
  walk(text_objs, function(x) arria_export_to_ld(text_obj = x,
                                                 sleep_time = config_arria_connector$upload_sleep_time,
                                                 catchword_index = config_arria_connector$catchword_index,
                                                 headline_index = config_arria_connector$headline_index,
                                                 lead_index = config_arria_connector$lead_index,
                                                 photo_path = paste0(config_arria_connector$photo_path_prefix, x$gde_nr , ".jpg"),
                                                 photo_caption_index = config_arria_connector$photo_caption_index,
                                                 department = get_department_by_bfs_nr(x$gde_nr, type_text = config_arria_connector$department_type_text),
                                                 city = get_city_id_by_bfs_nr(x$gde_nr)))
  
}


# function to UPDATE one text in LD
arria_update_in_ld <- function(text_obj, # list(gde_nr =, gde_name =, text_arria = , newsml_alg_path =) # the newsml_alg_path is added with parameter update_newsml = T in arria_create_text_objects() 
                               sleep_time = config_arria_connector$upload_sleep_time,
                               catchword_index = config_arria_connector$catchword_index,
                               headline_index = config_arria_connector$headline_index,
                               lead_index = config_arria_connector$lead_index,
                               photo_path = NULL, # paste0(config_arria_connector$photo_path_prefix, text_obj$gde_nr , ".jpg"),
                               default_photo = NULL,
                               photo_caption_index = config_arria_connector$photo_caption_index,
                               department, # get_department_by_bfs_nr(text_obj$gde_nr, type_text = config_arria_connector$department_type_text),
                               city = NULL) { 
  
  
  
  tryCatch({
  
    if(is.null(text_obj$newsml_alg_path)) {stop("No update possible: 'newsml_alg_path' in 'text_obj' is NULL.")}
    
    text_split <- arria_split_one_txt(text_obj$text_arria)
    
    text <- arria_replace_placeholders(text_split)
    
    if(is.null(photo_path) & is.null(default_photo)){stop("ERROR: If no `photo_path` supplied, `default_photo` cannot also be NULL.\n")}
    
    photo_path_def <- ifelse(!is.null(photo_path),
                             photo_path,
                             default_photo)

    # update_article (instead of combine_article)
    # the photo is sent again
    
    art <- update_article(article_2b_updated_path = text_obj$newsml_alg_path,
                          article_catchword = text[catchword_index],  
                          article_headline = text[headline_index], 
                          article_text = c(text[lead_index:(length(text))]),
                          photo_paths = paste0(photo_path),
                          photo_captions = text[photo_caption_index],
                          department = department,
                          cities = city)
    
    # ORIGINAL
    # art <- update_article(article_2b_updated_path = text_obj$newsml_alg_path,
    #                       article_catchword = text[config_arria_connector$catchword_index],
    #                       article_headline = text[config_arria_connector$headline_index],
    #                       article_text = c(text[config_arria_connector$lead_index:(length(text))]),
    #                       photo_paths = paste0(config_arria_connector$photo_path_prefix, text_obj$gde_nr , ".jpg"),
    #                       # photo_paths = "Single_3.jpg",
    #                       photo_captions = text[config_arria_connector$photo_caption_index],
    #                       department = get_department_by_bfs_nr(text_obj$gde_nr, type_text = config_arria_connector$department_type_text),
    #                       cities = get_city_id_by_bfs_nr(text_obj$gde_nr))  
    
    upload_status <- quick_upload(art)
    
    # print("Upload Status")
    # print(upload_status)
    
    # print("textobj")
    # print(text_obj)
    # 
    # print("art")
    # print(art)
    
    #  what does upload_status look like
    # saved_upload_status <<- upload_status
    # #  If the upload to LD failed, we create an entry to the error log!
    if(sum(unlist(upload_status) != 0) > 0) {
      
      add_entry_to_error_log(new_gde_nr = text_obj$gde_nr,
                             error_msg = paste0("UPLOAD to LD for UPDATE failed (ftp_status != 0); ftp_stati: ", paste(upload_status %>% unlist(), collapse = " - ")),
                             error_location = "arria_update_in_ld")
      
    }
    
    add_entry_to_export_journal(new_Gemeinde_Nr = text_obj$gde_nr,
                                new_Gemeinde_Name = text_obj$gde_name,
                                new_Headline = text[config_arria_connector$headline_index],
                                new_Lead = text[config_arria_connector$lead_index],
                                new_alg_path = art$text,
                                new_pha_path = art$photo,
                                new_Photo_path = art$jpg)
  },
  error = function(e) {
    
    cat(paste0("ERROR with UPDATE in arria_update_in_ld: ", e))
    
    add_entry_to_error_log(new_gde_nr = text_obj$gde_nr,
                           error_msg = e,
                           error_location = "arria_update_in_ld")
    
    
    
  }
  
  )
  
  if(sleep_time > 0) {
    
    cat(paste0("Upload process going to sleep for ", sleep_time, " seconds.\n"))
    
  }
  
  Sys.sleep(sleep_time)
  
  return(upload_status)
  
}

# wrapper function to UPDATE texts already SENT to LD

arria_update_batch <- function(text_objs) { # result of arria_create_text_objects
  
  walk(text_objs, function(x) arria_update_in_ld(text_obj = x,
                                                 sleep_time = config_arria_connector$upload_sleep_time,
                                                 catchword_index = config_arria_connector$catchword_index,
                                                 headline_index = config_arria_connector$headline_index,
                                                 lead_index = config_arria_connector$lead_index,
                                                 photo_path = paste0(config_arria_connector$photo_path_prefix, x$gde_nr , ".jpg"),
                                                 photo_caption_index = config_arria_connector$photo_caption_index,
                                                 department = get_department_by_bfs_nr(x$gde_nr, type_text = config_arria_connector$department_type_text),
                                                 city = get_city_id_by_bfs_nr(x$gde_nr)))
  
}

# arria_update_batch <- function(text_objs) { # result of arria_create_text_objects
#   
#   walk(text_objs, arria_update_in_ld)
#   
# }


# create empty export_journal table
empty_export_journal <- function(required_cols = c("Gemeinde_Nr",
                                                   "Gemeinde_Name",
                                                   "Newsml_Headline",
                                                   "Newsml_Lead",
                                                   "Newsml_alg_path",
                                                   "Newsml_pha_path",
                                                   "Photo_path",
                                                   "Time_Stamp"),
                                 do_save = T) {
  
  out <- tibble()
  
  for(i in required_cols) {
    
    out[[i]] <- character()
    
  }
  
  if(do_save) {
    
    write_csv(out, config_arria_connector$export_journal_path)
    
  }
  
  cat(paste0("Created empty export_journal and saved it into ", config_arria_connector$export_journal_path,  ".\n"))
  
  return(out)
  
}


# function to add one entry to export_journal
add_entry_to_export_journal <- function(new_Gemeinde_Nr = NA_real_,
                                        new_Gemeinde_Name = NA_character_,
                                        new_Headline = NA_character_,
                                        new_Lead  = NA_character_,
                                        new_alg_path = NA_character_,
                                        new_pha_path = NA_character_,
                                        new_Photo_path = NA_character_,
                                        export_journal_path = config_arria_connector$export_journal_path) {
  
  e <- read_csv(export_journal_path, col_types = cols(
    .default = col_character(),
    Gemeinde_Nr = col_double(),
    Gemeinde_Name = col_character(),
    Newsml_Headline = col_character(),
    Newsml_Lead = col_character(),
    Newsml_alg_path = col_character(),
    Newsml_pha_path = col_character(),
    Photo_path = col_character()))
  
  # tz(e$Time_Stamp) <- "Europe/Zurich"
  
  # This prevents an error when a text without photo is sent (NOT ELEGANT!)
  if(is.null(new_pha_path)) { new_pha_path = NA_character_ }
  if(is.null(new_Photo_path)) { new_Photo_path = NA_character_ }
  
  new_e <- e %>% 
    add_row(Gemeinde_Nr = new_Gemeinde_Nr,
            Gemeinde_Name = new_Gemeinde_Name,
            Newsml_Headline = new_Headline,
            Newsml_Lead = new_Lead,
            Newsml_alg_path = new_alg_path,
            Newsml_pha_path = new_pha_path,
            Photo_path = new_Photo_path,
            Time_Stamp = systime_ch(T))
  
  write_csv(new_e, export_journal_path)
  
  cat(paste0("Added Gemeinde ", new_Gemeinde_Name, " (", new_Gemeinde_Nr, ") to the list of exported texts.\n"))
  
}


# function to create empty error log
empty_error_log <- function(error_log_path = config_arria_connector$export_error_log_path,
                            do_save = F) {
  
  out <- tibble(Gemeinde_Nr = character(),
                Error_Msg = character(),
                Error_Location = character(),
                Time_Stamp = character())
  
  if(do_save) {
    
    write_csv(out, error_log_path)
    
  }
  
  return(out)
  
}

# function to add entry to error log
add_entry_to_error_log <- function(new_gde_nr,
                                   error_msg,
                                   error_location,
                                   error_log_path = config_arria_connector$export_error_log_path) {
  
  el <- read_csv(error_log_path, col_types = cols(
    Gemeinde_Nr = col_double(),
    Error_Msg = col_character(),
    Error_Location = col_character(),
    Time_Stamp = col_character()))
  
  # if(nrow(el) > 0) {
  #   tz(el$Time_Stamp) <- "Europe/Zurich"
  # }
  
  new_el <- el %>% 
    add_row(
      Gemeinde_Nr = new_gde_nr,
      Error_Msg = as.character(error_msg),
      Error_Location = error_location,
      Time_Stamp = systime_ch(T))
  
  write_csv(new_el, error_log_path)
 
  cat(paste0("Added new ERROR event to log for Gemeinde_Nr ", new_gde_nr, "\n"))
   
}

### 
### DIAGNOSTICS
### 

check_out_export_journal <- function(export_journal_path = config_arria_connector$export_journal_path) {
  
  e <- read_csv(export_journal_path, col_types = cols(
    Gemeinde_Nr = col_double(),
    Gemeinde_Name = col_character(),
    Newsml_Headline = col_character(),
    Newsml_Lead = col_character(),
    Newsml_alg_path = col_character(),
    Newsml_pha_path = col_character(),
    Photo_path = col_character(),
    Time_Stamp = col_datetime(format = "")
  ))
  
  return(e)
  
}


### 
### FOOTOMAT ADDITIONS
###

# this function serves as exporter for Footomat (untested as of 06-17)
footomat_arria_export_batch <- function() {
  
  # generate data needed for text generation
  games_to_export_df <- auto_generate(output_type = "df_for_arria_json")
  
  # get texts and export article, individually
  for(i in 1:nrow(game_to_export_df)) {
    
    # get texts (error_proofed), individually
    arria_text <- arria_get_texts(dta_4_arria = games_to_export_df[i,])
    
    # create text obj (to ensure compatibility with arria_export_ld)
    # game_text_obj <- pmap(games_to_export_df[i,] %>% transmute(a = game_id,
    #                                                         b = paste0(team_1, "-", team_2),
    #                                                         c = arria_text,
    #                                                        d = league_id),
    #                      function(a, b, c) list(gde_nr = a,
    #                                             gde_name = b,
    #                                             text_arria = c,
    #                                             league_id = d))
    #                        # function(a, b, c) list(subj_nr = a,
    #                        #                        subj_name = b,
    #                        #                        text_arria = c)) 
    
    # simplify (because it's only one row per list, anyway. List_obj is needed for compatibility with export_journal)
    game_text_obj <- list(gde_nr = games_to_export_df[i,]$game_id,
                          gde_name = paste0(games_to_export_df[i,]$team_1, "-", games_to_export_df[i,]$team_2),
                          text_arria = arria_text,
                          league_id = games_to_export_df[i,]$league_id)
    
    # use arria_export_ld to export (because it logs to export_journal, error_log)
    arria_export_ld(text_obj = game_text_obj,
                    sleep_time = config_arria_connector$upload_sleep_time,
                    catchword_index = config_arria_connector$catchword_index,
                    headline_index = config_arria_connector$headline_index,
                    lead_index = config_arria_connector$lead_index,
                    photo_path = "LD_drop/pics_repo/Regionalfussball - nur Bild.jpg",
                    photo_caption_index = config_arria_connector$photo_caption_index,
                    department = lookup_department(game_text_obj$league_id),
                    city = NULL) # TODO: replace with something like lookup_city(team1 = games_to_export_df[i,]$team_1, team2 = games_to_export_df[i,]$team_2)
    
  }
    
}
  
