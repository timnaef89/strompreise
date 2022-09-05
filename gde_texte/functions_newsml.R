# find nodes for manipulation
# 
# use brz006 as blueprint for single news article

# single_article_as_xml <- read_xml("uploader_fact_finding_mission/Schablonen/20200311101418082194158159041_brz006_copy.xml")
# single_article_as_list <- as_list(read_xml("uploader_fact_finding_mission/Schablonen/20200311101418082194158159041_brz006_copy.xml"))

# up_det <- list(revision_id = 1, previous_revision = 0, Update = "N")
# pic_det <- c("footomat:pic:20200311:phd9100", "footomat:pic:20200311:phd9100")
# 
# possibly extend to pictures <- list(caption = "Der Ball ist rund.", filename = "einphoto.jpg")

###########################################
###########################################
##### working title: Compile text etc. and output "article, optionally with photo"
###########################################
###########################################

combine_article <- function(article_catchword = NULL, 
                            article_headline = NULL,
                            article_text = NULL,
                            photo_paths = NULL, # must be vector
                            photo_captions = NULL,
                            department = NULL,
                            cities = NULL) {
  
  # check if input is valid (to be extended)
  if(is.null(article_headline) | is.null(article_text)) {stop("'article_headline' and/or 'article_text' not supplied.")}
  if(is.null(department)) {stop("No 'department' supplied.")}
  
  # create (empty) output
  out <- list(text = character(),
              photos = NULL,
              jpgs = NULL)
  
  picture_pids <- NULL
  
  
  # create photoelement(s)
  if(!is.null(photo_paths) & !is.null(photo_captions)) {
    # extract photo_filename (to be inserted into photo newsml)
    photo_filename <- map_chr(photo_paths, function(x) ifelse(str_detect(x, "/"), last(str_split(x, "/")[[1]]), x))
    photo_newsmls <- map2(photo_filename, photo_captions, function(x, y) create_photoelement_item(photo_filename = x, text_photo_caption = y, output_type = c("save", "id")))
    picture_pids <- map_chr(photo_newsmls, ~ .x$id)
    out$photos <- map_chr(photo_newsmls, ~ .x$full_path_photo_newsml)
    out$jpgs <- photo_paths
  } else {
    cat("no photo_filenames and/or photo_captions supplied.\n")
  }
  # create text_element
  text_newsml <- create_news_item(text_headline = article_headline, 
                                  text_main = article_text,
                                  text_catchword = article_catchword,
                                  pictures = picture_pids,
                                  output_type = c("save", "id"),
                                  department_to_publish = department,
                                  cityids = cities)
  out$text <- text_newsml$full_path_text_newsml
  # output list with paths for text, photo, and jpg (file path must be remembered outside of this function)
  # this can then be fed to an uploader function making use of upload_to_ftp_by_path()
  return(out)
}


###########################################
###########################################
##### Create NewsML Document for text article
###########################################
###########################################

create_news_item <- function(newsml_template = as_list(read_xml(config_gen$empty_xml_text)),
                             version_time = as.POSIXct(Sys.time()),
                             item_nr = NULL,
                             text_headline = "",
                             text_catchword = "Regio-Fussball",
                             text_main = c("Lead", "Abschnitt1", "Abschnitt2", "Abschnitt3"),
                             department_to_publish = "LD Spielberichte Regiofussball Ostschweiz",
                             cityids = NULL,
                             pictures = NULL,
                             output_type = "newsml") {
  
  version_time_full <- format(version_time, format = "%Y%m%dT%H%M%S%z")
  version_time_dmy <- format(version_time, format = "%Y%m%d")
  version_time_dmyhms <- format(version_time, format = "%Y%m%d%H%M%S")
  
  # find an unused item_nr (if none was provided to the function)
  if(is.null(item_nr)) {
    item_nr <- paste0("alg", get_next_item_id(date_to_check = as.Date(version_time), 
                                              service_to_check = "alg",
                                              digits_required = 4))
  }
  
  new_public_id <- paste("footomat", version_time_dmy, item_nr, "1N", sep = ":")
  
  # Date and Time
  # "20200311T101410+0100"
  # 
  newsml_template[["NewsML"]][["NewsEnvelope"]][["DateAndTime"]][[1]] <- version_time_full
  
  # "20200311"
  # 
  newsml_template[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["DateId"]][[1]] <- version_time_dmy
  
  # item nr (Meldungsnummer)
  # "brz006"
  # 
  newsml_template[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["NewsItemId"]][[1]] <- item_nr
  
  
  # ID
  # "urn:newsml:www.sda-ats.ch:20200311:brz006:1N"
  #
  newsml_template[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["PublicIdentifier"]][[1]] <- new_public_id
  
  # Created at (Erstellungsdatum)
  # "20200311T101410+0100"
  # 
  newsml_template[["NewsML"]][["NewsItem"]][["NewsManagement"]][["FirstCreated"]][[1]] <- version_time_full
  
  # Modified at (?nderungsdatum)
  # "20200311T101410+0100"
  newsml_template[["NewsML"]][["NewsItem"]][["NewsManagement"]][["ThisRevisionCreated"]][[1]] <- version_time_full
  
  newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["DescriptiveMetadata"]][["Department"]][[1]] <- department_to_publish
  
  
  # HERE CityID
  # 
  
  if(!is.null(cityids)) {
    for(ci in cityids) {
      newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["DescriptiveMetadata"]] <- c(newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["DescriptiveMetadata"]], create_keyword_element(ci))
    }  
  }
  
  
  
  #### TEXT
  # Headline:
  # "Eintracht Frankfurt - Basel vor vollen R?ngen"
  # 
  newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["NewsLines"]][["HeadLine"]][[1]] <- text_headline
  
  # Catchword (Spitzmarke)
  # "Europa League"
  # 
  newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["NewsLines"]][["NewsLine"]][["NewsLineText"]][[1]] <- text_catchword
  
  # Main content (Text)
  text_to_fill <- construct_list_with_text(text_main)
  
  newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["NewsComponent"]][["ContentItem"]][["DataContent"]][["nitf"]][["body"]][["body.content"]] <- text_to_fill
  
  # make sure that lead (lede) is designated as such
  attributes(newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["NewsComponent"]][["ContentItem"]][["DataContent"]][["nitf"]][["body"]][["body.content"]][[1]])$lede <- "true"
  
  #### BILDER
  #### 
  # Bildversion:
  # attributes(single_article_as_list[["NewsML"]][["NewsItem"]][["NewsManagement"]][["AssociatedWith"]])$NewsItem
  # "urn:newsml:www.sda-ats.ch:20200311:phd9076"
  # 
  if(!is.null(pictures)) {
    for(i in pictures) {
      photoelement_to_add <- create_photolement_list(i)
      newsml_template[["NewsML"]][["NewsItem"]][["NewsManagement"]] <- c(newsml_template[["NewsML"]][["NewsItem"]][["NewsManagement"]], photoelement_to_add)
    }  
  }
  
  out <- list()
  if("newsml" %in% output_type) {out$text_xml = newsml_template}
  if("id" %in% output_type) {out$id = new_public_id}
  if("save" %in% output_type)  {
    filename_of_text_newsml <- paste0(version_time_dmyhms, "_", item_nr, ".xml")
    full_path_text_newsml <- paste0(config_gen$newsml_folder, filename_of_text_newsml)
    write_xml(as_xml_document(newsml_template), full_path_text_newsml)
    # print(paste0("saved in: ", full_path_text_newsml))
    out$filename_text_newsml <- filename_of_text_newsml
    out$full_path_text_newsml <- full_path_text_newsml
  }
  return(out)
  
}

# NOT TO BE USED YET!
# TODOs: new and old ItemID (brd001 vs. brd002), text/headline/catchword not implemented yet
update_newsml <- function(previous_news_item,
                          new_catchword = NULL,
                          new_headline = NULL,
                          new_text = NULL,
                          nr_of_revision = NULL,
                          update_type = "N"
) {
  
  old_pid <- previous_news_item[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["PublicIdentifier"]][[1]]
  print(old_pid)
  print(substr(old_pid, str_length(old_pid)-1, str_length(old_pid)))
  old_revision_nr <- as.numeric(substr(old_pid, str_length(old_pid)-1, str_length(old_pid)-1))
  print(old_revision_nr)
  
  if(is.null(nr_of_revision)) {nr_of_revision <- old_revision_nr+1}
  
  # get new pid and insert
  new_pid <- modify_pid_for_update(original_pid = old_pid,
                                   revision_id = nr_of_revision,
                                   update_attr = update_type)
  previous_news_item[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["PublicIdentifier"]][[1]] <- new_pid
  
  # create and insert instruction node
  instr_node_to_insert <- create_instruction_node(revision_nr = nr_of_revision,
                                                  status_formal_name = update_type)
  previous_news_item[["NewsML"]][["NewsItem"]][["NewsManagement"]] <- c(previous_news_item[["NewsML"]][["NewsItem"]][["NewsManagement"]], instr_node_to_insert)
  
  # Modify revision ID and its attributes
  previous_news_item[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["RevisionId"]][[1]] <- as.numeric(nr_of_revision)
  attributes(previous_news_item[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["RevisionId"]]) <- list(PreviousRevision = as.character(old_revision_nr), Update = update_type)
  # "1"
  # Attention: hat auch Attribute! (PreviousRevision, Update - ?ndert sich nur bei Updates)
  # attributes(single_article_as_list[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["RevisionId"]])
  
  return(previous_news_item)
}

# PROBABLY NOT NEEDED
# save_nd_upload_newsml <- function(newsml_to_upload, 
#                                   newsml_folder = "saved_newsml",
#                                   do_upload = T,
#                                   ftp_destination = "nzz-test") {
#   
#   # figure out parameters for file name
#   version_time_dmyhms <- format(as.POSIXct(newsml_to_upload[["NewsML"]][["NewsEnvelope"]][["DateAndTime"]][[1]], format = "%Y%m%dT%H%M%S%z"), format = "%Y%m%d%H%M%S")
#   item_nr <- newsml_to_upload[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["NewsItemId"]][[1]]
#   
#   # construct file name, convert, save, and upload
#   correct_path <- paste0(version_time_dmyhms, "_", item_nr, ".xml")
#   full_path <- paste0(newsml_folder, "/", correct_path)
#   write_xml(as_xml_document(newsml_to_upload), full_path)
#   
#   cat(paste0("saved in ", full_path, "\n"))
#   
#   if(do_upload) {
#     switch(ftp_destination,
#            nzz-test = ftp_status <- ftpUpload(full_path, 
#                                               paste0(getOption("ftp_nzz_folder_test"), correct_path),
#                                               userpwd = getOption("ftp_nzz_userpwd")),
#            lovely = ftp_status <- ftpUpload(full_path,
#                             paste0(getOption("ftp_lovely_folder"), correct_path),
#                             userpwd = getOption("ftp_lovely_userpwd")))
#     cat("and uploaded.\n")
#     return(ftp_status)
#   }
# }


# upload routine
upload_article <- function(list_of_article_paths,
                           destination = "nzz_test") {
  
  out <- list()
  # upload photo
  if(!is.null(list_of_article_paths$jpgs)) {
    # map_chr(list_of_article_paths$jpgs, function(x) print(x))
    out$jpg_status <- map(list_of_article_paths$jpgs, upload_to_ftp_by_path, ftp_destination = destination)
  }
  
  # upload photo newsml 
  if(!is.null(list_of_article_paths$photos)) {
    # map_chr(list_of_article_paths$photos, function(x) print(x))
    out$photo_status <- map(list_of_article_paths$photos, upload_to_ftp_by_path, ftp_destination = destination)
  }
  
  # upload article
  # print(list_of_article_paths$text)
  out$text_status <- upload_to_ftp_by_path(list_of_article_paths$text, ftp_destination = destination)
  
  return(out)
  
}

# upload single file
upload_to_ftp_by_path <- function(path_of_file,
                                  filename_on_ftp = ifelse(str_detect(path_of_file, "/"), last(str_split(path_of_file, "/")[[1]]), path_of_file),
                                  ftp_destination = config_arria_connector$ftp_destination,
                                  do_html_decode = T,
                                  sleep_time = NULL) {
  
  if(do_html_decode) {
    escaped_html <- readLines(path_of_file)
    unescaped_html <- HTMLdecode(escaped_html)
    writeLines(unescaped_html, path_of_file)
  }
  # print("path of file")
  # print(path_of_file)
  # print("filename on ftp")
  # print(filename_on_ftp)
  switch(ftp_destination,
         nzz_test = ftp_status <- ftpUpload(path_of_file,
                                            paste0(getOption("ftp_nzz_folder_test"), filename_on_ftp),
                                            userpwd = getOption("ftp_nzz_userpwd")),
         nzz_stage = ftp_status <- ftpUpload(path_of_file,
                                             paste0(getOption("ftp_nzz_folder_stage"), filename_on_ftp),
                                             userpwd = getOption("ftp_nzz_userpwd")),
         nzz_prod = ftp_status <- ftpUpload(path_of_file,
                                            paste0(getOption("ftp_nzz_folder_prod"), filename_on_ftp),
                                            userpwd = getOption("ftp_nzz_userpwd")),
         lovely = ftp_status <- ftpUpload(path_of_file,
                                          paste0(getOption("ftp_lovely_folder"), filename_on_ftp),
                                          userpwd = getOption("ftp_lovely_userpwd")))
  cat(paste("uploaded", filename_on_ftp, "to", ftp_destination, "@", format(systime_ch(), format = "%Y-%m-%d %H:%M"), sep = " "))
  cat("\n")
  cat("FTP STATUS (0 = all good!):\n")
  cat(ftp_status)
  cat("\n")
  
  if(!is.null(sleep_time)) {
    
    cat(paste0("Waiting for ", sleep_time, " second(s).\n"))
    Sys.sleep(sleep_time)
    
  }
  
  return(ftp_status)
  
} 


###########################################
###########################################
##### Create NewsML Document for PICTURE
###########################################
###########################################

# create photoelement xml
# called from create_news_item, must return public_identifier (in order to create "AssociatedWith"-node)
# (or wrapper function...), should return full path of saved photoelement xml.

# give all the details and this will create and return or save a photoelement xml
create_photoelement_item <- function(photoelement_xml_template = read_xml(config_gen$empty_xml_photo),
                                     version_time = as.POSIXct(Sys.time()),
                                     photo_item_nr = NULL,
                                     text_photo_caption = "",
                                     photo_filename = NULL,
                                     output_type = "newsml") {
  photoel_xml <- photoelement_xml_template
  version_time_full <- format(version_time, format = "%Y%m%dT%H%M%S%z")
  version_time_dmy <- format(version_time, format = "%Y%m%d")
  version_time_dmyhms <- format(version_time, format = "%Y%m%d%H%M%S")
  
  # find an unused photo_item_nr (if none was provided to the function)
  if(is.null(photo_item_nr)) {
    photo_item_nr <- paste0("pha", get_next_item_id(date_to_check = as.Date(version_time),
                                                    service_to_check = "pha",
                                                    digits_required = 4))
  }
  new_photo_public_id <- paste("urn:footomat:picture", version_time_dmy, photo_item_nr, "1N", sep = ":")
  
  # # Date and Time
  # "20200311T101410+0100"
  xml_set_text(xml_child(xml_child(photoel_xml, 2), 1), version_time_full)
  
  # "20200311"
  xml_set_text(xml_child(xml_child(xml_child(xml_child(photoel_xml, 3), 1), 1), 2), version_time_dmy)
  
  # item nr (Meldungsnummer)
  # "phd9905"
  xml_set_text(xml_child(xml_child(xml_child(xml_child(photoel_xml, 3), 1), 1), 3), photo_item_nr)
  
  # ID
  # "urn:footomat:picture:20200514:phd9905:1N"
  xml_set_text(xml_child(xml_child(xml_child(xml_child(photoel_xml, 3), 1), 1), 5), new_photo_public_id) 
  
  # Created at (Erstellungsdatum)
  # "20200311T101410+0100"
  xml_set_text(xml_child(xml_child(xml_child(photoel_xml, 3), 2), 2), version_time_full)
  
  # Modified at (Aenderungsdatum)
  xml_set_text(xml_child(xml_child(xml_child(photoel_xml, 3), 2), 3), version_time_full)
  
  #### Caption
  # Headline, Subheadline, Text body:
  # "Der Ball ist rund."
  xml_set_text(xml_child(xml_child(xml_child(xml_child(photoel_xml, 3), 3), 1), 1), text_photo_caption)
  xml_set_text(xml_child(xml_child(xml_child(xml_child(photoel_xml, 3), 3), 1), 3), text_photo_caption)
  # First NewsComponent
  xml_set_text(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(photoel_xml, 3), 3), 4), 1), 3), 1), 1), 1), 1), text_photo_caption)
  # set lede attribute to true
  xml_set_attr(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(photoel_xml, 3), 3), 4), 1), 3), 1), 1), 1), 1), "lede", "true")
  # Third NewsComponent (THIS IS THE RELEVANT ONE)
  xml_set_text(xml_child(xml_child(xml_child(xml_child(xml_child(photoel_xml, 3), 3), 6), 2), 1), text_photo_caption)
  
  
  # Set filename of JPEG (twice)
  # Second NewsComponent (Main)
  xml_set_attr(xml_child(xml_child(xml_child(xml_child(photoel_xml, 3), 3), 5), 2), "Href", photo_filename)
  # Third NewsComponent (Caption)
  xml_set_attr(xml_child(xml_child(xml_child(xml_child(photoel_xml, 3), 3), 6), 2), "Href", photo_filename)
  
  out <- list()
  if("newsml" %in% output_type) {out$photo_xml = photoel_xml}
  if("id" %in% output_type) {out$id = new_photo_public_id}
  if("save" %in% output_type)  {
    filename_of_photo_newsml <- paste0(version_time_dmyhms, "_", photo_item_nr, ".xml")
    full_path_photo_newsml <- paste0(config_gen$newsml_folder, filename_of_photo_newsml)
    write_xml(as_xml_document(photoel_xml), full_path_photo_newsml)
    print(paste0("saved in: ", full_path_photo_newsml))
    out$filename_photo_newsml <- filename_of_photo_newsml
    out$full_path_photo_newsml <- full_path_photo_newsml
  }
  return(out)
}

###########################################
###########################################
##### HELPER
###########################################
###########################################

# NEW VERSION (WITH ZWTTL)
construct_list_with_text <- function(vector_of_parphs,
                                     short_for_h2 = "#hl2") {
  
  out <- list(p = list(vector_of_parphs[1]))
  
  for(i in 2:length(vector_of_parphs)) {
    
    if(str_detect(vector_of_parphs[i], short_for_h2)) {
      
      new_parph <- list()
      new_parph[["hl2"]] <- list(trim(str_replace_all(vector_of_parphs[i], short_for_h2, "")))
      
    } else {
      
      new_parph <- list(p = list(vector_of_parphs[i]))  
      
    }
    
    out <- c(out, new_parph)
    
  }
  
  return(out)
  
}


# get list from paragraphs (ORIGINAL)
# construct_list_with_text <- function(vector_of_parphs) {
#   
#   out <- list(p = list(vector_of_parphs[1]))
#   
#   for(i in 2:length(vector_of_parphs)) {
#     new_parph <- list(p = list(vector_of_parphs[i]))
#     out <- c(out, new_parph)
#   }
#   
#   return(out)
#   
# }

# this creates a list element needed for photo references (creates one reference for one photo document)
create_photolement_list <- function(picture_doc_name) {
  new_photoelement <- list(AssociatedWith = list())
  attributes(new_photoelement$AssociatedWith) <- list(NewsItem = picture_doc_name)
  return(new_photoelement)
}

# this creates the instruction node required for an update
create_instruction_node <- function(revision_nr = "1",
                                    status_formal_name = "Update") {
  
  if(!is.character(revision_nr)) {revision_nr <- as.character(revision_nr)}
  
  instruction_node <- list(Instruction = list(RevisionStatus = list(),
                                              Status = list()))
  attributes(instruction_node$Instruction$RevisionStatus) <- list(Revision = revision_nr)
  attributes(instruction_node$Instruction$Status) <- list(FormalName = status_formal_name)
  return(instruction_node)
  
}

# this modifies the public_identifier value for an update (or delete)
modify_pid_for_update <- function(original_pid, revision_id, update_attr = "N") {
  pid_insert <- paste0(revision_id, update_attr)
  new_pid <- paste0(substr(original_pid, 1, str_length(original_pid)-2), pid_insert)
  return(new_pid)
}


# function to log item id into csv table (used in get_next_item_id)
log_item_id <- function(id_to_log,
                        date_to_log,
                        service_to_log,
                        history_path = config_paths$id_logging_drop) {
  
  history_to_log_into <- suppressMessages(read_csv(history_path))
  
  log_row <- tibble(date = date_to_log,
                    service = service_to_log,
                    nr = id_to_log)
  
  new_id_history <- bind_rows(history_to_log_into,
                              log_row)
  
  write_csv(new_id_history, history_path)
  
  # return(new_id_history)
}

# receive a new (news item) id nr (to construct item ids like "brz101") by providing date and service
get_next_item_id <- function(date_to_check = as.Date(Sys.Date()),
                             digits_required = 3,
                             service_to_check = "brz",
                             history_to_check_path = config_paths$id_logging_drop,
                             start_value = 10^(digits_required-1)) {
  
  id_history <- suppressMessages(read_csv(history_to_check_path))
  
  ids_for_day <- id_history %>%
    filter(date == date_to_check) %>%
    filter(service == service_to_check) %>% 
    filter(nchar(nr) == digits_required)
  
  if(nrow(ids_for_day) == 0) {
    out <- start_value
  } else {
    max_id <- ids_for_day %>% summarise(max_id = max(nr)) %>% unlist()
    out <- max_id + 1
  }
  # log id as used (no matter if it is sent...)
  log_item_id(out, date_to_check, service_to_check)
  return(out)
}


# UNESCAPE Texts (for Sternwald!!)
# aaa <- readLines("saved_newsml/20200610113632_alg110.xml")
# bbb <- HTMLdecode(aaa)
# writeLines(bbb, "testfile_writeLines.xml")

unescape_texts <- function(article_obj) {
  aaa <- readLines(article_obj$text)
  bbb <- HTMLdecode(aaa)
  writeLines(bbb, article_obj$text)
}

# quick_upload <- function(list_with_all_paths) {
#   
#   map(list_with_all_paths$text, upload_to_ftp_by_path, do_html_decode = T)
#   
#   if(!is.null(list_with_all_paths$photos) & !is.null(list_with_all_paths$jpgs)) {
# 
#     map(list_with_all_paths$jpgs, 
#         upload_to_ftp_by_path, 
#         do_html_decode = F, 
#         sleep_time = 1)
#     
#     map(list_with_all_paths$photos, 
#         upload_to_ftp_by_path, 
#         do_html_decode = F, 
#         sleep_time = 1)
#   
#     } else {
#     
#       cat("No photos supplied.")
#   
#     }
#   
# }

# extended version of quick_upload which also returns stati
quick_upload <- function(list_with_all_paths) {
  
  out <- list()
  
  out$text_status <- map(list_with_all_paths$text, upload_to_ftp_by_path, do_html_decode = T)
  
  if(!is.null(list_with_all_paths$photos) & !is.null(list_with_all_paths$jpgs)) {
    
    out$photo_status <- map(list_with_all_paths$jpgs, 
                            upload_to_ftp_by_path, 
                            do_html_decode = F, 
                            sleep_time = 1)
    
    out$jpg_status <- map(list_with_all_paths$photos, 
                          upload_to_ftp_by_path, 
                          do_html_decode = F, 
                          sleep_time = 1)
    
  } else {
    
    cat("No photos supplied.\n")
    
  }
  
  return(out)
  
}

# this function copies the existing sample picture and returns the path for the new picture
# is used in tests to add a random picture to a story
get_random_pic <- function() {
  
  # TODO: integrate randomiser 
  
  new_pic_path <- paste0("LD_drop/pics_repo/", format(Sys.time(), format = "%Y%m%d%H%M%S"), ".jpg")
  file.copy(from = "LD_drop/pics_repo/Beispielbild_01.jpg", to = new_pic_path)
  return(new_pic_path)
  
}

create_keyword_element <- function(kw) {
  
  out <- list(keywords = list(kw))
  
  return(out)
  
} 


## attempt update_article 
# including possible rewrite of "create_news_item"


###########################################
###########################################
##### update_article (adapted from combine_article)
###########################################
###########################################

update_article <- function(article_2b_updated_path,
                           article_catchword = NULL, 
                           article_headline = NULL,
                           article_text = NULL,
                           photo_paths = NULL, # must be vector
                           photo_captions = NULL,
                           department = NULL,
                           cities = NULL) {
  
  # check if input is valid (to be extended)
  # is not relevant for update as all three canmay be left as is
  # if(is.null(article_headline) | is.null(article_text)) {stop("'article_headline' and/or 'article_text' not supplied.")}
  # if(is.null(department)) {stop("No 'department' supplied.")}
  
  # create (empty) output
  out <- list(text = character(),
              photos = NULL,
              jpgs = NULL)
  
  picture_pids <- NULL
  
  
  # create photoelement(s)
  if(!is.null(photo_paths) & !is.null(photo_captions)) {
    # extract photo_filename (to be inserted into photo newsml)
    photo_filename <- map_chr(photo_paths, function(x) ifelse(str_detect(x, "/"), last(str_split(x, "/")[[1]]), x))
    photo_newsmls <- map2(photo_filename, photo_captions, function(x, y) create_photoelement_item(photo_filename = x, text_photo_caption = y, output_type = c("save", "id")))
    picture_pids <- map_chr(photo_newsmls, ~ .x$id)
    out$photos <- map_chr(photo_newsmls, ~ .x$full_path_photo_newsml)
    out$jpgs <- photo_paths
  } else {
    cat("no photo_filenames and/or photo_captions supplied.\n")
  }
  # update text_element
  text_newsml <- update_news_item(newsml_path = article_2b_updated_path,
                                  text_headline = article_headline, 
                                  text_main = article_text,
                                  text_catchword = article_catchword,
                                  pictures = picture_pids,
                                  output_type = c("save"),
                                  department_to_publish = department,
                                  cityids = cities)
  
  out$text <- article_2b_updated_path
  # ORIGINAL out$text <- text_newsml$full_path_text_newsml
  # output list with paths for text, photo, and jpg (file path must be remembered outside of this function)
  # this can then be fed to an uploader function making use of upload_to_ftp_by_path()
  return(out)
}


###########################################
###########################################
##### Update existing NewsML Document for text article
###########################################
###########################################

update_news_item <- function(newsml_path,
                             version_time = as.POSIXct(Sys.time()),
                             item_nr = NULL,
                             text_headline = NULL,
                             text_catchword = NULL,
                             text_main = NULL,
                             department_to_publish = NULL,
                             cityids = NULL,
                             pictures = NULL,
                             output_type = "save") {
  
  newsml_template <- as_list(read_xml(newsml_path))
  
  version_time_full <- format(version_time, format = "%Y%m%dT%H%M%S%z")
  version_time_dmy <- format(version_time, format = "%Y%m%d")
  version_time_dmyhms <- format(version_time, format = "%Y%m%d%H%M%S")
  
  # HERE: save copy of "old" version
  path_for_old_version <- paste0(str_replace_all(newsml_path, ".xml", ""), "_s", version_time_dmyhms, ".xml")
  file.copy(newsml_path, path_for_old_version)
  # write_xml(as_xml_document(newsml_template), path_for_old_version)
  
  cat("Old version of newsml save in: ", path_for_old_version, "\n")
  
  # # find an unused item_nr (if none was provided to the function)
  # if(is.null(item_nr)) {
  #   item_nr <- paste0("alg", get_next_item_id(date_to_check = as.Date(version_time), 
  #                                             service_to_check = "alg",
  #                                             digits_required = 3))
  # }
  # 
  # new_public_id <- paste("footomat", version_time_dmy, item_nr, "1N", sep = ":")
  # 
  # # Date and Time
  # # "20200311T101410+0100"
  # # 
  # newsml_template[["NewsML"]][["NewsEnvelope"]][["DateAndTime"]][[1]] <- version_time_full
  # 
  # # "20200311"
  # # 
  # newsml_template[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["DateId"]][[1]] <- version_time_dmy
  # 
  # # item nr (Meldungsnummer)
  # # "brz006"
  # # 
  # newsml_template[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["NewsItemId"]][[1]] <- item_nr
  # 
  # 
  # # ID
  # # "urn:newsml:www.sda-ats.ch:20200311:brz006:1N"
  # #
  # newsml_template[["NewsML"]][["NewsItem"]][["Identification"]][["NewsIdentifier"]][["PublicIdentifier"]][[1]] <- new_public_id
  # 
  # # Created at (Erstellungsdatum)
  # # "20200311T101410+0100"
  # # 
  # newsml_template[["NewsML"]][["NewsItem"]][["NewsManagement"]][["FirstCreated"]][[1]] <- version_time_full
  
  # Modified at (?nderungsdatum)
  # "20200311T101410+0100"
  newsml_template[["NewsML"]][["NewsItem"]][["NewsManagement"]][["ThisRevisionCreated"]][[1]] <- version_time_full
  
  if(!is.null(department_to_publish)) { # department will only be changed if there is a value in the parameter
    newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["DescriptiveMetadata"]][["Department"]][[1]] <- department_to_publish
  }
  
  
  # HERE CityID
  # 
  
  if(!is.null(cityids)) { # cityid will only be changed if there is a value in the parameter
    for(ci in cityids) {
      newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["DescriptiveMetadata"]] <- c(newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["DescriptiveMetadata"]], create_keyword_element(ci))
    }  
  }
  
  
  
  #### TEXT
  # general rule for updates: only replace what is NOT NULL, rest is left as is
  # Headline:
  # "Eintracht Frankfurt - Basel vor vollen R?ngen"
  # 
  if(!is.null(text_headline)) {
    newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["NewsLines"]][["HeadLine"]][[1]] <- text_headline
  }
  
  # Catchword (Spitzmarke)
  # "Europa League"
  # 
  if(!is.null(text_catchword)) {
    newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["NewsLines"]][["NewsLine"]][["NewsLineText"]][[1]] <- text_catchword
  }
  
  # Main content (Text)
  if(!is.null(text_main)) {
    text_to_fill <- construct_list_with_text(text_main)
    
    newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["NewsComponent"]][["ContentItem"]][["DataContent"]][["nitf"]][["body"]][["body.content"]] <- text_to_fill
    
    # make sure that lead (lede) is designated as such
    attributes(newsml_template[["NewsML"]][["NewsItem"]][["NewsComponent"]][["NewsComponent"]][["ContentItem"]][["DataContent"]][["nitf"]][["body"]][["body.content"]][[1]])$lede <- "true"  
    
  }
  
  
  #### BILDER
  #### 
  # Bildversion:
  # attributes(single_article_as_list[["NewsML"]][["NewsItem"]][["NewsManagement"]][["AssociatedWith"]])$NewsItem
  # "urn:newsml:www.sda-ats.ch:20200311:phd9076"
  if(!is.null(pictures)) {
    # default case: empty photos, before adding new ones
    photoelement_to_add <- create_photolement_list(pictures[1])
    newsml_template[["NewsML"]][["NewsItem"]][["NewsManagement"]] <- photoelement_to_add
    
    if(length(pictures) > 1) {
      
      for(i in pictures[2:length(pictures)]) {
        photoelement_to_add <- create_photolement_list(i)
        newsml_template[["NewsML"]][["NewsItem"]][["NewsManagement"]] <- c(newsml_template[["NewsML"]][["NewsItem"]][["NewsManagement"]], photoelement_to_add)
      }    
      
    }
    
  }
  
  
  out <- list()
  if("newsml" %in% output_type) {out$text_xml = newsml_template}
  # if("id" %in% output_type) {out$id = new_public_id}
  if("save" %in% output_type)  {
    # filename_of_text_newsml <- newsml_path
    full_path_text_newsml <- newsml_path
    write_xml(as_xml_document(newsml_template), full_path_text_newsml)
    print(paste0("saved in: ", full_path_text_newsml))
    # out$filename_text_newsml = filename_of_text_newsml
    out$full_path_text_newsml = full_path_text_newsml
  }
  return(out)
  
}
