# Initialise all sorts of config and tables
# Should be loaded from github (at least config_paths, config_gen) 
# (for transparency reasons, and to reconstruct)

# here we store paths for all files that change regularly and are thus stored in gcs
config_paths <- list(
  logfile = "logfile_01.csv", # where the logfile lives
  league_journal = "league_journal01.csv", # where the league_journal lives
  game_journal = "game_journal_01.csv", # where the game_journal lives
  scraping_journal = "scraping_journal_01.csv", # where the scraping journal lives
  collected_scrapes = "collected_scrapes_01.csv",
  tbl_events = "tbl_events.csv",
  tbl_lineups = "tbl_lineups.csv",
  id_logging_drop = "id_logging.csv" # this is a table where all the ids used at a given day are stored in
)

# here we have all the files that stay the same (special case: "run_data_repo"), 
# and are thus stored in github
config_gen <- list(
  run_data_repo = "games_data_repo/", # where the downloaded (game) data should go  
  league_meta = "league_metadata - v2020 - 19leagues_v3.csv",
  mapping_team_names = "basis_mapping_long_short_v2020 - v7.csv",
  position_codes = "alle_positionen - reimport.csv", # this is where codes for the positions in the line up are stored
  schedule = "schedule.csv",
  empty_xml_text = "empty_xml.xml",
  empty_xml_photo = "empty_photo_xml_test.xml",
  # newsml_folder = "saved_newsml/",
  newsml_folder = "test_newsml/",
  bucket_tables = "twe_tables",
  bucket_newsml = "twe_newsml",
  bucket_texts = "twe_texts"
)



# tbl_logfile -> stores logging information (as a table as of now)
tbl_logfile <- tibble(datetime = Sys.time(),
                      message = "Initialise logfile",
                      status = "SUCCESS",
                      agent = "main")
write_csv(tbl_logfile, config_paths$logfile)

# initialise id_logging-file
id_history <- tibble(date = as.Date(Sys.Date()),
                     service = "alg",
                     nr = 3000) %>% 
  add_row(date = as.Date(Sys.Date()),
          service = "pha",
          nr = 3000)
write_csv(id_history, config_paths$id_logging_drop)

### GAME JOURNAL: must be created on game_dat


# scraping_journal -> stores information on (started) scraping job
# possible: extend with column status (success | failed) to keep track of ALL attempted scraping jobs and in order to compute an accurate tally
scraping_journal <- tibble(scraping_id = numeric(0),
                           time_started = NA,
                           type = character(0),
                           league_id = character(0),
                           game_id = character(0),
                           game_date = NA,
                           run_token = character(0),
                           collection_done = logical(0),
                           collection_failed_counter = numeric(0),
                           next_try = NA)
write_csv(scraping_journal, config_paths$scraping_journal)

## bb <- read_csv(config_paths$scraping_journal)

tbl_collected_scrapes <- tibble(id = numeric(0),
                                run_token = character(0),
                                game_id = character(0),
                                collection_time = NA,
                                path = character(0))

write_csv(tbl_collected_scrapes, config_paths$collected_scrapes)

# tbl_bare_game_data -> league_id, game_url, team_home, team_away, score_home, score_away

# tbl_events -> includes information per game on scorers, cards, exchanges

tbl_events <- tibble(game_id = character(0),
                     event_time_regular = character(0),
                     event_time_extra = character(0),
                     event_time_sequential = integer(0),
                     event_type = character(0),
                     event_team = character(0),
                     event_details_1 = character(0),
                     event_details_2 = character(0),
                     scorer = character(0),
                     penalty = double(0),
                     special = character(0),
                     player_out = character(0),
                     player_in = character(0),
                     team_home_short = character(0),
                     team_away_short = character(0),
                     goal_home = double(0),
                     goal_away = double(0),
                     running_score_home = double(0),
                     running_score_away = double(0),
                     running_differential_home = double(0),
                     running_differential_away = double(0),
                     goal_number_home = double(0),
                     goal_number_away = double(0))

write_csv(tbl_events, config_paths$tbl_events)

# tbl_lineup -> includes information per game on lineups of both teams 
tbl_lineups <- tibble(
  game_id = character(0),
  team = character(0),
  home_away = character(0),
  name = character(0),
  position = character(0),
  captain = double(0),
  code = double(0)
)

write_csv(tbl_lineups, config_paths$tbl_lineups)
