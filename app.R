# Trackman Live Dash ------------------------------------------------------
if (dir.exists("/srv/shiny-server/trackman_live_dash/library")) {
   .libPaths("/srv/shiny-server/trackman_live_dash/library")
}
# Packages -----------------------------------------------------------
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyjs))
suppressMessages(library(tablerDash))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinyEffects))
suppressMessages(library(pushbar))
suppressMessages(library(waiter))
suppressMessages(library(ggplot2))
suppressMessages(library(sysfonts))
suppressMessages(library(showtext))
suppressMessages(library(DT))
suppressMessages(library(reactable))
suppressMessages(library(htmltools))
suppressMessages(library(httr))
suppressMessages(library(RMySQL))
suppressMessages(library(zoo))
suppressMessages(library(dplyr))
suppressMessages(library(readr))
suppressMessages(library(tidyr))
suppressMessages(library(tidyverse))
suppressMessages(library(workflows))
suppressMessages(library(dials))
suppressMessages(library(xgboost))

# Pitch and Stuff Models --------------------------------------------
stuff_model_fb_right <- readRDS("www/fastball_R_Stuff_xgb1.rds")
stuff_model_fb_left <- readRDS("www/fastball_L_Stuff_xgb1.rds")
stuff_model_bb_right <- readRDS("www/breaking_ball_R_Stuff_xgb1.rds")
stuff_model_bb_left <- readRDS("www/breaking_ball_L_Stuff_xgb1.rds")
stuff_model_os_right <- readRDS("www/changeup_R_Stuff_xgb1.rds")
stuff_model_os_left <- readRDS("www/changeup_L_Stuff_xgb1.rds")
pitch_model_fb_right <- readRDS("www/fastball_R_xgb1.rds")
pitch_model_fb_left <- readRDS("www/fastball_L_xgb1.rds")
pitch_model_bb_right <- readRDS("www/breakingball_R_xgb1.rds")
pitch_model_bb_left <- readRDS("www/breaking_ball_L_xgb1.rds")
pitch_model_os_right <- readRDS("www/changeup_R_xgb1.rds")
pitch_model_os_left <- readRDS("www/changeup_L_xgb1.rds")
pfx_x_exp_mov <- readRDS("www/mov_x_dif_model.rds")
pfx_z_exp_mov <- readRDS("www/mov_z_dif_model.rds")

# Aesthetics --------------------------------------------------------------
font_add(family = "gotham", regular = "www/Gotham.ttf")
font_add(family = "lato", regular = "www/Lato.ttf")
showtext_auto()

# Colors & Fills for Pitch Types (Similar to Trackman Color Code).
pitch_fills <- scale_fill_manual(
  values = c("FF" = "#000000",
             "SI" = "#DC143C",
             "CB" = "#e4670a",
             "CT" = "#9728b2",
             "SL" = "#2e7bff",
             "CH" = "#008000",
             "SP" = "#309688"))

pitch_colors <- scale_color_manual(
  values = c("FF" = "#000000",
             "SI" = "#DC143C",
             "CB" = "#e4670a",
             "CT" = "#9728b2",
             "SL" = "#2e7bff",
             "CH" = "#008000",
             "SP" = "#309688"))

# Computer Vision Database ------------------------------------------------

# Access Trackman API data through R by creating database connection object.
# Credentials have been redacted to environment variables.
computer_vision_trackman_api <- dbConnect(MySQL(),
                                          user = Sys.getenv('CLUSTER_USERNAME_COMPUTER_VISION_CLUSTER'),
                                          password = Sys.getenv('CLUSTER_PASSWORD_COMPUTER_VISION_CLUSTER'),
                                          dbname = Sys.getenv('DATABASE_TRACKMAN_API_DB'),
                                          host = Sys.getenv('CLUSTER_HOST_COMPUTER_VISION_CLUSTER'),
                                          port = as.numeric(Sys.getenv('CLUSTER_PORT_COMPUTER_VISION_CLUSTER')))

# Query for all pitches thrown in facility paired with advanced metrics (Stuff+, xERA) and athlete, session, and location metadata.
trackman_api_query <- "SELECT *
                       FROM trackman_api_pitches_raw t1
                       LEFT JOIN trackman_live_dash_inputs t2 ON t2.pitchuid = t1.trackman_pitch_id
                       ORDER BY id DESC"

# Run the desired query above.
trackman_api_query_result <- dbSendQuery(computer_vision_trackman_api, trackman_api_query)

# Retrieve data from MySQL and save as a data frame object.
trackman_api <- fetch(trackman_api_query_result, n = -1)

# Query for last pitch thrown in facility without advanced metrics (Stuff+, xERA).
last_throw_query <- "SELECT *
                     FROM trackman_api_pitches_raw t1
                     ORDER BY id DESC
                     LIMIT 1"

last_throw_query_result <- dbSendQuery(computer_vision_trackman_api, last_throw_query)

last_throw_init <- fetch(last_throw_query_result, n = -1)

# Trackman Mobile Arm Angles

# Access Arm Angles data through R by creating database connection object.
computer_vision_pitching_data <- dbConnect(MySQL(),
                                           user = Sys.getenv('CLUSTER_USERNAME_COMPUTER_VISION_CLUSTER'),
                                           password = Sys.getenv('CLUSTER_PASSWORD_COMPUTER_VISION_CLUSTER'),
                                           dbname = Sys.getenv('DATABASE_PITCHING_DATA'),
                                           host = Sys.getenv('CLUSTER_HOST_COMPUTER_VISION_CLUSTER'),
                                           port = as.numeric(Sys.getenv('CLUSTER_PORT_COMPUTER_VISION_CLUSTER')))

# Query for arm angles necessary for Pitch & Stuff Models.
trackman_mobile_arm_angles_query <- "SELECT *
                                     FROM trackman_mobile_arm_angles"

trackman_mobile_arm_angles_query_result <- dbSendQuery(computer_vision_pitching_data, trackman_mobile_arm_angles_query)

trackman_mobile_arm_angles <- fetch(trackman_mobile_arm_angles_query_result, n = -1)

# Find Mean Arm Angles for Athletes with multiple Trackman IDs.
trackman_mobile_arm_angles <- trackman_mobile_arm_angles %>% 
  group_by(traq_id) %>%
  summarise(arm_angle = round(mean(arm_angle, na.rm = TRUE), 1))

# TRAQ API ----------------------------------------------------------------

# Function that retrieves athlete names, TRAQ IDs, and throwing handedness from TRAQ Software API.
traq_api_connection <- function() {
  
  # Access TRAQ API data through R by creating database connection object.
  traq_api_db <- dbConnect(MySQL(), 
                           user=Sys.getenv('CLUSTER_USERNAME_DB_BIOMECH'), 
                           password=Sys.getenv('CLUSTER_PASSWORD_DB_BIOMECH'), 
                           dbname=Sys.getenv('DATABASE_THIRD_PARTY_API_DB'), 
                           port=as.numeric(Sys.getenv('CLUSTER_PORT_DB_BIOMECH')), 
                           host=Sys.getenv('CLUSTER_HOST_DB_BIOMECH'))
  
  # Query for latest Access Token to TRAQ API.
  query <- "SELECT access_token FROM apis WHERE name = 'traq_api'"
  
  data <- dbSendQuery(traq_api_db, query)
  
  traq_bearer <- fetch(data, rs = -1)
  
  # Designate Bearer token.
  bearer_token = paste0("Bearer ",traq_bearer$access_token[1])
  
  # URL for users table.
  users_url <- "https://traq.drivelinebaseball.com/api/v1.1/users"
  
  # Use GET function to retrieve pitching athletes in-gym.
  get_users <- GET(url = users_url, 
                   config = add_headers(Authorization = bearer_token, accept = "application/json"), 
                   query = list(facility_id = 1, role_id = 0, program = 0))
  
  users_list <- content(get_users)$data
  
  for(i in 1:length(users_list)){
    users_list[[i]][sapply(users_list[[i]], is.null)] <- NULL
  }
  
  users <- bind_rows(users_list)
  
  users <- users %>%
    mutate(Name = paste0(name," ",last_name),
           p_throws = ifelse(handedness == 1, "R", "L"),
           name_id = paste0(Name, " (", id, ")")) %>%
    select(c(name_id, traq_id = id, full_name = Name, p_throws))
  
  dbDisconnect(traq_api_db)
  
  return(users)
}

# Retrieve athlete names, TRAQ IDs, and throwing handedness. 
users <- traq_api_connection() 

# Get list of athletes in alphabetical order and remove duplicates
traq_pitchers <- sort(unique(users$name_id))

# Initialize Data --------------------------------------------------------------------

# Initial data loaded when opening the dashboard.
tm_live_data_init <- trackman_api %>%
  filter(session_id == first(session_id)) %>%
  # Rename columns to match those of Pitch & Stuff Models.
  rename(release_speed = release_speed,
         rel_height_cor = release_height,
         rel_side_cor = release_side,
         extension_cor = release_extension,
         pfx_z_fourty_cor_brks = pfxz,
         pfx_x_fourty_cor_adj = pfxx) %>%
  mutate(pitch_type = case_when(is.na(pitch_type) ~ "FF",
                                TRUE ~ pitch_type),
         pitch_group = case_when(pitch_type %in% c("FF", "SI") ~ "Fastball",
                                 pitch_type %in% c("CT", "SL", "CB") ~ "Breaking Ball",
                                 pitch_type %in% c("CH", "SP") ~ "Offspeed"),
         plate_x_cor = plate_location_side / 12, # Location measured in feet changed to inches necessary for models.
         plate_z_cor = plate_location_height / 12,
         date = as.Date(time)) %>%
  replace_na(list(release_speed = 0, pfx_z_fourty_cor_brks = 0, pfx_x_fourty_cor_adj = 0, plate_x_cor = 0, plate_z_cor = 0, 
                  horizontal_movement = 0, induced_vertical_movement = 0, rel_height_cor = 0, rel_side_cor = 0, extension_cor = 0,
                  stuff_plus = 0, pitch_quality = 0))

# Functions ---------------------------------------------------------------

# Function to calculate velocity and movement differentials for each pitch thrown and loaded into the dashboard.
data_manip_function <- function(last_throw){
  
  # Movement Differentials Per Pitch
  last_throw$mov_x_dif <- predict(pfx_x_exp_mov, last_throw)
  last_throw$mov_z_dif <- predict(pfx_z_exp_mov, last_throw)
  
  # Notice how the model uses movement breaks from 40 ft. if not available use movement breaks from standard 60 ft.
  last_throw <- last_throw %>%
    mutate(mov_z_dif = pfx_z_fourty_cor_brks - mov_z_dif) %>%
    mutate(mov_x_dif = pfx_x_fourty_cor_adj - mov_x_dif)
  
  # Velocity & Movement Differentials for Secondary Pitches.
  last_throw <- last_throw %>%
    mutate(velo_dif = ifelse(pitch_type != "FF" & pitch_type != "SI",
                             release_speed - rel_speed_fb, release_speed - rel_speed_fb)) %>%
    mutate(mov_x_dif - ifelse(pitch_type != "FF" & pitch_type != "SI",
                              pfx_x_fourty_cor_adj - pfx_x_fb, mov_x_dif)) %>%
    mutate(mov_z_dif - ifelse(pitch_type != "FF" & pitch_type != "SI",
                              pfx_z_fourty_cor_brks - pfx_z_fb, mov_z_dif))
  
  # Same as above for pitchers with Cutter as their Primary Pitch.
  last_throw <- last_throw %>%
    mutate(velo_dif = ifelse(pitch_type != "CT" & is.na(velo_dif),
                             release_speed - rel_speed_ct, release_speed - rel_speed_fb)) %>%
    mutate(mov_x_dif - ifelse(pitch_type != "CT" & is.na(mov_x_dif),
                              pfx_x_fourty_cor_adj - pfx_x_ct, mov_x_dif)) %>%
    mutate(mov_z_dif - ifelse(pitch_type != "CT" & is.na(mov_z_dif),
                              pfx_z_fourty_cor_brks - pfx_z_ct, mov_z_dif))
  
  # Assume Batter, Catcher, Season, and Umpire Runs are league average, therefore we set them as 0 for the following ad-hoc adjustments.
  last_throw <- last_throw %>%
    mutate(batter_runs = 0) %>%
    mutate(catcher_runs = 0) %>%
    mutate(season_runs = 0) %>%
    mutate(ump_runs = 0)
  
  return(last_throw)
  
}

# Calculate Stuff Run Values and IRV for each Pitch Group and Handedness combination.
ad_hoc_adjustments_function <- function(last_throw){
  
  # Four-Seam Fastballs & Sinkers
  if (last_throw$pitch_group == "Fastball") {
    
    # Right-Handed Pitchers
    if (last_throw$p_throws == "R") {
      
      # Stuff Run Values
      last_throw$stuff_rvs <- predict(stuff_model_fb_right, last_throw)
      
      last_throw <- last_throw %>%
        mutate(stuff_rv = stuff_rvs$.pred + 0.017559049,
               stuff_rv = ifelse(release_speed > 89.5, (release_speed - 89.5)* -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed < 92.5, (release_speed - 92.5)* -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 8, (pfx_z_fourty_cor_brks - 8) * -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -4.5, (pfx_x_fourty_cor_adj - 4.5)*.00025 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -2.5, (pfx_x_fourty_cor_adj - 2.5)*.00025 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 94.5, (release_speed - 94.5)* -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 96.5, (release_speed - 96.5)* -.00125 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < - 6.5, (pfx_x_fourty_cor_adj + 6.5)*.000375 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv + .0022,
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -7 & pfx_x_fourty_cor_adj < -4 & pfx_z_fourty_cor_brks > 4 & pfx_z_fourty_cor_brks < 7, (pfx_z_fourty_cor_brks - 7)*-.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 8.5 & pfx_z_fourty_cor_brks > 4, (pfx_z_fourty_cor_brks - 8.5) * -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 4, (pfx_z_fourty_cor_brks - 4.5)* -.00075 + stuff_rv, stuff_rv),
               stuff_rv = abs(mov_z_dif) * -.0005 + stuff_rv,
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 8.25 & pfx_z_fourty_cor_brks > 6, (pfx_z_fourty_cor_brks - 8.25) * -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 6 & pfx_z_fourty_cor_brks > 4, (pfx_z_fourty_cor_brks - 4) * .00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > 8.5, (pfx_z_fourty_cor_brks - 8.5) * -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -6.5, (pfx_x_fourty_cor_adj + 6.5) * .00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 4.5, (pfx_z_fourty_cor_brks - 4.5)* -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 1, (pfx_z_fourty_cor_brks - 1.5)*.0005 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv + .000622499,
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 2.5, (pfx_z_fourty_cor_brks - 2.5) * .001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 0, pfx_z_fourty_cor_brks * .00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < -2, (pfx_z_fourty_cor_brks + 2)* .001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -7 & pfx_z_fourty_cor_brks < 8 & pfx_z_fourty_cor_brks > 0, (pfx_x_fourty_cor_adj + 7) * -.00025 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 93 & pfx_z_fourty_cor_brks > 8, (release_speed - 93) * -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 93 & release_speed < 97, (release_speed - 93) * -.00025 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -6 & pfx_x_fourty_cor_adj > -9 & pfx_z_fourty_cor_brks < 3, (pfx_z_fourty_cor_brks - 3) * .00045 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -3 & pfx_x_fourty_cor_adj > -7 & pfx_z_fourty_cor_brks > 4 & pfx_z_fourty_cor_brks < 7 & stuff_rv > 0, stuff_rv * .05 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed < 88, (release_speed - 88) * .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 92 & release_speed < 95 & pfx_z_fourty_cor_brks > 9 & pfx_x_fourty_cor_adj < -3 & pfx_x_fourty_cor_adj > - 6.5, (pfx_z_fourty_cor_brks -8) * -.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 92 & pfx_x_fourty_cor_adj > -1 & pfx_z_fourty_cor_brks > 9, (pfx_z_fourty_cor_brks - 8) * -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(rel_side_cor > - .65, ((rel_side_cor * -1)+.65)* -.00527 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed < 87 & pfx_z_fourty_cor_brks > -1, (release_speed - 87) * -.002 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed < 85 & pfx_z_fourty_cor_brks > -1, (release_speed - 87)* -.005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > 6 & pfx_z_fourty_cor_brks < 8.5 & pfx_x_fourty_cor_adj < -2, (pfx_z_fourty_cor_brks - 8.5) * -.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(mov_z_dif > 1, (mov_z_dif - 1) * -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > 9, (pfx_z_fourty_cor_brks - 9) * .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -7 & pfx_z_fourty_cor_brks > 4 & pfx_z_fourty_cor_brks < 7, (pfx_x_fourty_cor_adj + 7) * -.00025 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > 6 & pfx_z_fourty_cor_brks < 8.5 & pfx_x_fourty_cor_adj < -2, (pfx_z_fourty_cor_brks - 8.5) * -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 3.5 & pfx_z_fourty_cor_brks > 0, (pfx_z_fourty_cor_brks - 3.5) * .00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < -2 & pfx_z_fourty_cor_brks > -5, (pfx_z_fourty_cor_brks + 1.5) * .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -2 & pfx_x_fourty_cor_adj < .5, abs(pfx_x_fourty_cor_adj + 2) * - .00075 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv + .00125)
      
      # IRV
      last_throw$pred_rvs <- predict(pitch_model_fb_right, last_throw)
      
      last_throw <- last_throw %>%
        mutate(irv = pred_rvs$.pred + .015,
               irv = ifelse(release_speed > 91.5, (release_speed - 91.5)*-.00075 + irv, irv),
               irv = ifelse(release_speed < 92, (release_speed - 92)*-.001 + irv, irv),
               irv - ifelse(release_speed < 82, (release_speed - 82)*-.001 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks > 8.25, (pfx_z_fourty_cor_brks - 8.25)*-.001 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks < 6.5, (pfx_z_fourty_cor_brks - 6.5)*-.001 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks < 0, (pfx_z_fourty_cor_brks)*.00175 + irv, irv),
               irv = irv + .001249723,
               irv = ifelse(release_speed < 85, (release_speed - 85) * -.001 + irv, irv),
               irv = irv - .000119315,
               irv = ifelse(pfx_x_fourty_cor_adj > -7 & pfx_x_fourty_cor_adj < -4 & pfx_z_fourty_cor_brks > 4 & pfx_z_fourty_cor_brks < 7, (pfx_z_fourty_cor_brks - 7)*-.001 + irv, irv),
               irv = ifelse(release_speed > 95.5, (release_speed - 95.5) * -.00075 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks > 7.5, (pfx_z_fourty_cor_brks - 7.5) * -.00025 + irv, irv),
               irv = irv + .00026)
    }
    
    # Left-Handed Pitchers
    else if (last_throw$p_throws == "L") {
      
      last_throw$stuff_rvs <- predict(stuff_model_fb_left, last_throw)
      
      last_throw <- last_throw %>%
        mutate(stuff_rv = stuff_rvs$.pred + .014734243,
               stuff_rv = ifelse(release_speed > 92.5, (release_speed - 92.5)* -.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -8.5, (pfx_x_fourty_cor_adj + 8.5)* .001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -1.5, (pfx_x_fourty_cor_adj + 1.5)* -.00125 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > 8.5, (pfx_z_fourty_cor_brks - 8.5)* -.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 3.5, (pfx_z_fourty_cor_brks - 3.5)* .00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(mov_z_dif > 0, mov_z_dif * -.00125 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(mov_z_dif < 0, mov_z_dif * .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 1.5, (pfx_z_fourty_cor_brks - 1.5)* -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed < 91.5, (release_speed - 91.5)* -.001 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv + .002064214,
               stuff_rv = ifelse(release_speed > 97, (release_speed - 97) * -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed < 90, (release_speed - 90) * -.0075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -1.5, (pfx_x_fourty_cor_adj + 1.5)*-.0005 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv + .000588235,
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -7 & pfx_x_fourty_cor_adj < -4 & pfx_z_fourty_cor_brks > 4 & pfx_z_fourty_cor_brks < 7, (pfx_z_fourty_cor_brks - 7)*-.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 92.5, (release_speed - 92.5) * -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 8, (pfx_z_fourty_cor_brks - 8) * -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -6.5, (pfx_x_fourty_cor_adj + 6.5)* .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 1, (pfx_z_fourty_cor_brks - 1.5)*.0005 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv - .000108234,
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -1 & pfx_x_fourty_cor_adj > -8 & pfx_z_fourty_cor_brks > 4 & pfx_z_fourty_cor_brks < 8, (pfx_z_fourty_cor_brks - 8) * -.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > 9, (pfx_z_fourty_cor_brks - 9) * -.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -7 & pfx_z_fourty_cor_brks < 3, (pfx_z_fourty_cor_brks - 3) * .00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -2 & pfx_x_fourty_cor_adj > -7 & pfx_z_fourty_cor_brks > 5 & pfx_z_fourty_cor_brks < 8, (pfx_z_fourty_cor_brks - 8) * -.002 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed < 87, (release_speed - 87) * - .002 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 92 & release_speed < 94.5, (release_speed - 92) * -.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -6 & pfx_z_fourty_cor_brks < 3, (pfx_z_fourty_cor_brks - 3) * .001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -8 & pfx_z_fourty_cor_brks > 4, (pfx_x_fourty_cor_adj - 8) * -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -1 & release_speed > 83, (pfx_x_fourty_cor_adj + 1) * -.002 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 3, (pfx_z_fourty_cor_brks - 3) * .00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -1 & release_speed > 83 & pfx_z_fourty_cor_brks < 7.5, (pfx_x_fourty_cor_adj + 1) * -.006 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -7 & pfx_x_fourty_cor_adj < -1 & pfx_z_fourty_cor_brks > 5 & pfx_z_fourty_cor_brks < 7.5, (pfx_z_fourty_cor_brks - 8.5) * -.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -8 & pfx_z_fourty_cor_brks > 4, (pfx_z_fourty_cor_brks -4) * -.003 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -10 & pfx_z_fourty_cor_brks < 5 & pfx_z_fourty_cor_brks > -2, (pfx_z_fourty_cor_brks - 6) * .001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -4 & pfx_z_fourty_cor_brks > 9, (pfx_z_fourty_cor_brks - 9) * -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(rel_side_cor > 1.85, (rel_side_cor - 1.85) * -.002 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(mov_z_dif > 1, (mov_z_dif - 1) * -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 4 & pfx_x_fourty_cor_adj > -8 & pfx_z_fourty_cor_brks > 1, (pfx_z_fourty_cor_brks - 4) * .0025 + stuff_rv, stuff_rv),
               vaa = -7.069 + (rel_height_cor * -1.0806147) + (release_speed * .0657303) + (pfx_z_fourty_cor_brks * .2408415),
               stuff_rv = ifelse(vaa > -5.5, (vaa + 5.5) * -.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > -7 & pfx_x_fourty_cor_adj < -3 & pfx_z_fourty_cor_brks > 6 & pfx_z_fourty_cor_brks < 8, (pfx_z_fourty_cor_brks - 8) * -.001 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv - .001196109,
               stuff_rv = (stuff_rv * .8) - .000325853,
               stuff_rv = stuff_rv * .9 + .00125)
      
      last_throw <- last_throw %>%
        dplyr::select(-one_of('vaa'))
      
      last_throw$pred_rvs <- predict(pitch_model_fb_left, last_throw)
      
      last_throw <- last_throw %>%
        mutate(irv = pred_rvs$.pred + .013484255,
               irv = ifelse(release_speed > 91.5, (release_speed - 91.5)*-.00075 + irv, irv),
               irv = ifelse(release_speed < 90.5, (release_speed - 90.5)*-.00075 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks > 8.5, (pfx_z_fourty_cor_brks - 8.5)*-.001 + irv, irv),
               irv = ifelse(mov_z_dif < -3.5, (mov_z_dif + 3.5)*.00025 + irv, irv),
               irv = ifelse(pfx_x_fourty_cor_adj > -.5, (pfx_x_fourty_cor_adj + .5) * -.0005 + irv, irv),
               irv = irv + .001249723,
               irv = ifelse(release_speed < 91.5, (release_speed - 91.5) * -.0025 + irv, irv),
               irv = ifelse(pfx_x_fourty_cor_adj > -1.5, (pfx_x_fourty_cor_adj + 1.5)* -.0025 + irv, irv),
               irv = ifelse(mov_z_dif > -.5, (mov_z_dif + .5) * -.0015 + irv, irv),
               irv = ifelse(pfx_x_fourty_cor_adj < -5.5, (pfx_x_fourty_cor_adj + 5.5)* .001 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks < 2.5, (pfx_z_fourty_cor_brks - 2.5)*.001 + irv, irv),
               irv = irv + .001354635,
               irv = ifelse(pfx_x_fourty_cor_adj > -7 & pfx_x_fourty_cor_adj < -4 & pfx_z_fourty_cor_brks > 4 & pfx_z_fourty_cor_brks < 7, (pfx_z_fourty_cor_brks - 7)*-.001 + irv, irv),
               irv = ifelse(release_speed > 92.5, (release_speed - 92.5) * -.00075 + irv, irv),
               irv = ifelse(release_speed > 96.5, (release_speed - 96.5) * -.00075 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks > 2.5 & pfx_z_fourty_cor_brks < 6, (pfx_z_fourty_cor_brks - 6) * -.00075 + irv, irv),
               irv = irv + .001457166)
    }
  }
  
  # Cutters, Sliders, & Curveballs
  else if (last_throw$pitch_group == "Breaking Ball") {
    
    if (last_throw$p_throws == "R") {
      
      last_throw$stuff_rvs <- predict(stuff_model_bb_right, last_throw)
      
      last_throw <- last_throw %>%
        mutate(stuff_rv = stuff_rvs$.pred + .0093834,
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > 3.5, (pfx_x_fourty_cor_adj - 3.5)* -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 2.5, (pfx_z_fourty_cor_brks - 2.5)*.00025 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < -5.5, (pfx_z_fourty_cor_brks + 5.5)* -.00035 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed < 81.5, (release_speed - 81.5)* -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < 4.5, (pfx_x_fourty_cor_adj - 4.5)* -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 90, (release_speed -90)* -.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > 1.5 & pfx_z_fourty_cor_brks < 6.5, (pfx_z_fourty_cor_brks - 1.5)*.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > 5.5, (pfx_z_fourty_cor_brks - 5.5)* -.00075 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv - .000442807,
               stuff_rv = ifelse(release_speed > 87.5, (release_speed - 87.5) * -.00025 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > 1 & pfx_x_fourty_cor_adj < 4 & release_speed > 83, (pfx_x_fourty_cor_adj - 4.5)*-.0005 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv + .001199951)
      
      last_throw$pred_rvs <- predict(pitch_model_bb_right, last_throw)
      
      last_throw <- last_throw %>%
        mutate(irv = pred_rvs$.pred + 0.008300611,
               irv = ifelse(release_speed < 85.5, (release_speed - 85.5)*.0003 + irv, irv),
               irv = ifelse(pfx_x_fourty_cor_adj > 3.5, (pfx_x_fourty_cor_adj - 3.5) * -.0005 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks > 2.5 & pfx_z_fourty_cor_brks < 6.25, (pfx_z_fourty_cor_brks - 2.5)*.002 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks > -6.5 & pfx_z_fourty_cor_brks < .5, (pfx_z_fourty_cor_brks - .5)*.0005 + irv, irv),
               irv = irv + .0013,
               irv = ifelse(release_speed < 75.5, (release_speed - 75.5)*-.001 + irv, irv),
               irv = ifelse(release_speed < 77.5 & pfx_x_fourty_cor_adj > 6.5, (pfx_x_fourty_cor_adj - 6.5)*.001 + irv, irv),
               irv = ifelse(release_speed > 85.5 & pfx_x_fourty_cor_adj < 1.5 & pfx_z_fourty_cor_brks > 0, (pfx_x_fourty_cor_adj - 1.5)*-.0005 + irv, irv),
               irv = ifelse(release_speed > 86.5 & pfx_x_fourty_cor_adj > 1.25, (pfx_x_fourty_cor_adj - 1.25)*-.0005 + irv, irv),
               irv = ifelse(release_speed > 86 & pfx_z_fourty_cor_brks < 4, (release_speed - 86)*-.0005 + irv, irv),
               irv = ifelse(release_speed > 87.5, (release_speed - 87.5) * -.0005 + irv, irv),
               irv = ifelse(release_speed > 78.5 & pfx_x_fourty_cor_adj > 3.5, (pfx_x_fourty_cor_adj - 3.5)*-.00025 + irv, irv),
               irv = irv + .000707209)
    }
    
    else if (last_throw$p_throws == "L") {
      
      last_throw$stuff_rvs <- predict(stuff_model_bb_left, last_throw)
      
      last_throw <- last_throw %>%
        mutate(stuff_rv = stuff_rvs$.pred + .005808194,
               stuff_rv = ifelse(release_speed < 80, (release_speed - 80)* -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > 3.5, (pfx_x_fourty_cor_adj - 3.5)* -.0015 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 1.5 & pfx_z_fourty_cor_brks > -3.5, (pfx_z_fourty_cor_brks - 1.5)* -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > .5, (pfx_z_fourty_cor_brks - .5)* .00025 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 83.5, (release_speed - 83.5)* -.00125 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < .5, (pfx_x_fourty_cor_adj - .5)* .001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(release_speed > 80.5, (release_speed - 80.5)* -.00025 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > .5,(pfx_z_fourty_cor_brks -.5)*.00125 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > 2.5,(pfx_z_fourty_cor_brks -2.5)*.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < -3 & release_speed > 79, (pfx_z_fourty_cor_brks +3)*.001 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj > 3.5 & release_speed > 79, (pfx_x_fourty_cor_adj - 3.5)*-.00075 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv + .003250993)
      
      last_throw$pred_rvs <- predict(pitch_model_bb_left, last_throw)
      
      last_throw <- last_throw %>%
        mutate(irv = pred_rvs$.pred + .006541698,
               irv = ifelse(pfx_x_fourty_cor_adj > 3.5, (pfx_x_fourty_cor_adj - 3.5) * -.001 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks > 1.5, (pfx_z_fourty_cor_brks - 1.5)*.001 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks < 4.5 & pfx_z_fourty_cor_brks > 1, (pfx_z_fourty_cor_brks - 4.5)*.001 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks > -3.5 & pfx_z_fourty_cor_brks < -.5, (pfx_z_fourty_cor_brks + .5)* -.001 + irv, irv),
               irv = ifelse(release_speed > 85.5, (release_speed - 85.5) * -.00125 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks > .5, (pfx_z_fourty_cor_brks - .5)*.00075 + irv, irv),
               irv = ifelse(release_speed < 86.5, (release_speed - 86.5) * -.0001 + irv, irv),
               irv = ifelse(release_speed > 79.5 & pfx_x_fourty_cor_adj > 3.5, (pfx_x_fourty_cor_adj -3.5)*-.00075 + irv, irv),
               irv = ifelse(release_speed > 79.5 & pfx_x_fourty_cor_adj < 3.5, (pfx_x_fourty_cor_adj -3.5)*-.0005 + irv, irv),
               irv = ifelse(pfx_x_fourty_cor_adj > 2.5 & pfx_x_fourty_cor_adj < 5.5, (pfx_x_fourty_cor_adj -5.5)*-.00075 + irv, irv),
               irv = ifelse(release_speed < 85, (release_speed - 85)*-.00025 + irv, irv),
               irv = ifelse(release_speed > 79.5 & pfx_z_fourty_cor_brks < .5, (pfx_z_fourty_cor_brks - .5)*.00055 + irv, irv),
               irv = ifelse(release_speed > 84.5 & release_speed < 90.5, (release_speed - 84.5) * -.00035 + irv, irv),
               irv = ifelse(release_speed < 85 & release_speed > 79.5, (release_speed - 85) * -.00035 + irv, irv))
    }
  }
  
  # Changeups & Splitters
  else if (last_throw$pitch_group == "Offspeed") {
    
    if (last_throw$p_throws == "R") {
      
      last_throw$stuff_rvs <- predict(stuff_model_os_right, last_throw)
      
      last_throw <- last_throw %>%
        mutate(stuff_rv = stuff_rvs$.pred + .01,
               stuff_rv = ifelse(release_speed > 84.5, (release_speed - 84.5)* -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -8.5, (pfx_x_fourty_cor_adj + 8.5)* .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks > 2.5 & pfx_z_fourty_cor_brks < 6.5, (pfx_z_fourty_cor_brks - 6.5)* -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 3.5, (pfx_z_fourty_cor_brks - 3.5)* .00025 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 3 & pfx_z_fourty_cor_brks > .5, (pfx_z_fourty_cor_brks - .5)* -.00085 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(mov_x_dif < -4.5, (mov_x_dif + 4.5) * .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(mov_z_dif < -4 & mov_z_dif > -6.5, (mov_z_dif + 4)* -.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(velo_dif < -5.5, (velo_dif + 5.5)* .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(velo_dif < -8.5, (velo_dif + 8.5)* .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(velo_dif > -5.5, (velo_dif + 5.5)* .0005 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv + .00341664,
               stuff_rv = ifelse(release_speed > 89.5, (release_speed - 89.5)*-.0015 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv - .000749544)
      
      last_throw$pred_rvs <- predict(pitch_model_os_right, last_throw)
      
      last_throw <- last_throw %>%
        mutate(irv = pred_rvs$.pred + .013217749,
               irv = ifelse(release_speed > 85.5, (release_speed - 85.5)*-.001 + irv, irv),
               irv = ifelse(pfx_x_fourty_cor_adj < -8.5, (pfx_x_fourty_cor_adj + 8.5) * .0015 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks < 3.5, (pfx_z_fourty_cor_brks + 3.5)*-.0005 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks > 5.5, (pfx_z_fourty_cor_brks - 5.5)*-.00075 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks < .5, (pfx_z_fourty_cor_brks + .5)*.00075 + irv, irv),
               irv = ifelse(mov_x_dif > -1.5, (mov_x_dif - 1.5)*-.0015 + irv, irv),
               irv = ifelse(velo_dif > -6.5, (velo_dif + 6.5)*.0035 + irv, irv),
               irv = ifelse(pfx_x_fourty_cor_adj > -8.5, (pfx_x_fourty_cor_adj + 8.5) * -.001 + irv, irv),
               irv = ifelse(velo_dif < -9.5, (velo_dif + 9.5)*.001 + irv, irv),
               irv = ifelse(pfx_x_fourty_cor_adj > -5.5, (pfx_x_fourty_cor_adj + 5.5) * .005 + irv, irv),
               irv = ifelse(release_speed < 80.5, (release_speed - 80.5) * -.005 + irv, irv),
               irv = irv + .000718083)
    }
    
    else if (last_throw$p_throws == "L") {
      
      last_throw$stuff_rvs <- predict(stuff_model_os_left, last_throw)
      
      last_throw <- last_throw %>%
        mutate(stuff_rv = stuff_rvs$.pred + .007375998,
               stuff_rv = ifelse(release_speed < 82, (release_speed - 82)* -.0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_x_fourty_cor_adj < -8.5, (pfx_x_fourty_cor_adj + 8.5)* .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(pfx_z_fourty_cor_brks < 5.5, (pfx_x_fourty_cor_adj - 5.5)* .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(mov_z_dif < -5.5, (mov_z_dif + 5.5)* .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(mov_z_dif > -2.5, (mov_z_dif + 2.5)* .0005 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(velo_dif < - 7.5, (velo_dif + 7.5)* .00025 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv + .001,
               stuff_rv = ifelse(mov_z_dif < -4, (mov_z_dif + 4)*.00075 + stuff_rv, stuff_rv),
               stuff_rv = ifelse(velo_dif > -14 & velo_dif < -7.5, (velo_dif + 7.5)*.00075 + stuff_rv, stuff_rv),
               stuff_rv = stuff_rv + .005400054)
      
      last_throw$pred_rvs <- predict(pitch_model_os_left, last_throw)
      
      last_throw <- last_throw %>%
        mutate(irv = pred_rvs$.pred + 0.01172879 - 0.0014,
               irv = ifelse(mov_z_dif < -3.5, (mov_z_dif + 3.5)*.00075+irv, irv),
               irv = ifelse(mov_x_dif < -.5, (mov_x_dif-.5)*.00025+irv, irv),
               irv = ifelse(velo_dif > -8.5, (velo_dif + 8.5)*.0015 + irv, irv),
               irv = ifelse(velo_dif < -8.5 & velo_dif > -15, (velo_dif + 8.5)*.001 + irv, irv),
               irv = ifelse(velo_dif < -8.5 & velo_dif > -10, (velo_dif + 8.5)*.0015 + irv, irv),
               irv = ifelse(release_speed > 85.5, (release_speed - 85.5) * -.001 + irv, irv),
               irv = ifelse(release_speed > 84.5, (release_speed - 84.5) * -.0005 + irv, irv),
               irv = ifelse(pfx_z_fourty_cor_brks < 2.5, (pfx_z_fourty_cor_brks - 2.5) * .00025 + irv, irv),
               irv = ifelse(mov_z_dif > -1.5, (mov_z_dif + 1.5)*.00025 + irv, irv),
               irv = irv + .003511929)
    }
  }
  
  return(last_throw)
  
}

# Function to upload new advanced metrics (xERA, Stuff+) and input data from the dashboard (Pitch Type, Batter Handedness) into separate table used for pairing.
trackman_api_db_upload <- function(row) {
  
  # Access Trackman API data through R by creating database connection object.
  trackman_api_db <- dbConnect(MySQL(),
                               user = Sys.getenv('CLUSTER_USERNAME_COMPUTER_VISION_CLUSTER'),
                               password = Sys.getenv('CLUSTER_PASSWORD_COMPUTER_VISION_CLUSTER'),
                               dbname = Sys.getenv('DATABASE_TRACKMAN_API_DB'),
                               host = Sys.getenv('CLUSTER_HOST_COMPUTER_VISION_CLUSTER'),
                               port = as.numeric(Sys.getenv('CLUSTER_PORT_COMPUTER_VISION_CLUSTER')))
  
  # Insert new values into table and corresponding columns.
  insert_query <- paste0("INSERT INTO trackman_live_dash_inputs
                         VALUES ('", row$pitchuid, "', '",
                         row$p_throws, "', '",
                         row$batter_hand, "', '",
                         row$pitch_type, "', '",
                         row$stuff_plus, "', '",
                         row$pitch_quality, "')")
  
  rsInsert <- dbSendQuery(trackman_api_db, insert_query)
  
  # Frees all resources associated with a result set.
  dbClearResult(rsInsert)
  
  # Disconnect from database to avoid excess connections and free up memory space, etc. 
  dbDisconnect(trackman_api_db)
}

# Function to connect to the Trackman API database within the app.R Server.
trackman_api_db_connect <- function(sql_string){
  
  mydb <- dbConnect(MySQL(), 
                    user = Sys.getenv('CLUSTER_USERNAME_COMPUTER_VISION_CLUSTER'),
                    password = Sys.getenv('CLUSTER_PASSWORD_COMPUTER_VISION_CLUSTER'),
                    dbname = Sys.getenv('DATABASE_TRACKMAN_API_DB'),
                    host = Sys.getenv('CLUSTER_HOST_COMPUTER_VISION_CLUSTER'),
                    port = as.numeric(Sys.getenv('CLUSTER_PORT_COMPUTER_VISION_CLUSTER')))
  
  rs <- dbSendQuery(mydb, sql_string)
  
  df <- fetch(rs, n = -1)
  
  return(df)
  
  dbDisconnect(mydb)}

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  
  useShinydashboard(),
  
  setBackgroundColor(color = "ghostwhite"),
  
  # UI Header
  fluidRow(
    column(width = 3,
           fluidRow(
             br(), br(), 
             # Select Input for athletes and pitch type due to Trackman's method of tagging after pitch is thrown. 
             column(8, selectInput(inputId = "player_input", label = NULL, choices = sort(unique(traq_pitchers)), selected = traq_pitchers[1]), align = "left"),
             column(4, selectInput(inputId = "pitch_type_input", label = NULL, choices = c("FF", "SI", "CB", "SL", "CT", "CH", "SP", "KN"), selected = "FF"), align = "left"),
           )
    ),
    column(
      width = 2,
      fluidRow(
        br(), br(),
        # Select Input for Batter Handedness specifically for Live At Bats, default is set to a Right-Handed Hitter.
        column(4, selectInput(inputId = "handedness_input", label = NULL, choices = c("R", "L"), selected = "R"), align = "left"),
        # Action Buttons for undoing pitch type for selected pitch and submitting the correct pitch type.
        column(4, actionButton("submit_button", "Submit", class = "SubmitPitch", style="color: #000000; background-color: #ffa300; border-color: #000000")),
        column(4, actionButton("undo_button", "Undo", class = "UndoPitch", style="color: #ffffff; background-color: #000000; border-color: #000000")),
      )
    ),
    column(
      width = 2,
      # Trackman and Driveline Logos.
      fluidRow(
        column(12, img(src = "trackman_logo_letters_cropped.png", style = "margin-top: 10px;", height = 47/1.5, width = 314/1.5), align = "center")
      ),
      fluidRow(
        column(12, img(src = "driveline_logo_black.png", style = "margin-top: -5px; margin-bottom: -25px;", height = 575/8, width = 692/8), align = "center")
      )
    ),
    column(
      width = 2, offset = 3, align = "right",
      # Toggle Between Pitch-by-Pitch and Session Pages.
      fluidRow(style = "margin-right: 5px;",
               br(), br(),
               radioGroupButtons(
                 inputId = "TypeInput", label = NULL, choices = c("Pitch", "Session"),
                 justified = TRUE,
                 checkIcon = list(yes = icon("ok", lib = "glyphicon"))
               )
      )
    )
  ),
  hr(),
  
  # UI Body
  fluidRow(
    
    # Sidebar: Pitch History
    column(2,
           fluidRow(
             fluidRow(
               column(12,
                      # Select previous pitches within a session to view relative metrics, locations, movement.
                      fluidRow(
                        box(title = "Pitch History", 
                            status = "success", 
                            width = NULL, 
                            solidHeader = TRUE, 
                            switchInput(inputId = "switchInput", onLabel = "Pitch", offLabel = "Group", value = TRUE),
                            DTOutput("PitchHistoryTable")
                        ), align = "center"
                      ),
                      # Pitch Count Box below Pitch History.
                      fluidRow(
                        box(title = "Pitch Count", 
                            status = "info", 
                            width = NULL, 
                            solidHeader = TRUE, 
                            textOutput("pitchCountBox")
                        ), align = "center"
                      )
               )
             )
           ),
    ),
    
    # Start of Body Content
    column(10,
           
           # Layout: Pitch
           conditionalPanel(condition = "input.TypeInput == 'Pitch'",
                            fluidRow(
                              column(5,
                                     # Dragless Pitch Movement Plot
                                     fluidRow(
                                       box(width = NULL, 
                                           height = 650,
                                           status = "warning",
                                           solidHeader = TRUE,
                                           br(),
                                           plotOutput("pitchMovementPlotPitch"), br(),
                                           # Horizontal Break Value Box
                                           fluidRow(
                                             column(4, 
                                                    box(title = textOutput("horzBreakBoxTitlePitch"), 
                                                        status = "primary", 
                                                        width = NULL, 
                                                        solidHeader = TRUE, 
                                                        textOutput("horzBreakBoxValuePitch"),
                                                        h5("inches"))
                                             ),
                                             # Induced Vertical Break Value Box
                                             column(4, 
                                                    box(title = textOutput("vertBreakBoxTitlePitch"), 
                                                        status = "primary", 
                                                        width = NULL, 
                                                        solidHeader = TRUE,
                                                        textOutput("vertBreakBoxValuePitch"),
                                                        h5("inches"))
                                             ),
                                             # Velocity Value Box
                                             column(4,
                                                    box(title = textOutput("veloBoxTitlePitch"),
                                                        status = "primary",
                                                        width = NULL,
                                                        solidHeader = TRUE,
                                                        textOutput("veloBoxValuePitch"),
                                                        h5("miles per hour"))
                                             )
                                           )
                                       ), align = "center"
                                     )
                              ),
                              # Pitch Locations from Pitcher's Perspective (Code in Server).
                              column(5, uiOutput("PitchModelPitch")
                              ),
                              
                              column(2,
                                     # Stuff Plus Value Box
                                     box(title = textOutput("stuffPlusBoxTitlePitch"),
                                         status = "primary",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         textOutput("stuffPlusBoxValuePitch")
                                     ),
                                     # Pitch Quality or xERA Value Box
                                     box(title = textOutput("pitchQualityBoxTitlePitch"),
                                         status = "primary",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         textOutput("pitchQualityBoxValuePitch")
                                     )
                              )
                            )
           ),
           
           # Layout: Session
           conditionalPanel(condition = "input.TypeInput == 'Session'",
                            fluidRow(
                              # Dragless Pitch Movement Plot for All Pitches or Pitch Types Selected.
                              column(6,
                                     fluidRow(
                                       box(width = NULL, 
                                           status = "warning",
                                           solidHeader = TRUE,
                                           plotOutput("pitchMovementPlotSession"), br()
                                       ), align = "center"
                                     ) 
                              ),
                              # Plot for Stuff Plus Rolling Averages for the Session.
                              column(6,
                                     fluidRow(
                                       box(width = NULL, 
                                           status = "warning",
                                           solidHeader = TRUE,
                                           plotOutput("stuffPlusRollingSession"), br()
                                       ), align = "center"
                                     )
                              )
                            ),
                            fluidRow(
                              # Pitch Summary Table for Mean Stuff Plus, xERA, Movement Metrics for each Pitch Type during the session.
                              column(12, 
                                     box(width = NULL,
                                         status = "warning",
                                         solidHeader = TRUE,
                                         dataTableOutput("pitchSummaryTableSession"), br()
                                     )
                              )
                            )
           )
    )
  ),
  title = "Trackman Live Pitch-Level Interface",
  theme = 'stylesheet.css' 
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive timer triggered every second (1000 ms) during the session.
  autoInvalidate <- reactiveTimer(1000, session)
  
  # List of reactive values updated later within the session, starts with initialize data from above.
  update_later <- shiny::reactiveValues(last_throw = last_throw_init,
                                        tm_live_data = tm_live_data_init,
                                        prev_time_stamp = tm_live_data_init$time[1])
  
  shiny::observe({
    autoInvalidate()

    # Last thrown pitch within facility.
    update_later$last_throw <- trackman_api_db_connect("SELECT *
                                                        FROM trackman_api_pitches_raw t1
                                                        ORDER BY id DESC
                                                        LIMIT 1")
    
    lapply(dbListConnections(MySQL()), dbDisconnect)
    
    # If the time of last throw in the database does not match that of the last pitch in the initial data loaded, then it is considered a new pitch.
    if (update_later$prev_time_stamp != update_later$last_throw$time) {
      
      # Join last throw with TRAQ API and Trackman Mobile Arm Angles data.
      update_later$last_throw <- update_later$last_throw %>%
        mutate(name_id = input$player_input) %>%
        left_join(users, by = "name_id") %>%
        left_join(trackman_mobile_arm_angles, by = "traq_id") %>%
        rename(pfx_z_fourty_cor_brks = pfxz,
               pfx_x_fourty_cor_adj = pfxx,
               release_speed = release_speed,
               rel_height_cor = release_height,
               rel_side_cor = release_side,
               extension_cor = release_extension) %>%
        mutate(date = as.Date(time),
               plate_x_cor = plate_location_side / 12,
               plate_z_cor = plate_location_height /12,
               stand = input$handedness_input, # From Select Input for Batter Handedness.
               pitch_type = input$pitch_type_input, # From Select Input for Pitch Type
               arm_angle = case_when(is.na(arm_angle) ~ 63, # If athlete does not have previous sessions with arm angles, they are given average of 63 degrees.
                                     TRUE ~ arm_angle),
               pitch_group = case_when(pitch_type %in% c("FF", "SI") ~ "Fastball",
                                       pitch_type %in% c("CT", "SL", "CB") ~ "Breaking Ball",
                                       pitch_type %in% c("CH", "SP") ~ "Offspeed"),
               p_throws = case_when(is.na(p_throws) ~ "R",
                                    TRUE ~ p_throws),
               pfx_x_fourty_cor_adj = ifelse(p_throws == "L", pfx_x_fourty_cor_adj * -1, pfx_x_fourty_cor_adj),
               count = "1-1")
      
      # Identify Pitcher's Primary Pitch.
      session_id_first_pitch <- update_later$last_throw$session_id
      
      # Extract pitch type of first pitch or primary fastball.
      primary_fb <- update_later$tm_live_data %>%
        filter(session_id %in% session_id_first_pitch) %>%
        mutate(first_pitch_type = last(pitch_type)) %>% # In descending order, therefore first pitch is last row in session.
        filter(pitch_type == first_pitch_type)
      
      # Calculate Velocity and Movement Averages for Primary Pitch, necessary for valuing Breaking & Offspeed Pitches.
      
      # Fastball Averages.
      fb_avgs <- primary_fb %>%
        filter(first_pitch_type == "FF" | first_pitch_type == "SI") %>%
        group_by(session_id) %>%
        summarise(rel_speed_fb = mean(release_speed, na.rm = TRUE),
                  pfx_x_fb = mean(pfx_x_fourty_cor_adj, na.rm = TRUE),
                  pfx_z_fb = mean(pfx_z_fourty_cor_brks, na.rm = TRUE))
      
      # Cutter Averages.
      ct_avgs <- primary_fb %>%
        filter(first_pitch_type == "CT") %>%
        group_by(session_id) %>%
        summarise(rel_speed_ct = mean(release_speed, na.rm = TRUE),
                  pfx_x_ct = mean(pfx_x_fourty_cor_adj, na.rm = TRUE),
                  pfx_z_ct = mean(pfx_z_fourty_cor_brks, na.rm = TRUE))
      
      # Join Fastball and Cutter Averages to data frame by Session ID.
      update_later$last_throw <- left_join(update_later$last_throw, fb_avgs, by = "session_id")
      update_later$last_throw <- left_join(update_later$last_throw, ct_avgs, by = "session_id")
      
      # Have last throw go through functions to calculate Stuff RVs & IRVs.
      update_later$last_throw <- data_manip_function(update_later$last_throw)
      update_later$last_throw <- ad_hoc_adjustments_function(update_later$last_throw)
      
      # Calculate Stuff+ & Pitch Quality.
      last_throw <- update_later$last_throw %>%
        mutate(stuff_plus = round(500 / (1 + exp(66.5463 * stuff_rv + 1.5629)), 0),
               pitch_quality = round(irv * 145 + 4.5, 2),
               exp_velo_diff = velo_dif * round(runif(n(), 0.9, 1.1), 2),
               velo_gain = velo_dif - exp_velo_diff) %>%
        rename(batter_hand = stand) %>%
        select(trackman_pitch_id, time, date, name_id, batter_hand, p_throws, pitch_type, pitch_group, release_speed, stuff_plus, 
               pitch_quality, pfx_x_fourty_cor_adj, pfx_z_fourty_cor_brks, plate_x_cor, plate_z_cor, horizontal_movement, induced_vertical_movement)
      
      # Upload Inputs & Metrics into Trackman API Table.
      pitchuid <- last_throw$trackman_pitch_id
      p_throws <- last_throw$p_throws
      pitch_type <- input$pitch_type_input
      batter_hand <- input$handedness_input
      stuff_plus <- last_throw$stuff_plus
      pitch_quality <- last_throw$pitch_quality
      
      # Bind objects as columns to create data frame uploaded to table for new values.
      df <- data.frame(pitchuid, p_throws, batter_hand, pitch_type, stuff_plus, pitch_quality)
      
      # Function for uploading new values into table.
      trackman_api_db_upload(df)

      # Update Trackman Live Dash data with the new pitch/last throw and its characteristics.
      update_later$tm_live_data <- trackman_api_db_connect("SELECT *
                                                            FROM trackman_api_pitches_raw t1
                                                            LEFT JOIN trackman_live_dash_inputs t2 ON t2.pitchuid = t1.trackman_pitch_id
                                                            ORDER BY id DESC")
      
      lapply(dbListConnections(MySQL()), dbDisconnect)

      # Updated data frame goes through similar joining and changes as initial data loaded.
      update_later$tm_live_data <- update_later$tm_live_data %>%
        filter(session_id == first(session_id)) %>%
        # Rename columns to match those of Pitch & Stuff Models.
        rename(release_speed = release_speed,
               rel_height_cor = release_height,
               rel_side_cor = release_side,
               extension_cor = release_extension,
               pfx_z_fourty_cor_brks = pfxz,
               pfx_x_fourty_cor_adj = pfxx) %>%
        mutate(pitch_type = case_when(is.na(pitch_type) ~ "FF",
                                      TRUE ~ pitch_type),
               pitch_group = case_when(pitch_type %in% c("FF", "SI") ~ "Fastball",
                                       pitch_type %in% c("CT", "SL", "CB") ~ "Breaking Ball",
                                       pitch_type %in% c("CH", "SP") ~ "Offspeed"),
               plate_x_cor = plate_location_side / 12, # Location measured in feet changed to inches necessary for models.
               plate_z_cor = plate_location_height / 12,
               date = as.Date(time)) %>%
        replace_na(list(release_speed = 0, pfx_z_fourty_cor_brks = 0, pfx_x_fourty_cor_adj = 0, plate_x_cor = 0, plate_z_cor = 0, 
                        horizontal_movement = 0, induced_vertical_movement = 0, rel_height_cor = 0, rel_side_cor = 0, extension_cor = 0,
                        stuff_plus = 0, pitch_quality = 0))
      
      # Time stamp for the last throw in the dataset is updated with time from new pitch.
      update_later$prev_time_stamp = update_later$last_throw$time
    }
    
  })
  
  # Pitch-by-Pitch Data
  pitch_data <- reactive({
    df <- update_later$tm_live_data %>%
      mutate(name_id = input$player_input,
             batter_hand = input$handedness_input) %>%
      left_join(users, by = "name_id") %>%
      arrange(time) %>% # Making sure pitches are in descending order in Pitch History along with its relative metrics & information.
      mutate(pitch_number = ifelse(release_speed == 0, 0, row_number()), # If athlete has no data, pitch count number will state 0 to avoid dashboard from crashing.
             # Change location values to feet to accommodate code below for Pitch Locations.
             plate_x_cor = plate_x_cor * 12,
             plate_z_cor = plate_z_cor * 12,
             p_throws = ifelse(p_throws.x %in% c("R", "L"), p_throws.x, p_throws.y)) %>%
      select(pitch_number, p_throws, pitch_type, pitch_group, release_speed, stuff_plus, pitch_quality, pfx_x_fourty_cor_adj, pfx_z_fourty_cor_brks, 
             plate_x_cor, plate_z_cor, trackman_pitch_id, batter_hand, horizontal_movement, induced_vertical_movement, time, date, name_id, 
             rel_height_cor, rel_side_cor, extension_cor, session_id, traq_id) %>%
      arrange(desc(pitch_number))
  })
  
  # Session Data
  session_data <- reactive({
    df <- update_later$tm_live_data %>%
      arrange(time) %>%
      mutate(pitch_number = ifelse(release_speed == 0, 0, row_number()))
  })
  
  # Stuff Plus Rolling Averages Data
  rolling_data <- reactive({
    df <- update_later$tm_live_data %>%
      arrange(time) %>%
      mutate(pitch_number = ifelse(release_speed == 0, 0, row_number())) %>%
      mutate(rolling_stuff_plus = rollmean(stuff_plus, k = 5, fill = NA)) # Rolling Averages for every 5 pitches.
  })
  
  # Summary Table Data
  summary_data <- reactive({
    df <- update_later$tm_live_data %>%
      group_by(pitch_type) %>%
      summarize(count = n(),
                hb = mean(horizontal_movement, na.rm = T),
                vb = mean(induced_vertical_movement, na.rm = T),
                stuff_plus = round(mean(stuff_plus, na.rm = T),0),
                pitch_quality = mean(pitch_quality, na.rm = T))
  })
  
  # Pitch Count Box
  output$pitchCountBox <- renderText({paste(max(pitch_data()$pitch_number))})
  
  # Remove column headers.
  headerCallback <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  # Pitch History Table
  
  output$PitchHistoryTable <- renderDT({
    
    colnames_pitch_history <- c('#', 'Pitch')
    
    # Pitch-by-Pitch Tab
    
    if (input$TypeInput == "Pitch") {
      
      if (input$switchInput == TRUE) { # Pitch Toggle to select an individual pitch for viewing.
        
        datatable(pitch_data() %>% dplyr::select(c(pitch_number, pitch_type)), 
                  rowname = FALSE, extensions = 'Scroller',
                  class = 'cell-border stripe',
                  selection = list(mode = 'single', selected = c(1)),
                  options = list(dom = 't', 
                                 headerCallback = JS(headerCallback),
                                 ordering = F, 
                                 deferRender = F, 
                                 scrollY = 400,
                                 scroller = T,
                                 columnDefs = list(list(className = "dt-center", targets = "_all")))
        ) %>% formatString(c('pitch_number'), prefix = "#")
      } 
      
      else if (input$switchInput == FALSE) { # Group Toggle to select multiple pitches for viewing.
        
        datatable(pitch_data() %>% dplyr::select(c(pitch_number, pitch_type)), 
                  rowname = FALSE, extensions = c('Scroller', 'Select', 'Buttons'),
                  class = 'cell-border stripe',
                  selection = "none",
                  options = list(dom = 'tB',
                                 select = list(style = 'multi+shift', items = 'row'),
                                 buttons = c('selectAll', 'selectNone'),
                                 rowId = 0,
                                 headerCallback = JS(headerCallback),
                                 ordering = F, 
                                 deferRender = F, 
                                 scrollY = 400,
                                 scroller = T,
                                 columnDefs = list(list(className = "dt-center", targets = "_all")))
        ) %>% formatString(c('pitch_number'), prefix = "#")
      }
    }
    
    # Session Tab
    
    else if (input$TypeInput == "Session") {
      
      if (input$switchInput == TRUE) { # Pitch Toggle
        
        datatable(pitch_data() %>% 
                    dplyr::select(c(pitch_number, pitch_type)), 
                  rowname = FALSE, extensions = 'Scroller',
                  class = 'cell-border stripe',
                  selection = list(mode = 'none', selected = c(1)),
                  options = list(dom = 't', 
                                 headerCallback = JS(headerCallback),
                                 ordering = F, 
                                 deferRender = F, 
                                 scrollY = 400,
                                 scroller = T,
                                 columnDefs = list(list(className = "dt-center", targets = "_all")))
        ) %>% formatString(c('pitch_number'), prefix = "#")
        
      }
      else if (input$switchInput == FALSE) { # Group Toggle
        
        datatable(pitch_data() %>% 
                    dplyr::group_by(pitch_type) %>% 
                    summarize(count = n()) %>% 
                    select(pitch_type),
                  rowname = FALSE, 
                  class = 'cell-border stripe',
                  selection = list(mode = 'multiple', selected = c(1)),
                  options = list(dom = 't', 
                                 headerCallback = JS(headerCallback),
                                 ordering = F, 
                                 columnDefs = list(list(className = "dt-center", targets = "_all")))
        )
        
      }
    }
    
  }, server = FALSE)
  
  # Submit Button used to upload the correct pitch type to new values table and subsequently load into the dashboard.
  # Before using the Submit Button, the trainer must have selected the desired pitch tagged incorrectly and clicked the Undo Button.
  # Another scenario for when to use is when a session didn't have the dashboard running, therefore a trainer can go back and change the default "FF" for pitch type and "R" for batter handedness.
  observeEvent(input$submit_button, {
    
    # Select necessary variables from Pitch-By-Pitch Data.
    pitch_selected <- select(pitch_data(), c(release_speed, stuff_plus, pitch_quality, pfx_x_fourty_cor_adj, pfx_z_fourty_cor_brks, plate_x_cor, plate_z_cor, 
                                             trackman_pitch_id, horizontal_movement, induced_vertical_movement, time, date, rel_height_cor, 
                                             rel_side_cor, extension_cor, session_id, name_id, traq_id, p_throws))
    
    # Selected Pitch in Pitch History has corresponding metrics paired with it.
    pitch_selected <- (pitch_selected[input$PitchHistoryTable_rows_selected,])
    
    # Apply new inputs and reload values for the selected pitch.
    pitch_selected <- pitch_selected %>%
      left_join(trackman_mobile_arm_angles, by = "traq_id") %>%
      mutate(plate_x_cor = plate_x_cor / 12,
             plate_z_cor = plate_z_cor /12,
             stand = input$handedness_input,
             pitch_type = input$pitch_type_input,
             arm_angle = case_when(is.na(arm_angle) ~ 63,
                                   TRUE ~ arm_angle),
             pitch_group = case_when(pitch_type %in% c("FF", "SI") ~ "Fastball",
                                     pitch_type %in% c("CT", "SL", "CB") ~ "Breaking Ball",
                                     pitch_type %in% c("CH", "SP") ~ "Offspeed"),
             p_throws = case_when(is.na(p_throws) ~ "R",
                                  TRUE ~ p_throws),
             pfx_x_fourty_cor_adj = ifelse(p_throws == "L", pfx_x_fourty_cor_adj * -1, pfx_x_fourty_cor_adj),
             count = "1-1")
    
    # Identify Pitcher's Primary Pitch.
    session_id_first_pitch <- pitch_selected$session_id
    
    # Extract pitch type of first pitch or primary fastball.
    primary_fb <- update_later$tm_live_data %>%
      filter(session_id %in% session_id_first_pitch) %>%
      mutate(first_pitch_type = last(pitch_type)) %>%
      filter(pitch_type == first_pitch_type)
    
    # Fastball Averages.
    fb_avgs <- primary_fb %>%
      filter(first_pitch_type == "FF" | first_pitch_type == "SI") %>%
      group_by(session_id) %>%
      summarise(rel_speed_fb = mean(release_speed, na.rm = TRUE),
                pfx_x_fb = mean(pfx_x_fourty_cor_adj, na.rm = TRUE),
                pfx_z_fb = mean(pfx_z_fourty_cor_brks, na.rm = TRUE))
    
    # Cutter Averages.
    ct_avgs <- primary_fb %>%
      filter(first_pitch_type == "CT") %>%
      group_by(session_id) %>%
      summarise(rel_speed_ct = mean(release_speed, na.rm = TRUE),
                pfx_x_ct = mean(pfx_x_fourty_cor_adj, na.rm = TRUE),
                pfx_z_ct = mean(pfx_z_fourty_cor_brks, na.rm = TRUE))
    
    # Join Fastball and Cutter Averages to data frame by Session ID..
    pitch_selected <- left_join(pitch_selected, fb_avgs, by = "session_id")
    pitch_selected <- left_join(pitch_selected, ct_avgs, by = "session_id")
    
    # Have pitch go through functions to calculate Stuff RVs & IRVs again.
    pitch_selected <- data_manip_function(pitch_selected)
    pitch_selected <- ad_hoc_adjustments_function(pitch_selected)
    
    # Calculate Stuff+ & Pitch Quality.
    pitch_selected <- pitch_selected %>%
      mutate(stuff_plus = round(500 / (1 + exp(66.5463 * stuff_rv + 1.5629)), 0),
             pitch_quality = round(irv * 145 + 4.5, 2)) %>%
      rename(batter_hand = stand) %>%
      select(trackman_pitch_id, time, date, batter_hand, p_throws, pitch_type, pitch_group, release_speed, stuff_plus, pitch_quality, 
             pfx_x_fourty_cor_adj, pfx_z_fourty_cor_brks, plate_x_cor, plate_z_cor, horizontal_movement, induced_vertical_movement)
    
    # Upload Inputs & Metrics into Trackman API Table.
    pitchuid <- pitch_selected$trackman_pitch_id
    p_throws <- pitch_selected$p_throws
    pitch_type <- input$pitch_type_input
    batter_hand <- input$handedness_input
    stuff_plus <- pitch_selected$stuff_plus
    pitch_quality <- pitch_selected$pitch_quality
    
    # Bind objects as columns to create data frame uploaded to table for new values.
    df <- data.frame(pitchuid, p_throws, pitch_type, batter_hand, stuff_plus, pitch_quality)
    
    # Function for uploading new values into table.
    trackman_api_db_upload(df)
    
    # Update Trackman Live Dash data with the updated pitch and its characteristics.
    update_later$tm_live_data <- trackman_api_db_connect("SELECT *
                                                          FROM trackman_api_pitches_raw t1
                                                          LEFT JOIN trackman_live_dash_inputs t2 ON t2.pitchuid = t1.trackman_pitch_id
                                                          ORDER BY id DESC")
    
    lapply(dbListConnections(MySQL()), dbDisconnect)

    # Updated data frame goes through similar joining and changes as initial data loaded.
    update_later$tm_live_data <- update_later$tm_live_data %>%
      filter(session_id == first(session_id)) %>%
      # Rename columns to match those of Pitch & Stuff Models.
      rename(release_speed = release_speed,
             rel_height_cor = release_height,
             rel_side_cor = release_side,
             extension_cor = release_extension,
             pfx_z_fourty_cor_brks = pfxz,
             pfx_x_fourty_cor_adj = pfxx) %>%
      mutate(pitch_type = case_when(is.na(pitch_type) ~ "FF",
                                    TRUE ~ pitch_type),
             pitch_group = case_when(pitch_type %in% c("FF", "SI") ~ "Fastball",
                                     pitch_type %in% c("CT", "SL", "CB") ~ "Breaking Ball",
                                     pitch_type %in% c("CH", "SP") ~ "Offspeed"),
             plate_x_cor = plate_location_side / 12, # Location measured in feet changed to inches necessary for models.
             plate_z_cor = plate_location_height / 12,
             date = as.Date(time)) %>%
      replace_na(list(release_speed = 0, pfx_z_fourty_cor_brks = 0, pfx_x_fourty_cor_adj = 0, plate_x_cor = 0, plate_z_cor = 0, 
                      horizontal_movement = 0, induced_vertical_movement = 0, rel_height_cor = 0, rel_side_cor = 0, extension_cor = 0,
                      stuff_plus = 0, pitch_quality = 0))
  })
  
  # As specified above, the Undo Button is only to be used for incorrect tagging of Pitch Type and Batter Handedness.
  observeEvent(input$undo_button, {
    
    pitch_selected <- select(pitch_data(), c(trackman_pitch_id, batter_hand, pitch_type, p_throws, stuff_plus, pitch_quality))
    pitch_selected <- (pitch_selected[input$PitchHistoryTable_rows_selected,])
    
    pitchuid <- pitch_selected$trackman_pitch_id
    
    trackman_api_db <- dbConnect(MySQL(), 
                                 user = Sys.getenv('CLUSTER_USERNAME_COMPUTER_VISION_CLUSTER'),
                                 password = Sys.getenv('CLUSTER_PASSWORD_COMPUTER_VISION_CLUSTER'),
                                 dbname = Sys.getenv('DATABASE_TRACKMAN_API_DB'),
                                 host = Sys.getenv('CLUSTER_HOST_COMPUTER_VISION_CLUSTER'),
                                 port = as.numeric(Sys.getenv('CLUSTER_PORT_COMPUTER_VISION_CLUSTER')))
    
    # Query to delete the row where the selected pitch's ID matches that of the one in the trackman_live_dash_inputs table.
    delete_query <- paste0("DELETE FROM trackman_live_dash_inputs WHERE (pitchuid = '", pitchuid, "');")
    
    rsInsert <- dbSendQuery(trackman_api_db, delete_query)
    
    dbClearResult(rsInsert)
    
    dbDisconnect(trackman_api_db)
    
  })
  
  # Row Selection Event Code
  
  observeEvent(input$PitchHistoryTable_rows_selected, {
    
    s = input$PitchHistoryTable_rows_selected
    
    # Pitch Tab
    
    if (input$TypeInput == "Pitch") {
      
      if (input$switchInput == TRUE) { ### Pitch Toggle
        
        pitch_selected <- (pitch_data()[s,1])
        
        # Display metrics and specific point on the movement plot for pitch selected in Pitch History table.
        df <- pitch_data() %>% dplyr::filter(pitch_number == as.character(pitch_selected))
        
        # Dragless Pitch Movement Plot
        output$pitchMovementPlotPitch <- renderPlot({
          ggplot(df) +
            geom_rect(inherit.aes = F, mapping = aes(ymax = 30, ymin = -30, xmax = 0, xmin = 0), alpha = 0, size = 1.5, colour = "black") +
            geom_rect(inherit.aes = F, mapping = aes(ymax = 0, ymin = 0, xmax = 30, xmin = -30), alpha = 0, size = 1.5, colour = "black")  +
            geom_point(data = pitch_data(), aes(horizontal_movement, induced_vertical_movement, color = pitch_type), size = 3, alpha = 0.25) +
            geom_point(aes(horizontal_movement, induced_vertical_movement, fill = pitch_type), color = "black", pch = 21, size = 6, alpha = .9) + 
            pitch_colors + pitch_fills + 
            scale_x_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) + scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) + 
            labs(title = "Dragless Pitch Movement", x = "\nHorizontal Break", y = "Vertical Break\n") +
            theme_bw() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) + theme(axis.title = element_blank()) +
            theme(panel.border = element_blank(), panel.grid = element_blank(), panel.grid.major = element_line(color = "gray80", linetype = "dashed")) + 
            theme(legend.position = "none", text = element_text(family = "gotham"), axis.line = element_line(colour = "#808080")) +
            theme(plot.margin = unit(c(0,0.85,0,0), "cm"))
        }, height = 400, width = 600)
        
        # Horizontal Break Title Box
        output$horzBreakBoxTitlePitch <- renderText({"HB"})
        
        # Horizontal Break Value Box
        output$horzBreakBoxValuePitch <- renderText({paste(round(unique(df$horizontal_movement),1))})
        
        # Vertical Break Title Box
        output$vertBreakBoxTitlePitch <- renderText({"VB"})
        
        # Vertical Break Value Box
        output$vertBreakBoxValuePitch <- renderText({paste(round(unique(df$induced_vertical_movement),1))})
        
        # Velocity Title Box
        output$veloBoxTitlePitch <- renderText({"Velo"})
        
        # Velocity Value Box
        output$veloBoxValuePitch <- renderText({paste(round(unique(df$release_speed),1))})
        
        # Stuff+ Title Box
        output$stuffPlusBoxTitlePitch <- renderText({"Stuff+"})
        
        # Stuff+ Value Box
        output$stuffPlusBoxValuePitch <- renderText({paste(unique(df$stuff_plus))})
        
        # Pitch Quality Title Box
        output$pitchQualityBoxTitlePitch <- renderText({"xERA"})
        
        # Pitch Quality Value Box
        output$pitchQualityBoxValuePitch <- renderText({paste(unique(df$pitch_quality))})
        
        # Pitch Locations Plot
        if (df$pitch_group %in% c("Fastball", "Offspeed", "Breaking Ball")) {
          
          output$PitchModelPitch <- renderUI({
            
            fluidRow(
              box(width = NULL, 
                  height = 650,
                  status = "warning",
                  solidHeader = TRUE,
                  plotOutput("LocationsPlotPitch"),
              ), align = "center"
            )
            
          })
          
        }
        
        output$LocationsPlotPitch <- renderPlot({
          ggplot(df) + theme_bw() + 
            ggtitle("Pitch Locations (Pitcher's Perspective)") +
            geom_point(aes(x = plate_x_cor, y = plate_z_cor, color = pitch_type), size = 5, alpha = .85) +
            pitch_fills + pitch_colors + 
            geom_segment(aes(x = -.83083, y = 1.52166, xend = .83083, yend = 1.52166), size = 1.5, color = "black") + 
            geom_segment(aes(x = -.83083, y = 3.67333, xend = .83083, yend = 3.67333), size = 1.5, color = "black") + 
            geom_segment(aes(x = -.83083, y = 1.52166, xend = -.83083, yend = 3.67333), size = 1.5, color = "black") + 
            geom_segment(aes(x = .83083, y = 1.52166, xend = .83083, yend = 3.67333), size = 1.5, color = "black")+ 
            scale_x_continuous(name="Horizontal Location", limits = c(-3, 3)) + 
            scale_y_continuous(name="Vertical Location", limits = c(0,5))+ 
            geom_segment(aes( x = 0, y = .3, xend = -.83083, yend = .175), size = 1.5, color = "black")  +
            geom_segment(aes( x = -.83083, y = .175, xend = -.83083, yend = 0), size = 1.5, color = "black") + 
            geom_segment(aes( x = 0, y = .3, xend = .83083, yend = .175), size = 1.5, color = "black")  + 
            geom_segment(aes( x = .83083, y = .175, xend = .83083, yend = 0), size = 1.5, color = "black") +
            geom_segment(aes( x = -.83083, y = 0, xend = .83083, yend = 0), size = 1.5, color = "black") + 
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank()) + 
            geom_segment(aes(x = -1.5, y = 2.23887, xend = 1.5, yend = 2.23887), size = 1, color = "grey", linetype = "dashed") + geom_segment(aes(x = -1.5, y = 2.9561, xend = 1.5, yend = 2.9561), size = 1, color = "grey", linetype = "dashed") + 
            geom_segment(aes(x = -0.27694, y = 4, xend = -0.27694, yend = 1.19496), size = 1, color = "grey", linetype = "dashed") + geom_segment(aes(x = 0.27694, y = 4, xend = 0.27694, yend = 1.19496), size = 1, color = "grey", linetype = "dashed") + 
            theme(axis.title.x = element_text(face = "bold", size = 16, family = "gotham", vjust = -1))+ theme(axis.title.y = element_text(face = "bold", size = 16, family = "gotham"))+ theme(axis.text.x = element_text(face = "bold", size = 16, family = "gotham"))+ theme(axis.text.y = element_text(face = "bold", size = 16, family = "gotham"))+ 
            theme(plot.title = element_text(face = "bold", size = 18, family="gotham")) +
            theme(legend.position = c(.02, .95),
                  legend.title=element_blank(),
                  panel.border = element_blank(),
                  legend.justification = c("left", "top"),
                  legend.box.just = "left", legend.text = element_text(size=14, face = "bold", family = "gotham")) + guides(color = guide_legend(override.aes=list(size = 5, alpha = .85)))
        }, height = 600)
        
      } 
      
      else if (input$switchInput == FALSE) { # Group Toggle
        
        pitch_selected <- (pitch_data()[s, ])
        
        # Display average metrics and specific point on the movement plot for pitches selected in Pitch History table.
        df <- pitch_data() %>% dplyr::filter(pitch_number %in% (pitch_selected$pitch_number))
        
        # Dragless Pitch Movement Plot
        output$pitchMovementPlotPitch <- renderPlot({
          ggplot(df) +
            geom_rect(inherit.aes = F, mapping = aes(ymax = 30, ymin = -30, xmax = 0, xmin = 0), alpha = 0, size = 1.5, colour = "black") +
            geom_rect(inherit.aes = F, mapping = aes(ymax = 0, ymin = 0, xmax = 30, xmin = -30), alpha = 0, size = 1.5, colour = "black")  +
            geom_point(aes(horizontal_movement, induced_vertical_movement, fill = pitch_type), color = "black", pch = 21, size = 6, alpha = .9) + 
            pitch_colors + pitch_fills + 
            scale_x_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) + scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) + 
            labs(title = "Dragless Pitch Movement", x = "\nHorizontal Break", y = "Vertical Break\n") +
            theme_bw() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) + theme(axis.title = element_blank()) +
            theme(panel.border = element_blank(), panel.grid = element_blank(), panel.grid.major = element_line(color = "gray80", linetype = "dashed")) + 
            theme(legend.position = "none", text = element_text(family = "gotham"), axis.line = element_line(colour = "#808080")) +
            theme(plot.margin = unit(c(0,0.85,0,0), "cm"))
        }, height = 400, width = 600)
        
        # Mean Horizontal Break Title Box
        output$horzBreakBoxTitlePitch <- renderText({"Mean HB"})
        
        # Mean Horizontal Break Value Box
        output$horzBreakBoxValuePitch <- renderText({paste(round(mean(df$horizontal_movement),1))})
        
        # Mean Vertical Break Title Box
        output$vertBreakBoxTitlePitch <- renderText({"Mean VB"})
        
        # Mean Vertical Break Value Box
        output$vertBreakBoxValuePitch <- renderText({paste(round(mean(df$induced_vertical_movement),1))})
        
        # Mean Velocity Title Box
        output$veloBoxTitlePitch <- renderText({"Mean Velo"})
        
        # Mean Velocity Value Box
        output$veloBoxValuePitch <- renderText({paste(round(mean(df$release_speed),1))})
        
        # Mean Stuff Plus Title Box
        output$stuffPlusBoxTitlePitch <- renderText({"Mean Stuff+"})
        
        # Mean Stuff Plus Value Box
        output$stuffPlusBoxValuePitch <- renderText({paste(round(mean(df$stuff_plus),0))})
        
        # Mean Pitch Quality Title Box
        output$pitchQualityBoxTitlePitch <- renderText({"Mean xERA"})
        
        # Mean Pitch Quality Value Box
        output$pitchQualityBoxValuePitch <- renderText({paste(round(mean(df$pitch_quality),2))})
        
        # Pitch Locations Plot
        output$LocationsPlotPitch <- renderPlot({
          ggplot(df) + theme_bw() + 
            ggtitle("Pitch Locations (Pitcher's Perspective)") +
            geom_point(aes(x = plate_x_cor, y = plate_z_cor, color = pitch_type), size = 5, alpha = .85) +
            pitch_fills + pitch_colors + 
            geom_segment(aes(x = -.83083, y = 1.52166, xend = .83083, yend = 1.52166), size = 1.5, color = "black") + 
            geom_segment(aes(x = -.83083, y = 3.67333, xend = .83083, yend = 3.67333), size = 1.5, color = "black") + 
            geom_segment(aes(x = -.83083, y = 1.52166, xend = -.83083, yend = 3.67333), size = 1.5, color = "black") + 
            geom_segment(aes(x = .83083, y = 1.52166, xend = .83083, yend = 3.67333), size = 1.5, color = "black")+ 
            scale_x_continuous(name="Horizontal Location", limits = c(-3, 3)) + 
            scale_y_continuous(name="Vertical Location", limits = c(0,5))+ 
            geom_segment(aes( x = 0, y = .3, xend = -.83083, yend = .175), size = 1.5, color = "black")  +
            geom_segment(aes( x = -.83083, y = .175, xend = -.83083, yend = 0), size = 1.5, color = "black") + 
            geom_segment(aes( x = 0, y = .3, xend = .83083, yend = .175), size = 1.5, color = "black")  + 
            geom_segment(aes( x = .83083, y = .175, xend = .83083, yend = 0), size = 1.5, color = "black") +
            geom_segment(aes( x = -.83083, y = 0, xend = .83083, yend = 0), size = 1.5, color = "black") + 
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank()) + 
            geom_segment(aes(x = -1.5, y = 2.23887, xend = 1.5, yend = 2.23887), size = 1, color = "grey", linetype = "dashed") + geom_segment(aes(x = -1.5, y = 2.9561, xend = 1.5, yend = 2.9561), size = 1, color = "grey", linetype = "dashed") + 
            geom_segment(aes(x = -0.27694, y = 4, xend = -0.27694, yend = 1.19496), size = 1, color = "grey", linetype = "dashed") + geom_segment(aes(x = 0.27694, y = 4, xend = 0.27694, yend = 1.19496), size = 1, color = "grey", linetype = "dashed") + 
            theme(axis.title.x = element_text(face = "bold", size = 16, family = "gotham", vjust = -1))+ theme(axis.title.y = element_text(face = "bold", size = 16, family = "gotham"))+ theme(axis.text.x = element_text(face = "bold", size = 16, family = "gotham"))+ theme(axis.text.y = element_text(face = "bold", size = 16, family = "gotham"))+ 
            theme(plot.title = element_text(face = "bold", size = 18, family="gotham")) +
            theme(legend.position = c(.02, .95),
                  legend.title=element_blank(),
                  panel.border = element_blank(),
                  legend.justification = c("left", "top"),
                  legend.box.just = "left", legend.text = element_text(size=14, face = "bold", family = "gotham")) + guides(color = guide_legend(override.aes=list(size = 5, alpha = .85)))
        }, height = 600)
        
      } 
      
      
    }
    
    # Session Tab
    
    else if (input$TypeInput == "Session") {
      
      if (input$switchInput == TRUE) { # Pitch Toggle
        
      }
      
      else if (input$switchInput == FALSE) { # Group Toggle
        
        pitch_selected <- (summary_data()[s, ])
        
        # Session Data
        df <- session_data() %>% dplyr::filter(pitch_type %in% (pitch_selected$pitch_type))
        
        # Stuff+ Rolling Averages Data
        df_roll <- rolling_data() %>% dplyr::filter(pitch_type %in% (pitch_selected$pitch_type))
        
        # Summary Table Data
        df_sum <- summary_data() %>% dplyr::filter(pitch_type %in% pitch_selected$pitch_type)
        
        # Dragless Pitch Movement Plot
        output$pitchMovementPlotSession <- renderPlot({
          ggplot(df, aes(horizontal_movement, induced_vertical_movement, color = pitch_type)) +
            geom_rect(inherit.aes = F, mapping = aes(ymax = 30, ymin = -30, xmax = 0, xmin = 0), alpha = 0, size = 1.5, colour = "black") +
            geom_rect(inherit.aes = F, mapping = aes(ymax = 0, ymin = 0, xmax = 30, xmin = -30), alpha = 0, size = 1.5, colour = "black")  +
            geom_point(size = 5, alpha = .75) + pitch_colors +
            scale_x_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) + scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) + 
            labs(title = "Dragless Pitch Movement", x = "\nHorizontal Break", y = "Vertical Break\n") +
            theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank()) +
            theme(panel.border = element_blank(), panel.grid = element_blank(), panel.grid.major = element_line(color = "gray80", linetype = "dashed")) + 
            theme(legend.position = "none", text = element_text(family = "gotham"), axis.line = element_line(colour = "#808080")) +
            theme(plot.margin = unit(c(0,0.85,0,0), "cm"))
        }, height = 400, width = 700)
        
        # Rolling Stuff+ Averages Plot
        output$stuffPlusRollingSession <- renderPlot({
          ggplot(df_roll, aes(pitch_number, rolling_stuff_plus, group = pitch_type)) +
            geom_hline(aes(yintercept = 100), size = 1.5, color = "black") +
            geom_line(aes(color = pitch_type), size = 2) + geom_point(aes(color = pitch_type), size = 3) + pitch_colors +
            scale_y_continuous(limits = c(0, 200),  breaks = seq(0, 200, 25)) +
            labs(title = "Stuff+ Rolling Average (5-pitch)", x = "\nPitch Number", y = "Stuff+\n", color = "") + 
            theme_bw() + theme(axis.title = element_blank()) + 
            theme(panel.border = element_blank(), panel.grid = element_blank(), panel.grid.major.y = element_line(color = "gray80", linetype = "dashed")) + 
            theme(legend.text = element_text(hjust = .35, size = 10), text = element_text(family = "gotham"), axis.line = element_line(colour = "#808080"))
        }, height = 400, width = 700)
        
        # Summary Table with counts for each Pitch Type and Session Averages for Stuff+, xERA, Vertical Break, and Horizontal Break.
        output$pitchSummaryTableSession <- renderDataTable({
          datatable(df_sum, rowname = FALSE, options = list(dom = 't', ordering = F), selection = 'none', 
                    colnames = c('Pitch', '#', 'HB', 'VB', 
                                 'Stuff+', 'xERA'), class = 'cell-border stripe') %>%
            formatRound(c("hb", "vb", "pitch_quality"), 1) %>%
            formatRound(c("pitch_quality"), 2) 
        })
        
      }
    }
    
  })
  
  # Session Page - Pitch Toggle - Dragless Pitch Movement Plot
  output$pitchMovementPlotSession <- renderPlot({
    ggplot(session_data(), aes(horizontal_movement, induced_vertical_movement, color = pitch_type)) +
      geom_rect(inherit.aes = F, mapping = aes(ymax = 30, ymin = -30, xmax = 0, xmin = 0), alpha = 0, size = 1.5, colour = "black") +
      geom_rect(inherit.aes = F, mapping = aes(ymax = 0, ymin = 0, xmax = 30, xmin = -30), alpha = 0, size = 1.5, colour = "black")  +
      geom_point(size = 5, alpha = .75) + pitch_colors +
      scale_x_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) + scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) + 
      labs(title = "Dragless Pitch Movement", x = "\nHorizontal Break", y = "Vertical Break\n") +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank()) +
      theme(panel.border = element_blank(), panel.grid = element_blank(), panel.grid.major = element_line(color = "gray80", linetype = "dashed")) + 
      theme(legend.position = "none", text = element_text(family = "gotham"), axis.line = element_line(colour = "#808080")) +
      theme(plot.margin = unit(c(0,0.85,0,0), "cm"))
  }, height = 400, width = 700)
  
  # Session Page - Pitch Toggle - Stuff Plus Rolling Averages Plot 
  output$stuffPlusRollingSession <- renderPlot({
    ggplot(rolling_data(), aes(pitch_number, rolling_stuff_plus, group = pitch_type)) +
      geom_hline(aes(yintercept = 100), size = 1.5, color = "black") +
      geom_line(aes(color = pitch_type), size = 2) + geom_point(aes(color = pitch_type), size = 3) + pitch_colors +
      scale_y_continuous(limits = c(0, 200),  breaks = seq(0, 200, 25)) +
      labs(title = "Stuff+ Rolling Average (5-pitch)", x = "\nPitch Number", y = "Stuff+\n", color = "") + 
      theme_bw() + theme(axis.title = element_blank()) + 
      theme(panel.border = element_blank(), panel.grid = element_blank(), panel.grid.major.y = element_line(color = "gray80", linetype = "dashed")) + 
      theme(legend.text = element_text(hjust = .35, size = 10), text = element_text(family = "gotham"), axis.line = element_line(colour = "#808080"))
  }, height = 400, width = 700)
  
  # Session Page - Pitch Toggle - Summary Table
  output$pitchSummaryTableSession <- renderDataTable({
    datatable(summary_data(), rowname = FALSE, options = list(dom = 't', ordering = F), selection = 'none', 
              colnames = c('Pitch', '#', 'HB', 'VB',
                           'Stuff+', 'xERA'), class = 'cell-border stripe') %>%
      formatRound(c("hb", "vb", 
                    "pitch_quality"), 1) %>%
      formatRound(c("pitch_quality"), 2) 
  })
  
  
}

shinyApp(ui, server)
