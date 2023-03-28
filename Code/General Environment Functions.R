## Functions ##
library(ggplot2)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(rvest)
library(devtools)
library(baseballr)
library(na.tools)
library(scales)

library(gridExtra)
library(data.table)
library(kableExtra)
library(knitr)
library(mlbplotR)
library(gt)
library(gtExtras)

library(teamcolors)
library(ggridges)
library(hms)

#############################
##### Generic Functions #####
#############################
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
`%notin%` <- Negate(`%in%`)


############################
##### Lookup Functions #####
############################

teamidlookup <- function(abbr) {
  teamabbr %>%
    filter(TeamAbbr == abbr) %>%
    select(TeamID) %>%
    print() %>%
    as.integer()
}
gameidforfun <- function(x) {
  daily_schedule(x) %>%
    select(game_pk)
}
gamedateforfun <- function(x) {
  daily_schedule(x) %>%
    select(officialDate)
}
daily_schedule <- function(dt) {
  get_game_pks_mlb(dt, level_ids = c(1)) %>% 
    select(game_pk, teams.home.team.id, teams.away.team.id, gameNumber, officialDate)
}
p_lineupfunction <- function(gamedate, away.team, home.team, gamenum = 1) {
  get_probables_mlb(
    get_game_pks_mlb(gamedate, level_ids = c(1)) %>%
      filter(teams.away.team.id == away.team, teams.home.team.id == home.team, gameNumber == gamenum) %>%
      select(game_pk)
  )
}
pk_lu <- function(gamedate, away.team, home.team, gamenum = 1) {
  p <- get_game_pks_mlb(gamedate, level_ids = c(1)) %>%
    filter(teams.away.team.id == away.team, teams.home.team.id == home.team, gameNumber == gamenum) %>%
    select(game_pk)
  as.character(p)
}
team_p_war_lu <- function (x) {
  war_ref_team %>%
    filter(team == x) %>%
    select(total_p_war) %>%
    pull()
}
team_bat_war_lu <- function (x) {
  war_ref_team %>%
    filter(team == x) %>%
    select(tot_bat_war) %>%
    pull()
}
team_sp_war_lu <- function (x) {
  war_ref_team %>%
    filter(team == x) %>%
    select(tot_SP_war) %>%
    pull()
}
standings_RS_lu <- function(tm){
  standings %>%
    filter(Team == tm) %>%
    select(RS) %>%
    pull()
}
standings_RA_lu <- function(tm){
  standings %>%
    filter(Team == tm) %>%
    select(RA) %>%
    pull()
}
teamRP_lu <- function (x) {
  team_war %>%
    filter(team == x) %>%
    select(tot_RP_war) %>%
    pull()
}
CL_standings_RA_lu <- function(tm){
  standings %>%
    filter(Team == tm) %>%
    select(CL_RA) %>%
    pull()
}
CL_standings_RS_lu <- function(tm){
  standings %>%
    filter(Team == tm) %>%
    select(CL_RS) %>%
    pull()
}

#############################
##### SCRAPER FUNCTIONS #####
#############################

# Get PECOTA Standings #
PECOTA_FILE_DL <- function() {
  large_list <- read_html('https://www.baseballprospectus.com/standings/') %>%
    html_table()
  AL_df <- data.frame(large_list[[1]], stringsAsFactors = FALSE)
  NL_df <- data.frame(large_list[[2]], stringsAsFactors = FALSE)
  
  ###CLEAN TABLES###
  names(AL_df) <- c('Team', 'W', 'L', 'Win_Pct', 'RS', 'RA', 'Div_Pct', 'WC_Pct', 'Playoff_Pct', 'DivSeries_Pct', 'WS_Pct', 'Delta_1', 'Delta_7')
  names(NL_df) <- c('Team', 'W', 'L', 'Win_Pct', 'RS', 'RA', 'Div_Pct', 'WC_Pct', 'Playoff_Pct', 'DivSeries_Pct', 'WS_Pct', 'Delta_1', 'Delta_7')
  
  AL_df <- AL_df[-c(6,12),]
  NL_df <- NL_df[-c(6,12),]
  
  AL_df <- AL_df %>%
    mutate(
      team_Abbv = substrRight(Team, 3),
      Team = case_when(
        team_Abbv == 'yTB' ~ 'TB',
        team_Abbv == 'yKC' ~ 'KC',
        team_Abbv == 'CWS' ~ 'CHW',
        TRUE ~ team_Abbv
      )
    ) %>%
    select(1:13)
  
  NL_df <- NL_df %>%
    mutate(
      team_Abbv = substrRight(Team, 3),
      Team = case_when(
        team_Abbv == 'oSF' ~ 'SF',
        team_Abbv == 'oSD' ~ 'SD',
        team_Abbv == 'WSH' ~ 'WAS',
        TRUE ~ team_Abbv
      )
    ) %>%
    select(1:13)
  
  full_df <- rbind(AL_df,NL_df)
  
  cols.num <- c('W', 'L', 'Win_Pct', 'RS', 'RA', 'Div_Pct', 'WC_Pct', 'Playoff_Pct', 'DivSeries_Pct', 'WS_Pct', 'Delta_1', 'Delta_7')
  full_df[cols.num] <- sapply(full_df[cols.num], as.numeric)
  sapply(full_df, class)
  
  return(full_df)
}
CL_PECOTA_FILE_ADJ <- function(data) {
  test_df <- data %>%
    inner_join(teamabbr, by = c("Team" = "bp_teamabbr")) %>%
    select(Team, W, L, RS, RA, TeamID)
  
  standings1 <- test_df
  standings1 <- standings1 %>%
    inner_join(cl, by = c("TeamID")) %>%
    mutate(
      RS = as.integer(RS),
      RA = as.integer(RA),
      CL_RS = (RS - Off),
      CL_RA = (RA + Def),
    ) %>%
    select(Team.x, W, L, RS, RA, CL_RS, CL_RA, TeamID)
  names(standings1) = c("Team", "W", "L", "RS", "RA", "CL_RS", "CL_RA", "TeamID")
  return(standings1)
}

# Get ClusterLuck (BR) #
cluster_luck_func <- function() {
  tables <- read_html('https://www.fangraphs.com/depthcharts.aspx?position=BaseRuns') %>%
    html_nodes("table") %>%
    html_table(2)
  tables <- tables[[8]]
  names(tables) <- as.character(tables[1,])
  tables <- tables[-1,]
  
  names(tables) <- c( 'Team',
                      'Real_G', 'Real_W', 'Real_L', 'Real_WPct', 'Real_RDiff', 'Real_RSPerG', 'Real_RAPerG',
                      'Pythag_W', 'Pythag_L', 'Pythag_WPct', 'Pythag_WPlusMinus',
                      'CL_W', 'CL_L', 'CL_WPct','CL_PlusMinus', 'CL_RDiff', 'CL_RSPerG', 'CL_RAPerG'
  )
  
  final_df <- tables %>%
    mutate(
      CL_RSPerG = as.numeric(CL_RSPerG),
      Real_RSPerG = as.numeric(Real_RSPerG),
      Real_G = as.numeric(Real_G),
      CL_RAPerG = as.numeric(CL_RAPerG),
      Real_RAPerG = as.numeric(Real_RAPerG),
      Off = (CL_RSPerG-Real_RSPerG)*Real_G*-1,
      Def = (CL_RAPerG-Real_RAPerG)*Real_G,
      Total = as.numeric(specify_decimal((Off + Def),2))
    ) %>%
    select(Team, Off, Def, Total) %>%
    mutate(
      Team = case_when(
        Team == 'Angels' ~ 'L.A. Angels',
        Team == 'Diamondbacks' ~ 'Arizona',
        Team == 'Orioles' ~ 'Baltimore',
        Team == 'Red Sox' ~ 'Boston',
        Team == 'Cubs' ~ 'Chi. Cubs',
        Team == 'Reds' ~ 'Cincinnati',
        Team == 'Guardians' ~ 'Cleveland',
        Team == 'Rockies' ~ 'Colorado',
        Team == 'Tigers' ~ 'Detroit',
        Team == 'Astros' ~ 'Houston',
        Team == 'Royals' ~ 'Kansas City',
        Team == 'Dodgers' ~ 'L.A. Dodgers',
        Team == 'Nationals' ~ 'Washington',
        Team == 'Mets' ~ 'N.Y. Mets',
        Team == 'Athletics' ~ 'Oakland',
        Team == 'Pirates' ~ 'Pittsburgh',
        Team == 'Padres' ~ 'San Diego',
        Team == 'Mariners' ~ 'Seattle',
        Team == 'Giants' ~ 'San Francisco',
        Team == 'Cardinals' ~ 'St. Louis',
        Team == 'Rays' ~ 'Tampa Bay',
        Team == 'Rangers' ~ 'Texas',
        Team == 'Blue Jays' ~ 'Toronto',
        Team == 'Twins' ~ 'Minnesota',
        Team == 'Phillies' ~ 'Philadelphia',
        Team == 'Braves' ~ 'Atlanta',
        Team == 'White Sox' ~ 'Chi. White Sox',
        Team == 'Marlins' ~ 'Miami',
        Team == 'Yankees' ~ 'N.Y. Yankees',
        Team == 'Brewers' ~ 'Milwaukee',
        TRUE ~ Team
      )
    ) %>%
    inner_join(teamabbr, by = c("Team" = "vegasabbr"))
  
  return(final_df)
}

# Get Daily Lineups #
pitcher_prob_test <- function(dt, link = "https://baseballmonster.com/Lineups.aspx?csv=1") {
  path <- link
  day_rosters <- read_csv(link)
  names(day_rosters) = c("team", "game_date", "game_number", "mlb_id", "player", "batting_order", "confirmed", "position","weather")
  day_rosters <- day_rosters %>%
    filter(batting_order == 'SP') %>%
    select(team, mlb_id, player, game_number)
  day_rosters <- day_rosters %>%
    inner_join(teamabbr, by = c("team" = "bm_teamabbr")) %>%
    select(mlb_id, player, game_number, TeamID)
  
  schedule <- get_game_pks_mlb(dt, level_ids = c(1)) %>%
    select(game_pk, teams.away.team.id, teams.home.team.id)
  
  home_sched <- schedule %>%
    select(game_pk, teams.home.team.id)
  home_sched$teams.home.team.id <- as.character(home_sched$teams.home.team.id)
  home_sched <- home_sched %>%
    inner_join(day_rosters, by = c("teams.home.team.id" = "TeamID")) %>%
    select(game_pk, teams.home.team.id, mlb_id, player)
  
  away_sched <- schedule %>%
    select(game_pk, teams.away.team.id)
  away_sched$teams.away.team.id <- as.character(away_sched$teams.away.team.id)
  away_sched <- away_sched %>%
    inner_join(day_rosters, by = c("teams.away.team.id" = "TeamID")) %>%
    select(game_pk, teams.away.team.id, mlb_id, player)
  
  names(home_sched) = c("game_pk", "home.TeamID","home_p_id", "home_p_name")
  names(away_sched) = c("game_pk", "away.TeamID","away_p_id", "away_p_name")
  
  df <- home_sched %>%
    inner_join(away_sched, by = c("game_pk")) %>%
    mutate(
      home_p_id = case_when(
        home_p_name == 'Shohei (P) Ohtani' ~ 660271,
        home_p_name == 'Anthony Kay' ~ 641743,
        home_p_name == 'Chris Ellis' ~ 595939,
        home_p_name == 'Zack Thompson' ~ 668868,
        home_p_name == 'Jackson Tetreault' ~ 676194,
        home_p_name == 'Garrett Hill' ~ 682051,
        home_p_name == 'Cole Ragans' ~ 666142,
        home_p_name == 'Xzavion Curry' ~ 675540,
        home_p_name == 'Ken Waldichuk' ~ 686610,
        home_p_name == 'Ryne Nelson' ~ 669194,
        home_p_name == 'Hunter Gaddis' ~ 683769,
        home_p_name == 'Ryne Nelson' ~ 669194,
        TRUE ~ home_p_id
      ),
      away_p_id = case_when(
        away_p_name == 'Shohei (P) Ohtani' ~ 660271,
        away_p_name == 'Anthony Kay' ~ 641743,
        away_p_name == 'Chris Ellis' ~ 595939,
        away_p_name == 'Zack Thompson' ~ 668868,
        away_p_name == 'Jackson Tetreault' ~ 676194,
        away_p_name == 'Garrett Hill' ~ 682051,
        away_p_name == 'Cole Ragans' ~ 666142,
        away_p_name == 'Xzavion Curry' ~ 675540,
        away_p_name == 'Ken Waldichuk' ~ 686610,
        away_p_name == 'Ryne Nelson' ~ 669194,
        away_p_name == 'Hunter Gaddis' ~ 683769,
        away_p_name == 'Ryne Nelson' ~ 669194,
        TRUE ~ away_p_id
      )
    )
  df
}
#pitcher_prob_test(dt = "2023-03-28")
lineup_prob_test <- function(dt, link = "https://baseballmonster.com/Lineups.aspx?csv=1") {
  path <- link
  day_rosters <- read_csv(link)
  names(day_rosters) = c("team", "game_date", "game_number", "mlb_id", "player", "batting_order", "confirmed", "position","weather")
  
  day_rosters <- day_rosters %>%
    filter(batting_order != 'SP') %>%
    select(team, mlb_id, player, position, game_number, batting_order)
  
  day_rosters <- day_rosters %>%
    inner_join(teamabbr, by = c("team" = "bm_teamabbr")) %>%
    select(mlb_id, player, position,batting_order, game_number, TeamID)
  
  schedule <- get_game_pks_mlb(dt, level_ids = c(1)) %>%
    select(game_pk, teams.away.team.id, teams.home.team.id)
  
  home_sched <- schedule %>%
    select(game_pk, teams.home.team.id)
  home_sched$teams.home.team.id <- as.character(home_sched$teams.home.team.id)
  
  home_sched <- home_sched %>%
    left_join(day_rosters, by = c("teams.home.team.id" = "TeamID"), multiple = "all") %>%
    select(game_pk, teams.home.team.id, mlb_id, player, position, batting_order)
  
  away_sched <- schedule %>%
    select(game_pk, teams.away.team.id)
  away_sched$teams.away.team.id <- as.character(away_sched$teams.away.team.id)
  away_sched <- away_sched %>%
    left_join(day_rosters, by = c("teams.away.team.id" = "TeamID"), multiple = "all") %>%
    select(game_pk, teams.away.team.id, mlb_id, player, position, batting_order)
  
  names(home_sched) = c("game_pk", "home.TeamID","home_bat_id", "home_bat_name", "home_pos", "home_batting_order")
  names(away_sched) = c("game_pk", "away.TeamID","away_bat_id", "away_bat_name", "away_pos", "away_batting_order")
  
  df <- home_sched %>%
    inner_join(away_sched, by = c("game_pk", "home_batting_order" = "away_batting_order")) %>%
    mutate(
      home_bat_id = case_when(
        home_bat_name == 'Shohei (H) Ohtani' ~ 660271,
        home_bat_name == 'Masataka Yoshida' ~ 807799,
        TRUE ~ home_bat_id
      ),
      away_bat_id = case_when(
        away_bat_name == 'Shohei (H) Ohtani' ~ 660271,
        away_bat_name == 'Masataka Yoshida' ~ 807799,
        TRUE ~ away_bat_id
      )
    ) %>%
    filter(!is.na(home_bat_name) | !is.na(away_bat_name))
    
  df
}
#lineup_prob_test(dt = "2023-03-28")


#################################
##### CALCULATION FUNCTIONS #####
#################################

# Calculate Win Percentages Based on Lineups #
wp_calc <- function(g_id) {
  ## Pitchers ##
  h_team <- mlb_api_pitchers %>%
    filter(game_pk %in% g_id) %>%
    select(game_pk, home.TeamID, home_p_id, home_p_name)
  h_t <- as.integer(h_team$home.TeamID)
  h_p <- as.integer(h_team$home_p_id)
  
  a_team <- mlb_api_pitchers %>%
    filter(game_pk %in% g_id) %>%
    select(game_pk, away.TeamID, away_p_id, away_p_name)
  a_t <- as.integer(a_team$away.TeamID)
  a_p <- as.integer(a_team$away_p_id)

  ### Batter Adjustment ##
h_rs_adj <- lineup_adjust(gid = g_id, loc = 'H')
a_rs_adj <- lineup_adjust(gid = g_id, loc = 'A')
  
  
  home_wp <- PITCH_PECOTA_FULL %>%
    filter(TeamID == h_t, mlbid == h_p) %>%
    select(name, team, gs, warp, ip, IP_GS) %>%
    mutate(IP_GS = if_else(IP_GS == 0, 4, IP_GS),
           if60war = ((warp*IP_GS)/ip)*162,
           p_war = team_sp_war_lu(team),
           p_adv = (if60war - p_war)*10,
           #h_team_rs = as.numeric(CL_standings_RS_lu(team)) + (as.numeric(inj_RS_lu(team))*.5),
           #h_team_ra = as.numeric(CL_standings_RA_lu(team)),
           h_team_rs = as.numeric(standings_RS_lu(team)) + h_rs_adj,
           h_team_ra = as.numeric(standings_RA_lu(team)),
           if60RA = (h_team_ra - p_adv)) %>%
    mutate(p_wp = ((h_team_rs)^1.83)/(((h_team_rs)^1.83)+((if60RA)^1.83)))%>%
    select(team, name, p_wp)

  away_wp <- PITCH_PECOTA_FULL %>%
    filter(TeamID == a_t, mlbid == a_p) %>%
    select(name, team, gs, warp, ip, IP_GS) %>%
    mutate(IP_GS = if_else(IP_GS == 0 | is.na(IP_GS), 4, IP_GS),
           if60war = ((warp*IP_GS)/ip)*162,
           p_war = team_sp_war_lu(team),
           p_adv = (if60war - p_war)*10,
           #a_team_rs = as.numeric(CL_standings_RS_lu(team)) + (as.numeric(inj_RS_lu(team))*.5),
           #a_team_ra = as.numeric(CL_standings_RA_lu(team)),
           a_team_rs = as.numeric(standings_RS_lu(team)) + a_rs_adj,
           a_team_ra = as.numeric(standings_RA_lu(team)),
           if60RA = (a_team_ra - p_adv)) %>%
    mutate(p_wp = ((a_team_rs)^1.83)/(((a_team_rs)^1.83)+((if60RA)^1.83)))%>%
    select(team, name, p_wp)

  wp_df <- data.frame(home_team = home_wp$team,
                      home_win = home_wp$p_wp,
                      away_team = away_wp$team,
                      away_win = away_wp$p_wp,
                      game_id = h_team$game_pk)
  names(wp_df) = c("home_team","home_win","away_team","away_win", "game_id")
  wp_df <- wp_df %>%
    mutate(
      home_loss = 1 - home_win,
      away_loss = 1 - away_win,
      home_w_away_l = home_win*away_loss,
      home_l_away_w = home_loss*away_win,
      home_winpercent = (home_w_away_l/(home_w_away_l+home_l_away_w))+0.03,
      away_winpercent = (home_l_away_w/(home_w_away_l+home_l_away_w))-0.03,
      h_odds_raw = 1/home_winpercent,
      a_odds_raw = 1/away_winpercent,
      home_ML_odds = ifelse(h_odds_raw > 2, 100*(h_odds_raw-1), (-100)/(h_odds_raw-1)),
      away_ML_odds = ifelse(a_odds_raw > 2, 100*(a_odds_raw-1), (-100)/(a_odds_raw-1))
    ) %>%
    select(game_id, home_team, home_ML_odds, away_team, away_ML_odds)
  data.frame(wp_df)

}
lineup_adjust <- function(gid, loc) {
  hit_pecota <- HIT_PECOTA_FULL %>%
    select(mlbid, name, team, pa, g, warp, on_DC, TeamID)
  
  h_team_bat <- mlb_api_batters %>%
    filter(game_pk %in% gid) %>%
    select(game_pk, home.TeamID, home_bat_id, home_bat_name, home_pos) %>%
    mutate(
      home_bat_id = as.integer(home_bat_id)
    ) %>%
    left_join(hit_pecota, by = c('home.TeamID' = 'TeamID', "home_bat_id" = "mlbid"))
  
  a_team_bat <- mlb_api_batters %>%
    filter(game_pk %in% gid) %>%
    select(game_pk, away.TeamID, away_bat_id, away_bat_name, away_pos) %>%
    mutate(
      away_bat_id = as.integer(away_bat_id)
    ) %>%
    left_join(hit_pecota, by = c('away.TeamID' = 'TeamID', "away_bat_id" = "mlbid"))
  
  new_a_team_bat <- a_team_bat
  names(new_a_team_bat) = names(h_team_bat)
  
  test <- rbind(h_team_bat, new_a_team_bat)

  
  if(length(names(which(colSums(is.na(test))>0))) > 0){
    teamabbr_slim <- teamabbr %>%
      select(TeamID, bp_teamabbr)
    
    Name_Values <- test %>%
      filter(is.na(name)) %>%
      select(home.TeamID, home_bat_name) %>%
      left_join(teamabbr_slim, by = c("home.TeamID" = "TeamID")) %>%
      mutate(
        full_warning = paste0(home_bat_name, ' on ', bp_teamabbr)
      )
    names_warn <- sapply(Name_Values$full_warning, paste, collapse=" ")
    
     print(paste0("ERROR: Could Not Match ", names_warn, ' To PECOTA'))
  } else {
    print('All Good')
  }

  
  h_team <- h_team_bat %>%
    group_by(team) %>%
    summarise(
      warp = sum(warp),
      g = sum(g),
    ) %>%
    ungroup()%>%
      mutate(
    RS = as.numeric(standings_RS_lu(team)),
    warp162 = (warp/g)*162*9,
    warp_reg = team_bat_war_lu(team),
    lu_adv = (warp162-warp_reg)*10
    )
  
  a_team <- a_team_bat %>%
    group_by(team) %>%
    summarise(
      warp = sum(warp),
      g = sum(g),
    ) %>%
    ungroup()%>%
    mutate(
      RS = as.numeric(standings_RS_lu(team)),
      warp162 = (warp/g)*162*9,
      warp_reg = team_bat_war_lu(team),
      lu_adv = (warp162-warp_reg)*10
    )
  h_team_war_adv <- as.vector(h_team[,7])
  a_team_war_adv <- as.vector(a_team[,7])
  
  ha <- loc
  if(ha == 'H'){
    return(h_team_war_adv%>%pull())
  } else {
    return(a_team_war_adv%>%pull())
  }
}

#HIT_PECOTA_FULL %>% select(name, TeamID, mlbid) %>% filter(name == 'Bryce Willits')
#PITCH_PECOTA_FULL %>% filter(mlbid == 	678024)
# Apply Win Percentages To Gambling Lines - Get Bets #
money_maker <- function(dt) {
  vegas_test  <- mm_hcb #get_lines(dt) #
  tommy_test <- hardcodebets
  
  bets <- vegas_test %>%
    inner_join(tommy_test, by = c("game_id")) %>%
    select(game_id, home, home_ML_odds, away, away_ML_odds
           ,home_line, away_line, betonline_away, betonline_home)
  bets$home_line <- gsub("[+]", "", bets$home_line)
  bets$away_line <- gsub("[+]", "", bets$away_line)
  bets$home_line <- as.numeric(bets$home_line)
  bets$away_line <- as.numeric(bets$away_line)
  
  
  bets <- bets %>%
    mutate(
      home_actual_abs = abs(home_line),
      away_actual_abs = abs(away_line),
      home_predict_abs = abs(home_ML_odds),
      away_predict_abs = abs(away_ML_odds),
      home_BOL_abs = abs(betonline_home),
      away_BOL_abs = abs(betonline_away),
      home_prob_actual = if_else(home_line > 0,
                                 100/(home_line+100),
                                 home_actual_abs/(home_actual_abs+100)),
      away_prob_actual = if_else(away_line > 0,
                                 100/(away_line+100),
                                 away_actual_abs/(away_actual_abs+100)),
      home_prob_model = if_else(home_ML_odds > 0,
                                100/(home_ML_odds+100),
                                home_predict_abs/(home_predict_abs+100)),
      away_prob_model = if_else(away_ML_odds > 0,
                                100/(away_ML_odds+100),
                                away_predict_abs/(away_predict_abs+100)),
      home_prob_BOL = if_else(betonline_home > 0,
                              100/(betonline_home+100),
                              home_BOL_abs/(home_BOL_abs+100)),
      away_prob_BOL = if_else(betonline_away > 0,
                              100/(betonline_away+100),
                              away_BOL_abs/(away_BOL_abs+100)),
      home_imp_prob_actual = home_prob_actual/(home_prob_actual+away_prob_actual),
      away_imp_prob_actual = away_prob_actual/(home_prob_actual+away_prob_actual),
      home_imp_prob_model = home_prob_model/(home_prob_model+away_prob_model),
      away_imp_prob_model = away_prob_model/(home_prob_model+away_prob_model),
      home_imp_prob_BOL = home_prob_BOL/(home_prob_BOL+away_prob_BOL),
      away_imp_prob_BOL = away_prob_BOL/(home_prob_BOL+away_prob_BOL),
      home_adv = home_imp_prob_model - home_imp_prob_actual,
      away_adv = away_imp_prob_model - away_imp_prob_actual,
      home_BOL_adv = home_imp_prob_model - home_imp_prob_BOL,
      away_BOL_adv = away_imp_prob_model - away_imp_prob_BOL,
      abs_adv = abs(home_adv),
      abs_bol_adv = abs(home_BOL_adv),
      BOLAdvantage = abs(home_BOL_adv),
      Advantage = abs(home_adv),
      BOLAdvantage = scales::percent(BOLAdvantage),
      Advantage = scales::percent(Advantage),
      bet = if_else(home_adv > 0, home, away),
      BOLbet = if_else(home_BOL_adv > 0, home, away),
      unit_bet = if_else(abs_adv > .2, 10, if_else(abs_adv > .15, 8, if_else(abs_adv > .1, 6, if_else(abs_adv > .0499, 4, if_else(abs_adv >.025, 2, 1))))),
      BOL_unit_bet = if_else(abs_bol_adv > .2, 10, if_else(abs_bol_adv > .15, 8, if_else(abs_bol_adv > .1, 6, if_else(abs_bol_adv > .0499, 4, if_else(abs_bol_adv >.025, 2, 1)))))
    )
  bets <- bets %>%
    select(game_id, home, home_ML_odds,home_line, away, away_ML_odds, away_line, Advantage, bet, unit_bet, betonline_home, betonline_away,BOLAdvantage, BOLbet, BOL_unit_bet)
  bets <- bets %>%
    mutate(
      BOL_bet_odds = if_else(BOLbet == home, betonline_home, betonline_away),
      BOL_unit_bet = if_else(BOL_unit_bet >= 4 & BOL_bet_odds <= -275, 1, BOL_unit_bet),
      BOLAdvantage = if_else(BOL_unit_bet >= 4 & BOL_bet_odds <= -275, '1.00%', BOLAdvantage),
      abs_bol_odds = abs(BOL_bet_odds),
      euro_odds = if_else(BOL_bet_odds > 0, ((BOL_bet_odds/100)+1), (((100/(abs_bol_odds))+1))),
      risk = if_else(euro_odds < 2, (BOL_unit_bet/(euro_odds - 1)), BOL_unit_bet),
      to_win = ((risk*euro_odds) - risk)
    )
  bets
} #Old
money_maker2 <- function(dt, gid_filter) {
  Book_Odds <- daily_odds %>%
    left_join(tb_A %>% select(game_pk, gameNumber, bp_teamabbr)%>%rename(away_team = bp_teamabbr), by = c('gameNumber', 'away_team')) %>%
    mutate(
      game_pk = as.character(game_pk)
    )
  #print(Book_Odds)
  
  g_filt <- gid_filter
  sched <-  if(length(g_filt) > 0 ){
    good_pks <- mlb_api_pitchers %>% select(game_pk) %>% unique() %>% pull()
    
    df <- daily_schedule(dt = dt) %>%
      mutate(
        teams.home.team.id = as.character(teams.home.team.id),
        teams.away.team.id = as.character(teams.away.team.id)
      ) %>%
      filter(game_pk %notin% g_filt & game_pk %in% good_pks) %>%
      mutate(game_pk = as.character(game_pk))
    
    df
  } else {
    good_pks <- mlb_api_pitchers %>% select(game_pk) %>% unique() %>% pull()
    
    df <- daily_schedule(dt = dt) %>%
      mutate(
        teams.home.team.id = as.character(teams.home.team.id),
        teams.away.team.id = as.character(teams.away.team.id)
      ) %>%
      filter(game_pk %in% good_pks) %>%
      mutate(game_pk = as.character(game_pk))
    
    df
  }
  
  ## GET PROSPECTIVE REBIRTHA CALC TO RETURN BEST LINES WITH TIME STAMP ##  
  
  sched_len <- length(sched$game_pk)
  listofdfs <- list()
  for(i in 1:sched_len) {
    gid <- sched[i,1]
    g <- wp_calc(gid)
    listofdfs[[i]] <- g
  }
  Rebirtha_Odds <- bind_rows(listofdfs) %>%
    mutate(
      game_pk = as.character(game_id),
      home_team = as.character(home_team),
      away_team = as.character(away_team)
    )
  
  ##JOIN REBIRTHA LINE TO BOL LINE##
  full_bets <- Book_Odds %>%
    left_join(Rebirtha_Odds, by = c('game_pk')) %>%
    select(game_pk, Time_Local, Date, gameNumber, home_team.x, away_team.x,
           betonline_home, betonline_away, BetTimeStamp, home_ML_odds, away_ML_odds) %>%
    rename(
      Home_Abbr = home_team.x,
      Away_Abbr = away_team.x,
      Home_Book_ML = betonline_home,
      Away_Book_ML = betonline_away,
      Home_Rebirtha_ML = home_ML_odds,
      Away_Rebirtha_ML = away_ML_odds
    ) %>%
    left_join(teamabbr %>% select(bp_teamabbr, full_name), by =c('Home_Abbr' = 'bp_teamabbr')) %>%
    left_join(teamabbr %>% select(bp_teamabbr, full_name), by =c('Away_Abbr' = 'bp_teamabbr')) %>%
    rename(
      Home = full_name.x,
      Away = full_name.y
    )
  
  #print(full_bets)
  
  
  
  final_bets <- full_bets %>%
    mutate(
      Home_ImpProb_Book = if_else(Home_Book_ML > 0,
                                  100/(Home_Book_ML+100),
                                  abs(Home_Book_ML)/(abs(Home_Book_ML)+100)),
      Away_ImpProb_Book = if_else(Away_Book_ML > 0,
                                  100/(Away_Book_ML+100),
                                  abs(Away_Book_ML)/(abs(Away_Book_ML)+100)),
      Home_ImpProb_Rebirtha = if_else(Home_Rebirtha_ML > 0,
                                      100/(Home_Rebirtha_ML+100),
                                      abs(Home_Rebirtha_ML)/(abs(Home_Rebirtha_ML)+100)),
      Away_ImpProb_Rebirtha = if_else(Away_Rebirtha_ML > 0,
                                      100/(Away_Rebirtha_ML+100),
                                      abs(Away_Rebirtha_ML)/(abs(Away_Rebirtha_ML)+100)),
      home_adv = Home_ImpProb_Rebirtha - Home_ImpProb_Book,
      away_adv = Away_ImpProb_Rebirtha - Away_ImpProb_Book,
      Bet_Team = if_else(home_adv > away_adv, Home, Away),
      Bet_TeamAbbr = if_else(home_adv > away_adv, Home_Abbr, Away_Abbr),
      Advantage = if_else(home_adv > away_adv, home_adv, away_adv),
      Unit_Bet = if_else(Advantage > .15, 10, if_else(Advantage > .10, 8, if_else(Advantage > .07, 6, if_else(Advantage > .04, 4, if_else(Advantage >.02, 2, if_else(Advantage > 0, 1, 0)))))),
      Bet_Odds = if_else(Bet_Team == Home, Home_Book_ML, Away_Book_ML),
      Bet_Euro_Odds = if_else(Bet_Odds > 0, ((Bet_Odds/100)+1), (((100/(abs(Bet_Odds)))+1))),
      Unit_Risk = if_else(Bet_Euro_Odds < 2, (Unit_Bet/(Bet_Euro_Odds - 1)), Unit_Bet),
      To_Win = round(((Unit_Risk*Bet_Euro_Odds) - Unit_Risk),digits = 2)
    )
  final_bets$game_pk <- as.integer(final_bets$game_pk)
  return(final_bets)
}

# Summarize Results and Plot #
summary_results <- function(var = c(1,2,4,6,8,10),
                            tm = teamabbr$vegasabbr,
                            odd = c(-500:500)){
  df  <- Results %>% 
    filter(BOL_unit_bet %in% var,
           BOLbet %in% tm,
           BOL_bet_odds %in% odd,
           w_l != 2)
  
  unit_profit_raw <- ((sum(df$profit)))
  unit_profit <- round(unit_profit_raw, digits = 2)
  unit_profit <- paste(unit_profit, "u", sep = "")
  dollar_profit <- (unit_profit_raw*10)
  dollar_profit <- round(dollar_profit, digits = 2)
  dollar_profit <- dollar(dollar_profit)
  ROI <- unit_profit_raw/(sum(df$risk))
  ROI <- round(ROI*100, digits = 2)
  ROI <- paste(ROI, "%", sep = "")
  wins <- (sum(df$w_l == 1, na.rm=TRUE))
  losses <- (sum(df$w_l == 0, na.rm=TRUE))
  win_pct <- wins/(wins+losses)
  win_pct <- round(win_pct*100, digits = 2)
  win_pct <- paste(win_pct, "%", sep = "")
  df_t <- data_frame(unit_profit, dollar_profit, ROI, win_pct, wins, losses)
  names(df_t) <- c("Unit Profit", "Dollar Profit", "ROI", "Win %", "Wins", "Losses")
  unit_profit_var_raw <- (sum(df$profit))
  unit_profit_var <- round(unit_profit_var_raw, digits = 2)
  unit_profit_var <- paste(unit_profit_var, "u", sep = "")
  dollar_profit_var <- (unit_profit_var_raw*10)
  dollar_profit_var <- round(dollar_profit_var, digits = 2)
  dollar_profit_var <- dollar(dollar_profit_var)
  ROI_var <- (unit_profit_var_raw/(sum(df$risk)))
  ROI_var <- round(ROI_var*100, digits = 2)
  ROI_var <- paste(ROI_var, "%", sep = "")
  wins_var <- sum(df$w_l == 1, na.rm=TRUE)
  losses_var <- sum(df$w_l == 0, na.rm=TRUE)
  win_pct_var <- wins_var/(wins_var+losses_var)
  win_pct_var <- round(win_pct_var*100, digits = 2)
  win_pct_var <- paste(win_pct_var, "%", sep = "")
  df_var <- data_frame(unit_profit_var, dollar_profit_var, ROI_var, win_pct_var, wins_var, losses_var)
  names(df_var) <- c("Unit Profit", "Dollar Profit", "ROI", "Win %", "Wins", "Losses")
  
  
  d <- df
  d$game_id <- as.integer(d$game_id)
  d <- d %>%
    inner_join(completed_games, by = c("game_id" = "game_pk"))
  
  g <- d
  g <- g %>%
    group_by(officialDate)%>%
    summarise(profit = sum(profit)) %>%
    as.data.frame(g)
  
  len_b <- (length(g$officialDate)+1)
  day_num <- c(2:len_b)
  g <- cbind(g,day_num)
  day_len <- length(g$officialDate)
  g <- g %>%
    select(day_num, profit) %>%
    mutate(
      profit = (profit)
    ) %>%
    select(day_num, profit)
  
  g <- rbind(c(1,0),g)
  g$day_number <- factor(g$day_num, levels = c(1:day_len))
  #g <- g %>%
  # drop_na()
  
  
  
  #len_a <- (length(d$game_id)+1)
  #game_num <- c(2:len_a)
  #d <- cbind(d,game_num)
  #len <- (length(d$game_id))
  #
  #d <- d %>%
  #  select(game_num, profit) %>%
  #  mutate(
  #    profit_dol = profit*5
  #  ) %>%
  #  select(game_num, profit_dol)
  #
  #d <- rbind(c(1, 0), d)
  #d$game_number <- factor(d$game_num, levels = c(1:len))
  #
  #
  #p <- d %>%
  #  ggplot(aes(x=game_number, y=cumsum(profit_dol), group=1)) +
  #  geom_line()+
  #  geom_hline(yintercept = 0, color = "red", linetype = 2)+
  #  geom_point() +
  #  ggtitle(paste0("Cumulative Profits For 2021 MLB Regular Season YTD"))+
  #  theme_classic()
  #
  q <- g %>%
    ggplot(aes(x=day_number, y=cumsum(profit), group=1)) +
    geom_line()+
    geom_hline(yintercept = 0, color = "red", linetype = 2)+
    geom_vline(xintercept = 80, color = 'blue', linetype = 2) +
    geom_point() +
    labs(title="Cumulative Profits For 2022 MLB Regular Season YTD",x="Day Number", y = "Profit/Loss (Units)")+
    theme_classic()
  
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)),
                       base_size = 10,
                       padding = unit(c(2, 4), "mm"))
  xyz <- df_t
  tbl <- tableGrob(xyz, rows=NULL, theme=tt)
  n <- grid.arrange(q, tbl, nrow = 2, heights = c(2, 0.5))
  n
}

################################
##### BUILD GEN ENV TABLES #####
################################

##Create Team Ref Table
teamabbr_build <- function(){
teamabbr <- data.frame(c('Los Angeles Angels', 'Arizona Diamondbacks', 'Baltimore Orioles', 'Boston Red Sox', 'Chicago Cubs', 'Cincinnati Reds',
                         'Cleveland Guardians', 'Colorado Rockies', 'Detroit Tigers', 'Houston Astros', 'Kansas City Royals', 'Los Angeles Dodgers',
                         'Washington Nationals', 'New York Mets', 'Oakland Athletics', 'Pittsburgh Pirates', 'San Diego Padres', 'Seattle Mariners',
                         'San Francisco Giants', 'St. Louis Cardinals', 'Tampa Bay Rays', 'Texas Rangers', 'Toronto Blue Jays', 'Minnesota Twins',
                         'Philadelphia Phillies', 'Atlanta Braves', 'Chicago White Sox', 'Miami Marlins', 'New York Yankees', 'Milwaukee Brewers'),
                       stringsAsFactors = FALSE)
names(teamabbr) <- c("full_name")

teamabbr$fg_teamabbr <- c('LAA', 'ARI', 'BAL', 'BOS', 'CHN', 'CIN',
                          'CLE', 'COL', 'DET', 'HOU', 'KCA', 'LAN',
                          'WAS', 'NYN', 'OAK', 'PIT', 'SDN', 'SEA',
                          'SFN', 'SLN', 'TBA', 'TEX', 'TOR', 'MIN',
                          'PHI', 'ATL', 'CHA', 'MIA', 'NYA', 'MIL')

teamabbr$bp_teamabbr <- c('LAA', 'ARI', 'BAL', 'BOS', 'CHC', 'CIN',
                          'CLE', 'COL', 'DET', 'HOU', 'KC', 'LAD',
                          'WAS', 'NYM', 'OAK', 'PIT', 'SD', 'SEA',
                          'SF', 'STL', 'TB', 'TEX', 'TOR', 'MIN',
                          'PHI', 'ATL', 'CHW', 'MIA', 'NYY', 'MIL')
teamabbr$vegasabbr <- c('L.A. Angels', 'Arizona', 'Baltimore', 'Boston', 'Chi. Cubs', 'Cincinnati',
                        'Cleveland', 'Colorado', 'Detroit', 'Houston', 'Kansas City', 'L.A. Dodgers',
                        'Washington', 'N.Y. Mets', 'Oakland', 'Pittsburgh', 'San Diego', 'Seattle',
                        'San Francisco', 'St. Louis', 'Tampa Bay', 'Texas', 'Toronto', 'Minnesota',
                        'Philadelphia', 'Atlanta', 'Chi. White Sox', 'Miami', 'N.Y. Yankees', 'Milwaukee')
teamabbr$bm_teamabbr <- c('LAA', 'ARI', 'BAL', 'BOS', 'CHC', 'CIN',
                          'CLE', 'COL', 'DET', 'HOU', 'KC', 'LAD',
                          'WAS', 'NYM', 'OAK', 'PIT', 'SD', 'SEA',
                          'SF', 'STL', 'TB', 'TEX', 'TOR', 'MIN',
                          'PHI', 'ATL', 'CHW', 'MIA', 'NYY', 'MIL')

tm_id_helper <- data.frame(as.character(c(
  108,109,110,111,112,113,114,115,116,117,118,119,120,121,133,
  134,135,136,137,138,139,140,141,142,143,144,145,146,147,158
)))

tm_id_helper$tm <- c(
  "LAA","ARI","BAL","BOS","CHC","CIN","CLE","COL","DET","HOU","KC","LAD","WAS","NYM","OAK",
  "PIT","SD","SEA","SF","STL","TB","TEX","TOR","MIN","PHI","ATL","CHW","MIA","NYY","MIL"
)
names(tm_id_helper) <- c("TeamID", "TM")
teamabbr <- teamabbr %>%
  inner_join(tm_id_helper, by = c("bm_teamabbr" = "TM"))

## Build PECOTA Scrape Depth Chart Reference and Join ##
PECOTA_DF <- data.frame(
  bp_teamabbr = c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KC", "LAA", "LAD",
                  "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT","SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WAS"),
  link_pattern = c("arizona-diamondbacks", "atlanta-braves", "baltimore-orioles", "boston-red-sox", "chicago-cubs", "chicago-white-sox",
                   "cincinnati-reds", "cleveland-guardians", "colorado-rockies", "detroit-tigers", "houston-astros", "kansas-city-royals",
                   "los-angeles-angels", "los-angeles-dodgers", "miami-marlins", "milwaukee-brewers", "minnesota-twins", "new-york-mets",
                   "new-york-yankees", "oakland-athletics", "philadelphia-phillies", "pittsburgh-pirates", "san-diego-padres", "seattle-mariners",
                   "san-francisco-giants", "st-louis-cardinals", "tampa-bay-rays", "texas-rangers", "toronto-blue-jays", "washington-nationals"),
  stringsAsFactors = FALSE
)

PECOTA_DF <- PECOTA_DF %>% 
  mutate(
    League = case_when(
      bp_teamabbr %in% c("ARI", "ATL", "CHC", "CIN", "COL", "LAD", "MIA", "MIL", "NYM", "PHI", "PIT",
                       "SD", "SF", "STL", "WAS") ~ 'NL',
      bp_teamabbr %in% c("BAL", "BOS", "CHW", "CLE", "DET", "HOU", "KC", "LAA", "MIN", "NYY", "OAK",
                       "SEA", "TB", "TEX", "TOR") ~ 'AL'
    ),
    Division = case_when(
      bp_teamabbr %in% c("ATL", "MIA", "NYM", "PHI", "WAS") ~ 'NL East',
      bp_teamabbr %in% c("CHC", "CIN", "MIL", "PIT", "STL") ~ 'NL Central',
      bp_teamabbr %in% c("ARI", "COL", "LAD", "SD" , "SF" ) ~ 'NL West',
      bp_teamabbr %in% c("BAL", "BOS", "NYY", "TB" , "TOR") ~ 'AL East',
      bp_teamabbr %in% c("CHW", "DET", "CLE", "KC" , "MIN") ~ 'AL Central',
      bp_teamabbr %in% c("HOU", "OAK", "SEA", "TEX", "LAA") ~ 'AL West',
    )
  )

teamabbr <- teamabbr %>%
  inner_join(PECOTA_DF, by = c('bp_teamabbr'))

teamabbr$TeamID <- as.character(teamabbr$TeamID)
return(teamabbr)
}

##### DEPTH CHARTS #####

## Clean Depth Chart CSVs To Join To PECOTA DF ##
p_roles_clean <- function(){
  df <- P_Roles_Raw
  df$PT_PCT <- gsub(" ", "", df$PT_PCT)
  suppressWarnings({
    df <- df %>%
      separate(PT_PCT, c("first", "second", "third")) %>%
      separate(first, into = c("Role_1", "Role_1_PCT"), "(?<=[A-Z])(?=[0-9])") %>%
      separate(second, into = c("Role_2", "Role_2_PCT"), "(?<=[A-Z])(?=[0-9])") %>%
      mutate(
        SP_PCT = if_else(Role_1=='GS', as.numeric(Role_1_PCT), if_else(Role_2=='GS', as.numeric(Role_2_PCT), 0)),
        RP_PCT = if_else(Role_1=='RP', as.numeric(Role_1_PCT), if_else(Role_2=='RP', as.numeric(Role_2_PCT), 0)),
        SV_PCT = if_else(Role_1=='SV', as.numeric(Role_1_PCT), if_else(Role_2=='SV', as.numeric(Role_2_PCT), 0)),
        IP = as.numeric(str_extract(ROS, "\\d+")),
        IP_GS = as.numeric(IP_GS)
      )
  })
  
  df[is.na(df)] <- 0
  
  df <- df %>%
    group_by(Team, Player, SP_PCT, RP_PCT, SV_PCT, IP, IP_GS) %>%
    summarise(
      PLY_SP =  SP_PCT / sum(SP_PCT, RP_PCT, SV_PCT),
      PLY_RP =  RP_PCT / sum(SP_PCT, RP_PCT, SV_PCT),
      PLY_SV =  SV_PCT / sum(SP_PCT, RP_PCT, SV_PCT)
    )
  
  names(df)[names(df) == 'Player'] <- 'join_name'
  names(df)[names(df) == 'Team'] <- 'team'
  return(df)
}
h_roles_clean <- function(){
  df <- H_Roles_Raw
  df$PT_PCT <- gsub(" ", "", df$PT_PCT)
  suppressWarnings({
    df <- df %>%
      separate(PT_PCT, c("Role_1", "Role_1_PA", "Role_2", "Role_2_PA", "Role_3", "Role_3_PA", "Role_4", "Role_4_PA", "Role_5", "Role_5_PA", "Role_6", "Role_6_PA")) %>%
      separate(Role_1, into = c("Pos_1", "Pos_1_PCT"), "(?<=[A-Z])(?=[0-9])") %>%
      separate(Role_2, into = c("Pos_2", "Pos_2_PCT"), "(?<=[A-Z])(?=[0-9])") %>%
      separate(Role_3, into = c("Pos_3", "Pos_3_PCT"), "(?<=[A-Z])(?=[0-9])") %>%
      separate(Role_4, into = c("Pos_4", "Pos_4_PCT"), "(?<=[A-Z])(?=[0-9])") %>%
      separate(Role_5, into = c("Pos_5", "Pos_5_PCT"), "(?<=[A-Z])(?=[0-9])") %>%
      separate(Role_6, into = c("Pos_6", "Pos_6_PCT"), "(?<=[A-Z])(?=[0-9])") %>%
      mutate(
        C_PCT = if_else(Pos_1=='C', as.numeric(Pos_1_PCT), if_else(Pos_2=='C', as.numeric(Pos_2_PCT), if_else(Pos_3=='C', as.numeric(Pos_3_PCT), if_else(Pos_4=='C', as.numeric(Pos_4_PCT), if_else(Pos_5=='C', as.numeric(Pos_5_PCT), if_else(Pos_6=='C', as.numeric(Pos_6_PCT), 0)))))),
        B1_PCT = if_else(Pos_1=='1B', as.numeric(Pos_1_PCT), if_else(Pos_2=='1B', as.numeric(Pos_2_PCT), if_else(Pos_3=='1B', as.numeric(Pos_3_PCT), if_else(Pos_4=='1B', as.numeric(Pos_4_PCT), if_else(Pos_5=='1B', as.numeric(Pos_5_PCT), if_else(Pos_6=='1B', as.numeric(Pos_6_PCT), 0)))))),
        B2_PCT = if_else(Pos_1=='2B', as.numeric(Pos_1_PCT), if_else(Pos_2=='2B', as.numeric(Pos_2_PCT), if_else(Pos_3=='2B', as.numeric(Pos_3_PCT), if_else(Pos_4=='2B', as.numeric(Pos_4_PCT), if_else(Pos_5=='2B', as.numeric(Pos_5_PCT), if_else(Pos_6=='2B', as.numeric(Pos_6_PCT), 0)))))),
        B3_PCT = if_else(Pos_1=='3B', as.numeric(Pos_1_PCT), if_else(Pos_2=='3B', as.numeric(Pos_2_PCT), if_else(Pos_3=='3B', as.numeric(Pos_3_PCT), if_else(Pos_4=='3B', as.numeric(Pos_4_PCT), if_else(Pos_5=='3B', as.numeric(Pos_5_PCT), if_else(Pos_6=='3B', as.numeric(Pos_6_PCT), 0)))))),
        SS_PCT = if_else(Pos_1=='SS', as.numeric(Pos_1_PCT), if_else(Pos_2=='SS', as.numeric(Pos_2_PCT), if_else(Pos_3=='SS', as.numeric(Pos_3_PCT), if_else(Pos_4=='SS', as.numeric(Pos_4_PCT), if_else(Pos_5=='SS', as.numeric(Pos_5_PCT), if_else(Pos_6=='SS', as.numeric(Pos_6_PCT), 0)))))),
        LF_PCT = if_else(Pos_1=='LF', as.numeric(Pos_1_PCT), if_else(Pos_2=='LF', as.numeric(Pos_2_PCT), if_else(Pos_3=='LF', as.numeric(Pos_3_PCT), if_else(Pos_4=='LF', as.numeric(Pos_4_PCT), if_else(Pos_5=='LF', as.numeric(Pos_5_PCT), if_else(Pos_6=='LF', as.numeric(Pos_6_PCT), 0)))))),
        RF_PCT = if_else(Pos_1=='RF', as.numeric(Pos_1_PCT), if_else(Pos_2=='RF', as.numeric(Pos_2_PCT), if_else(Pos_3=='RF', as.numeric(Pos_3_PCT), if_else(Pos_4=='RF', as.numeric(Pos_4_PCT), if_else(Pos_5=='RF', as.numeric(Pos_5_PCT), if_else(Pos_6=='RF', as.numeric(Pos_6_PCT), 0)))))),
        CF_PCT = if_else(Pos_1=='CF', as.numeric(Pos_1_PCT), if_else(Pos_2=='CF', as.numeric(Pos_2_PCT), if_else(Pos_3=='CF', as.numeric(Pos_3_PCT), if_else(Pos_4=='CF', as.numeric(Pos_4_PCT), if_else(Pos_5=='CF', as.numeric(Pos_5_PCT), if_else(Pos_6=='CF', as.numeric(Pos_6_PCT), 0)))))),
        DH_PCT = if_else(Pos_1=='DH', as.numeric(Pos_1_PCT), if_else(Pos_2=='DH', as.numeric(Pos_2_PCT), if_else(Pos_3=='DH', as.numeric(Pos_3_PCT), if_else(Pos_4=='DH', as.numeric(Pos_4_PCT), if_else(Pos_5=='DH', as.numeric(Pos_5_PCT), if_else(Pos_6=='DH', as.numeric(Pos_6_PCT), 0)))))),
        PA = as.numeric(str_extract(ROS, "\\d+"))
      )  %>%
      select(1,2,22:31)
  })
  
  df[is.na(df)] <- 0
  
  df <- df %>%
    group_by(Team, Batters, C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, LF_PCT, RF_PCT, CF_PCT, DH_PCT, PA) %>%
    summarise(
      PLY_C  =  C_PCT  / sum(C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, LF_PCT, RF_PCT, CF_PCT, DH_PCT),
      PLY_1B =  B1_PCT / sum(C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, LF_PCT, RF_PCT, CF_PCT, DH_PCT),
      PLY_2B =  B2_PCT / sum(C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, LF_PCT, RF_PCT, CF_PCT, DH_PCT),
      PLY_3B =  B3_PCT / sum(C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, LF_PCT, RF_PCT, CF_PCT, DH_PCT),
      PLY_SS =  SS_PCT / sum(C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, LF_PCT, RF_PCT, CF_PCT, DH_PCT),
      PLY_LF =  LF_PCT / sum(C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, LF_PCT, RF_PCT, CF_PCT, DH_PCT),
      PLY_RF =  RF_PCT / sum(C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, LF_PCT, RF_PCT, CF_PCT, DH_PCT),
      PLY_CF =  CF_PCT / sum(C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, LF_PCT, RF_PCT, CF_PCT, DH_PCT),
      PLY_DH =  DH_PCT / sum(C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, LF_PCT, RF_PCT, CF_PCT, DH_PCT)
    )
  
  names(df)[names(df) == 'Batters'] <- 'join_name'
  names(df)[names(df) == 'Team'] <- 'team'
  
  return(df)
}

#### JOINING TO PECOTA ON NAME AND TEAM ####

## Test That Names Are Matching - IF NOT, ADD TO CASE WHEN HERE AND BELOW ##
h_test_match_depth <- function() {
  dc_num <- length(H_Roles_Clean$join_name)
  df <- HIT_PECOTA_RAW %>% mutate(join_name = paste0(first_name, " ", last_name)) %>%
    mutate(
      join_name = case_when(
        join_name == 'Lourdes Gurriel' ~ 'Lourdes Gurriel Jr.',
        join_name == 'Hoy Park' ~ 'Hoy Jun Park',
        join_name == 'Michael Harris' ~ 'Michael Harris II',
        join_name == 'Ronald Acuna' ~ 'Ronald Acuna Jr.',
        join_name == 'Carlos Perez' ~ 'Carlos Pérez',
        join_name == 'Leury García' ~ 'Leury Garcia',
        join_name == 'Yoelqui Cespedes' ~ 'Yoelqui Céspedes',
        join_name == 'Jose  Barrero' ~ 'Jose Barrero',
        join_name == 'Michael Siani' ~ 'Mike Siani',
        join_name == 'Andy Ibanez' ~ 'Andy Ibáñez',
        join_name == 'Bobby Witt' ~ 'Bobby Witt Jr.',
        join_name == 'Jerar Encarnacion' ~ 'Jerar Encarnación',
        join_name == 'Mike Brosseau' ~ 'Michael Brosseau',
        join_name == 'Francisco Alvarez' ~ 'Francisco Álvarez',
        join_name == 'Simon Muzziotti' ~ 'Símon Muzziotti',
        join_name == 'Fernando Tatis' ~ 'Fernando Tatis Jr.',
        join_name == 'Ha-Seong Kim' ~ 'Ha-seong Kim',
        join_name == 'Jose Azocar' ~ 'José Azocar',
        join_name == 'AJ Pollock' ~ 'A.J. Pollock',
        join_name == 'Julio Rodriguez' ~ 'Julio Rodríguez',
        join_name == 'LaMonte Wade' ~ 'LaMonte Wade Jr',
        join_name == 'Luis Gonzalez' ~ 'Luis González',
        join_name == 'Roberto Pérez' ~ 'Roberto Perez',
        join_name == 'Rene Pinto' ~ 'René Pinto',
        join_name == 'Nathaniel Lowe' ~ 'Nate Lowe',
        join_name == 'Vladimir Guerrero' ~ 'Vladimir Guerrero Jr.',
        join_name == 'CJ Abrams' ~ 'C.J. Abrams',
        join_name == 'A.J. Alexy' ~ 'AJ Alexy',
        TRUE ~ join_name
      )
    ) %>%
    inner_join(H_Roles_Clean, by=c('join_name', 'team')) %>%
    mutate(n = 1) %>%
    group_by() %>%
    summarise(tot = sum(n))
  
  nm_df <- HIT_PECOTA_RAW %>%
    mutate(
      join_name = paste0(first_name, " ", last_name)
    ) %>%
    mutate(
      join_name = case_when(
        join_name == 'Lourdes Gurriel' ~ 'Lourdes Gurriel Jr.',
        join_name == 'Hoy Park' ~ 'Hoy Jun Park',
        join_name == 'Michael Harris' ~ 'Michael Harris II',
        join_name == 'Ronald Acuna' ~ 'Ronald Acuna Jr.',
        join_name == 'Carlos Perez' ~ 'Carlos Pérez',
        join_name == 'Leury García' ~ 'Leury Garcia',
        join_name == 'Yoelqui Cespedes' ~ 'Yoelqui Céspedes',
        join_name == 'Jose  Barrero' ~ 'Jose Barrero',
        join_name == 'Michael Siani' ~ 'Mike Siani',
        join_name == 'Andy Ibanez' ~ 'Andy Ibáñez',
        join_name == 'Bobby Witt' ~ 'Bobby Witt Jr.',
        join_name == 'Jerar Encarnacion' ~ 'Jerar Encarnación',
        join_name == 'Mike Brosseau' ~ 'Michael Brosseau',
        join_name == 'Francisco Alvarez' ~ 'Francisco Álvarez',
        join_name == 'Simon Muzziotti' ~ 'Símon Muzziotti',
        join_name == 'Fernando Tatis' ~ 'Fernando Tatis Jr.',
        join_name == 'Ha-Seong Kim' ~ 'Ha-seong Kim',
        join_name == 'Jose Azocar' ~ 'José Azocar',
        join_name == 'AJ Pollock' ~ 'A.J. Pollock',
        join_name == 'Julio Rodriguez' ~ 'Julio Rodríguez',
        join_name == 'LaMonte Wade' ~ 'LaMonte Wade Jr',
        join_name == 'Luis Gonzalez' ~ 'Luis González',
        join_name == 'Roberto Pérez' ~ 'Roberto Perez',
        join_name == 'Rene Pinto' ~ 'René Pinto',
        join_name == 'Nathaniel Lowe' ~ 'Nate Lowe',
        join_name == 'Vladimir Guerrero' ~ 'Vladimir Guerrero Jr.',
        join_name == 'CJ Abrams' ~ 'C.J. Abrams',
        join_name == 'A.J. Alexy' ~ 'AJ Alexy',
        TRUE ~ join_name
      )
    ) %>%
    full_join(H_Roles_Clean, by=c('join_name', 'team')) %>%
    filter(is.na(name)) %>%
    select(first_name, last_name,  name, join_name, team)
  
  full_num <- df[1,1]
  
  print(paste0('Depth Chart Has ',dc_num,' Rows | Full Data Has ', full_num, ' Rows'))
  print(nm_df)
}
p_test_match_depth <- function() {
  dc_num <- length(P_Roles_Clean$join_name)
  df <- PITCH_PECOTA_RAW %>%
    mutate(
      join_name = paste0(first_name, " ", last_name)
    ) %>%
    mutate(
      join_name = case_when(
        join_name == 'Luis Frias' ~ 'Luis Frías',
        join_name == 'Michael Soroka' ~ 'Mike Soroka',
        join_name == 'Mike Baumann' ~ 'Michael Baumann',
        join_name == 'Franklin German' ~ 'Frank German',
        join_name == 'Mark Leiter' ~ 'Mark Leiter Jr.',
        join_name == 'Manuel Rodríguez' ~ 'Manuel Rodriguez',
        join_name == 'Lance McCullers' ~ 'Lance McCullers Jr.',
        join_name == 'Jonathan Heasley' ~ 'Jon Heasley',
        join_name == 'J.C. Mejia' ~ 'Jean Carlos Mejia',
        join_name == 'Nestor Cortes' ~ 'Nestor Cortes Jr.',
        join_name == 'Yerry De Los Santos' ~ 'Luis De Los Santos',
        join_name == 'Duane Underwood' ~ 'Duane Underwood Jr.',
        join_name == 'Nick Martinez' ~ 'Nick Martínez',
        join_name == 'Brent Honeywell' ~ 'Brent Honeywell Jr.',
        join_name == 'Luis Patino' ~ 'Luis Patiño',
        join_name == 'Pete Fairbanks' ~ 'Peter Fairbanks',
        join_name == 'Hyun Jin Ryu' ~ 'Hyun-Jin Ryu',
        join_name == 'Mitch White' ~ 'Mitchell White',
        join_name == 'Carl Edwards' ~ 'Carl Edwards Jr.',
        join_name == 'Eury Perez' ~ 'Eury Pérez',
        join_name == 'Edwin O Diaz' ~ 'Edwin Diaz',
        join_name == 'Thaddeus Ward' ~ 'Thad Ward',
        TRUE ~ join_name
      )
    ) %>%
    inner_join(P_Roles_Clean, by=c('join_name', 'team')) %>%
    mutate(n = 1) %>% group_by() %>%  summarise(tot = sum(n))
  
  nm_df <- PITCH_PECOTA_RAW %>%
    mutate(
      join_name = paste0(first_name, " ", last_name)
    ) %>%
    mutate(
      join_name = case_when(
        join_name == 'Luis Frias' ~ 'Luis Frías',
        join_name == 'Michael Soroka' ~ 'Mike Soroka',
        join_name == 'Mike Baumann' ~ 'Michael Baumann',
        join_name == 'Franklin German' ~ 'Frank German',
        join_name == 'Mark Leiter' ~ 'Mark Leiter Jr.',
        join_name == 'Manuel Rodríguez' ~ 'Manuel Rodriguez',
        join_name == 'Lance McCullers' ~ 'Lance McCullers Jr.',
        join_name == 'Jonathan Heasley' ~ 'Jon Heasley',
        join_name == 'J.C. Mejia' ~ 'Jean Carlos Mejia',
        join_name == 'Nestor Cortes' ~ 'Nestor Cortes Jr.',
        join_name == 'Yerry De Los Santos' ~ 'Luis De Los Santos',
        join_name == 'Duane Underwood' ~ 'Duane Underwood Jr.',
        join_name == 'Nick Martinez' ~ 'Nick Martínez',
        join_name == 'Brent Honeywell' ~ 'Brent Honeywell Jr.',
        join_name == 'Luis Patino' ~ 'Luis Patiño',
        join_name == 'Pete Fairbanks' ~ 'Peter Fairbanks',
        join_name == 'Hyun Jin Ryu' ~ 'Hyun-Jin Ryu',
        join_name == 'Mitch White' ~ 'Mitchell White',
        join_name == 'Carl Edwards' ~ 'Carl Edwards Jr.',
        join_name == 'Eury Perez' ~ 'Eury Pérez',
        join_name == 'Edwin O Diaz' ~ 'Edwin Diaz',
        join_name == 'Thaddeus Ward' ~ 'Thad Ward',
        TRUE ~ join_name
      )
    ) %>%
    full_join(P_Roles_Clean, by=c('join_name', 'team')) %>%
    filter(is.na(name)) %>%
    select(first_name, last_name,  name, join_name, team)
  
  full_num <- df[1,1]
  print(paste0('Depth Chart Has ',dc_num,' Rows | Full Data Has ', full_num, ' Rows'))
  print(nm_df)
}

## Check And Change Case When In Below Function ##
match_hit_depth <- function() { 
  df <- HIT_PECOTA_RAW %>%
    mutate(
      join_name = paste0(first_name, " ", last_name)
    ) %>%
    mutate(
      join_name = case_when(
        join_name == 'Lourdes Gurriel' ~ 'Lourdes Gurriel Jr.',
        join_name == 'Hoy Park' ~ 'Hoy Jun Park',
        join_name == 'Michael Harris' ~ 'Michael Harris II',
        join_name == 'Ronald Acuna' ~ 'Ronald Acuna Jr.',
        join_name == 'Carlos Perez' ~ 'Carlos Pérez',
        join_name == 'Leury García' ~ 'Leury Garcia',
        join_name == 'Yoelqui Cespedes' ~ 'Yoelqui Céspedes',
        join_name == 'Jose  Barrero' ~ 'Jose Barrero',
        join_name == 'Michael Siani' ~ 'Mike Siani',
        join_name == 'Andy Ibanez' ~ 'Andy Ibáñez',
        join_name == 'Bobby Witt' ~ 'Bobby Witt Jr.',
        join_name == 'Jerar Encarnacion' ~ 'Jerar Encarnación',
        join_name == 'Mike Brosseau' ~ 'Michael Brosseau',
        join_name == 'Francisco Alvarez' ~ 'Francisco Álvarez',
        join_name == 'Simon Muzziotti' ~ 'Símon Muzziotti',
        join_name == 'Fernando Tatis' ~ 'Fernando Tatis Jr.',
        join_name == 'Ha-Seong Kim' ~ 'Ha-seong Kim',
        join_name == 'Jose Azocar' ~ 'José Azocar',
        join_name == 'AJ Pollock' ~ 'A.J. Pollock',
        join_name == 'Julio Rodriguez' ~ 'Julio Rodríguez',
        join_name == 'LaMonte Wade' ~ 'LaMonte Wade Jr',
        join_name == 'Luis Gonzalez' ~ 'Luis González',
        join_name == 'Roberto Pérez' ~ 'Roberto Perez',
        join_name == 'Rene Pinto' ~ 'René Pinto',
        join_name == 'Nathaniel Lowe' ~ 'Nate Lowe',
        join_name == 'Vladimir Guerrero' ~ 'Vladimir Guerrero Jr.',
        join_name == 'CJ Abrams' ~ 'C.J. Abrams',
        join_name == 'A.J. Alexy' ~ 'AJ Alexy',
        TRUE ~ join_name
      )
    ) %>%
    left_join(H_Roles_Clean, by=c('join_name', 'team')) %>%
    mutate(on_DC = if_else(!is.na(C_PCT), 1, 0))
  
  df <- df %>%
    left_join(hit_all, by =c("bpid", 'team', 'pos'))

  
  return(df)
}
match_pitch_depth <- function() { 
  df <- PITCH_PECOTA_RAW %>%
    mutate(
      join_name = paste0(first_name, " ", last_name)
    ) %>%
    mutate(
      join_name = case_when(
        join_name == 'Luis Frias' ~ 'Luis Frías',
        join_name == 'Michael Soroka' ~ 'Mike Soroka',
        join_name == 'Mike Baumann' ~ 'Michael Baumann',
        join_name == 'Franklin German' ~ 'Frank German',
        join_name == 'Mark Leiter' ~ 'Mark Leiter Jr.',
        join_name == 'Manuel Rodríguez' ~ 'Manuel Rodriguez',
        join_name == 'Lance McCullers' ~ 'Lance McCullers Jr.',
        join_name == 'Jonathan Heasley' ~ 'Jon Heasley',
        join_name == 'J.C. Mejia' ~ 'Jean Carlos Mejia',
        join_name == 'Nestor Cortes' ~ 'Nestor Cortes Jr.',
        join_name == 'Yerry De Los Santos' ~ 'Luis De Los Santos',
        join_name == 'Duane Underwood' ~ 'Duane Underwood Jr.',
        join_name == 'Nick Martinez' ~ 'Nick Martínez',
        join_name == 'Brent Honeywell' ~ 'Brent Honeywell Jr.',
        join_name == 'Luis Patino' ~ 'Luis Patiño',
        join_name == 'Pete Fairbanks' ~ 'Peter Fairbanks',
        join_name == 'Hyun Jin Ryu' ~ 'Hyun-Jin Ryu',
        join_name == 'Mitch White' ~ 'Mitchell White',
        join_name == 'Carl Edwards' ~ 'Carl Edwards Jr.',
        join_name == 'Eury Perez' ~ 'Eury Pérez',
        join_name == 'Edwin O Diaz' ~ 'Edwin Diaz',
        join_name == 'Thaddeus Ward' ~ 'Thad Ward',
        TRUE ~ join_name
      )
    ) %>%
    left_join(P_Roles_Clean, by=c('join_name', 'team')) %>%
    mutate(on_DC = if_else(!is.na(IP), 1, 0))
  
  df <- df %>%
    left_join(pitch_all, by =c("bpid", 'team'))

  
  return(df)
}

## Build WAR Summary Chart
build_war_tbl <- function() {
  hit <- HIT_PECOTA_FULL %>% dplyr::filter(on_DC == 1)
  
  hit <- hit %>% select(1,5, 15,starts_with('warp'), ends_with("PCT"), starts_with('PLY')) %>%
    group_by(bpid, name, team, PLY_C, PLY_1B, PLY_2B, PLY_3B, PLY_SS, PLY_LF, PLY_RF, PLY_CF, PLY_DH,
             C_PCT, B1_PCT, B2_PCT, B3_PCT, SS_PCT, RF_PCT, CF_PCT, LF_PCT, DH_PCT) %>%
    summarise(
      warp_c_50 = sum(PLY_C*warp_50),
      warp_1B_50 = sum(PLY_1B*warp_50),
      warp_2B_50 = sum(PLY_2B*warp_50),
      warp_3B_50 = sum(PLY_3B*warp_50),
      warp_SS_50 = sum(PLY_SS*warp_50),
      warp_LF_50 = sum(PLY_LF*warp_50),
      warp_RF_50 = sum(PLY_RF*warp_50),
      warp_CF_50 = sum(PLY_CF*warp_50),
      warp_DH_50 = sum(PLY_DH*warp_50)
    )
  
  tm_hit <- hit %>%
    group_by(team) %>%
    summarise(
      warp_c_50 = sum(warp_c_50),
      warp_1B_50 = sum(warp_1B_50),
      warp_2B_50 = sum(warp_2B_50),
      warp_3B_50 = sum(warp_3B_50),
      warp_SS_50 = sum(warp_SS_50),
      warp_LF_50 = sum(warp_LF_50),
      warp_RF_50 = sum(warp_RF_50),
      warp_CF_50 = sum(warp_CF_50),
      warp_DH_50 = sum(warp_DH_50),
      tot_bat_war = sum(warp_c_50, 
                        warp_1B_50,
                        warp_2B_50,
                        warp_3B_50,
                        warp_SS_50,
                        warp_LF_50,
                        warp_RF_50,
                        warp_CF_50,
                        warp_DH_50)
    )
  
  ### Pitch ###
  
  pitch <- PITCH_PECOTA_FULL %>% dplyr::filter(on_DC == 1)
  
  pitch <- pitch %>% select(1, 5, 14,starts_with('warp'), ends_with("PCT"), starts_with('PLY')) %>%
    group_by(bpid, name, team, PLY_SP, PLY_RP, PLY_SV, SP_PCT, RP_PCT, SV_PCT) %>%
    summarise(
      warp_SP_50 = sum(PLY_SP*warp_50),
      warp_RP_50 = sum(PLY_RP*warp_50),
      warp_SV_50 = sum(PLY_SV*warp_50)
    )
  
  tm_pitch <- pitch %>%
    group_by(team) %>%
    summarise(
      warp_SP_50 = sum(warp_SP_50),
      warp_RP_50 = sum(warp_RP_50),
      warp_SV_50 = sum(warp_SV_50),
      tot_p_war = sum(warp_SP_50, warp_RP_50, warp_SV_50)
    )
  
  full_tm <- tm_hit %>%
    inner_join(tm_pitch, by='team') %>%
    group_by(team, tot_bat_war, tot_p_war, warp_c_50, warp_1B_50, warp_2B_50, warp_3B_50, warp_SS_50, warp_LF_50, warp_RF_50, warp_CF_50, warp_DH_50, warp_SP_50, warp_RP_50, warp_SV_50) %>%
    summarise(total_war = sum(tot_bat_war, tot_p_war)) %>%
    select(1,16,2:15)
  
  # Join To get TeamID #
  full_tm <- full_tm %>%
    inner_join(teamabbr, by = c("team" = "bp_teamabbr")) %>%
    mutate(
      tot_RP_war = sum(warp_SV_50, warp_RP_50),
      tot_SP_war = warp_SP_50
      ) %>%
    select(team, TeamID, total_war, tot_bat_war, tot_p_war, tot_SP_war, tot_RP_war, starts_with("warp"))
  return(full_tm)
}

#####################################
##### TRADE ADJUSTMENT FUNCTION #####
#####################################
Trade_Adj <- function(data) {
  get_trans_data <- function (year) {
    root_url <- 'http://lookup-service-prod.mlb.com'
    params_dates <- sprintf('start_date=%s0101&end_date=%s1231', year, year+1)
    params <- paste('/json/named.transaction_all.bam?&sport_code=%27mlb%27', params_dates, sep = '&')
    js <- jsonlite::fromJSON(paste0(root_url, params))
    return (js)
  }
  get_processed_data <- function (year) get_trans_data(year=year)$transaction_all$queryResults$row
  
  for (year in seq(2023, 2023, 2)) print(nrow(get_trans_data(year)$transaction_all$queryResults$row))
  
  transactions <- get_processed_data(2023)
  
  transactions <- transactions %>%
    mutate(
      trans_date = substr(trans_date, 1, 10),
      trans_date = as.Date(trans_date, "%Y-%m-%d")
    )

  ## Bring in PECOTA ##
  hit <- data
  colnames(hit)[1] <- 'bpid'
  
  ##BRING IN REF##
  abbr_df <- teamabbr
  
  ## Separate Into Free Agent Signings and Trades
  t_df <- transactions %>%
    filter(type_cd %in% c('SFA', 'TR', 'CLW')) %>%
    group_by(player_id) %>%
    mutate(
      last_trans = max(trans_date)
    ) %>%
    ungroup() %>%
    mutate(
      active_team = if_else(last_trans == trans_date, 1, 0),
      player_id = as.numeric(player_id)
    ) %>%
    filter(active_team == 1) %>%
    select(player_id, team_id, type_cd) %>%
    left_join(abbr_df, by = c("team_id" = "TeamID")) %>%
    select(player_id, team_id, type_cd, bp_teamabbr) %>%
    unique()

  
  ## Join Transactions to PECOTA ##
  hit <- hit %>%
    left_join(t_df, by = c("mlbid" = "player_id")) %>%
    mutate(
      team = case_when(
        !is.na(bp_teamabbr) & team != bp_teamabbr ~ bp_teamabbr,
        TRUE ~ team
        
      )
    ) %>%
    select(-c(team_id, type_cd, bp_teamabbr))
  
  return(hit)
}
#Trade_Adj(data = HIT_PECOTA_RAW)
#tst_transactions <- Trade_Adj(data = PITCH_PECOTA_RAW)
#tst_transactions %>% filter(player_id == "664202")

######################## PLOTTING AND DISTRIBUTION - BUILD GENERAL ENVIRONMENT TABLES #######################################

## Pivoting Wide Data into Long ##
## Wide Format TO Long For Positional WAR Percentiles ##
p_make_long_df <- function() {
  df = PITCH_PECOTA_FULL %>%
    filter(on_DC == 1) %>%
    select(bpid, name, team, starts_with("PLY_"), warp_01, warp_05, warp_10, warp_20, warp_30, warp_40, warp_50,
           warp_60, warp_70, warp_80, warp_90, warp_95, warp_99)
  
  IDs <- df$bpid
  Names <- df$name
  Teams <- df$team
  
  warp_cols <- select(df, starts_with("warp_"))
  ply_cols <- select(df, starts_with("PLY_"))
  
  
  warp_col_indices <- as.integer(substring(names(warp_cols), 6))
  ply_col_indices <- as.character(substring(names(ply_cols), 5))
  
  listylist <- rep(list(warp_col_indices), length(ply_col_indices))
  
  LEN <- length(ply_col_indices)
  
  # Multiply each "warp_xx" value by the corresponding "PLY_yy" value
  combine_df <- function() {
    listofdfs <- list()
    for (n in 1:LEN) {
      result <- sapply(listylist[[n]], function(i) {
        warp_col_name <- paste0("warp_", formatC(i, width = 2, flag = "0"))
        col_name <- paste0(warp_col_name, "_", substr(names(ply_cols)[n], 5, 6))
        data.frame(df[[warp_col_name]] * df[[names(ply_cols)[n]]])
      })
      listofdfs[[n]] <- result
    }
    #result_df <- bind_rows(listofdfs)
    return(listofdfs)
  }
  # Combine the result into a data frame with column names
  df_list <- combine_df()
  
  SP_DF <- data.frame(df_list[[1]])
  SP_DF$bpid <- IDs
  SP_DF$name <- Names
  SP_DF$team <- Teams
  
  RP_DF <- data.frame(df_list[[2]])
  RP_DF$bpid <- IDs
  
  SV_DF <- data.frame(df_list[[3]])
  SV_DF$bpid <- IDs
  
  combined_df <- SP_DF %>% inner_join(RP_DF, by = "bpid") %>% inner_join(SV_DF, by = "bpid") %>% select(14:16, 1:13, 17:42)
  names(combined_df) <- c("bpid", "name", "team",
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[1], 5, 6)),
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[2], 5, 6)),
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[3], 5, 6)))
  
  return(combined_df)
}
h_make_long_df <- function() {
  df = HIT_PECOTA_FULL %>%
    filter(on_DC == 1) %>%
    select(bpid, name, team, starts_with("PLY_"), warp_01, warp_05, warp_10, warp_20, warp_30, warp_40, warp_50,
           warp_60, warp_70, warp_80, warp_90, warp_95, warp_99)
  
  IDs <- df$bpid
  Names <- df$name
  Teams <- df$team
  
  warp_cols <- select(df, starts_with("warp_"))
  ply_cols <- select(df, starts_with("PLY_"))
  
  
  warp_col_indices <- as.integer(substring(names(warp_cols), 6))
  ply_col_indices <- as.character(substring(names(ply_cols), 5))
  
  listylist <- rep(list(warp_col_indices), length(ply_col_indices))
  
  LEN <- length(ply_col_indices)
  
  # Multiply each "warp_xx" value by the corresponding "PLY_yy" value
  combine_df <- function() {
    listofdfs <- list()
    for (n in 1:LEN) {
      result <- sapply(listylist[[n]], function(i) {
        warp_col_name <- paste0("warp_", formatC(i, width = 2, flag = "0"))
        col_name <- paste0(warp_col_name, "_", substr(names(ply_cols)[n], 5, 6))
        data.frame(df[[warp_col_name]] * df[[names(ply_cols)[n]]])
      })
      listofdfs[[n]] <- result
    }
    #result_df <- bind_rows(listofdfs)
    return(listofdfs)
  }
  # Combine the result into a data frame with column names
  df_list <- combine_df()
  
  C_DF <- data.frame(df_list[[1]])
  C_DF$bpid <- IDs
  C_DF$name <- Names
  C_DF$team <- Teams
  
  B1_DF <- data.frame(df_list[[2]])
  B1_DF$bpid <- IDs
  
  B2_DF <- data.frame(df_list[[3]])
  B2_DF$bpid <- IDs
  
  B3_DF <- data.frame(df_list[[4]])
  B3_DF$bpid <- IDs
  
  SS_DF <- data.frame(df_list[[5]])
  SS_DF$bpid <- IDs
  
  LF_DF <- data.frame(df_list[[6]])
  LF_DF$bpid <- IDs
  
  RF_DF <- data.frame(df_list[[7]])
  RF_DF$bpid <- IDs
  
  CF_DF <- data.frame(df_list[[8]])
  CF_DF$bpid <- IDs
  
  DH_DF <- data.frame(df_list[[9]])
  DH_DF$bpid <- IDs
  
  combined_df <- C_DF %>%
    inner_join(B1_DF, by = "bpid") %>%
    inner_join(B2_DF, by = "bpid") %>%
    inner_join(B3_DF, by = "bpid") %>%
    inner_join(SS_DF, by = "bpid") %>%
    inner_join(LF_DF, by = "bpid") %>%
    inner_join(RF_DF, by = "bpid") %>%
    inner_join(CF_DF, by = "bpid") %>%
    inner_join(DH_DF, by = "bpid") %>%
    select(14:16, 1:13, 17:120)
  names(combined_df) <- c("bpid", "name", "team",
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[1], 5, 5)),
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[2], 5, 6)),
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[3], 5, 6)),
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[4], 5, 6)),
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[5], 5, 6)),
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[6], 5, 6)),
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[7], 5, 6)),
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[8], 5, 6)),
                          paste0("warp_", formatC(warp_col_indices, width = 2, flag = "0"), "_", substr(names(ply_cols)[9], 5, 6))
  )
  
  return(combined_df)
}
full_team_ptile_long_build <- function() {
  P_WAR_Percentile_Pos_long <- P_WAR_Percentile_Pos %>%
    pivot_longer(
      cols = starts_with("warp_"),
      names_to = c("warp_name", 'warp_ptile', 'position'),
      names_sep = "_"
    ) %>% 
    select(bpid, name, team, position, warp_ptile,value) %>%
    group_by(team, position, warp_ptile) %>%
    summarise(
      total_warp_prod = sum(value),
      total_percentile_prod = prod(as.numeric(warp_ptile))
    )
  H_WAR_Percentile_Pos_long <- H_WAR_Percentile_Pos %>%
    pivot_longer(
      cols = starts_with("warp_"),
      names_to = c("warp_name", 'warp_ptile', 'position'),
      names_sep = "_"
    ) %>% 
    select(bpid, name, team, position, warp_ptile,value) %>%
    group_by(team, position, warp_ptile) %>%
    summarise(
      total_warp_prod = sum(value),
      total_percentile_prod = prod(as.numeric(warp_ptile))
    )
  full_df <- rbind(P_WAR_Percentile_Pos_long, H_WAR_Percentile_Pos_long)
  
  full_df <- full_df %>%
    mutate(
      macro_pos = case_when(
        position %in% c('1B', '2B', '3B', 'SS') ~ 'INF',
        position %in% c('C') ~ 'C',
        position %in% c('DH') ~ 'DH',
        position %in% c('RF', 'LF', 'CF') ~ 'OF',
        position %in% c('SP', 'RP', 'SV') ~ 'P'
      ),
      full_pos_name = paste0(macro_pos,' - ',position)
    )
  
  return(full_df)
}


## Functions For Distribution ##
distro_rep <- function(data, pos) {
  df1 = data %>%
    filter(position %in% pos) %>%
    mutate(
      warp_ptile_num = as.numeric(warp_ptile),
      ptile_n = if_else(warp_ptile_num <= 50, warp_ptile_num, 100-warp_ptile_num)
    )
  
  df3 <- df1[rep(seq_len(nrow(df1)), df1$ptile_n), 1:7]
  
  return(df3)
}
distro_rep_wp <- function(data, pos) {
  df1 = data %>%
    filter(position %in% pos) %>%
    mutate(
      warp_ptile_num = as.numeric(warp_ptile),
      ptile_n = if_else(warp_ptile_num <= 50, warp_ptile_num, 100-warp_ptile_num)
    )
  
  df3 <- df1[rep(seq_len(nrow(df1)), df1$ptile_n), 1:6]
  
  return(df3)
}
distro_rep_macro <- function(data, pos) {
  df1 = data %>%
    filter(macro_pos %in% pos) %>%
    mutate(
      warp_ptile_num = as.numeric(warp_ptile),
      ptile_n = if_else(warp_ptile_num <= 50, warp_ptile_num, 100-warp_ptile_num)
    )
  
  df3 <- df1[rep(seq_len(nrow(df1)), df1$ptile_n), 1:6]
  
  return(df3)
}
distro_rep_team <- function(data, pos) {
  df1 = data %>%
    mutate(
      warp_ptile_num = as.numeric(warp_ptile),
      ptile_n = if_else(warp_ptile_num <= 50, warp_ptile_num, 100-warp_ptile_num)
    )
  
  df3 <- df1[rep(seq_len(nrow(df1)), df1$ptile_n), 1:5]
  
  return(df3)
}
distro_rep_plyr <- function(data, pos) {
  df1 = data %>%
    mutate(
      warp_ptile_num = as.numeric(warp_ptile),
      ptile_n = if_else(warp_ptile_num <= 50, warp_ptile_num, 100-warp_ptile_num)
    )
  
  df3 <- df1[rep(seq_len(nrow(df1)), df1$ptile_n), 1:8]
  
  return(df3)
}




#################################################################################################
##############################SHINY APP FUNCTIONS################################################
#################################################################################################

### Build Daily Static Lineup Table ###
get_mlb_lu <- function(id, type) {
  
  pk <- id
  
  df <- get_batting_orders(pk, type = "starting") %>%
    mutate(
      teamID = as.character(teamID)
    )
  
  df$game_pk <- pk
  
  a_df <- df %>% 
    filter(team == 'away') %>%
    select(game_pk, teamID, id, fullName, abbreviation, batting_order) 
  names(a_df) <- c("game_pk", "away.TeamID", "away_bat_id", "away_bat_name", "away_pos", "away_batting_order")
  
  h_df <- df %>% 
    filter(team == 'home') %>%
    select(game_pk, teamID, id, fullName, abbreviation, batting_order)
  names(h_df) <- c("game_pk", "home.TeamID", "home_bat_id", "home_bat_name", "home_pos", "home_batting_order")
  
  full_df <- h_df %>%
    left_join(a_df, by = c('game_pk', 'home_batting_order' = 'away_batting_order')) %>%
    select(game_pk, home.TeamID, home_bat_id, home_bat_name, home_pos, home_batting_order,
           away.TeamID, away_bat_id, away_bat_name, away_pos)
  
  ## END BATTING ##
  
  ## Begin Pitching ###
  
  #Get Home Away IDs#
  a_id <- full_df %>% select(away.TeamID) %>% unique() %>% pull()
  h_id <- full_df %>% select(home.TeamID) %>% unique() %>% pull()
  
  
  p_df <- get_probables_mlb(pk) %>%
    mutate(team_id = as.character(team_id)) %>%
    select(game_pk, team_id, id, fullName)
  names(p_df) <- c("game_pk", "TeamID", "p_id", "p_name")
  
  a_p_df <- p_df %>% filter(TeamID == a_id)
  names(a_p_df) <- c("game_pk", "away.TeamID", "away_p_id", "away_p_name")
  h_p_df <- p_df %>% filter(TeamID == h_id)
  names(h_p_df) <- c("game_pk", "home.TeamID", "home_p_id", "home_p_name")
  
  p_full_df <- a_p_df %>%
    left_join(h_p_df, by = c('game_pk')) %>%
    select(game_pk, home.TeamID, home_p_id, home_p_name, away.TeamID, away_p_id, away_p_name)
  
  var <- paste0(type)
  
  if(var == "P"){
    return(p_full_df)
  } else if (var == "BAT") {
    return(full_df)
  }
  
}
build_logos <- function() {
  mlbplot_col <- load_mlb_teams()[1:30,] %>%
    mutate(TeamID = as.character(team_id_num))
    
  
  mlbplot_col$header_logo = c(
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/AZ-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/ATL-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/BAL-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/BOS-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/CHC-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/CWS-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/CIN-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/CLE-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/COL-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/DET-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/HOU-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/KC-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/LAA-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/LAD-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/MIA-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/MIL-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/MIN-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/NYM-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/NYY-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/OAK-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/PHI-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/PIT-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/SD-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/SF-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/SEA-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/STL-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/TB-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/TEX-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/TOR-headerlogo.png',
  'https://raw.github.com/twinfield10/Rebirtha-Model/main/Images/WSH-headerlogo.png'
  )
  
  mlbplot_col$header_logo_bg = c(
    '#B3B3B3',
    '#BA0C2F',
    '#A2AAAD',
    '#0C2244',
    '#929F9F',
    '#0C2340',
    '#919D9D',
    '#0C2340',
    '#330072',
    '#0C2244',
    '#E24912',
    '#FFFFFF',
    '#BC0E2C',
    '#FFFFFF',
    '#000000',
    '#FFF5EA',
    '#FFFFFF',
    '#002C74',
    '#A5ACB0',
    '#006544',
    '#FFFAEC',
    '#000000',
    '#3E342F',
    '#FF4819',
    '#FFFAEC',
    '#FFFFFF',
    '#69B3E7',
    '#6CACE4',
    '#003BB2',
    '#071A45'
  )
  return(mlbplot_col)
}

## COLOR SCALES ##
p_tWAR_Col_Func <- function(x) {
  ifelse(as.numeric(x) < 5, "#ff6666",
         ifelse(as.numeric(x) < 7.5 & as.numeric(x) >= 5, "#ffcccc",
                ifelse(as.numeric(x) < 9 & as.numeric(x) >= 7.5, "#ffffb3",
                       ifelse(as.numeric(x) < 11.5 & as.numeric(x) >= 9, "#98e6bb",
                              ifelse(as.numeric(x) < 13.5 & as.numeric(x) >= 11.5, "#71CA97",
                                    ifelse(as.numeric(x) >= 13.5, "#00994d", NA))))))
}
Wins_Col_Func <- function(x) {
  ifelse(as.numeric(x) < 55, "#FF0000",
         ifelse(as.numeric(x) < 65 & as.numeric(x) >= 55, "#FF6868",
                ifelse(as.numeric(x) < 75 & as.numeric(x) >= 65, "#FC9D9D",
                       ifelse(as.numeric(x) < 81 & as.numeric(x) >= 75, "#FFEEEE",
                              ifelse(as.numeric(x) < 87.5 & as.numeric(x) >= 81, "#D7FAE7",
                                     ifelse(as.numeric(x) < 92.5 & as.numeric(x) >= 87.5, "#98e6bb",
                                            ifelse(as.numeric(x) < 100 & as.numeric(x) >= 92.5, "#71CA97",
                                                   ifelse(as.numeric(x) >= 100, "#00994d", NA))))))))
}
WinPct_Col_Func <- function(x) {
  ifelse(as.numeric(x) < (55/162), "#FF0000",
         ifelse(as.numeric(x) < (65/162) & as.numeric(x) >= (55/162), "#FF6868",
                ifelse(as.numeric(x) < (75/162) & as.numeric(x) >= (65/162), "#FC9D9D",
                       ifelse(as.numeric(x) < (81/162) & as.numeric(x) >= (75/162), "#FFEEEE",
                              ifelse(as.numeric(x) < (87.5/162) & as.numeric(x) >= (81/162), "#D7FAE7",
                                     ifelse(as.numeric(x) < (92.5/162) & as.numeric(x) >= (87.5/162), "#98e6bb",
                                            ifelse(as.numeric(x) < (100/162) & as.numeric(x) >= (92.5/162), "#71CA97",
                                                   ifelse(as.numeric(x) >= (100/162), "#00994d", NA))))))))
}
Delta_Wins_Col_Func <- function(x) {
  ifelse(as.numeric(x) < -15, "#FF0000",
         ifelse(as.numeric(x) < -10 & as.numeric(x) >= -15, "#FF6868",
                ifelse(as.numeric(x) < -5 & as.numeric(x) >= -10, "#FC9D9D",
                      ifelse(as.numeric(x) < 0 & as.numeric(x) >= -5, "#FFEEEE",
                             ifelse(as.numeric(x) < 5 & as.numeric(x) >= 0, "#D7FAE7",
                                    ifelse(as.numeric(x) < 10 & as.numeric(x) >= 5, "#98e6bb",
                                           ifelse(as.numeric(x) < 15 & as.numeric(x) >= 10, "#71CA97",
                                                  ifelse(as.numeric(x) >= 15, "#00994d", NA))))))))
}
Delta_Wins_Txt_Func <- function(x) {
  ifelse(as.numeric(x) < -15, "#FFFFFF",
         ifelse(as.numeric(x) < -10 & as.numeric(x) >= -15, "#FFFFFF",
                ifelse(as.numeric(x) < -5 & as.numeric(x) >= -10, "#000000",
                       ifelse(as.numeric(x) < 0 & as.numeric(x) >= -5, "#000000",
                              ifelse(as.numeric(x) < 5 & as.numeric(x) >= 0, "#000000",
                                     ifelse(as.numeric(x) < 10 & as.numeric(x) >= 5, "#000000",
                                            ifelse(as.numeric(x) < 15 & as.numeric(x) >= 10, "#000000",
                                                   ifelse(as.numeric(x) >= 15, "#FFFFFF", NA))))))))
}
RS_Col_Func <- function(x) {
  ifelse(x < -100, "#ff0000",
         ifelse(x < -90 & x >= -100, "#ff1a1a",
                ifelse(x < -80 & x >= -90, "#ff3333",
                       ifelse(x < -70 & x >= -80, "#ff4d4d",
                              ifelse(x < -60 & x >= -70, "#ff6666",
                                     ifelse(x < -50 & x >= -60, "#ff8080",
                                            ifelse(x < -40 & x >= -50, "#ff9999",
                                                   ifelse(x < -30 & x >= -40, "#ffb3b3",
                                                          ifelse(x < -20 & x >= -30, "#ffcccc",
                                                                 ifelse(x < -10 & x >= -20, "#ffe6e6",
                                                                        ifelse(x >= -10 & x <= 10, "#ffffff",
                                                                               ifelse(x < 20 & x > 10, "#eafaf1",
                                                                                      ifelse(x < 30 & x >= 20, "#98E6BB",
                                                                                             ifelse(x < 40 & x >= 30, "#c1f0d6",
                                                                                                    ifelse(x < 50 & x >= 40, "#98e6bb",
                                                                                                           ifelse(x < 60 & x >= 50, "#6fdca0",
                                                                                                                  ifelse(x < 70 & x >= 60, "#46d285",
                                                                                                                         ifelse(x < 80 & x >= 70, "#2db96c",
                                                                                                                                ifelse(x < 90 & x >= 80, "#28a460",
                                                                                                                                       ifelse(x < 100 & x >= 90, "#239054",
                                                                                                                                              ifelse(x >= 100, "#1e7b48", NA)))))))))))))))))))))
}
RA_Col_Func <- function(x) {
  ifelse(x < -100, "#1e7b48",
         ifelse(x < -90 & x >= -100, "#239054",
                ifelse(x < -80 & x >= -90,"#28a460",
                       ifelse(x < -70 & x >= -80, "#2db96c",
                              ifelse(x < -60 & x >= -70, "#2db96c",
                                     ifelse(x < -50 & x >= -60, "#6fdca0",
                                            ifelse(x < -40 & x >= -50, "#98e6bb",
                                                   ifelse(x < -30 & x >= -40, "#c1f0d6",
                                                          ifelse(x < -20 & x >= -30, "#98E6BB",
                                                                 ifelse(x < -10 & x >= -20, "#eafaf1",
                                                                        ifelse(x >= -10 & x <= 10, "#ffffff",
                                                                               ifelse(x < 20 & x > 10, "#ffe6e6",
                                                                                      ifelse(x < 30 & x >= 20,  "#ffcccc",
                                                                                             ifelse(x < 40 & x >= 30, "#ffb3b3",
                                                                                                    ifelse(x < 50 & x >= 40, "#ff9999",
                                                                                                           ifelse(x < 60 & x >= 50, "#ff8080",
                                                                                                                  ifelse(x < 70 & x >= 60, "#ff6666",
                                                                                                                         ifelse(x < 80 & x >= 70,  "#ff4d4d",
                                                                                                                                ifelse(x < 90 & x >= 80,  "#ff3333",
                                                                                                                                       ifelse(x < 100 & x >= 90,"#ff1a1a",
                                                                                                                                              ifelse(x >= 100, "#ff0000", NA)))))))))))))))))))))
}
Adv_Scale <- function(x) {
  ifelse(x >= .15, "#1e7b48",
          ifelse(x >= .10 & x < .15, "#28a460",
                  ifelse(x >= .07 & x < .10, "#32cd78",
                          ifelse(x >= .04 & x < .07, "#6fdca0",
                                  ifelse(x >= .02 & x < .04, "#FAFFA1",
                                          ifelse(x >= 0 & x < .02, "#FAFFA1",
                                                 ifelse(x >= -0.0250 & x < 0, "#ff9999",
                                                        ifelse(x < -0.025, "#ff0000", NA))))))))
}


## Plotting Functions ##
build_cols <- function() {
  mlb_cols <- data.frame(
    prim_col = league_pal("mlb", 1),
    sec_col = league_pal("mlb", 2),
    stringsAsFactors = FALSE
  )
  mlb_cols$team_full <- row.names(mlb_cols)
  mlb_cols <- mlb_cols %>% select(team_full, prim_col, sec_col)
  team_abbr <- unique(H_WAR_Percentile_Pos_long$team)
  mlb_cols$team <- team_abbr
  mlb_cols <- mlb_cols %>%
    mutate(
      team = case_when(
        team_full == 'San Francisco Giants' ~ 'SF',
        team_full == 'Seattle Mariners' ~ 'SEA',
        TRUE ~ team
      ),
      prim_col = case_when(
        team == 'SD' ~ '#2f241d',
        team == 'CHC' ~ '#0e3386',
        team == 'CLE' ~ '#e50022',
        team == 'MIA' ~ '#00a3e0',
        team == 'MIL' ~ '#12284b',
        team == 'NYM' ~ '#002d72',
        team == 'NYY' ~ '#0c2340',
        team == 'PHI' ~ '#002d72',
        TRUE ~ prim_col
      ),
      sec_col = case_when(
        team == 'SD' ~ '#ffc425',
        team == 'CHC' ~ '#cc3433',
        team == 'CLE' ~ '#00385D',
        team == 'COL' ~ '#c4ced4',
        team == 'DET' ~ '#fa4616',
        team == 'KC'  ~ '#bd9b60',
        team == 'MIA' ~ '#ef3340',
        team == 'MIL' ~ '#ffc52f',
        team == 'NYM' ~ '#ff5910',
        team == 'NYY' ~ '#c4ced3',
        team == 'PHI' ~ '#e81828',
        team == 'PIT' ~ '#27251f',
        team == 'STL' ~ '#0c2340',
        team == 'SF'  ~ '#27251f',
        team == 'WAS' ~ '#14225a',
        TRUE ~ sec_col
      )
    ) %>%
    select(4,1,2,3)
  
  return(mlb_cols)
}


#####################################################
########### APP FUNCTIONS - PRETTY TABLES ###########
#####################################################

## General Information Table Build
gen_info_build <- function(dta = Sys.Date()-1){
  ## Begin DF Build ##
  df <- teamabbr %>%
    select(1,3,6,8,9)
  
  ## Add Current Standings ##  
  dte = Sys.Date()
  al_stands <- suppressMessages(standings_on_date_bref(date = paste(dte), division = "AL Overall"))
  nl_stands <- suppressMessages(standings_on_date_bref(date = paste(dte), division = "NL Overall"))
  full_stands <- rbind(al_stands[[1]], nl_stands[[1]]) %>%
    mutate(
      Tm = case_when(
        Tm == "KCR" ~ "KC",
        Tm == 'WSN' ~ "WAS",
        Tm == 'SDP' ~ 'SD',
        Tm == 'SFG' ~ 'SF',
        Tm == 'TBR' ~ 'TB',
        TRUE ~ Tm
      )
    ) %>%
    rename(WP = `W-L%`) %>%
    select(-8)
  names(full_stands) <- c("Tm", "Actual_W", "Actual_L", "Actual_WP", "Div_GB", "Actual RS", "Actual_RA")
  
  df <- df %>%
    left_join(full_stands, by = c('bp_teamabbr' = 'Tm'))
  
  ## Add PECOTA Standings ##
  
  PECOTA_Stands <- PECOTA_FILE_DL()
  #PECOTA_Stands <- CL_PECOTA_FILE_ADJ(data = standings)
  names(PECOTA_Stands) <- c("Team", "PECOTA_W", "PECOTA_L", "PECOTA_WP", "PECOTA_RS", "PECOTA_RA", "PECOTA_Div_Pct", "PECOTA_WC_Pct", "PECOTA_Playoff_Pct", "PECOTA_DiVSeries_Pct", "PECOTA_WS_Pct", "Delta_1", "Delta_7")
  
  df <- df %>%
    left_join(PECOTA_Stands, by = c("bp_teamabbr" = "Team"))
  
  ## Add Logos And Colors ##
  logos <- build_logos() %>%
    select(TeamID, team_logo_espn, header_logo, header_logo_bg)
  
  colors <- build_cols() %>%
    select(team, prim_col, sec_col)
  
  df <- df %>%
    left_join(logos, by = c("TeamID")) %>%
    left_join(colors, by = c("bp_teamabbr" = "team"))
  
  return(df)
}

## Load 2022 Stats
load_2022_stats <- function(dte){
  path <- 'https://raw.githubusercontent.com/twinfield10/Rebirtha-Model/main/Data/bp_export_'
  dt = dte
  
  df <- read.csv(paste0(path, dte,'.csv'))
  colnames(df)[1] <- 'bpid'
  colnames(df)[7] <- 'DRA_Minus'
  colnames(df)[22:25] <- c("K_Rate", "BB_Rate", "K_BB_Ratio", "Whiff_Rt")
  
  
  return(df)
}

## Load 2023 Stats
load_2023_stats <- function() {
}

#Scrape Games on Today's Schedule
today_build <- function(loc){
  df <- mlb_api_pitchers %>% filter(!is.na(home_p_name) & !is.na(away_p_name))
  df <- df %>%
    mutate(
      TimeStmp = format(Sys.time(), format='%m-%d-%Y %I:%M %p')
    )
  ## Load Plotting Table FOr Teams And Headshots ##
  RefTbl <- APP_RefTbl %>%
    select(full_name, TeamID, Actual_W, Actual_L, Actual_WP,
           PECOTA_W, PECOTA_L, PECOTA_WP, PECOTA_Playoff_Pct, PECOTA_WS_Pct, Delta_7,
           team_logo_espn, prim_col, sec_col)
  
  headshots <- load_headshots() %>% select(savant_id, espn_headshot) %>%
    mutate(
      espn_headshot = case_when(
        is.na(espn_headshot) ~ 'https://a.espncdn.com/combiner/i?img=/i/teamlogos/leagues/500/mlb.png',
        TRUE ~ espn_headshot
      )
    )
  
  g_n_check <-daily_schedule(paste(Sys.Date())) %>% select(game_pk, gameNumber)
  
  h_info <- df %>%
    select(game_pk, home.TeamID, home_p_id, home_p_name)
  names(h_info) <- c("game_pk", "TeamID", "p_id", "p_name")
  h_info <- h_info %>%
    left_join(RefTbl, by = c("TeamID")) %>%
    mutate(
      Actual_Label = paste0("Current Record: ", Actual_W, "-", Actual_L, " (", Actual_WP,")"),
      PECOTA_Label = paste0("Projected Record: ", PECOTA_W, "-", PECOTA_L, " (", PECOTA_WP,")"),
      playoff_change_format = if_else(Delta_7 > 0, paste0(abs(Delta_7), '% increase'), if_else(Delta_7 < 0, paste0(abs(Delta_7), '% decrease'), "no change")),
      PECOTA_Playoff_Proj_Label = paste0("Probability of Making Playoffs: ", PECOTA_Playoff_Pct, "% (", playoff_change_format, " in last 7 days)")
    ) %>%
    left_join(headshots, by = c("p_id" = "savant_id")) %>%
    left_join(teamabbr %>% select(TeamID, bp_teamabbr), by = c("TeamID")) %>%
    left_join(g_n_check, by = c("game_pk")) %>%
    left_join(daily_odds, by = c('bp_teamabbr' = 'home_team', 'gameNumber'))
  
  
  a_info <- df %>%
    select(game_pk, away.TeamID, away_p_id, away_p_name)
  names(a_info) <- c("game_pk", "TeamID", "p_id", "p_name")
  a_info <- a_info %>%
    left_join(RefTbl, by = c("TeamID")) %>%
    mutate(
      Actual_Label = paste0("Current Record: ", Actual_W, "-", Actual_L, " (", Actual_WP,")"),
      PECOTA_Label = paste0("Projected Record: ", PECOTA_W, "-", PECOTA_L, " (", PECOTA_WP,")"),
      playoff_change_format = if_else(Delta_7 > 0, paste0(abs(Delta_7), '% increase'), if_else(Delta_7 < 0, paste0(abs(Delta_7), '% decrease'), "no change")),
      PECOTA_Playoff_Proj_Label = paste0("Probability of Making Playoffs: ", PECOTA_Playoff_Pct, "% (", playoff_change_format, " in last 7 days)")
    ) %>%
    left_join(headshots, by = c("p_id" = "savant_id")) %>%
    left_join(teamabbr %>% select(TeamID, bp_teamabbr), by = c("TeamID")) %>%
    left_join(g_n_check, by = c("game_pk")) %>%
    left_join(daily_odds, by = c('bp_teamabbr' = 'away_team', 'gameNumber'))
  
  ST_SplitSquad_PKs <- get_game_pks_mlb(date = Sys.Date()) %>%
    filter(teams.away.splitSquad == FALSE & teams.home.splitSquad == FALSE) %>%
    select(game_pk) %>%
    pull()
  
  full_df <- h_info %>% select(game_pk, bp_teamabbr) %>% rename(home_tm = bp_teamabbr) %>%
    left_join(a_info %>% select(game_pk, bp_teamabbr), by = c("game_pk")) %>% rename(away_tm = bp_teamabbr) %>%
    left_join(g_n_check, by = c("game_pk")) %>%
    group_by(game_pk, home_tm, away_tm, gameNumber) %>%
    summarise(
      gms = n()
    ) %>%
    ungroup() %>%
    mutate(
      label = if_else(gms < 2, paste0(away_tm, " @ ", home_tm), paste0(away_tm, " @ ", home_tm, " (G", gameNumber, ")"))
    ) %>%
    select(game_pk, label) %>%
    filter(game_pk %in% ST_SplitSquad_PKs)
  
  
  
  var = loc
  
  if(var == "H"){
    return(h_info)
  } else if (var == "A") {
    return(a_info)
  } else if (var == "FULL") {
    return(full_df)
  }
  
}

## Scrape BOL Lines
Pull_BOL_Object <- function(){
  BOL_counter <<- BOL_counter + 1 # increment the counter by 1
  
  book_url <- 'https://classic.betonline.ag/sportsbook/baseball/exhibition'
  xml_obj <- read_html(book_url) %>%
    html_node("body") %>%
    html_node("div#appbody") %>%
    html_node("div#bd.clearfix") %>%
    html_node("div#center") %>%
    html_node("div.contentWrapper") %>%
    html_node("div#contentArea") %>%
    html_node("div#contentBody.unauthorized") %>%
    html_node("table#contestDetailTable.league")
  pull_time <- Sys.time()
  pull <- list(xml_obj, pull_time, BOL_counter)
  
  BOL_XML_Obj_List[[BOL_counter]] <<- pull
  
  return(pull)
}
Get_Long_BOL <- function(dt) {
  #Format Like: Wednesday, Mar 08, 2023
  
  today <- format(dt, "%A, %b %d, %Y")
  tomorrow <- format(as.Date(dt)+1, "%A, %b %d, %Y")
  
  length_BOL_XML <- length(BOL_XML_Obj_List)
  
  long_df_list <- list()
  for(i in 1:length_BOL_XML) {
    
    raw_df <- BOL_XML_Obj_List[[i]][[1]] %>%
      html_table() %>%
      data.frame(stringsAsFactors = FALSE)
    
    #print(raw_df)
    
    ## Filter All Dates Not On Today ##
    filt_val_today <- paste0(today, ' - Exhibition Baseball Game')
    filt_val <- paste0(tomorrow,' - Exhibition Baseball Game')
    
    check_date_vec <- raw_df %>% filter(Var.1 %in% c(filt_val, filt_val_today)) %>% select(1) %>% pull()
    len_check <- length(check_date_vec)
    
    
    #print(len_check)
    
    ## Build If Statement ##
    filt_raw_df <-   if(len_check == 1) {
      df <- raw_df %>%
        mutate(dt_match = if_else(Team == filt_val, 1, 0)) %>%
        filter(Team != '') %>%
        select(1,2,3,10)
      names(df) <- c('Time_EST', 'GM_ID', 'Team', 'Moneyline')
      df
    } else if(len_check > 1) {
      
      df <- raw_df %>%
        mutate(dt_match = if_else(Team == filt_val, 1, 0)) %>%
        filter(row_number() < match(max(dt_match), dt_match)) %>%
        filter(Team != '') %>%
        select(1,2,3,10)
      names(df) <- c('Time_EST', 'GM_ID', 'Team', 'Moneyline')
      df
    }
    
    #print(filt_raw_df)
    
    #Remove Unnecessary Rows
    Day_Full <- filt_raw_df[1,1]
    final_df <- filt_raw_df[-1,]
    
    #Clean Up Moneyline column
    final_df$Moneyline <- as.numeric(gsub("[+]", "", final_df$Moneyline ))
    
    #print(final_df)
    
    #Build Home/Away Column
    gm_len <- 1:length(final_df$GM_ID)
    final_df$len <- gm_len
    final_df <- final_df %>%
      mutate(
        site = if_else((len %% 2) == 0, 'Home', 'Away')
      ) %>%
      select(-5)
    
    
    
    ## Build a Quasi Game ID For Pivot ##
    len_gs <- 1:(length(final_df$GM_ID)/2)
    n <- 2
    final_df$f_gid_vec <- rep(len_gs, each = n)
    
    
    #Add Date Column
    final_df$date <- as.character(dt)
    
    long_final_df <- final_df
    
    #Join Team Abbr For Join To Bets#
    final_df <- final_df %>%
      left_join(teamabbr, by = c('Team' = 'full_name')) %>%
      select(1:7, TeamID) %>%
      pivot_wider(names_from = site,
                  names_sep = "_",
                  values_from = c(Team, GM_ID, Moneyline, TeamID)) %>%
      mutate(combination = paste(TeamID_Home, TeamID_Away, sep = "-")) %>%
      group_by(combination) %>%
      mutate(count = n()) %>%
      ungroup() %>%
      select(-combination)
    
    
    
    long_final_df <- long_final_df %>%
      left_join(final_df %>% select(GM_ID_Home, count, TeamID_Home), by = c('GM_ID' = 'GM_ID_Home')) %>%
      left_join(final_df %>% select(GM_ID_Away, count, TeamID_Away), by = c('GM_ID' = 'GM_ID_Away')) %>%
      mutate(
        gameNumber = coalesce(count.x, count.y),
        TeamID = coalesce(TeamID_Home, TeamID_Away)
      ) %>%
      left_join(teamabbr %>% select(TeamID, bp_teamabbr), by = c('TeamID')) %>%
      rename(team = bp_teamabbr) %>%
      select(-c(count.x,count.y, TeamID_Home, TeamID_Away))
    
    TS <- BOL_XML_Obj_List[[i]][[2]]
    TS <- format(TS, format='%m-%d-%Y %I:%M %p')
    long_final_df$BetTimeStamp <- TS
    
    long_final_df$Pull_ID <- BOL_XML_Obj_List[[i]][[3]]
    
    long_df_list[[i]] <- long_final_df
  }
  df <- bind_rows(long_df_list) %>%
    distinct(Time_EST, GM_ID, Team, Moneyline, site, date, gameNumber, TeamID, team, .keep_all = TRUE) %>%
    ungroup()
  
  return(df)
}


## Scrape Lineups And Pitchers From MLB API
mlb_api_todays_lineup <- function(date = paste-(sys.Date()), type) {
  sched <- daily_schedule(date) %>%
    mutate(
      teams.home.team.id = as.character(teams.home.team.id),
      teams.away.team.id = as.character(teams.away.team.id)
    ) %>%
    left_join(teamabbr, by = c('teams.home.team.id' = 'TeamID')) %>%
    left_join(teamabbr, by = c('teams.away.team.id' = 'TeamID')) %>%
    select(game_pk, gameNumber, full_name.x, full_name.y)
  names(sched) = c('game_pk', 'gameNumber', 'home_full_name', 'away_full_name')
  
  ## Create Empty Lists for Lineup DFs and Bad IDs
  listofdfs <- list()
  bad_ids <- list()
  
  ## Define Function inside loop ##
  for(i in 1:nrow(sched)) {
    gid <- sched[i,1]
    gnumb <- sched[i,2]
    h_tm <- sched[i,3]
    a_tm <- sched[i,4]
    cat <- type
    
    result <- tryCatch({
      # Function to run in the loop
      g <- get_mlb_lu(id = gid, type = cat)
      g
      
    }, error = function(e) {
      # Custom error message to return if an error occurs
      message(paste0('ERROR - No Lineups: GameID = ',gid,' (',a_tm,'@ ',h_tm,')'))
      bad_ids[[length(bad_ids) + 1]] <- gid
    })
    
    # Check if an error occurred and print the appropriate message
    if (inherits(result, "error")) {
      print(result)
    } else {
      listofdfs[[length(listofdfs) + 1]] <- result
      df_indices <- sapply(listofdfs, is.data.frame)
      df_list <- listofdfs[df_indices]
    }
  }
  Final_Lineup <- bind_rows(df_list)
  
  return(Final_Lineup)
}


## Lineup Table Build
gt_lu_output_tbl_build <- function(id, ctype = "P") {
  
  pk <- id
  ## Bring in Lineup Tables ##
  lu_full <- mlb_api_batters %>% filter(game_pk == pk)
  p_full <- mlb_api_pitchers %>% filter(game_pk == pk)
  
  ## Set General Variables ##
  h_tm_id <- p_full %>% select(home.TeamID) %>% pull() %>% as.character()
  a_tm_id <- p_full %>% select(away.TeamID) %>% pull() %>% as.character()
  h_tm_abbr <- teamabbr %>% filter(TeamID == h_tm_id) %>% select(bp_teamabbr) %>% pull() %>% as.character()
  a_tm_abbr <- teamabbr %>% filter(TeamID == a_tm_id) %>% select(bp_teamabbr) %>% pull() %>% as.character()
  
  ## Build Clean Home and Away Tables ##
  home_tbl <- lu_full %>%
    select(game_pk, home.TeamID, home_bat_id, home_bat_name, home_pos, home_batting_order) %>%
    mutate(loc = 'H')
  names(home_tbl) <- c("game_pk", "TeamID", "mlbid", "name", "position", "bat_order", "loc")
  
  h_p <- p_full %>% select(game_pk, home.TeamID, home_p_id, home_p_name) %>%
    mutate(
      position = 'SP',
      bat_order = 0,
      loc = 'H',
    )
  names(h_p) <- c("game_pk", "TeamID", "mlbid", "name", "position", "bat_order", "loc")
  
  away_tbl <- lu_full %>%
    select(game_pk, away.TeamID, away_bat_id, away_bat_name, away_pos, home_batting_order) %>%
    mutate(loc = 'A')
  names(away_tbl) <- c("game_pk", "TeamID", "mlbid", "name", "position", "bat_order", "loc")
  
  a_p <- p_full %>% select(game_pk, away.TeamID, away_p_id, away_p_name) %>%
    mutate(
      position = 'SP',
      bat_order = 0,
      loc = 'A',
    )
  names(a_p) <- c("game_pk", "TeamID", "mlbid", "name", "position", "bat_order", "loc")
  
  ## Bring In PECOTA ##
  hit_pecota <- HIT_PECOTA_FULL %>% select(mlbid, g, pa, warp, full_name)
  pitch_pecota <- PITCH_PECOTA_FULL %>% select(mlbid, throws, g, gs, ip, era, fip, dra, dra_minus, warp, SP_PCT, RP_PCT, SV_PCT,starts_with('PLY_'), on_DC, full_name, IP_GS)
  
  ## Bring in Team War Totals And Mutate ##
  tm_war <- war_ref_team %>% filter(TeamID %in% c(h_tm_id, a_tm_id)) %>%
    select(-c(total_war, tot_bat_war, tot_p_war, tot_SP_war, tot_RP_war)) %>%
    pivot_longer(
      cols = warp_c_50:warp_SV_50,
      names_to = c("position"),
      names_pattern = "warp_?(.*)_50",
      values_to = 'tm_warp'
    ) %>%
    mutate(
      position = case_when(
        position == 'c' ~ 'C',
        TRUE ~ position
      ),
      tm_warp = round(tm_warp, digits = 2)
    )
  
  ## Join PECOTA to Clean Lineup tables ##
  lu_full_tbl <- rbind(home_tbl, away_tbl)
  lu_full_tbl <- lu_full_tbl %>%
    left_join(hit_pecota, by = c('mlbid')) %>%
    mutate(
      if162_warp = round((warp/g)*162, digits=2)
    ) %>%
    left_join(tm_war, by = c('TeamID', 'position')) %>%
    mutate(
      RS_Adj = (if162_warp - tm_warp) * 10,
      NamePosOrder = paste0(name, ' - ', position)
    )
  
  h_lu_tbl <- lu_full_tbl %>% filter(loc == 'H') %>% select(RS_Adj, tm_warp, if162_warp, NamePosOrder, TeamID)
  a_lu_tbl <- lu_full_tbl %>% filter(loc == 'A') %>% select(TeamID, NamePosOrder, if162_warp, tm_warp, RS_Adj)
  
  
  ## Join PECOTA TO Pitchers ##
  p_full_tbl <- rbind(h_p, a_p)
  p_full_tbl <- p_full_tbl %>%
    left_join(pitch_pecota, by = c('mlbid')) %>%
    mutate(
      IP_GS = if_else(on_DC == 0, 4, if_else(IP_GS == 0, 4, IP_GS)),
      if162_warp = round(((warp*IP_GS)/ip)*162, digits = 2)
    ) %>%
    left_join(tm_war, by = c('TeamID', 'position')) %>%
    mutate(
      RA_Adj = (if162_warp - tm_warp) * 10,
      PLY_SP = round(PLY_SP*100, digits = 0),
      PLY_RP = round(PLY_RP*100, digits = 0),
      PLY_SV = round(PLY_SV*100, digits = 0),
      Pos_Full = case_when(
        PLY_SP == 100 ~ paste0('Full-Time Starter'),
        PLY_RP == 100 ~ 'Full-Time Reliever',
        PLY_SV == 100 ~ 'Full-Time Closer',
        PLY_SP > 0 & PLY_RP > 0 & PLY_SV == 0 ~ 'Starter/Long Reliever',
        PLY_SP == 0 & PLY_RP > 0 & PLY_SV > 0 ~ 'Set-Up/Closer',
        on_DC == 0 ~ 'Minor League Pitcher'
      ),
      Pos_Sub = case_when(
        PLY_SP == 100 ~ paste0(SP_PCT,'% of Starter IP'),
        PLY_RP == 100 ~ paste0(RP_PCT,'% of Relief IP'),
        PLY_SV == 100 ~ paste0(SV_PCT,'% of Closing IP'),
        PLY_SP > 0 & PLY_RP > 0 & PLY_SV == 0 ~ paste0('Starter: ', PLY_SP,'% | Reliever: ', PLY_RP,'%'),
        PLY_SP == 0 & PLY_RP > 0 & PLY_SV > 0 ~ paste0('Middle Relief/Set-Up: ', PLY_RP,'% | Closer: ', PLY_SV,'%'),
        on_DC == 0 ~ 'Replacement Player'
      )
    )
  
  #Pitchers Table Format - Build ##
  p_format_tbl  <- p_full_tbl %>%
    mutate(
      dra_stat = paste0(dra, ' (',dra_minus,')'),
      throws = if_else(throws == 'R', 'Right', if_else(throws == 'L', 'Left', 'Switch')),
      war_diff = as.numeric(if162_warp - tm_warp),
      war_diff_adj = if_else(war_diff > 0, paste0(abs(war_diff), ' MORE'), if_else(war_diff < 0, paste0(abs(war_diff), ' LESS'), 'the same number of')),
      war_dif_print = paste0(name, ' is worth ', war_diff_adj, ' wins compared to the ', full_name,"'s Starting Rotation")
    ) %>%
    select(loc, full_name, name, Pos_Full, Pos_Sub, throws, era, fip, dra_stat, warp, if162_warp, tm_warp, RA_Adj)
  names(p_format_tbl) <- c("loc", "Team Name", "Pitcher Name", "Pitcher Role", "Role Details", "Throws", "PECOTA ERA", "PECOTA FIP", "PECOTA DRA (DRA-)", "WAR - Standard Usage", "WAR - Pitch 162", "Team SP WAR", "Runs Against - Plus Minus")
  p_format_tbl <- p_format_tbl[-1] %>% t() %>% data.frame(stringsAsFactors = FALSE) %>% setNames(p_format_tbl[,1]) %>% rownames_to_column("Category") %>%
    select(A, Category, H)
  names(p_format_tbl) <- c('Away Pitcher', 'Category', 'Home Pitcher')
  ############### END TABLE MANIPULATION ##############################  
  #### BUILD HITTERS TABLE #####
  #Load Colors
  a_clr <- col_scheme %>% filter(team == a_tm_abbr)
  h_clr <- col_scheme %>% filter(team == h_tm_abbr)
  
  a_prim <- a_clr[1,3]
  h_prim <- h_clr[1,3]
  a_sec <- a_clr[1,4]
  h_sec <- h_clr[1,4]
  
  customGreen0 = "#DeF7E9"
  customGreen = "#71CA97"
  customRed0 = "#ffe6e6"
  customRed = "#ff000"
  
  
  ############## BUILD GT #################################
  ### Clean Names ##
  names_vec <- c("TeamID", "Player Name", "Player", "Pos Avg", "RS +/-")
  names(h_lu_tbl) <- rev(names_vec)
  names(a_lu_tbl) <- names_vec
  
  mlbplot_col <- load_mlb_teams() %>%
    mutate(TeamID = as.character(team_id_num))
  
  
  ## Create WAR Color Function ##
  WAR_Col_Func <- function(x) {
    ifelse(x < 0, "#ff6666",
           ifelse(x < 1 & x >= 0, "#ffcccc",
                  ifelse(x < 2 & x >= 1, "#ffffb3",
                         ifelse(x < 3 & x >= 2, "#DeF7E9",
                                ifelse(x < 4 & x >= 3, "#98e6bb",
                                       ifelse(x >= 4, "#37B96F", NA))))))
  }
  
  tWAR_Col_Func <- function(x) {
    ifelse(x < 10, "#ff6666",
           ifelse(x < 14 & x >= 10, "#ffcccc",
                  ifelse(x < 18 & x >= 14, "#ffffb3",
                         ifelse(x < 22 & x >= 18, "#DeF7E9",
                                ifelse(x < 25 & x >= 22, "#98e6bb",
                                       ifelse(x >= 25, "#37B96F", NA))))))
  }
  
  RS_Col_Func <- function(x) {
    ifelse(x < -100, "#ff0000",
           ifelse(x < -90 & x >= -100, "#ff1a1a",
                  ifelse(x < -80 & x >= -90, "#ff3333",
                         ifelse(x < -70 & x >= -80, "#ff4d4d",
                                ifelse(x < -60 & x >= -70, "#ff6666",
                                       ifelse(x < -50 & x >= -60, "#ff8080",
                                              ifelse(x < -40 & x >= -50, "#ff9999",
                                                     ifelse(x < -30 & x >= -40, "#ffb3b3",
                                                            ifelse(x < -20 & x >= -30, "#ffcccc",
                                                                   ifelse(x < -10 & x >= -20, "#ffe6e6",
                                                                          ifelse(x >= -10 & x <= 10, "#ffffff",
                                                                                 ifelse(x < 20 & x > 10, "#eafaf1",
                                                                                        ifelse(x < 30 & x >= 20, "#98E6BB",
                                                                                               ifelse(x < 40 & x >= 30, "#c1f0d6",
                                                                                                      ifelse(x < 50 & x >= 40, "#98e6bb",
                                                                                                             ifelse(x < 60 & x >= 50, "#6fdca0",
                                                                                                                    ifelse(x < 70 & x >= 60, "#46d285",
                                                                                                                           ifelse(x < 80 & x >= 70, "#2db96c",
                                                                                                                                  ifelse(x < 90 & x >= 80, "#28a460",
                                                                                                                                         ifelse(x < 100 & x >= 90, "#239054",
                                                                                                                                                ifelse(x >= 100, "#1e7b48", NA)))))))))))))))))))))
  }
  
  RA_Col_Func <- function(x) {
    ifelse(x < -100, "#1e7b48",
           ifelse(x < -90 & x >= -100, "#239054",
                  ifelse(x < -80 & x >= -90, "#28a460",
                         ifelse(x < -70 & x >= -80, "#2db96c",
                                ifelse(x < -60 & x >= -70, "#46d285",
                                       ifelse(x < -50 & x >= -60, "#6fdca0",
                                              ifelse(x < -40 & x >= -50, "#98e6bb",
                                                     ifelse(x < -30 & x >= -40, "#c1f0d6",
                                                            ifelse(x < -20 & x >= -30, "#98E6BB",
                                                                   ifelse(x < -10 & x >= -20, "#eafaf1",
                                                                          ifelse(x >= -10 & x <= 10, "#ffffff",
                                                                                 ifelse(x < 20 & x > 10, "#ffe6e6",
                                                                                        ifelse(x < 30 & x >= 20, "#ffcccc",
                                                                                               ifelse(x < 40 & x >= 30, "#ffb3b3",
                                                                                                      ifelse(x < 50 & x >= 40, "#ff9999",
                                                                                                             ifelse(x < 60 & x >= 50, "#ff8080",
                                                                                                                    ifelse(x < 70 & x >= 60, "#ff6666",
                                                                                                                           ifelse(x < 80 & x >= 70, "#ff4d4d",
                                                                                                                                  ifelse(x < 90 & x >= 80, "#ff3333",
                                                                                                                                         ifelse(x < 100 & x >= 90, "#ff1a1a",
                                                                                                                                                ifelse(x >= 100, "#ff0000", NA)))))))))))))))))))))
  }
  
  fill_column <- function(gtobj, d){
    ## Create Helper DF
    df <- d
    
    for(i in 2:3){
      val1 <- tWAR_Col_Func(df[10,i])
      gtobj <- gtobj %>%
        tab_style(style = list(
          cell_fill(color = val1),
          cell_text(weight = 'bold')
        ),
        locations = cells_body(columns = i, rows = 10))
    }
    gtobj
  }
  a_fill_column <- function(gtobj, d){
    ## Create Helper DF
    df <- d
    
    for(i in 2:3){
      val1 <- tWAR_Col_Func(df[10,i+1])
      gtobj <- gtobj %>%
        tab_style(style = list(
          cell_fill(color = val1),
          cell_text(weight = 'bold')
        ),
        locations = cells_body(columns = i, rows = 10))
    }
    gtobj
  }
  
  ## Create Summary Row Tables ##
  a_sum_row <- a_lu_tbl %>% group_by() %>% summarise(
    `TeamID` = '111',
    `Player Name` = paste0("Lineup Totals"),
    `Player` = sum(`Player`),
    `Pos Avg` = sum(`Pos Avg`),
    `RS +/-` = sum(`RS +/-`)
  )
  names(a_sum_row) <- names_vec
  
  a_lu_tbl <- rbind(a_lu_tbl, a_sum_row)
  
  h_sum_row <- h_lu_tbl %>% group_by() %>% summarise(
    `RS +/-` = sum(`RS +/-`),
    `Pos Avg` = sum(`Pos Avg`),
    `Player` = sum(`Player`),
    `Player Name` = paste0("Lineup Totals"),
    `TeamID` = '111'
  )
  names(h_sum_row) <- rev(names_vec)
  
  h_lu_tbl <- rbind(h_lu_tbl, h_sum_row)
  
  
  a_sb_logo <- build_logos() %>% filter(TeamID == a_tm_id) %>% select(header_logo) %>% pull()
  h_sb_logo <- build_logos() %>% filter(TeamID == h_tm_id) %>% select(header_logo) %>% pull()
  a_sbbg_logo <- build_logos() %>% filter(TeamID == a_tm_id) %>% select(header_logo_bg) %>% pull()
  h_sbbg_logo <- build_logos() %>% filter(TeamID == h_tm_id) %>% select(header_logo_bg) %>% pull()
  
  style_gradient <- function(x, low = 0, high = 1, colors = c("white", "black")) {
    n <- length(colors) - 1
    bins <- seq(low, high, length.out = n + 1)
    idx <- findInterval(x, vec = bins)
    col <- grDevices::colorRampPalette(colors)(n + 1)[idx + 1]
    paste0("background-color: ", col)
  }
  
  a_gt <- a_lu_tbl  %>%
    left_join(mlbplot_col, by = c('TeamID')) %>%
    select(2:5) %>%
    gt() %>%
    tab_style(
      locations = cells_body(columns = c(2, 3)),
      style = list(
        css(text_align = "center")
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(4)),
      style = list(
        css(text_align = "center",
            font_style = "italic",
            border_left = "1px solid black")
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(1)),
      style = list(
        css(text_align = "right",
            border_right = "1px solid black")
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(),
      style = list(
        css(font_weight = "bold",
            text_align = "center")
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(4)),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            border_left = "1px solid black")
      )
    )  %>%
    tab_style(
      locations = cells_column_labels(columns = c(1)),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            border_right = "1px solid black")
      )
    ) %>%
    data_color(
      columns = `RS +/-`,
      colors = RS_Col_Func
    ) %>%
    data_color(
      columns = c(`Pos Avg`, `Player`),
      colors = WAR_Col_Func
    ) %>%
    a_fill_column(d = a_lu_tbl) %>%
    tab_style(
      locations = cells_body(rows = 10),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            border_top = "2px solid black",
            border_bottom = "2px solid black")
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = 4, rows = 10),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            font_style = "italic",
            border_top = "2px solid black",
            border_left = "1px solid black",
            border_bottom = "2px solid black")
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = 1, rows = 10),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            border_top = "2px solid black",
            border_right = "1px solid black",
            border_bottom = "2px solid black")
      )
    ) %>%
    tab_spanner(
      label = "WAR Values",
      columns = c(`Player`, `Pos Avg`),
      id = 'WAR'
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("left"),
          color = "black",
          weight = px(1)
        ),
        cell_text(
          weight = "bold"
        )
      ),
      locations = cells_column_spanners(spanners = "WAR")
    ) %>%
    tab_header(
      title = md(
        paste0(
          "<div style='background-color:",a_sbbg_logo,"; padding: 5px;'>",
          "<img src='", a_sb_logo, "' style='height: 80px;'>",
          "</div>"
        )
      )
    ) %>%
    tab_footnote(
      footnote = md("**WAR Below 0:** Minor Leaguer | **0-1 WAR:** Below Average<br>**1-2 WAR:** Average | **2-4 WAR:** Above Average | **>4 WAR:** Star Player"),
      locations = cells_column_labels(
        columns = Player
      )
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "5px",
      table.border.top.color = paste0(a_prim),
      table.border.right.style = "solid",
      table.border.right.width = "5px",
      table.border.right.color = paste0(a_prim),
      table.border.bottom.style = "solid",
      table.border.bottom.width = "5px",
      table.border.bottom.color = paste0(a_prim),
      table.border.left.style = "solid",
      table.border.left.width = "5px",
      table.border.left.color = paste0(a_prim),
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = "1px",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = "1px",
      column_labels.border.bottom.color = "black"
    )
  
  h_gt <- h_lu_tbl  %>%
    left_join(mlbplot_col, by = c('TeamID')) %>%
    select(1:4) %>%
    gt() %>%
    tab_style(
      locations = cells_body(columns = c(2, 3)),
      style = list(
        css(text_align = "center")
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(1)),
      style = list(
        css(text_align = "center",
            font_style = "italic",
            border_right = "1px solid black")
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(4)),
      style = list(
        css(text_align = "right",
            border_left = "1px solid black")
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(),
      style = list(
        css(font_weight = "bold",
            text_align = "center")
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(1,3)),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            border_right = "1px solid black")
      )
    ) %>%
    data_color(
      columns = `RS +/-`,
      colors = RS_Col_Func
    ) %>%
    data_color(
      columns = c(`Player`, `Pos Avg`),
      colors = WAR_Col_Func
    ) %>%
    fill_column(d = h_lu_tbl) %>%
    tab_style(
      locations = cells_body(rows = 10),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            border_top = "2px solid black",
            border_bottom = "2px solid black")
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = 1, rows = 10),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            font_style = "italic",
            border_top = "2px solid black",
            border_right = "1px solid black",
            border_bottom = "2px solid black")
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = 4, rows = 10),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            border_top = "2px solid black",
            border_left = "1px solid black",
            border_bottom = "2px solid black")
      )
    ) %>%
    tab_spanner(
      label = "WAR Values",
      columns = c(`Pos Avg`, `Player`),
      id = 'WAR'
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right"),
          color = "black",
          weight = px(1)
        ),
        cell_text(
          weight = "bold"
        )
      ),
      locations = cells_column_spanners(spanners = "WAR")
    ) %>%
    tab_header(
      title = md(
        paste0(
          "<div style='background-color:",h_sbbg_logo,"; padding: 5px;'>",
          "<img src='", h_sb_logo, "' style='height: 80px;'>",
          "</div>"
        )
      )
    ) %>%
    tab_footnote(
      footnote = md("**WAR Below 0:** Minor Leaguer | **0-1 WAR:** Below Average<br>**1-2 WAR:** Average | **2-4 WAR:** Above Average | **>4 WAR:** Star Player"),
      locations = cells_column_labels(
        columns = Player
      )
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "5px",
      table.border.top.color = paste0(h_prim),
      table.border.right.style = "solid",
      table.border.right.width = "5px",
      table.border.right.color = paste0(h_prim),
      table.border.bottom.style = "solid",
      table.border.bottom.width = "5px",
      table.border.bottom.color = paste0(h_prim),
      table.border.left.style = "solid",
      table.border.left.width = "5px",
      table.border.left.color = paste0(h_prim),
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = "1px",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = "1px",
      column_labels.border.bottom.color = "black"
    )
  
  var = ctype
  
  if(var == "P"){
    return(pitch_final)
  } else if (var == "H_BAT") {
    return(h_gt)
  } else if (var == "A_BAT") {
    return(a_gt)
  }
  #print(tWAR_Col_Func(a_lu_tbl[10,3]))
  #print(a_lu_tbl[10,])
  #
}

## Pitcher Table Builds
APP_p_pos_plot_tm <- function(data = full_team_pos_ptile_long, pk, loc) {
  sched_df <- mlb_api_pitchers %>% filter(game_pk == pk)
  tmid <- ifelse(loc == "H", sched_df %>% select(home.TeamID)%>%pull(), sched_df %>% select(away.TeamID)%>%pull())
  plyr <- ifelse(loc == "H", sched_df %>% select(home_p_id)%>%pull(), sched_df %>% select(away_p_id)%>%pull())
  
  
  ## LOAD DATA ## 
  tm <- teamabbr %>% filter(TeamID == tmid) %>% select(bp_teamabbr) %>% pull()
  d <- data
  col <- col_scheme
  tm_full_name <- teamabbr %>% filter(bp_teamabbr == tm)
  team_sp_name <- paste0(tm_full_name %>% select(full_name) %>% pull(), " Rotation")
  
  
  ## FILTER AND MUTATE ##  
  d <- d %>% filter(team == tm)  %>%
    filter(position %in% c("SP")) %>%
    select(team, position, full_pos_name, warp_ptile, total_warp_prod) %>%
    group_by(team, position, full_pos_name, warp_ptile) %>%
    summarise(
      total_warp_prod = sum(total_warp_prod)
    ) %>%
    select(team, position, full_pos_name, warp_ptile, total_warp_prod)
  
  
  d_plyr <- PITCH_PECOTA_FULL %>% filter(mlbid == plyr) %>%
    select(mlbid, name, team, IP_GS, ip, starts_with("warp_")) %>%
    mutate(
      IP_GS = if_else(is.na(IP_GS), 4, IP_GS),
      IP_GS = if_else(IP_GS == 0, 4, IP_GS)
    ) %>%
    pivot_longer(
      cols = starts_with("warp_"),
      names_to = c("warp_name", 'warp_ptile'),
      names_sep = "_"
    ) %>%
    mutate(
      value = ((value*IP_GS)/ip)*162,
    ) %>%
    select(-warp_name) %>%
    group_by(mlbid, name, team, IP_GS, ip, warp_ptile) %>%
    summarise(
      total_warp_prod = sum(value)
    ) %>%
    dplyr::select(team, name, mlbid, warp_ptile, total_warp_prod)
  
  ## EXPAND TEAM BASED ON PERCENTILE ##     
  Tm_SP_d = distro_rep(data = d, pos ="SP")
  Plyr_SP_d = distro_rep_plyr(data = d_plyr)
  
  ### Combine into One ##
  Plyr_Full_Helper <- data.frame(
    team = Plyr_SP_d$team,
    name = Plyr_SP_d$name,
    warp_ptile = Plyr_SP_d$warp_ptile,
    total_warp_prod = Plyr_SP_d$total_warp_prod,
    stringsAsFactors = FALSE
  )
  
  Team_Full_Helper <- data.frame(
    team = Tm_SP_d$team,
    name = team_sp_name,
    warp_ptile = Tm_SP_d$warp_ptile,
    total_warp_prod = Tm_SP_d$total_warp_prod
  )
  
  d <- rbind(Plyr_Full_Helper, Team_Full_Helper)
  
  ##### JOIN COLORS ##
  d <- d %>%
    left_join(col, by = 'team') %>%
    select(team, name, warp_ptile, total_warp_prod, prim_col, sec_col) %>%
    mutate(
      alpha_wt = if_else(name == team_sp_name, 0.7, 0.9),
      plot_col = if_else(name == team_sp_name, prim_col, sec_col)
    )
  
  
  #### ADD MEAN LINES ##
  means_pos <- d %>%
    filter(warp_ptile == '50' & name == team_sp_name) %>%
    select(team, name, total_warp_prod) %>%
    unique()
  tm_mean_col <- p_tWAR_Col_Func(means_pos %>% select(total_warp_prod) %>% pull())
  
  mean_col <- paste0(unique(d$sec_col))
  primfill_col <- paste0(unique(d$prim_col))
  
  ### Add MLB Avg
  player_means <- d %>%
    filter(warp_ptile == '50' & name != team_sp_name) %>%
    select(team, name, total_warp_prod) %>%
    unique()
  ply_sp_name <- player_means %>% select(name) %>% unique() %>% pull()
  ply_mean_col <- p_tWAR_Col_Func(player_means %>% select(total_warp_prod) %>% pull())
  
  
  ### PLOT THAT MF ##
  p <- d %>%
    ggplot( aes(x = total_warp_prod, group = name, fill = name)) +
    geom_density(alpha = 0.7) +
    scale_alpha_continuous(guide=FALSE) +
    scale_fill_manual(name = "", values = c(primfill_col, mean_col)) +
    geom_vline(data = player_means, 
               aes(xintercept=total_warp_prod, color = paste0(ply_sp_name, " - WAR (Starting 162 Games)")), linetype = "solid" , size = 2, color = ply_mean_col) +
    geom_vline(data = means_pos ,
               aes(xintercept=total_warp_prod, color = paste0(ply_sp_name, " - WAR (Starting 162 Games)")), linetype = "longdash", size = 2, color = tm_mean_col) +
    xlim(-10, 25) +
    labs(title = paste0(ply_sp_name, " vs. ", team_sp_name),
         x = "WAR (Wins Above Replacement)",
         y = "Density"
    ) +
    theme_bw() +
    theme(strip.text = element_text(face = "bold"),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 1),
          legend.key.size = unit(1, "cm"),
          legend.spacing.x = unit(0.5, 'cm'))
  return(p)
  #return(Plyr_SP_d)
}
SP_Stats_Chart_Build <- function(pk, loc, type) {
  
  #LOAD VARIABLES#
  loc_var = loc
  sched_df <- today_build(loc = loc_var) %>% filter(game_pk == pk)
  tmid <- sched_df %>% select(TeamID) %>% pull()
  plyr <- sched_df %>% select(p_id) %>% pull()
  p_name_full <- sched_df %>% select(p_name) %>% pull()
  tm_name_full <- sched_df %>% select(full_name) %>% pull()
  hs <- sched_df %>% select(espn_headshot) %>% mutate(
    espn_headshot = case_when(
      is.na(espn_headshot) ~ 'https://a.espncdn.com/combiner/i?img=/i/teamlogos/leagues/500/mlb.png',
      TRUE ~ espn_headshot
    )
  ) %>% pull()
  prim_col_tm = sched_df %>% select(prim_col) %>% pull()
  #print(sched_df)
  
  ## LOAD Team WAR ##
  tm_sp_war <- war_ref_team %>% filter(TeamID == tmid) %>%
    select(-c(total_war, tot_bat_war, tot_p_war, tot_SP_war, tot_RP_war)) %>%
    pivot_longer(
      cols = warp_c_50:warp_SV_50,
      names_to = c("position"),
      names_pattern = "warp_?(.*)_50",
      values_to = 'tm_warp'
    ) %>%
    mutate(
      position = case_when(
        position == 'c' ~ 'C',
        TRUE ~ position
      ),
      tm_warp = round(tm_warp, digits = 2)
    ) %>%
    filter(position == 'SP')
  
  ## LOAD PECOTA ##
  p_PECOTA <- PITCH_PECOTA_FULL %>% filter(mlbid == plyr) %>%
    select(mlbid, TeamID, name, throws, g, gs, ip, era, fip, dra, dra_minus, bb9, so9, whip,
           warp, SP_PCT, RP_PCT, SV_PCT,starts_with('PLY_'), on_DC, full_name, IP_GS, bf, bb, so)
  
  ## Create Combination of Schedule, PECOTA, WAR ##
  df <- data.frame(
    game_pk = sched_df$game_pk,
    TeamID = tmid,
    mlbid = plyr,
    TM_SP_WAR = tm_sp_war$tm_warp,
    headshot = hs,
    stringsAsFactors = FALSE
  )
  
  df <- df %>%
    left_join(p_PECOTA, by = c('mlbid' , 'TeamID')) %>%
    mutate(
      IP_GS = if_else(on_DC == 0, 4, if_else(IP_GS == 0, 4, IP_GS)),
      if162_warp = round(((warp*IP_GS)/ip)*162, digits = 2)
    ) %>%
    mutate(
      RA_Adj = (if162_warp - TM_SP_WAR) * 10,
      PLY_SP = round(PLY_SP*100, digits = 0),
      PLY_RP = round(PLY_RP*100, digits = 0),
      PLY_SV = round(PLY_SV*100, digits = 0),
      Pos_Full = case_when(
        PLY_SP == 100 ~ paste0('Full-Time Starter'),
        PLY_RP == 100 ~ 'Full-Time Reliever',
        PLY_SV == 100 ~ 'Full-Time Closer',
        PLY_SP > 0 & PLY_RP > 0 & PLY_SV == 0 ~ 'Starter/Long Reliever',
        PLY_SP == 0 & PLY_RP > 0 & PLY_SV > 0 ~ 'Set-Up/Closer',
        on_DC == 0 ~ 'Minor League Pitcher'
      ),
      Pos_Sub = case_when(
        PLY_SP == 100 ~ paste0(SP_PCT,'% of Starter IP'),
        PLY_RP == 100 ~ paste0(RP_PCT,'% of Relief IP'),
        PLY_SV == 100 ~ paste0(SV_PCT,'% of Closing IP'),
        PLY_SP > 0 & PLY_RP > 0 & PLY_SV == 0 ~ paste0('Starter: ', PLY_SP,'% | Reliever: ', PLY_RP,'%'),
        PLY_SP == 0 & PLY_RP > 0 & PLY_SV > 0 ~ paste0('Middle Relief/Set-Up: ', PLY_RP,'% | Closer: ', PLY_SV,'%'),
        on_DC == 0 ~ 'Replacement Player'
      )
    ) %>%
    select(-c(SP_PCT, RP_PCT, SV_PCT, PLY_SP, PLY_RP, PLY_SV, on_DC))
  
  info_df <- df %>% select(game_pk, TeamID, mlbid, name, throws, Pos_Full, Pos_Sub, headshot) 
  
  
  # Create Pretty Chart for Pitcher
  chart_df <- df %>%
    select(-c(name, throws, Pos_Full, Pos_Sub, headshot, full_name)) %>%
    mutate(
      BB_Rate = round((bb/bf)*100, digits = 1),
      K_Rate = round((so/bf)*100, digits = 1),
      K_BB_Ratio = round((K_Rate/BB_Rate), digits = 1)
    ) %>%
    select(-c(bf, bb, so, bb9, so9))
  
  names(chart_df) <- c("game_pk","TeamID","mlbid","CALC_TM_SP_WAR","PECOTA_G","PECOTA_GS","PECOTA_IP",
                       "PECOTA_ERA", "PECOTA_FIP", "PECOTA_DRA", "PECOTA_DRAMinus","PECOTA_WHIP",
                       "PECOTA_WARP", "CALC_IP_GS", "CALC_if162warp", "CALC_RA_Adj","PECOTA_BBRate",
                       "PECOTA_KRate", "PECOTA_KBBRatio")
  
  ##Load 2022 Stats Table#
  LY_LB <- Pitch_LB %>%
    select(mlbid, G, GS, IP, ERA, FIP, DRA, DRA_Minus,WHIP,WARP,BB_Rate, K_Rate, K_BB_Ratio)
  names(LY_LB) <- c("mlbid", "LB_G","LB_GS", "LB_IP", "LB_ERA", "LB_FIP", "LB_DRA", "LB_DRAMinus",
                    "LB_WHIP", "LB_WARP", "LB_BBRate", "LB_KRate", "LB_KBBRatio")
  
  stat_chart_df <- chart_df %>%
    select(mlbid, starts_with("PECOTA")) %>%
    left_join(LY_LB, by = c('mlbid')) %>%
    pivot_longer(cols = c(starts_with("PECOTA"), starts_with("LB")), names_to = "column", values_to = "value") %>%
    separate(column, into = c("Source", "category"), sep = "_") %>%
    pivot_wider(names_from = Source, values_from = value) %>%
    mutate(
      PECOTA = case_when(
        category == "G" ~ round(PECOTA, digits = 0),
        category == "GS" ~ round(PECOTA, digits = 0),
        category == "IP" ~ round(PECOTA, digits = 1),
        category == "ERA" ~ round(PECOTA, digits = 2), 
        category == "FIP" ~ round(PECOTA, digits = 2), 
        category == "DRA" ~ round(PECOTA, digits = 2),
        category == "DRAMinus" ~ round(PECOTA, digits = 0),
        category == "WHIP" ~ round(PECOTA, digits = 2),
        category == "WARP" ~ round(PECOTA, digits = 1),
        category == "BBRate" ~ round(PECOTA, digits = 1),
        category == "KRate" ~ round(PECOTA, digits = 1),
        category == "KBBRatio" ~ round(PECOTA, digits = 2),
        TRUE ~ PECOTA
      ),
      LB = case_when(
        category == "G" ~ round(LB, digits = 0),
        category == "GS" ~ round(LB, digits = 0),
        category == "IP" ~ round(LB, digits = 1),
        category == "ERA" ~ round(LB, digits = 2), 
        category == "FIP" ~ round(LB, digits = 2), 
        category == "DRA" ~ round(LB, digits = 2),
        category == "DRAMinus" ~ round(LB, digits = 0),
        category == "WHIP" ~ round(LB, digits = 2),
        category == "WARP" ~ round(LB, digits = 1),
        category == "BBRate" ~ round(LB, digits = 1),
        category == "KRate" ~ round(LB, digits = 1),
        category == "KBBRatio" ~ round(LB, digits = 2),
        TRUE ~ LB
      ),
      category = case_when(
        category == "G" ~ "Appearances",
        category == "GS" ~ "Games Started",
        category == "DRAMinus" ~ "DRA Minus",
        category == "WARP" ~ "WAR",
        category == "BBRate" ~ "BB %",
        category == "KRate" ~ "K %",
        category == "KBBRatio" ~ "K% to BB% Ratio",
        TRUE ~ category
      )
    ) %>%
    select(-mlbid)
  
  war_row <- stat_chart_df %>% filter(category == 'WAR')
  
  stat_chart_df <- stat_chart_df %>% filter(category != "WAR") %>%
    add_row(war_row)
  
  gt_stat_chart_tbl <- stat_chart_df %>%
    rename(Statistic = category) %>%
    rename(`2023 Projection` = PECOTA) %>%
    rename(`2022 Season` = LB) %>%
    filter(Statistic != 'WHIP') %>%
    gt() %>%
    fmt_number(
      columns = c(2,3),
      rows = c(1,2,7),
      decimals = 0
    ) %>%
    fmt_number(
      columns = c(2,3),
      rows = c(3,9,10,11),
      decimals = 1
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(1,2,3)),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            color = "#FFFFFF",
            background_color = paste0(prim_col_tm),
            font_size = "1.05em")
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(1)),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            color = "#FFFFFF",
            background_color = paste0(prim_col_tm),
            border_right = "2px solid #BBBBBB",
            font_size = "1.05em")
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(1)),
      style = list(
        css(
          text_align = "right",
          font_style = "italic",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(1), rows = c(3, 7, 10)),
      style = list(
        css(
          text_align = "right",
          font_style = "italic",
          border_right = "2px solid #BBBBBB",
          border_bottom = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(1), rows = c(11)),
      style = list(
        css(
          text_align = "center",
          font_weight = "bold",
          font_style = "italic",
          border_right = "2px solid #BBBBBB",
          border_bottom = "2px solid black",
          font_size = "1.25em"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(2,3), rows = c(3, 7, 10)),
      style = list(
        css(
          border_bottom = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(2,3), rows = c(11)),
      style = list(
        css(
          font_weight = "bold",
          font_size = "1.25em"
        )
      )
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "3px",
      table.border.top.color = paste0(prim_col_tm),
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = paste0(prim_col_tm),
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = paste0(prim_col_tm),
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = paste0(prim_col_tm)
    )
  
  ##### BUILD REBIRTHA CALC CHART #######
  calc_chart_raw <- chart_df %>%
    select(game_pk,TeamID,mlbid,CALC_TM_SP_WAR,PECOTA_WARP, CALC_IP_GS, CALC_if162warp, CALC_RA_Adj)
  
  slim_ref <- APP_RefTbl %>%
    filter(TeamID == tmid) %>%
    select(PECOTA_WP, PECOTA_RS, PECOTA_RA)
  
  calc_chart <- data.frame(
    Scenario = c(paste0(p_name_full, " Starts 162 Games"), paste0(tm_name_full, " Starting Rotation")),
    IndividualWAR = c(calc_chart_raw[1,5], (calc_chart_raw[1,4])/5),
    TeamWAR = c(calc_chart_raw[1,7],calc_chart_raw[1,4]),
    RA_Adjustment = c(calc_chart_raw[1,8], 0),
    Proj_RA = c(slim_ref[1,3],slim_ref[1,3]),
    Proj_RS = c(slim_ref[1,2],slim_ref[1,2]),
    Proj_WP = c(slim_ref[1,1], slim_ref[1,1])
  )
  
  
  
  calc_chart <- calc_chart %>%
    mutate(
      IndividualWAR = round(IndividualWAR, digits = 1),
      RA_Adjustment = RA_Adjustment*-1,
      Proj_RA_new = (Proj_RA + RA_Adjustment),
      proj_WP_new = if_else(Scenario != paste0(p_name_full, " Starts 162 Games"), Proj_WP,
                            ((Proj_RS)^1.83)/(((Proj_RS)^1.83)+((Proj_RA_new)^1.83))),
      proj_wins = round(proj_WP_new*162, digits = 1),
      proj_WP_new = round(proj_WP_new, digits = 3)
    ) %>%
    select(-c(Proj_RA, Proj_RS, Proj_WP))
  
  totals_df <- data.frame(
    Scenario = "Difference",
    IndividualWAR = (calc_chart[1,2] - calc_chart[2,2]),
    TeamWAR = (calc_chart[1,3] - calc_chart[2,3]),
    RA_Adjustment = (calc_chart[1,4] + calc_chart[2,4]),
    Proj_RA_new = (calc_chart[1,5] - calc_chart[2,5]),
    proj_WP_new = (calc_chart[1,6] - calc_chart[2,6]),
    proj_wins = (calc_chart[1,7] - calc_chart[2,7])
  )
  
  fill_column <- function(gtobj, d){
    ## Create Helper DF
    df <- d
    
    for(i in 7){
      val1 <- Delta_Wins_Col_Func(df[3,i])
      val2 <- Delta_Wins_Txt_Func(df[3,i])
      gtobj <- gtobj %>%
        tab_style(style = list(
          cell_fill(color = val1),
          cell_text(weight = 'bold', color = val2)
        ),
        locations = cells_body(columns = i, rows = 3))
    }
    gtobj
  }
  
  calc_chart <- calc_chart %>%
    add_row(totals_df)
  
  gt_calc_chart_tbl <- calc_chart %>%
    rename(`Individual` = IndividualWAR) %>%
    rename(`Season Total` = TeamWAR) %>%
    rename(`RA +/-` = RA_Adjustment) %>%
    rename(`Total RA` = Proj_RA_new) %>%
    rename(`Win %` = proj_WP_new) %>%
    rename(`Wins` = proj_wins) %>%
    gt() %>%
    tab_style(
      locations = cells_column_labels(columns = c(1,2,3,4,5,6,7)),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            color = "#FFFFFF",
            background_color = paste0(prim_col_tm),
            font_size = "1.05em")
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(1)),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            color = "#FFFFFF",
            background_color = paste0(prim_col_tm),
            border_right = "2px solid #BBBBBB",
            font_size = "1.05em")
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(3,5)),
      style = list(
        css(text_align = "center",
            font_weight = "bold",
            color = "#FFFFFF",
            background_color = paste0(prim_col_tm),
            border_right = "2px solid #BBBBBB",
            font_size = "1.05em")
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(1)),
      style = list(
        css(
          text_align = "left",
          font_style = "italic",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(2,3,4,5,6)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(3,5)),
      style = list(
        css(
          text_align = "center",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(7)),
      style = list(
        css(
          text_align = "center",
          font_weight = "bold"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(1), rows = 3),
      style = list(
        css(
          text_align = "right",
          font_weight = "bold",
          font_style = "italic",
          border_right = "2px solid #BBBBBB",
          border_top = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(2,3,4,5), rows = 3),
      style = list(
        css(
          text_align = "center",
          font_weight = "bold",
          font_style = "italic",
          border_top = "2px solid black"
        )
      )
    )  %>%
    tab_style(
      locations = cells_body(columns = c(3,5), rows = 3),
      style = list(
        css(
          text_align = "center",
          font_weight = "bold",
          font_style = "italic",
          border_top = "2px solid black",
          border_right = "2px solid #BBBBBB",
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(6, 7), rows = 3),
      style = list(
        css(
          text_align = "center",
          font_style = "italic",
          font_weight = "bold",
          border_top = "2px solid black"
        )
      )
    )  %>%
    data_color(
      columns = `Wins`,
      colors = Wins_Col_Func
    ) %>%
    fill_column(d = calc_chart) %>%
    tab_spanner(
      label = "PECOTA WAR Values",
      columns = c(`Individual`, `Season Total`),
      id = 'WAR'
    ) %>%
    tab_spanner(
      label = "Runs Against & Win Values",
      columns = c(`RA +/-`, `Total RA`, `Win %`, `Wins`),
      id = 'PROJ'
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right", "left"),
          color = "#BBBBBB",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "white"
        ),
        cell_fill(
          color = paste0(prim_col_tm)
        )
      ),
      locations = cells_column_spanners(spanners = "WAR")
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("left"),
          color = "#BBBBBB",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "white"
        ),
        cell_fill(
          color = paste0(prim_col_tm)
        )
      ),
      locations = cells_column_spanners(spanners = "PROJ")
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "3px",
      table.border.top.color = paste0(prim_col_tm),
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = paste0(prim_col_tm),
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = paste0(prim_col_tm),
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = paste0(prim_col_tm)
    )
  
  
  vartype = type
  if(vartype == "INFO") {
    return(info_df)
  } else if (vartype == "CHART") {
    return(gt_stat_chart_tbl)
  } else if(vartype == "CALC"){
    return(gt_calc_chart_tbl)
  }
  
}

## Pretty Bets Table Build
pretty_bets <- function(){
  df <- bets_table
  #print(df)
  
  ## RS/RA Analysis Build ##
  listofdfs <- list()
  for(i in 1:length(df$game_pk)) {
    pk = df$game_pk[i]
    
    ## Bring in Lineup Tables ##
    lu_full <- mlb_api_batters %>% filter(game_pk == pk)
    p_full <- mlb_api_pitchers %>% filter(game_pk == pk)
    
    ## Set General Variables ##
    h_tm_id <- p_full %>% select(home.TeamID) %>% pull() %>% as.character()
    a_tm_id <- p_full %>% select(away.TeamID) %>% pull() %>% as.character()
    h_tm_abbr <- teamabbr %>% filter(TeamID == h_tm_id) %>% select(bp_teamabbr) %>% pull() %>% as.character()
    a_tm_abbr <- teamabbr %>% filter(TeamID == a_tm_id) %>% select(bp_teamabbr) %>% pull() %>% as.character()
    
    ## Build Clean Home and Away Tables ##
    home_tbl <- lu_full %>%
      select(game_pk, home.TeamID, home_bat_id, home_bat_name, home_pos, home_batting_order) %>%
      mutate(loc = 'H')
    names(home_tbl) <- c("game_pk", "TeamID", "mlbid", "name", "position", "bat_order", "loc")
    
    h_p <- p_full %>% select(game_pk, home.TeamID, home_p_id, home_p_name) %>%
      mutate(
        position = 'SP',
        bat_order = 0,
        loc = 'H',
      )
    names(h_p) <- c("game_pk", "TeamID", "mlbid", "name", "position", "bat_order", "loc")
    
    away_tbl <- lu_full %>%
      select(game_pk, away.TeamID, away_bat_id, away_bat_name, away_pos, home_batting_order) %>%
      mutate(loc = 'A')
    names(away_tbl) <- c("game_pk", "TeamID", "mlbid", "name", "position", "bat_order", "loc")
    
    a_p <- p_full %>% select(game_pk, away.TeamID, away_p_id, away_p_name) %>%
      mutate(
        position = 'SP',
        bat_order = 0,
        loc = 'A',
      )
    names(a_p) <- c("game_pk", "TeamID", "mlbid", "name", "position", "bat_order", "loc")
    
    ## Bring In PECOTA ##
    hit_pecota <- HIT_PECOTA_FULL %>% select(mlbid, g, pa, warp, full_name)
    pitch_pecota <- PITCH_PECOTA_FULL %>% select(mlbid, throws, g, gs, ip, warp, SP_PCT, RP_PCT, SV_PCT, starts_with('PLY_'), on_DC, full_name, IP_GS)
    
    ## Bring in Team War Totals And Mutate ##
    tm_war <- war_ref_team %>% filter(TeamID %in% c(h_tm_id, a_tm_id)) %>%
      select(-c(total_war, tot_bat_war, tot_p_war, tot_SP_war, tot_RP_war)) %>%
      pivot_longer(
        cols = warp_c_50:warp_SV_50,
        names_to = c("position"),
        names_pattern = "warp_?(.*)_50",
        values_to = 'tm_warp'
      ) %>%
      mutate(
        position = case_when(
          position == 'c' ~ 'C',
          TRUE ~ position
        ),
        tm_warp = round(tm_warp, digits = 2)
      )
    
    standings_slim <- standings %>%
      select(Team, Win_Pct, RS, RA) %>%
      left_join(teamabbr %>% select(bp_teamabbr, TeamID), by = c('Team' = 'bp_teamabbr'))
    
    ## Join PECOTA to Clean Lineup tables ##
    lu_full_tbl <- rbind(home_tbl, away_tbl)
    lu_full_tbl <- lu_full_tbl %>%
      left_join(hit_pecota, by = c('mlbid')) %>%
      mutate(
        if162_warp = round((warp/g)*162, digits=2)
      ) %>%
      left_join(tm_war, by = c('TeamID', 'position')) %>%
      mutate(
        RS_Adj = (if162_warp - tm_warp) * 10,
        NamePosOrder = paste0(name, ' - ', position, ' (', bat_order,')')
      ) %>%
      select(game_pk, TeamID, loc, RS_Adj) %>%
      group_by(game_pk, TeamID, loc) %>%
      summarise(
        RS_Adj = sum(RS_Adj)
      )
    
    
    # ## Join PECOTA TO Pitchers ##
    p_full_tbl <- rbind(h_p, a_p)
    p_full_tbl <- p_full_tbl %>%
      left_join(pitch_pecota, by = c('mlbid')) %>%
      mutate(
        IP_GS = if_else(on_DC == 0, 4, if_else(IP_GS == 0, 4, IP_GS)),
        if162_warp = round(((warp*IP_GS)/ip)*162, digits = 2)
      ) %>%
      left_join(tm_war, by = c('TeamID', 'position')) %>%
      mutate(
        RA_Adj = ((if162_warp - tm_warp) * 10)*-1
      ) %>%
      select(game_pk, TeamID, loc, RA_Adj) %>%
      group_by(game_pk, TeamID, loc) %>%
      summarise(
        RA_Adj = sum(RA_Adj)
      ) %>%
      left_join(standings_slim, by = c("TeamID"))
    
    full_tbl <- lu_full_tbl %>%
      left_join(p_full_tbl, by = c("game_pk", "TeamID", "loc")) %>%
      ungroup()
    
    listofdfs[[i]] <- full_tbl
  }
  all_bets <- bind_rows(listofdfs) %>%
    pivot_wider(
      names_from = loc,
      values_from = c(TeamID, RS_Adj, RA_Adj, Team, Win_Pct, RS, RA)
    )
  #print(all_bets)
  
  #Bet_Team, Bet_TeamAbbr, Advantage, Unit_Bet, Bet_Odds, Unit_Risk, To_Win
  
  all_bets <- df %>%
    left_join(all_bets, by = c('game_pk', "Home_Abbr" = "Team_H", "Away_Abbr" = "Team_A")) %>%
    select(game_pk, TeamID_H, TeamID_A, Home, Away, Home_Abbr, Away_Abbr, RS_H, RS_A, RA_H, RA_A,
           RS_Adj_H, RS_Adj_A, RA_Adj_H, RA_Adj_A, Win_Pct_H, Win_Pct_A, Home_Book_ML, Away_Book_ML,
           Home_Rebirtha_ML, Away_Rebirtha_ML, Home_ImpProb_Book, Away_ImpProb_Book, Home_ImpProb_Rebirtha, Away_ImpProb_Rebirtha,
           home_adv, away_adv) %>%
    rename(
      Home_TeamID = TeamID_H,
      Home_FullTeam = Home,
      Home_RS = RS_H,
      Home_RA = RA_H,
      Home_AdjRS = RS_Adj_H,
      Home_AdjRA = RA_Adj_H,
      Home_WinPct = Win_Pct_H,
      Home_BookML = Home_Book_ML,
      Home_RebirthaML = Home_Rebirtha_ML,
      Home_ImpProbBook = Home_ImpProb_Book,
      Home_ImpProbRebirtha = Home_ImpProb_Rebirtha,
      Home_Advantage = home_adv,
      Away_TeamID = TeamID_A,
      Away_FullTeam = Away,
      Away_RS = RS_A,
      Away_RA = RA_A,
      Away_AdjRS = RS_Adj_A,
      Away_AdjRA = RA_Adj_A,
      Away_WinPct = Win_Pct_A,
      Away_BookML = Away_Book_ML,
      Away_RebirthaML = Away_Rebirtha_ML,
      Away_ImpProbBook = Away_ImpProb_Book,
      Away_ImpProbRebirtha = Away_ImpProb_Rebirtha,
      Away_Advantage = away_adv
    ) %>%
    pivot_longer(
      cols = matches(c("Home_", "Away_"), everything()),
      names_to = c("Site", ".value"),
      names_pattern = "(Home_|Away_)(.*)"
    ) %>%
    mutate(
      Site = gsub("_", "", Site)
    )
  
  
  return(all_bets)
}
pretty_bets_gt <- function(pk){
  
  #Birng In Asthetics#
  cols <- APP_RefTbl %>% select(TeamID, prim_col, sec_col)
  
  df <- pretty_bets_tbl %>%
    filter(game_pk == pk) %>%
    left_join(cols, by = c('TeamID'))
  
  full_h_prim <- df %>% filter(Site == 'Home') %>% select(prim_col) %>% pull()
  full_a_prim <- df %>% filter(Site == 'Away') %>% select(prim_col) %>% pull()
  full_h_sec <- df %>% filter(Site == 'Home') %>% select(sec_col) %>% pull()
  full_a_sec <- df %>% filter(Site == 'Away') %>% select(sec_col) %>% pull()
  
  t_col <- function(color, percent = 50, name = NULL) {
    #      color = color name
    #    percent = % transparency
    #       name = an optional name for the color
    
    ## Get RGB values for named color
    rgb.val <- col2rgb(color)
    
    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)
    
    ## Save the color
    invisible(t.col)
  }
  
  h_col_prim <- t_col(full_h_prim)
  h_col_sec <- t_col(full_h_sec)
  a_col_prim <- t_col(full_a_prim)
  a_col_sec <- t_col(full_a_sec)
  
  # Format column A with a "+" sign for positive values and percent format with two decimal places
  custom_fmt <- function(x) {
    paste0(ifelse(x > 0, "+", ""), fmt_percent(x, decimals = 2))
  }
  
  
  
  df <- df %>%
    select(-game_pk, -TeamID, -Abbr, -prim_col, -sec_col) %>%
    mutate(
      WinPct = round(((RS + AdjRS)^1.83)/(((RS + AdjRS)^1.83)+((RA + AdjRA)^1.83)), digits = 3),
    ) %>%
    rename(
      `Team Name` = FullTeam,
      `RS +/-` = AdjRS,
      `RA +/-` = AdjRA,
      `Win %` = WinPct,
      `Vegas ML` = BookML,
      `Rebirtha ML` = RebirthaML,
      `Vegas` = ImpProbBook,
      `Rebirtha` = ImpProbRebirtha
    ) %>%
    arrange(Site) %>%
    mutate(
      Rebirtha = if_else(`Rebirtha ML` > 0,
                         paste0('+',round(`Rebirtha ML`, digits = 1), ' (',round(`Rebirtha`*100, digits = 1),'%)'),
                         paste0(round(`Rebirtha ML`, digits = 1), ' (',round(`Rebirtha`*100, digits = 1),'%)')),
      Vegas = if_else(`Vegas ML` > 0,
                      paste0("+",`Vegas ML`, ' (',round(`Vegas`*100, digits = 1),'%)'),
                      paste0(`Vegas ML`, ' (',round(`Vegas`*100, digits = 1),'%)'))
    ) %>% select(-c(8,9))
  
  
  
  gt_df <- df %>%
    select(-Site) %>%
    gt()  %>%
    fmt_percent(
      columns = 9,
      decimals = 2,
      pattern = ifelse("{x}" > 0, "{sign}{x}", "{x}")
    ) %>%
    fmt_number(
      columns = c(4,5),
      decimals = 1,
      pattern = ifelse("{x}" > 0, "{sign}{x}", "{x}")
    ) %>%
    tab_style(
      locations = cells_column_labels(),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(1,6,8)),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(3,5)),
      style = list(
        css(
          border_right = "2px solid #BBBBBB",
          text_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(2,4)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(6)),
      style = list(
        css(
          border_right = "2px solid #BBBBBB",
          font_weight = "bold",
          font_style = "italic"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(7)),
      style = list(
        css(
          tex_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(8)),
      style = list(
        css(
          border_right = "2px solid #BBBBBB",
          tex_align = "center",
          font_style = "italic"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(9)),
      style = list(
        css(
          tex_align = "center",
          font_style = "italic",
          font_weight = "bold"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = 1, rows = 2),
      style = list(
        css(
          background_color = h_col_prim,
          color = full_h_sec,
          "font-weight" = "bold",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = 1, rows = 1),
      style = list(
        css(
          background_color = a_col_prim,
          color = full_a_sec,
          "font-weight" = "bold",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    data_color(
      columns = c(4),
      colors = RS_Col_Func
    ) %>%
    data_color(
      columns = c(5),
      colors = RA_Col_Func
    ) %>%
    data_color(
      columns = c(6),
      colors = WinPct_Col_Func
    ) %>%
    data_color(
      columns = c(9),
      colors = Adv_Scale
    ) %>%
    tab_spanner(
      label = "Runs And Win Adjustments",
      columns = c(`RS`,`RA`,`RS +/-`,`RA +/-`,`Win %`),
      id = 'RUNS'
    ) %>%
    tab_spanner(
      label = "Moneyline & Implied Probability",
      columns = c(Vegas, Rebirtha),
      id = 'VEGAS'
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right", "left"),
          color = "#BBBBBB",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "black"
        ),
        cell_fill(
          color = "white"
        )
      ),
      locations = cells_column_spanners(spanners = c("RUNS"))
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right", "left"),
          color = "#BBBBBB",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "black"
        ),
        cell_fill(
          color = "white"
        )
      ),
      locations = cells_column_spanners(spanners = c("VEGAS"))
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "3px",
      table.border.top.color = "#BBBBBB",
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = "#BBBBBB",
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = "#BBBBBB",
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = "#BBBBBB"
    )
  
  
  return(gt_df)
}
pretty_total_bets <- function() {
  df <- bets_table %>%
    mutate(
      Home_Rebirtha = if_else(Home_Rebirtha_ML > 0,
                              paste0('+',round(Home_Rebirtha_ML, digits = 0), ' (',round(Home_ImpProb_Rebirtha*100, digits = 1),'%)'),
                              paste0(round(Home_Rebirtha_ML, digits = 0), ' (',round(Home_ImpProb_Rebirtha*100, digits = 1),'%)')),
      Home_Vegas = if_else(Home_Book_ML > 0,
                           paste0("+",Home_Book_ML, ' (',round(Home_ImpProb_Book*100, digits = 1),'%)'),
                           paste0(Home_Book_ML, ' (',round(Home_ImpProb_Book*100, digits = 1),'%)')),
      Away_Rebirtha = if_else(Away_Rebirtha_ML > 0,
                              paste0('+',round(Away_Rebirtha_ML, digits = 0), ' (',round(Away_ImpProb_Rebirtha*100, digits = 1),'%)'),
                              paste0(round(Away_Rebirtha_ML, digits = 0), ' (',round(Away_ImpProb_Rebirtha*100, digits = 1),'%)')),
      Away_Vegas = if_else(Away_Book_ML > 0,
                           paste0("+",Away_Book_ML, ' (',round(Away_ImpProb_Book*100, digits = 1),'%)'),
                           paste0(Away_Book_ML, ' (',round(Away_ImpProb_Book*100, digits = 1),'%)')),
      Bet_Odds = if_else(Bet_Odds > 0, paste0("+",Bet_Odds), paste0(Bet_Odds))
    ) %>%
    select(Time_Local, Home_Abbr, Home_Vegas, Home_Rebirtha, Away_Abbr, Away_Vegas, Away_Rebirtha,
           Bet_Team, Bet_Odds, Advantage, Unit_Bet, Unit_Risk, To_Win) %>%
    mutate(
      Home_Abbr = case_when(
        Home_Abbr == 'ARI' ~ 'AZ',
        Home_Abbr == 'CHW' ~ 'CWS',
        Home_Abbr == 'WAS' ~ 'WSH',
        TRUE ~ Home_Abbr
      ),
      Away_Abbr = case_when(
        Away_Abbr == 'ARI' ~ 'AZ',
        Away_Abbr == 'CHW' ~ 'CWS',
        Away_Abbr == 'WAS' ~ 'WSH',
        TRUE ~ Away_Abbr
      )
    ) %>%
    arrange(desc(Advantage))
  
  gt_df <- df %>%
    gt() %>%
    tab_spanner(
      label = "Home Team Lines",
      columns = c(`Home_Abbr`,`Home_Vegas`,`Home_Rebirtha`),
      id = 'HOME'
    ) %>%
    tab_spanner(
      label = "Away Team Lines",
      columns = c(`Away_Abbr`,`Away_Vegas`,`Away_Rebirtha`),
      id = 'AWAY'
    ) %>%
    tab_spanner(
      label = "Rebirtha Bet Information",
      columns = c(Bet_Team,Bet_Odds, Advantage, Unit_Bet, Unit_Risk, To_Win),
      id = 'BETS'
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right", "left"),
          color = "#BBBBBB",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "black"
        ),
        cell_fill(
          color = "white"
        )
      ),
      locations = cells_column_spanners(spanners = c("HOME", "AWAY", "BETS"))
    ) %>%
    tab_style(
      locations = cells_column_labels(),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(1,4,7)),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    gt_fmt_mlb_scoreboard_logo(columns = "Home_Abbr", height = 40) %>%
    gt_fmt_mlb_scoreboard_logo(columns = "Away_Abbr", height = 40) %>%
    tab_style(
      locations = cells_body(columns = c(1,4,7)),
      style = list(
        css(
          text_align = "center",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(2,3,5,6)),
      style = list(css(text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(8,9,10,11,12,13), rows = df$Advantage >= 0.15),
      style = list(css(background_color = "#1e7b48", font_weight = "bold", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(8,9,10,11,12,13), rows = (df$Advantage < 0.15 & df$Advantage >= 0.10)),
      style = list(css(background_color = "#28a460", font_weight = "bold", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(8,9,10,11,12,13), rows = (df$Advantage < 0.10 & df$Advantage >= 0.07)),
      style = list(css(background_color = "#32cd78", font_weight = "bold", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(8,9,10,11,12,13), rows = (df$Advantage < 0.07 & df$Advantage >= 0.04)),
      style = list(css(background_color = "#6fdca0", font_weight = "bold", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(8,9,10,11,12,13), rows = (df$Advantage < 0.04 & df$Advantage >= 0.02)),
      style = list(css(background_color = "#FAFFA1", font_weight = "bold", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(8,9,10,11,12,13), rows = (df$Advantage < 0.02 & df$Advantage >= 0)),
      style = list(css(background_color = "#FAFFA1", font_weight = "bold", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(8,9,10,11,12,13), rows = (df$Advantage < 0 & df$Advantage >= -0.025)),
      style = list(css(background_color = "#ff9999", font_weight = "bold", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(8,9,10,11,12,13), rows = (df$Advantage < -0.025)),
      style = list(css(background_color = "#ff0000", font_weight = "bold", text_align = "center"))
    ) %>%
    fmt_percent(
      columns = 10,
      decimals = 2,
      pattern = ifelse("{x}" > 0, "{sign}{x}", "{x}")
    ) %>%
    cols_label(
      Time_Local = "Time (EST)",
      Home_Abbr = "Team",
      Home_Vegas = "Vegas",
      Home_Rebirtha = "Rebirtha",
      Away_Abbr = "Team",
      Away_Vegas = "Vegas",
      Away_Rebirtha = "Rebirtha",
      Bet_Team = "Bet",
      Bet_Odds = "Odds",
      Unit_Bet = "Unit",
      Unit_Risk = "Risk",
      To_Win = "Win"
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "3px",
      table.border.top.color = "#BBBBBB",
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = "#BBBBBB",
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = "#BBBBBB",
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = "#BBBBBB"
    )
  
  return(gt_df)
  
}

#############################################################################################################################
####################################END LOAD OF MODEL GEN ENVIRONMENT FUNCTIONS##############################################
#############################################################################################################################


