####### Rebirtha Model - Set Up General Environment Tables #######

## Build Reference Table ##
teamabbr <- teamabbr_build()
col_scheme <- build_cols()

#### PECOTA LOAD ####

#Home Path
#h_base_path <- '//Users//tommywinfield//Documents//R Data - All//Rebirtha_2023//Data//pecota2023_hitting_feb16//'
#p_base_path <- '//Users//tommywinfield//Documents//R Data - All//Rebirtha_2023//Data//pecota2023_pitching_feb16//'
#schema <- '-Table 1.csv'

#Work Path
#h_base_path <- 'C:\\Users\\TWinfield\\Desktop\\MLB - 2023 PECOTA\\hit_2023\\pecota2023_hitting_mar21_'
#p_base_path <- 'C:\\Users\\TWinfield\\Desktop\\MLB - 2023 PECOTA\\pitch_2023\\pecota2023_pitching_mar21_'

#GIT Path
h_base_path <- 'https://raw.githubusercontent.com/twinfield10/Rebirtha-Model/main/Data/pecota2023_hitting_mar21_'
p_base_path <- 'https://raw.githubusercontent.com/twinfield10/Rebirtha-Model/main/Data/pecota2023_pitching_mar21_'
schema <- '.csv'

# Load Tables
HIT_PECOTA_RAW <- read.csv(paste0(h_base_path,'50',schema), header = TRUE, stringsAsFactors = FALSE, encoding="UTF-8")
PITCH_PECOTA_RAW <- read.csv(paste0(p_base_path,'50',schema), header = TRUE, stringsAsFactors = FALSE, encoding="UTF-8")

PITCH_PECOTA_RAW <- Trade_Adj(data = PITCH_PECOTA_RAW)
HIT_PECOTA_RAW <- Trade_Adj(data = HIT_PECOTA_RAW)

hit_01<- read.csv(paste0(h_base_path,'01',schema), header = TRUE, stringsAsFactors = FALSE)
hit_05<- read.csv(paste0(h_base_path,'05',schema), header = TRUE, stringsAsFactors = FALSE)
hit_10<- read.csv(paste0(h_base_path,'10',schema), header = TRUE, stringsAsFactors = FALSE)
hit_20<- read.csv(paste0(h_base_path,'20',schema), header = TRUE, stringsAsFactors = FALSE)
hit_30<- read.csv(paste0(h_base_path,'30',schema), header = TRUE, stringsAsFactors = FALSE)
hit_40<- read.csv(paste0(h_base_path,'40',schema), header = TRUE, stringsAsFactors = FALSE)
hit_50<- read.csv(paste0(h_base_path,'50',schema), header = TRUE, stringsAsFactors = FALSE)
hit_60<- read.csv(paste0(h_base_path,'60',schema), header = TRUE, stringsAsFactors = FALSE)
hit_70<- read.csv(paste0(h_base_path,'70',schema), header = TRUE, stringsAsFactors = FALSE)
hit_80<- read.csv(paste0(h_base_path,'80',schema), header = TRUE, stringsAsFactors = FALSE)
hit_90<- read.csv(paste0(h_base_path,'90',schema), header = TRUE, stringsAsFactors = FALSE)
hit_95<- read.csv(paste0(h_base_path,'95',schema), header = TRUE, stringsAsFactors = FALSE)
hit_99<- read.csv(paste0(h_base_path,'99',schema), header = TRUE, stringsAsFactors = FALSE)

#pitch_pecotaapr01<- read.csv('//Users//tommywinfield//Documents//R Data - All//Baseball Model//pecota2022_pitching_apr07//50-Table 1.csv', header = TRUE, stringsAsFactors = FALSE)
colnames(hit_01)[1] <- 'bpid'
colnames(hit_05)[1] <- 'bpid'
colnames(hit_10)[1] <- 'bpid'
colnames(hit_20)[1] <- 'bpid'
colnames(hit_30)[1] <- 'bpid'
colnames(hit_40)[1] <- 'bpid'
colnames(hit_50)[1] <- 'bpid'
colnames(hit_60)[1] <- 'bpid'
colnames(hit_70)[1] <- 'bpid'
colnames(hit_80)[1] <- 'bpid'
colnames(hit_90)[1] <- 'bpid'
colnames(hit_95)[1] <- 'bpid'
colnames(hit_99)[1] <- 'bpid'

hit_01<-hit_01 %>% select(bpid, warp) %>% rename(warp_01  = warp)
hit_05<-hit_05 %>% select(bpid, warp) %>% rename(warp_05  = warp)
hit_10<-hit_10 %>% select(bpid, warp) %>% rename(warp_10  = warp)
hit_20<-hit_20 %>% select(bpid, warp) %>% rename(warp_20 = warp)
hit_30<-hit_30 %>% select(bpid, warp) %>% rename(warp_30 = warp)
hit_40<-hit_40 %>% select(bpid, warp) %>% rename(warp_40 = warp)
hit_50<-HIT_PECOTA_RAW %>% select(bpid, warp, team, pos) %>% rename(warp_50 = warp)
hit_60<-hit_60 %>% select(bpid, warp) %>% rename(warp_60 = warp)
hit_70<-hit_70 %>% select(bpid, warp) %>% rename(warp_70 = warp)
hit_80<-hit_80 %>% select(bpid, warp) %>% rename(warp_80 = warp)
hit_90<-hit_90 %>% select(bpid, warp) %>% rename(warp_90 = warp)
hit_95<-hit_95 %>% select(bpid, warp) %>% rename(warp_95 = warp)
hit_99<-hit_99 %>% select(bpid, warp) %>% rename(warp_99 = warp)

hit_all <- hit_50 %>% 
  left_join(hit_01, by = 'bpid') %>% 
  left_join(hit_05, by = 'bpid') %>%
  left_join(hit_10, by = 'bpid') %>%
  left_join(hit_20, by = 'bpid') %>%
  left_join(hit_30, by = 'bpid') %>%
  left_join(hit_40, by = 'bpid') %>% 
  left_join(hit_60, by = 'bpid') %>%
  left_join(hit_70, by = 'bpid') %>%
  left_join(hit_80, by = 'bpid') %>%
  left_join(hit_90, by = 'bpid') %>%
  left_join(hit_95, by = 'bpid') %>%
  left_join(hit_99, by = 'bpid')


####################################################################################################

pitch_01<- read.csv(paste0(p_base_path,'01',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_05<- read.csv(paste0(p_base_path,'05',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_10<- read.csv(paste0(p_base_path,'10',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_20<- read.csv(paste0(p_base_path,'20',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_30<- read.csv(paste0(p_base_path,'30',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_40<- read.csv(paste0(p_base_path,'40',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_50<- read.csv(paste0(p_base_path,'50',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_60<- read.csv(paste0(p_base_path,'60',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_70<- read.csv(paste0(p_base_path,'70',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_80<- read.csv(paste0(p_base_path,'80',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_90<- read.csv(paste0(p_base_path,'90',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_95<- read.csv(paste0(p_base_path,'95',schema), header = TRUE, stringsAsFactors = FALSE)
pitch_99<- read.csv(paste0(p_base_path,'99',schema), header = TRUE, stringsAsFactors = FALSE)

#pitch_pecotaapr01<- read.csv('//Users//tommywinfield//Documents//R Data - All//Baseball Model//pecota2022_pitching_apr07//50-Table 1.csv', header = TRUE, stringsAsFactors = FALSE)
colnames(pitch_01)[1] <- 'bpid'
colnames(pitch_05)[1] <- 'bpid'
colnames(pitch_10)[1] <- 'bpid'
colnames(pitch_20)[1] <- 'bpid'
colnames(pitch_30)[1] <- 'bpid'
colnames(pitch_40)[1] <- 'bpid'
colnames(pitch_50)[1] <- 'bpid'
colnames(pitch_60)[1] <- 'bpid'
colnames(pitch_70)[1] <- 'bpid'
colnames(pitch_80)[1] <- 'bpid'
colnames(pitch_90)[1] <- 'bpid'
colnames(pitch_95)[1] <- 'bpid'
colnames(pitch_99)[1] <- 'bpid'

pitch_01<-pitch_01 %>% select(bpid, warp) %>% rename(warp_01  = warp)
pitch_05<-pitch_05 %>% select(bpid, warp) %>% rename(warp_05  = warp)
pitch_10<-pitch_10 %>% select(bpid, warp) %>% rename(warp_10  = warp)
pitch_20<-pitch_20 %>% select(bpid, warp) %>% rename(warp_20 = warp)
pitch_30<-pitch_30 %>% select(bpid, warp) %>% rename(warp_30 = warp)
pitch_40<-pitch_40 %>% select(bpid, warp) %>% rename(warp_40 = warp)
pitch_50<-PITCH_PECOTA_RAW %>% select(bpid, warp, team) %>% rename(warp_50 = warp)
pitch_60<-pitch_60 %>% select(bpid, warp) %>% rename(warp_60 = warp)
pitch_70<-pitch_70 %>% select(bpid, warp) %>% rename(warp_70 = warp)
pitch_80<-pitch_80 %>% select(bpid, warp) %>% rename(warp_80 = warp)
pitch_90<-pitch_90 %>% select(bpid, warp) %>% rename(warp_90 = warp)
pitch_95<-pitch_95 %>% select(bpid, warp) %>% rename(warp_95 = warp)
pitch_99<-pitch_99 %>% select(bpid, warp) %>% rename(warp_99 = warp)

pitch_all <- pitch_50 %>% 
  left_join(pitch_01, by = 'bpid') %>% 
  left_join(pitch_05, by = 'bpid') %>%
  left_join(pitch_10, by = 'bpid') %>%
  left_join(pitch_20, by = 'bpid') %>%
  left_join(pitch_30, by = 'bpid') %>%
  left_join(pitch_40, by = 'bpid') %>% 
  left_join(pitch_60, by = 'bpid') %>%
  left_join(pitch_70, by = 'bpid') %>%
  left_join(pitch_80, by = 'bpid') %>%
  left_join(pitch_90, by = 'bpid') %>%
  left_join(pitch_95, by = 'bpid') %>%
  left_join(pitch_99, by = 'bpid')

## Remove Percentile Tables For Cleanliness ##
rm(hit_01,hit_05,hit_10, hit_20, hit_30, hit_40, hit_50, hit_60, hit_70, hit_80, hit_90, hit_95, hit_99)
rm(pitch_01,pitch_05,pitch_10, pitch_20, pitch_30, pitch_40, pitch_50, pitch_60, pitch_70, pitch_80, pitch_90, pitch_95, pitch_99)


## Add TeamID To Pecota Stats
HIT_PECOTA_RAW <- HIT_PECOTA_RAW %>%
  inner_join(teamabbr, by = c("team" = "bp_teamabbr"))
PITCH_PECOTA_RAW <- PITCH_PECOTA_RAW %>%
  inner_join(teamabbr, by = c("team" = "bp_teamabbr")) 


################################
####### GET DEPTH CHARTS #######
################################

## Home Path ##
#p_roles_path <- '//Users//tommywinfield//Documents//R Data - All//Rebirtha_2023//Data//Pitch_DepthCharts_Feb27.csv'
#h_roles_path <- '//Users//tommywinfield//Documents//R Data - All//Rebirtha_2023//Data//Hit_DepthCharts_Feb127.csv'

## Work Path ##
#p_roles_path <- 'C:\\Users\\TWinfield\\Desktop\\MLB - 2023 PECOTA\\Pitch_DepthCharts_Mar27.csv'
#h_roles_path <- 'C:\\Users\\TWinfield\\Desktop\\MLB - 2023 PECOTA\\Hit_DepthCharts_Mar27.csv'

## GIT Path ##
p_roles_path <- 'https://raw.githubusercontent.com/twinfield10/Rebirtha-Model/main/Data/Pitch_DepthCharts_Mar27.csv'
h_roles_path <- 'https://raw.githubusercontent.com/twinfield10/Rebirtha-Model/main/Data/Hit_DepthCharts_Mar27.csv'

P_Roles_Raw <- read.csv(p_roles_path, header = TRUE, stringsAsFactors = FALSE, encoding="UTF-8")
names(P_Roles_Raw) <- c("Team", "Player", "ROS", "PT_PCT", "Role", "IP_GS")
H_Roles_Raw <- read.csv(h_roles_path, header = TRUE, stringsAsFactors = FALSE, encoding="UTF-8")
names(H_Roles_Raw) <- c("Team", "Batters", "ROS", "PT_PCT")

## Clean Player Roles (Playing Time by Pos) Using Function
P_Roles_Clean <- p_roles_clean()
H_Roles_Clean <- h_roles_clean()

###  # Join Roles Tables To PECOTA Tables | CREATE FULL TABLES # ###
## 1) Test Join To Find Missing Names ##
h_test_match_depth()
p_test_match_depth()

# In Case Players Do Not Match, use this to figure out:
#PITCH_PECOTA_RAW %>% filter(bpid == 105806)

## 2) Create PECOTA FULL Table For Pitchers and Hitters##
HIT_PECOTA_FULL <- match_hit_depth()
PITCH_PECOTA_FULL <- match_pitch_depth()

###################################
##### BUILD WAR SUMMARY CHART #####
###################################

### Create Team WAR by POS ###
war_ref_team <- data.frame(build_war_tbl(), stringsAsFactors = FALSE)

####################################
##### Create ClusterLuck Table #####
####################################
cl <- cluster_luck_func()

#########################################
##### Create PECOTA Standings Table #####
#########################################
standings <- PECOTA_FILE_DL()
#standings <- CL_PECOTA_FILE_ADJ(data = standings)

#########################################################################################################################
####################################END LOAD OF MODEL GEN ENVIRONMENT TABLE##############################################
#########################################################################################################################


###################PLOTS AND DISTRIBUTION LOADING#################
## Build Pitcher Percentile WAR Chart by Position ##
P_WAR_Percentile_Pos <- p_make_long_df()
H_WAR_Percentile_Pos <- h_make_long_df()

## Pivot Longer ##
P_WAR_Percentile_Pos_long <- P_WAR_Percentile_Pos %>%
  pivot_longer(
    cols = starts_with("warp_"),
    names_to = c("warp_name", 'warp_ptile', 'position'),
    names_sep = "_"
  ) %>% 
  select(bpid, name, team, position, warp_ptile, value) %>%
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
  select(bpid, name, team, position, warp_ptile, value) %>%
  group_by(team, position, warp_ptile) %>%
  summarise(
    total_warp_prod = sum(value)
  )
full_team_pos_ptile_long <- full_team_ptile_long_build()


## Load Reference Tables And Vectors ##
## Set up colors and Vectors for All Positions/Divisions ##
all_div = sort(c("AL West", "AL Central", "AL East", "NL West", "NL Central", "NL East"))
all_pos = sort(unique(full_team_pos_ptile_long$position))
APP_RefTbl <- gen_info_build()
Pitch_LB <- load_2022_stats(20230321)