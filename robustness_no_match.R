#### TRANSPORT ACCESSIBILITY VARIATION AND DECOMPOSITION ####

rm(list = ls())

# libraries
library(sf)
library(mapview)
library(stargazer)
library(plm)
library(R.matlab)
library(RColorBrewer)
library(tidyverse)
library(data.table)

#paths
path_data = "C:/Users/charl/OneDrive/Bureau/GPE2/Data/"
path_outputs = "C:/Users/charl/OneDrive/Bureau/GPE2/Outputs/"

#options
merge_CSP2_CSP3 = TRUE
appariement_emplois = FALSE #false
car_only = FALSE

if (appariement_emplois == TRUE){
  var_match = "match"
}else{
  var_match = "no_match"
}

time_threshold = 40

#Import compositions of neighborhood (residence/employment), by CSP, by year
COM_res <- read.csv(paste(path_data, 'COM_RES_csp_1968_2015.csv', sep = ""))
COM_emp <- read.csv(paste(path_data, 'COM_EMP_csp_1968_2015.csv', sep = ""))

#Import transport times
ttPT <- read.csv(paste(path_data, 'MODUS_public_transport.csv', sep = ""))
ttPV <- read.csv(paste(path_data, 'MODUS_private_cars.csv', sep = ""))

#List years
listYear <- c(1975, 1982, 1990, 1999, 2010, 2015)
listYearLagged <- c(1968, 1975, 1982, 1990, 1999, 2010)

#Aggregate socioeconomic data by group
COM_res_aggr <- COM_res %>% 
  filter(year >= 1968) %>%
  mutate(group_all = csp_2 + csp_3 + csp_4 + csp_5 + csp_6,
         group1 = csp_2, 
         group2 = csp_3, #/group_all,
         group3 = csp_4, #/group_all) #,
         group4 = csp_5,
         group5 = csp_6) 


COM_emp_aggr <- COM_emp %>% 
  filter(year >= 1968) %>% 
  mutate(group1 = csp_2,
         group2 = csp_3, 
         group3 = csp_4,
         group4 = csp_5, 
         group5 = csp_6,
         group_all = group1 + group2 + group3 + group4 + group5,
         dept = floor(CODGEO/1000),
         paris = dept == 75,
         PC = dept %in% c(92,93,94),
         couronne = 3 - 2*paris - 1*PC) %>% 
  dplyr::select(CODGEO, year, group1, group2, group3, group4, group5, group_all, couronne)

#combine transport times
if (car_only == FALSE){
  tt <- ttPT %>% dplyr::select(-tt_GPE) %>% 
    merge(ttPV, by = c('COD_ORIG', 'COD_DEST')) %>% 
    gather('year', 'time_PT', tt1968:tt2010) %>%
    mutate(year = as.numeric(substr(year, 3, 7)),
           time_modes = pmin(time_PV + 12, time_PT)) %>% 
    dplyr::select(COD_ORIG, COD_DEST, year, time_PT, time_modes)
}else{
  tt <- ttPT %>% dplyr::select(-tt_GPE) %>% 
    merge(ttPV, by = c('COD_ORIG', 'COD_DEST')) %>% 
    gather('year', 'time_PT', tt1968:tt2010) %>%
    mutate(year = as.numeric(substr(year, 3, 7)),
           time_modes = time_PV + 12) %>% 
    dplyr::select(COD_ORIG, COD_DEST, year, time_PT, time_modes)
}


### TOTAL REAL ACCESSIBILITY GAINS

#variables Aigt

if (merge_CSP2_CSP3 == TRUE){
  COM_emp_aggr$group2 = COM_emp_aggr$group1 + COM_emp_aggr$group2
  COM_emp_aggr = subset(COM_emp_aggr, select = -c(group1))
  COM_res_aggr$group2 = COM_res_aggr$group1 + COM_res_aggr$group2
  COM_res_aggr = subset(COM_res_aggr, select = -c(group1))
}

com_accessibility_jobs <- data.frame(0,0,0,0,0,0)
for (i in 1:(length(listYearLagged) - 2)){
  if (appariement_emplois == TRUE){
    emp_after <- COM_emp_aggr %>% filter(year == listYearLagged[i+2]) %>% dplyr::select(-year, -group_all, -couronne) %>% 
      gather('group', 'emp_after', group2:group5)}else{
        emp_after <- COM_emp_aggr %>% filter(year == listYearLagged[i+2]) %>% dplyr::select(-year, -group2, -group3, -group4, -group5, -couronne) %>% 
          gather('group', 'emp_after', group_all)
      }
  if (appariement_emplois == TRUE){
    emp_final <- COM_emp_aggr %>% filter(year == listYearLagged[i+1]) %>% dplyr::select(-year, -group_all)}else{
      emp_final <- COM_emp_aggr %>% filter(year == listYearLagged[i+1]) %>% dplyr::select(-year, -group2, -group3, -group4, -group5)
    }
  if (appariement_emplois == TRUE){
    emp_init_long <- COM_emp_aggr %>% filter(year == listYearLagged[i]) %>% dplyr::select(-year, -group_all, -couronne) %>% 
      gather('group', 'emp_init', group2:group5)}else{
        emp_init_long <- COM_emp_aggr %>% filter(year == listYearLagged[i]) %>% dplyr::select(-year, -group2, -group3, -group4, -group5, -couronne) %>% 
          gather('group', 'emp_init', group_all)
      }
  tt_final <- tt %>% filter(year == listYearLagged[i+1]) %>% dplyr::select(-year) %>% mutate(COD_OD = paste(COD_ORIG, COD_DEST, sep = ''))
  tt_after <- tt %>% filter(year == listYearLagged[i+2]) %>% dplyr::select(-year) %>% mutate(COD_OD = paste(COD_ORIG, COD_DEST, sep = ''))
  com_temp <- tt %>% 
    filter(year == listYearLagged[i]) %>% 
    dplyr::select(-year) %>% 
    rename(time_init = time_modes) %>%
    mutate(COD_OD = paste(COD_ORIG, COD_DEST, sep = '')) %>% 
    merge(tt_final, by = 'COD_OD') %>% 
    rename(time_final = time_modes) %>%
    merge(tt_after, by = 'COD_OD') %>% 
    rename(time_after = time_modes) %>%
    merge(emp_final, by.x = 'COD_DEST', by.y = 'CODGEO') %>% 
    gather('group', 'emp_final', group_all) %>%
    merge(emp_init_long, by.x = c('COD_DEST', 'group'), by.y = c('CODGEO', 'group')) %>% 
    merge(emp_after, by.x = c('COD_DEST', 'group'), by.y = c('CODGEO', 'group')) %>% 
    group_by(COD_ORIG, group) %>% 
    summarise(sum_jobs_init = sum(emp_init, na.rm = TRUE),
              sum_jobs_final = sum(emp_final, na.rm = TRUE),
              sum_jobs_after = sum(emp_after, na.rm = TRUE),
              access_emp_final_tt_final_40 = sum(emp_final * (time_final < time_threshold), na.rm = TRUE), #/sum_jobs_final,
              access_emp_init_tt_init_40 = sum(emp_init * (time_init < time_threshold), na.rm = TRUE), #/sum_jobs_init,
              access_emp_after_tt_after_40 = sum(emp_after * (time_after < time_threshold), na.rm = TRUE) #/sum_jobs_init,
    ) %>% 
    dplyr::select(-sum_jobs_final, -sum_jobs_init, -sum_jobs_after) %>%
    data.frame()
  com_temp <- com_temp %>% mutate(year = listYearLagged[i])
  names(com_accessibility_jobs) <- names(com_temp)
  com_accessibility_jobs <- rbind(com_accessibility_jobs, com_temp)
}
com_accessibility_jobs <- com_accessibility_jobs[2:nrow(com_accessibility_jobs),]
rm(emp_after, emp_final, emp_init_long, tt_final, com_temp)

#A(bar)gt

weighted_accessibility <- data.frame(matrix(ncol = 5, nrow = 6))
x <- c("group2", "group3", "group4", "group5")
colnames(weighted_accessibility) <- x
row.names(weighted_accessibility) <- c("1968", "1975", "1982", "1990", "1999", "2010")
for (i in 1:(length(listYearLagged) - 2)){
  for (group in c("group2", "group3", "group4", "group5")){
    year = listYearLagged[i]
    if (appariement_emplois == TRUE){
      com_accessibility_jobs_temp = com_accessibility_jobs[com_accessibility_jobs$group == group,]
      com_accessibility_jobs_temp = com_accessibility_jobs_temp[com_accessibility_jobs_temp$year == year,]}else{
        com_accessibility_jobs_temp = com_accessibility_jobs[com_accessibility_jobs$year == year,]
      }
    COM_res_aggr_temp = COM_res_aggr[COM_res_aggr$year == year, c("CODGEO", group)]
    df_temp = merge(com_accessibility_jobs_temp, COM_res_aggr_temp,  by.x = "COD_ORIG", by.y = "CODGEO")
    df_temp$weighted = df_temp$access_emp_init_tt_init_40 * df_temp[group]
    result = sum(df_temp$weighted, na.rm = TRUE) / sum(df_temp[group], na.rm = TRUE)
    weighted_accessibility[i, group] = result
  }
}

for (group in c("group2", "group3", "group4", "group5")){
  year = "1990"
  if (appariement_emplois == TRUE){
    com_accessibility_jobs_temp = com_accessibility_jobs[com_accessibility_jobs$group == group,]
    com_accessibility_jobs_temp = com_accessibility_jobs_temp[com_accessibility_jobs_temp$year == year,]}else{
      com_accessibility_jobs_temp = com_accessibility_jobs[com_accessibility_jobs$year == year,]
    }
  COM_res_aggr_temp = COM_res_aggr[COM_res_aggr$year == "1999", c("CODGEO", group)]
  df_temp = merge(com_accessibility_jobs_temp, COM_res_aggr_temp,  by.x = "COD_ORIG", by.y = "CODGEO")
  df_temp$weighted = df_temp$access_emp_final_tt_final_40 * df_temp[group]
  result = sum(df_temp$weighted, na.rm = TRUE) / sum(df_temp[group], na.rm = TRUE)
  weighted_accessibility[5, group] = result
}

for (group in c("group2", "group3", "group4", "group5")){
  year = "1990"
  if (appariement_emplois == TRUE){
    com_accessibility_jobs_temp = com_accessibility_jobs[com_accessibility_jobs$group == group,]
    com_accessibility_jobs_temp = com_accessibility_jobs_temp[com_accessibility_jobs_temp$year == year,]}else{
      com_accessibility_jobs_temp = com_accessibility_jobs[com_accessibility_jobs$year == year,]
    }
  COM_res_aggr_temp = COM_res_aggr[COM_res_aggr$year == "2010", c("CODGEO", group)]
  df_temp = merge(com_accessibility_jobs_temp, COM_res_aggr_temp,  by.x = "COD_ORIG", by.y = "CODGEO")
  df_temp$weighted = df_temp$access_emp_after_tt_after_40 * df_temp[group]
  result = sum(df_temp$weighted, na.rm = TRUE) / sum(df_temp[group], na.rm = TRUE)
  weighted_accessibility[6, group] = result
}

#Compute accessibility variations

delta_accessibility = weighted_accessibility
delta_accessibility[1,] = NaN
for (i in 2:(length(listYearLagged))){
  delta_accessibility[i,] = weighted_accessibility[i,] - weighted_accessibility[i-1,]
}

delta_accessibility_percent = weighted_accessibility
delta_accessibility_percent[1,] = NaN
for (i in 2:(length(listYearLagged))){
  delta_accessibility_percent[i,] = 100 * (weighted_accessibility[i,] - weighted_accessibility[i-1,]) / weighted_accessibility[i-1,]
}

### ISOLATE THE EFFECT OF JOB GROWTH (RELATIVE TO T+1)

#variables Aigt

accesibility_isolate_job_growth <- data.frame(0,0,0,0,0)
for (i in 1:(length(listYearLagged) - 2)){
  if (appariement_emplois == TRUE){
    emp_after <- COM_emp_aggr %>% filter(year == listYearLagged[i+2]) %>% dplyr::select(-year, -group_all, -couronne) %>% 
      gather('group', 'emp_after', group2:group5)}else{
        emp_after <- COM_emp_aggr %>% filter(year == listYearLagged[i+2]) %>% dplyr::select(-year, -group2, -group3, -group4, -group5, -couronne) %>% 
          gather('group', 'emp_after', group_all)
      }
  if (appariement_emplois == TRUE){
    emp_final <- COM_emp_aggr %>% filter(year == listYearLagged[i+1]) %>% dplyr::select(-year, -group_all)}else{
      emp_final <- COM_emp_aggr %>% filter(year == listYearLagged[i+1]) %>% dplyr::select(-year, -group2, -group3, -group4, -group5)
    }
  if (appariement_emplois == TRUE){
    emp_init_long <- COM_emp_aggr %>% filter(year == listYearLagged[i]) %>% dplyr::select(-year, -group_all, -couronne) %>% 
      gather('group', 'emp_init', group2:group5)}else{
        emp_init_long <- COM_emp_aggr %>% filter(year == listYearLagged[i]) %>% dplyr::select(-year, -group2, -group3, -group4, -group5, -couronne) %>% 
          gather('group', 'emp_init', group_all)
      }
  tt_final <- tt %>% filter(year == listYearLagged[i+1]) %>% dplyr::select(-year) %>% mutate(COD_OD = paste(COD_ORIG, COD_DEST, sep = ''))
  tt_after <- tt %>% filter(year == listYearLagged[i+2]) %>% dplyr::select(-year) %>% mutate(COD_OD = paste(COD_ORIG, COD_DEST, sep = ''))
  com_temp <- tt %>% 
    filter(year == listYearLagged[i]) %>% 
    dplyr::select(-year) %>% 
    rename(time_init = time_modes) %>%
    mutate(COD_OD = paste(COD_ORIG, COD_DEST, sep = '')) %>% 
    merge(tt_final, by = 'COD_OD') %>% 
    rename(time_final = time_modes) %>%
    merge(tt_after, by = 'COD_OD') %>% 
    rename(time_after = time_modes) %>%
    merge(emp_final, by.x = 'COD_DEST', by.y = 'CODGEO') %>% 
    gather('group', 'emp_final', group_all) %>%
    merge(emp_init_long, by.x = c('COD_DEST', 'group'), by.y = c('CODGEO', 'group')) %>% 
    merge(emp_after, by.x = c('COD_DEST', 'group'), by.y = c('CODGEO', 'group')) %>% 
    group_by(COD_ORIG, group) %>% 
    summarise(sum_jobs_init = sum(emp_init, na.rm = TRUE),
              sum_jobs_final = sum(emp_final, na.rm = TRUE),
              sum_jobs_after = sum(emp_after, na.rm = TRUE),
              access_emp_final_tt_final_40 = sum(emp_final * (sum_jobs_init / sum_jobs_final) * (time_final < time_threshold), na.rm = TRUE), #/sum_jobs_final,
              access_emp_after_tt_after_40 = sum(emp_after* (sum_jobs_final / sum_jobs_after) * (time_after < time_threshold), na.rm = TRUE) #/sum_jobs_init,
    ) %>% 
    dplyr::select(-sum_jobs_final, -sum_jobs_init, -sum_jobs_after) %>%
    data.frame()
  com_temp <- com_temp %>% mutate(year = listYearLagged[i])
  names(accesibility_isolate_job_growth) <- names(com_temp)
  accesibility_isolate_job_growth <- rbind(accesibility_isolate_job_growth, com_temp)
}
accesibility_isolate_job_growth <- accesibility_isolate_job_growth[2:nrow(accesibility_isolate_job_growth),]
rm(emp_after, emp_final, emp_init_long, tt_final, com_temp)

#Calcul des variables A(bar)gt

weighted_accessibility_isolate_job_growth <- data.frame(matrix(ncol = 5, nrow = 6))
x <- c("group2", "group3", "group4", "group5")
colnames(weighted_accessibility_isolate_job_growth) <- x
row.names(weighted_accessibility_isolate_job_growth) <- c("1968", "1975", "1982", "1990", "1999", "2010")
for (i in 1:(length(listYearLagged) - 2)){
  for (group in c("group2", "group3", "group4", "group5")){
    year = listYearLagged[i]
    if (appariement_emplois == TRUE){
      com_accessibility_jobs_temp = accesibility_isolate_job_growth[accesibility_isolate_job_growth$group == group,]
      com_accessibility_jobs_temp = com_accessibility_jobs_temp[com_accessibility_jobs_temp$year == year,]}else{
        com_accessibility_jobs_temp = accesibility_isolate_job_growth[accesibility_isolate_job_growth$year == year,]
      }
    COM_res_aggr_temp = COM_res_aggr[COM_res_aggr$year == listYearLagged[i+1], c("CODGEO", group)]
    df_temp = merge(com_accessibility_jobs_temp, COM_res_aggr_temp,  by.x = "COD_ORIG", by.y = "CODGEO")
    df_temp$weighted = df_temp$access_emp_final_tt_final_40 * df_temp[group]
    result = sum(df_temp$weighted, na.rm = TRUE) / sum(df_temp[group], na.rm = TRUE)
    weighted_accessibility_isolate_job_growth[i+1, group] = result
  }
}


for (group in c("group2", "group3", "group4", "group5")){
  year = "1990"
  if (appariement_emplois == TRUE){
    com_accessibility_jobs_temp = accesibility_isolate_job_growth[accesibility_isolate_job_growth$group == group,]
    com_accessibility_jobs_temp = com_accessibility_jobs_temp[com_accessibility_jobs_temp$year == year,]}else{
      com_accessibility_jobs_temp = accesibility_isolate_job_growth[accesibility_isolate_job_growth$year == year,]
    }
  
  COM_res_aggr_temp = COM_res_aggr[COM_res_aggr$year == "2010", c("CODGEO", group)]
  df_temp = merge(com_accessibility_jobs_temp, COM_res_aggr_temp,  by.x = "COD_ORIG", by.y = "CODGEO")
  df_temp$weighted = df_temp$access_emp_after_tt_after_40 * df_temp[group]
  result = sum(df_temp$weighted, na.rm = TRUE) / sum(df_temp[group], na.rm = TRUE)
  weighted_accessibility_isolate_job_growth[6, group] = result
}


### ISOLATE THE EFFECT OF SPEED (RELATIVE TO T)

#variables Aigt

accesibility_isolate_speed <- data.frame(0,0,0,0,0)
for (i in 1:(length(listYearLagged) - 2)){
  if (appariement_emplois == TRUE){
    emp_after <- COM_emp_aggr %>% filter(year == listYearLagged[i+2]) %>% dplyr::select(-year, -group_all, -couronne) %>% 
      gather('group', 'emp_after', group2:group5)}else{
        emp_after <- COM_emp_aggr %>% filter(year == listYearLagged[i+2]) %>% dplyr::select(-year, -group2, -group3, -group4, -group5, -couronne) %>% 
          gather('group', 'emp_after', group_all)
      }
  if (appariement_emplois == TRUE){
    emp_final <- COM_emp_aggr %>% filter(year == listYearLagged[i+1]) %>% dplyr::select(-year, -group_all)}else{
      emp_final <- COM_emp_aggr %>% filter(year == listYearLagged[i+1]) %>% dplyr::select(-year, -group2, -group3, -group4, -group5)
    }
  if (appariement_emplois == TRUE){
    emp_init_long <- COM_emp_aggr %>% filter(year == listYearLagged[i]) %>% dplyr::select(-year, -group_all, -couronne) %>% 
      gather('group', 'emp_init', group2:group5)}else{
        emp_init_long <- COM_emp_aggr %>% filter(year == listYearLagged[i]) %>% dplyr::select(-year, -group2, -group3, -group4, -group5, -couronne) %>% 
          gather('group', 'emp_init', group_all)
      }
  tt_final <- tt %>% filter(year == listYearLagged[i+1]) %>% dplyr::select(-year) %>% mutate(COD_OD = paste(COD_ORIG, COD_DEST, sep = ''))
  tt_after <- tt %>% filter(year == listYearLagged[i+2]) %>% dplyr::select(-year) %>% mutate(COD_OD = paste(COD_ORIG, COD_DEST, sep = ''))
  com_temp <- tt %>% 
    filter(year == listYearLagged[i]) %>% 
    dplyr::select(-year) %>% 
    rename(time_init = time_modes) %>%
    mutate(COD_OD = paste(COD_ORIG, COD_DEST, sep = '')) %>% 
    merge(tt_final, by = 'COD_OD') %>% 
    rename(time_final = time_modes) %>%
    merge(tt_after, by = 'COD_OD') %>% 
    rename(time_after = time_modes) %>%
    merge(emp_final, by.x = 'COD_DEST', by.y = 'CODGEO') %>% 
    gather('group', 'emp_final', group_all) %>%
    merge(emp_init_long, by.x = c('COD_DEST', 'group'), by.y = c('CODGEO', 'group')) %>% 
    merge(emp_after, by.x = c('COD_DEST', 'group'), by.y = c('CODGEO', 'group')) %>% 
    group_by(COD_ORIG, group) %>% 
    summarise(sum_jobs_init = sum(emp_init, na.rm = TRUE),
              sum_jobs_final = sum(emp_final, na.rm = TRUE),
              sum_jobs_after = sum(emp_after, na.rm = TRUE),
              access_emp_final_tt_final_40 = sum(emp_init * (time_final < time_threshold), na.rm = TRUE), #/sum_jobs_final,
              access_emp_after_tt_after_40 = sum(emp_final * (time_after < time_threshold), na.rm = TRUE), #/sum_jobs_init
              
    ) %>% 
    dplyr::select(-sum_jobs_final, -sum_jobs_init, -sum_jobs_after) %>%
    data.frame()
  com_temp <- com_temp %>% mutate(year = listYearLagged[i])
  names(accesibility_isolate_speed) <- names(com_temp)
  accesibility_isolate_speed <- rbind(accesibility_isolate_speed, com_temp)
}
accesibility_isolate_speed <- accesibility_isolate_speed[2:nrow(accesibility_isolate_speed),]
rm(emp_after, emp_final, emp_init_long, tt_final, com_temp)

#variables A(bar)gt

weighted_accesibility_isolate_speed <- data.frame(matrix(ncol = 5, nrow = 6))
x <- c("group2", "group3", "group4", "group5")
colnames(weighted_accesibility_isolate_speed) <- x
row.names(weighted_accesibility_isolate_speed) <- c("1968", "1975", "1982", "1990", "1999", "2010")
for (i in 1:(length(listYearLagged) - 2)){
  for (group in c( "group2", "group3", "group4", "group5")){
    year = listYearLagged[i]
    if (appariement_emplois == TRUE){
      com_accessibility_jobs_temp = accesibility_isolate_speed[accesibility_isolate_speed$group == group,]
      com_accessibility_jobs_temp = com_accessibility_jobs_temp[com_accessibility_jobs_temp$year == year,]}else{
        com_accessibility_jobs_temp = accesibility_isolate_speed[accesibility_isolate_speed$year == year,]
      }
    COM_res_aggr_temp = COM_res_aggr[COM_res_aggr$year == listYearLagged[i], c("CODGEO", group)]
    df_temp = merge(com_accessibility_jobs_temp, COM_res_aggr_temp,  by.x = "COD_ORIG", by.y = "CODGEO")
    df_temp$weighted = df_temp$access_emp_final_tt_final_40 * df_temp[group]
    result = sum(df_temp$weighted, na.rm = TRUE) / sum(df_temp[group], na.rm = TRUE)
    weighted_accesibility_isolate_speed[i, group] = result
  }
}


for (group in c("group2", "group3", "group4", "group5")){
  year = "1990"
  if (appariement_emplois == TRUE){
    com_accessibility_jobs_temp = accesibility_isolate_speed[accesibility_isolate_speed$group == group,]
    com_accessibility_jobs_temp = com_accessibility_jobs_temp[com_accessibility_jobs_temp$year == year,]}else{
      com_accessibility_jobs_temp = accesibility_isolate_speed[accesibility_isolate_speed$year == year,]
    }
  COM_res_aggr_temp = COM_res_aggr[COM_res_aggr$year == "1999", c("CODGEO", group)]
  df_temp = merge(com_accessibility_jobs_temp, COM_res_aggr_temp,  by.x = "COD_ORIG", by.y = "CODGEO")
  df_temp$weighted = df_temp$access_emp_after_tt_after_40 * df_temp[group]
  result = sum(df_temp$weighted, na.rm = TRUE) / sum(df_temp[group], na.rm = TRUE)
  weighted_accesibility_isolate_speed[5, group] = result
}

#Decomposition of proximity

weighted_accesibility_isolate_proximity_hh_only <- data.frame(matrix(ncol = 5, nrow = 6))
x <- c("group2", "group3", "group4", "group5")
colnames(weighted_accesibility_isolate_proximity_hh_only) <- x
row.names(weighted_accesibility_isolate_proximity_hh_only) <- c("1968", "1975", "1982", "1990", "1999", "2010")
for (i in 1:(length(listYearLagged) - 2)){
  for (group in c( "group2", "group3", "group4", "group5")){
    year = listYearLagged[i]
    if (appariement_emplois == TRUE){
      com_accessibility_jobs_temp = accesibility_isolate_speed[accesibility_isolate_speed$group == group,]
      com_accessibility_jobs_temp = com_accessibility_jobs_temp[com_accessibility_jobs_temp$year == year,]}else{
        com_accessibility_jobs_temp = accesibility_isolate_speed[accesibility_isolate_speed$year == year,]
      }
    COM_res_aggr_temp = COM_res_aggr[COM_res_aggr$year == listYearLagged[i+1], c("CODGEO", group)]
    df_temp = merge(com_accessibility_jobs_temp, COM_res_aggr_temp,  by.x = "COD_ORIG", by.y = "CODGEO")
    df_temp$weighted = df_temp$access_emp_final_tt_final_40 * df_temp[group]
    result = sum(df_temp$weighted, na.rm = TRUE) / sum(df_temp[group], na.rm = TRUE)
    weighted_accesibility_isolate_proximity_hh_only[i, group] = result
  }
}


for (group in c("group2", "group3", "group4", "group5")){
  year = "1990"
  if (appariement_emplois == TRUE){
    com_accessibility_jobs_temp = accesibility_isolate_speed[accesibility_isolate_speed$group == group,]
    com_accessibility_jobs_temp = com_accessibility_jobs_temp[com_accessibility_jobs_temp$year == year,]}else{
      com_accessibility_jobs_temp = accesibility_isolate_speed[accesibility_isolate_speed$year == year,]
    }
  COM_res_aggr_temp = COM_res_aggr[COM_res_aggr$year == "2010", c("CODGEO", group)]
  df_temp = merge(com_accessibility_jobs_temp, COM_res_aggr_temp,  by.x = "COD_ORIG", by.y = "CODGEO")
  df_temp$weighted = df_temp$access_emp_after_tt_after_40 * df_temp[group]
  result = sum(df_temp$weighted, na.rm = TRUE) / sum(df_temp[group], na.rm = TRUE)
  weighted_accesibility_isolate_proximity_hh_only[5, group] = result
}

##### ADDITIVE DECOMPOSITION

diff_proximity = weighted_accessibility_isolate_job_growth
diff_proximity[1,] = NaN
for (i in 2:(length(listYearLagged))){
  diff_proximity[i,] = 100 * (weighted_accessibility_isolate_job_growth[i,] - weighted_accesibility_isolate_speed[i-1,]) / weighted_accessibility[1,] 
}

diff_proximity_hh = weighted_accesibility_isolate_proximity_hh_only
diff_proximity_hh[1,] = NaN
for (i in 1:(length(listYearLagged))){
  diff_proximity_hh[i,] = 100 * (weighted_accesibility_isolate_proximity_hh_only[i,] - weighted_accesibility_isolate_speed[i,]) / weighted_accessibility[1,] 
}

diff_proximity_jobs = weighted_accesibility_isolate_proximity_hh_only
diff_proximity_jobs[1,] = NaN
for (i in 2:(length(listYearLagged))){
  diff_proximity_jobs[i,] = 100 * (weighted_accessibility_isolate_job_growth[i,] - weighted_accesibility_isolate_proximity_hh_only[i-1,]) / weighted_accessibility[1,] 
}

diff_proximity = weighted_accessibility_isolate_job_growth
diff_proximity[1,] = NaN
for (i in 2:(length(listYearLagged))){
  diff_proximity[i,] = 100 * (weighted_accessibility_isolate_job_growth[i,] - weighted_accesibility_isolate_speed[i-1,]) / weighted_accessibility[1,] 
}

diff_speed = weighted_accesibility_isolate_speed
diff_speed[1,] = NaN
for (i in 1:(length(listYearLagged))){
  diff_speed[i,] = 100 * (weighted_accesibility_isolate_speed[i,] - weighted_accessibility[i,]) / weighted_accessibility[1,] 
}

diff_jg = weighted_accessibility_isolate_job_growth
diff_jg[1,] = NaN
for (i in 2:(length(listYearLagged))){
  diff_jg[i,] = 100 * (weighted_accessibility[i,] - weighted_accessibility_isolate_job_growth[i,]) / weighted_accessibility[1,] 
}

diff_jg = diff_jg[2:6, 1:4]
diff_jg$year = c("1968-1975", "1975-1982", "1982-1990", "1990-1999", "1999-2010")
diff_jg$name = 'Job growth (%)'

diff_speed = diff_speed[1:5, 1:4]
diff_speed$year = c("1968-1975", "1975-1982", "1982-1990", "1990-1999", "1999-2010")
diff_speed$name = 'Speed (%)'

diff_proximity = diff_proximity[2:6, 1:4]
diff_proximity$year = c("1968-1975", "1975-1982", "1982-1990", "1990-1999", "1999-2010")
diff_proximity$name = 'Proximity (%)'

diff_proximity_jobs = diff_proximity_jobs[2:6, 1:4]
diff_proximity_jobs$year = c("1968-1975", "1975-1982", "1982-1990", "1990-1999", "1999-2010")
diff_proximity_jobs$name = 'Proximity - jobs (%)'

diff_proximity_hh = diff_proximity_hh[1:5, 1:4]
diff_proximity_hh$year = c("1968-1975", "1975-1982", "1982-1990", "1990-1999", "1999-2010")
diff_proximity_hh$name = 'Proximity - hh (%)'

df = rbind(diff_jg, diff_speed, diff_proximity, diff_proximity_jobs, diff_proximity_hh)

write.csv(df, paste0(path_outputs,var_match,'_',time_threshold,'_additive.csv'))

write.csv(weighted_accessibility[,1:4], paste0(path_outputs,var_match,'_',time_threshold,'_weighted_accessibility.csv'))

### MULTIPLICATIVE DECOMPOSITION

diff_proximity = weighted_accessibility_isolate_job_growth
diff_proximity[1,] = NaN
for (i in 2:(length(listYearLagged))){
  diff_proximity[i,] = 100 * (weighted_accessibility_isolate_job_growth[i,] - weighted_accesibility_isolate_speed[i-1,]) / weighted_accesibility_isolate_speed[i-1,] 
}

diff_proximity_hh = weighted_accesibility_isolate_proximity_hh_only
diff_proximity_hh[1,] = NaN
for (i in 1:(length(listYearLagged))){
  diff_proximity_hh[i,] = 100 * (weighted_accesibility_isolate_proximity_hh_only[i,] - weighted_accesibility_isolate_speed[i,]) / weighted_accesibility_isolate_speed[i,] 
}

diff_proximity_jobs = weighted_accesibility_isolate_proximity_hh_only
diff_proximity_jobs[1,] = NaN
for (i in 2:(length(listYearLagged))){
  diff_proximity_jobs[i,] = 100 * (weighted_accessibility_isolate_job_growth[i,] - weighted_accesibility_isolate_proximity_hh_only[i-1,]) / weighted_accesibility_isolate_proximity_hh_only[i-1,]
}

diff_proximity = weighted_accessibility_isolate_job_growth
diff_proximity[1,] = NaN
for (i in 2:(length(listYearLagged))){
  diff_proximity[i,] = 100 * (weighted_accessibility_isolate_job_growth[i,] - weighted_accesibility_isolate_speed[i-1,]) / weighted_accesibility_isolate_speed[i-1,]
}

diff_speed = weighted_accesibility_isolate_speed
diff_speed[1,] = NaN
for (i in 1:(length(listYearLagged))){
  diff_speed[i,] = 100 * (weighted_accesibility_isolate_speed[i,] - weighted_accessibility[i,]) / weighted_accessibility[i,]
}

diff_jg = weighted_accessibility_isolate_job_growth
diff_jg[1,] = NaN
for (i in 2:(length(listYearLagged))){
  diff_jg[i,] = 100 * (weighted_accessibility[i,] - weighted_accessibility_isolate_job_growth[i,]) / weighted_accessibility_isolate_job_growth[i,]
}

diff_jg = diff_jg[2:6, 1:4]
diff_jg$year = c("1968-1975", "1975-1982", "1982-1990", "1990-1999", "1999-2010")
diff_jg$name = 'Job growth (%)'

diff_speed = diff_speed[1:5, 1:4]
diff_speed$year = c("1968-1975", "1975-1982", "1982-1990", "1990-1999", "1999-2010")
diff_speed$name = 'Speed (%)'

diff_proximity = diff_proximity[2:6, 1:4]
diff_proximity$year = c("1968-1975", "1975-1982", "1982-1990", "1990-1999", "1999-2010")
diff_proximity$name = 'Proximity (%)'

diff_proximity_jobs = diff_proximity_jobs[2:6, 1:4]
diff_proximity_jobs$year = c("1968-1975", "1975-1982", "1982-1990", "1990-1999", "1999-2010")
diff_proximity_jobs$name = 'Proximity - jobs (%)'

diff_proximity_hh = diff_proximity_hh[1:5, 1:4]
diff_proximity_hh$year = c("1968-1975", "1975-1982", "1982-1990", "1990-1999", "1999-2010")
diff_proximity_hh$name = 'Proximity - hh (%)'

df = rbind(diff_jg, diff_speed, diff_proximity, diff_proximity_jobs, diff_proximity_hh)

write.csv(df, paste0(path_outputs,var_match,'_',time_threshold,'_multiplicative.csv'))
