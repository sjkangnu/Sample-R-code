#### Example of R codes
#### Cleaning Survey Data
#### Sep 20, 2022
#### Suji Kang

library(dplyr)
library(sjlabelled)

## load survey dataset
file_path <- "wave2.sav"
wave2 <- haven::read_sav(file_path)

## remove previews
wave2$Status
wave2 <- wave2 %>% 
  dplyr::filter(Status==0) # 0: IP Address

## only those who finished the survey
wave2$Finished
wave2 <- wave2 %>% 
  dplyr::filter(Finished==1)

## only those who have panel id
wave2 <- wave2 %>% 
  dplyr::filter(!PID=="")

## check the num of respondents
length(unique(wave2$PID))

## OK, we have XX observations

## change some column names
wave2 <- wave2 %>% 
  dplyr::rename(conf_econ_dem = conf_econ_dem_1,
         conf_covid1_dem = conf_covid1_dem_6,
         conf_covid2_dem = conf_covid2_dem_6,
         conf_econ_rep = conf_econ_rep_1,
         conf_covid1_rep = conf_covid1_rep_6,
         conf_covid2_rep = conf_covid2_rep_6)
colnames(wave2)

########################################
## merge with the first survey (wave 1)
########################################
## load wave 1 data
file_path <- "wave1_final.sav"
wave1 <- haven::read_sav(file_path)

## merge
# not all respondents who completed the first survey completed the second survey
# thus, N in wave1 > N in wave2.
wave2_merged <- merge(wave2, wave1, by="PID", all.x=T)
dim(wave2_merged)
colnames(wave2_merged)

########################################
## recode pre-treatment covariates
########################################
### party
wave2_merged$party # looks good

### Ideology
wave2_merged$ideology # looks good

### imp_econ
wave2_merged$imp_econ
# need to re-code so that 1 (Not Important) and 5 (Very Important)
wave2_merged <- wave2_merged %>% 
  mutate(imp_econ = case_when(imp_econ==2 ~ 5,
                              imp_econ==3 ~ 4,
                              imp_econ==9 ~ 3,
                              imp_econ==10 ~ 2,
                              imp_econ==11 ~ 1))
wave2_merged$imp_econ

### imp_covid
wave2_merged$imp_covid
wave2_merged <- wave2_merged %>% 
  mutate(imp_covid = case_when(imp_covid==1 ~ 5,
                               imp_covid==2 ~ 4,
                               imp_covid==3 ~ 3,
                               imp_covid==4 ~ 2,
                               imp_covid==5 ~ 1))

#################################
## I removed some R codes which repeat the same recoding for other variables.
#################################
### Religion
wave2_merged$Religion
wave2_merged <- wave2_merged %>% 
  mutate(Religion = case_when(Religion==1 ~ "Protestant",
                              Religion==2 ~ "Catholic",
                              Religion==3 ~ "Jewish",
                              Religion==4 ~ "Muslim",
                              Religion==5 ~ "Hindu",
                              Religion==6 ~ "Other",
                              Religion==7 ~ "Not religious"))

unique(wave2_merged$Religion)

### Gender
wave2_merged$Gender
wave2_merged <- wave2_merged %>% 
  mutate(Gender = case_when(Gender==1 ~ "Male",
                            Gender==2 ~ "Female",
                            Gender==3 ~ "Other",
                            Gender==4 ~ "Other"))
unique(wave2_merged$Gender)

### Race
## Race_1 = white
wave2_merged$Race_1
wave2_merged <- wave2_merged %>%
  mutate(Race_1 = replace(Race_1, is.na(Race_1), 0))

## Race_2 = Black/African American
wave2_merged$Race_2
wave2_merged <- wave2_merged %>%
  mutate(Race_2 = replace(Race_2, is.na(Race_2), 0))

## Race_3 = Hispanic/Latino
wave2_merged$Race_3
wave2_merged <- wave2_merged %>%
  mutate(Race_3 = replace(Race_3, is.na(Race_3), 0))

## Race_4 = Asian/Pacific Islander
wave2_merged$Race_4
wave2_merged <- wave2_merged %>%
  mutate(Race_4 = replace(Race_4, is.na(Race_4), 0))

## Race_5 = Middle Eastern/Northern African
wave2_merged$Race_5
wave2_merged <- wave2_merged %>%
  mutate(Race_5 = replace(Race_5, is.na(Race_5), 0))

## Race_6 = Native American/Alaska Native
wave2_merged$Race_6
wave2_merged <- wave2_merged %>%
  mutate(Race_6 = replace(Race_6, is.na(Race_6), 0))

## Race_7 = Other
wave2_merged$Race_7
wave2_merged <- wave2_merged %>%
  mutate(Race_7 = replace(Race_7, is.na(Race_7), 0))

wave2_merged <- wave2_merged %>%
  mutate(Race_Other = case_when(Race_5==1 | Race_6==1 | Race_7==1 ~ 1,
                                                           TRUE ~ 0))
wave2_merged <- wave2_merged %>% 
  dplyr::rename(race_white = Race_1,
         race_black = Race_2,
         race_hispanic = Race_3,
         race_asian = Race_4,
         race_other = Race_Other)

### Income
wave2_merged$Income

########################################
## convert to longform
########################################
long_df <- tidyr::pivot_longer(wave2_merged, 
                               cols = c("belief_econ_dem","conf_econ_dem",
                                        "belief_covid1_dem", "conf_covid1_dem",
                                        "belief_covid2_dem", "conf_covid2_dem", 
                                        "belief_econ_rep","conf_econ_rep",
                                        "belief_covid1_rep", "conf_covid1_rep",
                                        "belief_covid2_rep", "conf_covid2_rep"), 
                               names_to = c("outcome_type", "topic_question", "party_question"), 
                               names_sep = '_')
dim(long_df)

## convert to wideform again so that outcome type become two separate columns
long_df <- tidyr::spread(long_df, outcome_type, value)

## remove rows whose both belief and conf == 0 (those are automatically created by pivot_longer)
long_df <- long_df %>% tidyr::drop_na(belief, conf)

## save data
write.csv(long_df, "long_df.csv")
sjlabelled::write_spss(long_df, "long_df.sav")



