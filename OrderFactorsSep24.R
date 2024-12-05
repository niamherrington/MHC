# Order factor levels

#libraries
library(tidyverse)
library(readr)
library(readxl)
library(data.table)

QuestionKey <- read_excel("QuestionKey.xlsx") %>% dplyr::rename(variable = "Identifier")

us_cohort <- read_csv("2024_us_cohort.csv")

MHQ_long_sep24 <- read_csv("MHQ_long_sep24.csv") %>% left_join(., us_cohort, by = "healthCode")

#Filter to only US cohort (patients defined as having a group in us_cohort)
US_pats <- MHQ_long_sep24 %>% filter(!is.na(group))

#1st entry for each patient in each q (Check healthCode is unique patients, NOT multiple codes per patient). Filter NAs??
US_1st <- US_pats %>%
  arrange(dayInStudy) %>%
  group_by(healthCode, Question) %>%
  filter(dayInStudy == min(dayInStudy)) %>%
  filter(!is.na(first(Question)) | row_number() == 1) %>%
  slice(1) %>%
  ungroup()

variable_list_illnessmindset <- QuestionKey[QuestionKey$Sheet == "Illness_mindset_inventory",2:4]

variable_list_activity <- QuestionKey[QuestionKey$Sheet == "Adequacy_of_activity_mindset_me", 2:4]

variable_list_exproc <- QuestionKey[QuestionKey$Sheet == "Exercise_process_mindset_measur", 2:4]

likert_levels_IM <- c(
  "Strongly Disagree",
  "Disagree",
  "Somewhat Disagree",
  "Somewhat Agree",
  "Agree",
  "Strongly Agree")

likert_levels_agree1 <- c(
  "Strongly Disagree",
  "Disagree",
  "Somewhat Disagree",
  "Neither agree or disagree",
  "Somewhat Agree",
  "Agree",
  "Strongly Agree")

likert_levels_risk <- c(
  "Not at all",
  "A little",
  "Moderately",
  "A lot",
  "Extremely")

likert_levels_risk2 <- c(
  "Much higher than average",
  "Higher than average",
  "Average",
  "Lower than average",
  "Much lower than average")


US_1stWide <- US_1st %>% dplyr::select(-dayInStudy, -createdOn) %>% pivot_wider(., names_from = Question, values_from = value) %>% mutate_all( ~replace(., lengths(.)==0, NA)) %>% mutate_at(c('moderate_act', 'vigorous_act', 'feel_worthwhile1', 'feel_worthwhile2', 'feel_worthwhile3', 'feel_worthwhile4', 'satisfiedwith_life', 'happiness', 'sleep_time', 'sleep_time1', 'fruit', 'vegetable', 'fish', 'grains', 'sugar_drinks'), as.numeric) 

US_1stWide <-  US_1stWide %>% mutate(across(all_of(variable_list_illnessmindset$Identifier),~factor(.,levels = likert_levels_IM)))  %>% mutate(easy = factor(easy, levels = c("Very difficult",
                                  "Somewhat difficult",
                                  "Somewhat easy",
                                  "Very easy")), 
                                                                                              relaxing = factor(relaxing, levels =  c(  "Very stressful",
                                             "Somewhat stressful",
                                             "Somewhat relaxing",
                                              "Very relaxing")),
                                                                                                                                                                                             social = factor(social, levels = c("Very lonely",
                                      "Somewhat lonely",
                                      "Somewhat social",
                                      "Very social")),
                                                                                                  indulgent = factor(indulgent, levels = c("Very depriving",
                                             "Somewhat depriving",
                                             "Somewhat indulgent",                                                                        "Very indulgent")),
                                                                                                                                                                                   pleasurable = factor(pleasurable, levels = c("Very unpleasant",
                                                 "Somewhat unpleasant",
                                                "Somewhat pleasurable",
                                                "Very pleasurable")),
                                                                                                convenient = factor(convenient, levels = c("Very inconvenient",
                                              "Somewhat inconvenient",
                                              "Somewhat convenient",
                                              "Very convenient")),
                                                                                              fun = factor(fun, levels = c("Very boring",
                                "Somewhat boring",
                                "Somewhat fun",
                                "Very fun"))  ) %>% 
  mutate_at(c("unhealthy", "weight"),~factor(.,levels = likert_levels_agree1) ) %>% mutate(beneficial = factor(beneficial, levels = c("Very harmful for my health", 
                                             "Moderately harmful for my health",
                                             "Slightly harmful for my health",
                                            "Neither harmful nor beneficial for my health",
                                             "Moderately beneficial for my health",
                                             "Very beneficial for my health",
                                            "Extremely beneficial for my health")),
                                                                                           disease = factor(disease, levels = c("Increases my risk very much", 
                                    "Increases my risk moderately",
                                   "Increases my risk slightly",
                                   "Neither increases nor decreases my risk",
                                   "Decreases my risk slightly",
                                   "Decreases my risk moderately",
                                   "Decreases my risk very much")),
                                                                                           muscles = factor(muscles, levels = c("Weakening very much", 
                                     "Weakening moderately",
                                     "Weakening slightly",
                                     "Neither strengthening nor weakening",
                                     "Strengthening slightly",
                                     "Strengthening moderately",
                                     "Strengthening very much")),
                                                                                           phys_activity = factor(phys_activity, levels = c("I did not do much physical activity",                 "Once or twice a week, I did light activities",
           "About three times a week, I did moderate activities",
           "About three times a week, I did vigorous activities",
           "Almost daily, that is five or more times a week, I did moderate activities",
            "Almost daily, that is, five or more times a week, I did vigorous activities" ) ),
                                                                                           atwork = factor(atwork, levels = c("I spent most of the day sitting or standing",
                                                                                                                              "I spent most of the day walking or using my hands and arms in work that required moderate exertion",
                                                                                                                              "I spent most of the day lifting or carrying heavy objects or moving most of my body in some other way" ))) %>% 
  mutate_at(c("riskfactors1", "riskfactors3"),~factor(.,levels = likert_levels_risk) ) %>%   mutate_at(c("riskfactors2", "riskfactors4"),~factor(.,levels = likert_levels_risk2) ) %>% 
  mutate(current_nicotine = case_when(currentSmoking %in% c("daily", "less than daily") | currentVaping %in% c("daily", "less than daily") | currentSmokeless %in% c("daily", "less than daily") ~ "TRUE",
                                      currentSmoking == "not at all" & currentVaping == "not at all" & currentSmokeless == "not at all" ~ "FALSE",    
                                      currentSmoking == "don't know" & currentVaping == "don't know" & currentSmokeless == "don't know" ~ NA, .default = NA))  %>% mutate_at(c("heartCondition", "chestPain", "chestPainInLastMonth", "dizziness", "jointProblem", "physicallyCapable", "prescriptionDrugs", "work", "sleep_diagnosis1", "current_nicotine" ), ~factor(., levels = c("TRUE", "FALSE")))

US_1stWide$vigorous_act <- ifelse(US_1stWide$vigorous_act > 1000, NA, US_1stWide$vigorous_act)

US_1stWide$sleep_time1 <- ifelse(US_1stWide$sleep_time1 > 12, NA, US_1stWide$sleep_time1)

US_1stWide$sleep_time <- ifelse(US_1stWide$sleep_time > 12, NA, US_1stWide$sleep_time)

US_1st_num <- US_1stWide 

TF_vars <- c("heartCondition", "chestPain", "chestPainInLastMonth", "dizziness", "jointProblem", "physicallyCapable", "prescriptionDrugs", "work", "sleep_diagnosis1", "current_nicotine" )

ordinal_vars <- c(variable_list_illnessmindset$Identifier, "atwork","phys_activity", "muscles", "disease",  "beneficial", "easy", "unhealthy", "weight", "relaxing", "social", "indulgent", 'pleasurable', "convenient", "fun",  'riskfactors1', 'riskfactors2', 'riskfactors3', 'riskfactors4')

num_vars <- c('moderate_act', 'vigorous_act', 'feel_worthwhile1', 'feel_worthwhile2', 'feel_worthwhile3', 'feel_worthwhile4', 'satisfiedwith_life', 'happiness', 'sleep_time', 'sleep_time1', 'fruit', 'vegetable', 'fish', 'grains', 'sugar_drinks')

US_1st_num[ordinal_vars] <- lapply(US_1st_num[ordinal_vars], function(x) as.numeric(as.ordered(x)))
US_1st_num[TF_vars] <- lapply(US_1st_num[TF_vars], function(x) as.numeric(as.ordered(x)))

a <- sapply(US_1st_num, class) %>% as.data.frame()

US_1stWide_clus <- US_1stWide %>% dplyr::select(all_of(ordinal_vars), all_of(TF_vars), group, healthCode) 

columns_to_keep <- c("group", "healthCode")

#Create a logical condition for rows where all columns except the specified ones are NA
condition <- apply(US_1stWide_clus[ , !(names(US_1stWide_clus) %in% columns_to_keep)], 1, function(row) !all(is.na(row)))

# Subset the data frame based on the condition
US_1stWide_clus <- US_1stWide_clus[condition, ]

#write_csv(US_1stWide_clus, "MHQ_ord_TF_1st.csv") (cleaned in OpenRefine)
#US_1stWide_clus <- read_csv("MHQ_ord_TF_1st.csv")


US_1stWide_clus <- US_1stWide_clus  %>% mutate(across(all_of(variable_list_illnessmindset$Identifier),~factor(.,levels = likert_levels_IM)))   %>% mutate(easy = factor(easy, levels = c("Very difficult", "Somewhat difficult","Somewhat easy","Very easy")),                                                                                                        relaxing = factor(relaxing, levels =  c(  "Very stressful", "Somewhat stressful","Somewhat relaxing","Very relaxing")),
                                                                                                                                                                  social = factor(social, levels = c("Very lonely","Somewhat lonely","Somewhat social","Very social")),                                                                                                indulgent = factor(indulgent, levels = c("Very depriving","Somewhat depriving", "Somewhat indulgent", "Very indulgent")),
                                                                                                                                                                  pleasurable = factor(pleasurable, levels = c("Very unpleasant","Somewhat unpleasant","Somewhat pleasurable","Very pleasurable")),                                                             convenient = factor(convenient, levels = c("Very inconvenient","Somewhat inconvenient","Somewhat convenient","Very convenient")),
                                                                                                                                                                  fun = factor(fun, levels = c("Very boring", "Somewhat boring","Somewhat fun", "Very fun"))  ) %>% 
  mutate_at(c("unhealthy", "weight"),~factor(.,levels = likert_levels_agree1) ) %>% mutate(beneficial = factor(beneficial, levels = c("Very harmful for my health", "Moderately harmful for my health", "Slightly harmful for my health","Neither harmful nor beneficial for my health","Moderately beneficial for my health", "Very beneficial for my health","Extremely beneficial for my health")),
                                                                                           disease = factor(disease, levels = c("Increases my risk very much", "Increases my risk moderately", "Increases my risk slightly", "Neither increases nor decreases my risk", "Decreases my risk slightly", "Decreases my risk moderately", "Decreases my risk very much")),
                                                                                           muscles = factor(muscles, levels = c("Weakening very much", "Weakening moderately", "Weakening slightly", "Neither strengthening nor weakening", "Strengthening slightly", "Strengthening moderately", "Strengthening very much")),                                                             phys_activity = factor(phys_activity, levels = c("I did not do much physical activity",                 "Once or twice a week, I did light activities", "About three times a week, I did moderate activities", "About three times a week, I did vigorous activities", "Almost daily, that is five or more times a week, I did moderate activities", "Almost daily, that is, five or more times a week, I did vigorous activities" ) ),
                                                                                           atwork = factor(atwork, levels = c("I spent most of the day sitting or standing", "I spent most of the day walking or using my hands and arms in work that required moderate exertion","I spent most of the day lifting or carrying heavy objects or moving most of my body in some other way" ))) %>% 
  mutate_at(c("riskfactors1", "riskfactors3"),~factor(.,levels = likert_levels_risk) ) %>%   mutate_at(c("riskfactors2", "riskfactors4"),~factor(.,levels = likert_levels_risk2) ) %>%  mutate_at(c("heartCondition", "chestPain", "chestPainInLastMonth", "dizziness", "jointProblem", "physicallyCapable", "prescriptionDrugs", "work", "sleep_diagnosis1", "current_nicotine" ), ~factor(., levels = c("TRUE", "FALSE")))


rm(likert_levels_IM)
rm(likert_levels_agree1)
rm(likert_levels_risk)
rm(likert_levels_risk2)
rm(a)