# Order factor levels

#libraries
library(tidyverse)
library(readr)
library(readxl)
library(data.table)

QuestionKey <- read_excel("QuestionKey.xlsx") %>% dplyr::rename(variable = "Identifier")

us_cohort <- read_csv("2024_us_cohort.csv")

MasterMHC_IDs_Dx_Mar2024 <- read_excel("Data/MasterMHC_IDs_Dx_Mar2024.xlsx")

#Filter to only non-US cohort 
Other_pats <- read_csv("MHQ_long_sep24.csv") %>% filter(!is.na(external_identifier))  %>% dplyr::rename("MHC_ID" = 'external_identifier')

#%>% left_join(., us_cohort, by = "MHC_ID")

MHQ <- left_join(Other_pats, dplyr::select(MasterMHC_IDs_Dx_Mar2024, MHC_ID, Comparison1, Comparison2), by = "MHC_ID") %>% mutate(Comparison3 = Comparison1) %>% mutate(Comparison1 = dplyr::recode(Comparison1, "COVID" = "DC", "NotPH" = "DC", "PAH" = "PH", "PH5" = "PH")) %>% filter(MHC_ID != "SPVDU20050") %>% filter(MHC_ID != "SPVDU20025") %>% filter(MHC_ID != "SPVDU01149", MHC_ID != "SPVDU01292")

#1st entry for each patient in each q
Other_1st <- MHQ %>%
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


Other_1stWide <- Other_1st %>% dplyr::select(-dayInStudy, -createdOn) %>% pivot_wider(., names_from = Question, values_from = value) %>% mutate_all( ~replace(., lengths(.)==0, NA)) %>% mutate_at(c('moderate_act', 'vigorous_act', 'feel_worthwhile1', 'feel_worthwhile2', 'feel_worthwhile3', 'feel_worthwhile4', 'satisfiedwith_life', 'happiness', 'sleep_time', 'sleep_time1', 'fruit', 'vegetable', 'fish', 'grains', 'sugar_drinks'), as.numeric) 

Other_1stWide <-  Other_1stWide %>% mutate(across(all_of(variable_list_illnessmindset$Identifier),~factor(.,levels = likert_levels_IM)))  %>% mutate(easy = factor(easy, levels = c("Very difficult",
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

Other_1stWide$vigorous_act <- ifelse(Other_1stWide$vigorous_act > 1000, NA, Other_1stWide$vigorous_act)

Other_1stWide$sleep_time1 <- ifelse(Other_1stWide$sleep_time1 > 12, NA, Other_1stWide$sleep_time1)

Other_1stWide$sleep_time <- ifelse(Other_1stWide$sleep_time > 12, NA, Other_1stWide$sleep_time)

Other_1st_num <- Other_1stWide 

TF_vars <- c("heartCondition", "chestPain", "chestPainInLastMonth", "dizziness", "jointProblem", "physicallyCapable", "prescriptionDrugs", "work", "sleep_diagnosis1", "current_nicotine" )

ordinal_vars <- c(variable_list_illnessmindset$Identifier, "atwork","phys_activity", "muscles", "disease",  "beneficial", "easy", "unhealthy", "weight", "relaxing", "social", "indulgent", 'pleasurable', "convenient", "fun",  'riskfactors1', 'riskfactors2', 'riskfactors3', 'riskfactors4')

num_vars <- c('moderate_act', 'vigorous_act', 'feel_worthwhile1', 'feel_worthwhile2', 'feel_worthwhile3', 'feel_worthwhile4', 'satisfiedwith_life', 'happiness', 'sleep_time', 'sleep_time1', 'fruit', 'vegetable', 'fish', 'grains', 'sugar_drinks')

Other_1st_num[ordinal_vars] <- lapply(Other_1st_num[ordinal_vars], function(x) as.numeric(as.ordered(x)))
Other_1st_num[TF_vars] <- lapply(Other_1st_num[TF_vars], function(x) as.numeric(as.ordered(x)))

a <- sapply(Other_1st_num, class) %>% as.data.frame()

Other_1stWide_clus <- Other_1stWide %>% dplyr::select(all_of(ordinal_vars), all_of(TF_vars), Comparison1, MHC_ID) 

columns_to_keep <- c("Comparison1", "MHC_ID")

#Create a logical condition for rows where all columns except the specified ones are NA
condition <- apply(Other_1stWide_clus[ , !(names(Other_1stWide_clus) %in% columns_to_keep)], 1, function(row) !all(is.na(row)))

# Subset the data frame based on the condition
Other_1stWide_clus <- Other_1stWide_clus[condition, ]

#write_csv(Other_1stWide_clus, "MHQ_ord_TF_1st.csv") (cleaned in OpenRefine)
#Other_1stWide_clus <- read_csv("MHQ_ord_TF_1st.csv")


Other_1stWide_clus <- Other_1stWide_clus  %>% mutate(across(all_of(variable_list_illnessmindset$Identifier),~factor(.,levels = likert_levels_IM)))   %>% mutate(easy = factor(easy, levels = c("Very difficult", "Somewhat difficult","Somewhat easy","Very easy")),                                                                                                        relaxing = factor(relaxing, levels =  c(  "Very stressful", "Somewhat stressful","Somewhat relaxing","Very relaxing")),
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
