setwd("/Users/piyushzaware/Documents/Microecon_dev/Final_Presentation")
getwd()
# Load required packages
install.packages(c("haven", "dplyr", "ggplot2", "readr"))
install.packages("gt")
install.packages("survey")
library(haven)
library(dplyr)
library(ggplot2)
library(readr)
library(gt)
library(survey)
library(knitr)
library(broom)
library(stringr)


# Load datasets
individual <- read_dta("/Users/piyushzaware/Documents/Microecon_dev/Final_Presentation/Individual/DS0001/36151-0001-Data.dta")%>%filter(STATEID == 21)# Individual data
household <- read_dta("/Users/piyushzaware/Documents/Microecon_dev/Final_Presentation/Household/DS0002/36151-0002-Data.dta")%>% filter(STATEID == 21) # Household-level data
women <- read_dta("/Users/piyushzaware/Documents/Microecon_dev/Final_Presentation/Eligible_women/DS0003/36151-0003-Data.dta")%>% filter(STATEID == 21)            # Ever-married women 15-49
births <- read_dta("/Users/piyushzaware/Documents/Microecon_dev/Final_Presentation/Birth_history/DS0004/36151-0004-Data.dta")%>% filter(STATEID == 21)    # Birth history

# Deduplicate individual and household
individual_clean <- individual %>% distinct(HHID, PERSONID, .keep_all = TRUE)
household_clean  <- household %>% distinct(HHID, .keep_all = TRUE)

# Merge women + individual
women_indiv <- women %>%
  left_join(individual_clean, by = c("HHID", "PERSONID"))

# Merge with household
women_full <- women_indiv %>%
  left_join(household_clean, by = "HHID") %>%
  distinct(HHID, PERSONID, .keep_all = TRUE)

# Rename EW3 in births to PERSONID for matching
births <- births %>% rename(PERSONID = EW3)

# Final merge: births + women_full (retaining all births)
merged_df <- births %>%
  left_join(women_full, by = c("HHID", "PERSONID"))

names(household_clean)[str_detect(names(household_clean), "READ|TV|RADIO")]
names(merged_df)[grepl("ANC|VISIT", names(merged_df), ignore.case = TRUE)]
names(women)
names(household_clean)[grepl("URBAN", names(household_clean), ignore.case = TRUE)]
names(household_clean)[grepl("URBAN", names(household_clean), ignore.case = TRUE)]

"OG1" %in% names(women)  # Should return TRUE
# Select just HHID, PERSONID, OG1 from the original women file
og1_data <- women %>%
  select(HHID, PERSONID, OG1)

# Add OG1 to your merged_df
merged_df <- merged_df %>%
  left_join(og1_data, by = c("HHID", "PERSONID"))

# First extract HHID + URBAN2011
urban_data <- household_clean %>%
  select(HHID, URBAN2011)

# Then left_join to merged_df
merged_df <- merged_df %>%
  left_join(urban_data, by = "HHID")

analysis_df <- merged_df %>%
  filter(URBAN2011 == 0) %>%  # Keep only rural women
  filter(!is.na(LB2)) %>%
  mutate(
    institutional_delivery = ifelse(LB2 %in% c(1, 2), 1, 0),
    educated = ifelse(EW8 >= 10, 1, 0),
    media = ifelse(CGTV == 1, 1, 0),
    anc_visits = LB28,
    confidence_health = OG1,
    age = EW6
  ) %>%
  select(HHID, PERSONID, institutional_delivery, anc_visits, educated,
         media, confidence_health, age)

model_secschool <- glm(institutional_delivery ~ anc_visits + educated + media +
                         confidence_health + age,
                       family = binomial(link = "logit"),
                       data = analysis_df)

summary(model_secschool)

cor(analysis_df$anc_visits, analysis_df$educated, use = "complete.obs")

model_edu1 <- glm(institutional_delivery ~ anc_visits + media + age,
                  data = filter(analysis_df, educated == 1),
                  family = binomial())
summary(model_edu1)

model_edu0 <- glm(institutional_delivery ~ anc_visits + media + age,
                  data = filter(analysis_df, educated == 0),
                  family = binomial())

summary(model_edu0)

table(filter(analysis_df, educated == 1)$institutional_delivery)