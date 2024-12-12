###########################################################################
## Project: ANES data
## Script purpose: R code for Entire FYP
## Date: 17/04/24
## Author: Rory O Sullivan 20232721
###########################################################################

library(tidyverse)
library(cowplot)
library(kableExtra)
library(knitr)
library(skimr)
library(forcats)
library(tidyr)

setwd("C:/Users/Dell/OneDrive/Desktop/LM124/LM124 YEAR4 SEM7/FYP/Election Project/R code")
anes2020 <- read_csv("anes_timeseries_2020_csv_20220210.csv")


############################################## Exploratory

# look at the data
#View(anes2020)
dim(anes2020)
trump_bidendf <- anes2020 %>%
  filter(V202073 %in% c("1", "2")) 


dim(trump_bidendf)
#View(trump_bidendf)
################################################## sex and vote
trump_bidendf %>%
  mutate(
    gender = ifelse(V201600 == 1, "Male",
                    ifelse(V201600 == 2, "Female", "Refused")),
    vote_post = ifelse(V202073 == 1, "Biden", "Trump")
  ) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)
  ) %>% 
  filter(!(gender %in% c("Refused"))) %>%
  group_by(gender, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot() +
  geom_col(aes(y = prop, x = gender, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Sex",
    y = "Proportion",
    fill = "Vote Category"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )

table(trump_bidendf$V201600)
############################################## Age and voting
############ age
table(anes2020$V201507x)
summary(anes2020$V201507x)
trump_bidendf %>%
  mutate(age = as.numeric(V201507x),
         age_group = case_when(
           age == -9 ~ "Refused",
           between(age, 18, 24) ~ '18-24',
           between(age, 25, 30) ~ '25-30',
           between(age, 31, 36) ~ '31-36',
           between(age, 37, 42) ~ '37-42',
           between(age, 43, 48) ~ '43-48',
           between(age, 49, 54) ~ '49-54',
           between(age, 55, 60) ~ '55-60',
           between(age, 61, 66) ~ '61-66',
           between(age, 67, 72) ~ '67-72',
           between(age, 72, 79) ~ '72-79',
           age >= 80 ~ '80+',
           TRUE ~ 'Unknown'
         ),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(!(age_group %in% c("Refused"))) %>%
  group_by(age_group, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = age_group, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Age Group",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )


##### Summary age
trump_bidendf %>%
  filter(V201507x != -9) %>%
  skimr::skim(V201507x)


#### table
age_counts <- trump_bidendf %>%
  mutate(age = as.numeric(V201507x),
         age_group = case_when(
           age == -9 ~ "Refused",
           between(age, 18, 24) ~ '18-24',
           between(age, 25, 30) ~ '25-30',
           between(age, 31, 36) ~ '31-36',
           between(age, 37, 42) ~ '37-42',
           between(age, 43, 48) ~ '43-48',
           between(age, 49, 54) ~ '49-54',
           between(age, 55, 60) ~ '55-60',
           between(age, 61, 79) ~ '61-79',
           age >= 80 ~ '80+',
           TRUE ~ 'Unknown'
         )) %>%
  group_by(age_group) %>%
  summarise(count = n())

# Print the table
kable(age_counts, caption = "Counts of Respondents in Each Age Group")


############################################## Ethnicity and voting
trump_bidendf %>%
  mutate(ethnic = fct_recode(factor(V201549x),
                             "Refused" = "-9",
                             "Don't know" = "-8",
                             "White" = "1",
                             "Black" = "2",
                             "Hispanic" = "3",
                             "Asian" = "4",
                             "Native A." = "5",
                             "Multiple" = "6"),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(!(ethnic %in% c("Don't know", "Refused"))) %>%
  group_by(ethnic, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = ethnic, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Ethnicity",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal()  +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )


######### Num White
trump_bidendf %>%
  filter(V201549x == "1") %>%
  summarise(count = n())
# 5963 whites, 72%
############ Black only

trump_bidendf %>%
  filter(V201549x == "2") %>%
  mutate(vote_post = case_when(
    V202073 == 1 ~ "Biden",
    V202073 == 2 ~ "Trump",
    TRUE ~ "Other response"
  )) %>%
  group_by(vote_post) %>%
  summarise(count = n())
# 726 Total Black
# 420 Biden
# 25 Trump
# 281 Other
table(trump_bidendf$V201549x)

############################ Social Class
# Q: How would you describe your social class? Are you in the lower
# class, the working class, the middle class, or the upper class?


# -9. Refused
# -8. Don't know
# -7. No post-election data, deleted due to incomplete interview
# -6. No post-election interview
# -5. Interview breakoff (sufficient partial IW)
# 1. Lower class
# 2. Working class
# 3. Middle class
# 4. Upper class

trump_bidendf %>% 
  mutate(social = fct_recode(factor(V202352),
                             "Refused" = "-9",
                             "Don't know" = "-8",
                             "No post-election Data" = "-7",
                             "No post-election interview" = "-6",
                             "Interview Breakoff" = "-5",
                             "Lower" = "1",
                             "Working" = "2",
                             "Middle" = "3",
                             "Upper" = "4"),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(social %in% c("Lower", "Working", "Middle", 
                       "Upper")) %>%
  group_by(social, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = social, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2"))+
  labs(
    x = "Social Class",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal()  +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )

############################ Rural or Urban
# Q: Do you currently live in a rural area, small town, suburb, or a city?

# -9. Refused
# -8. Don't know
# -7. No post-election data, deleted due to incomplete interview
# -6. No post-election interview
# -5. Interview breakoff (sufficient partial IW)
# 1. Rural area
# 2. Small town
# 3. Suburb
# 4. City

trump_bidendf %>% 
  mutate(ruralurban = fct_recode(factor(V202355),
                                 "Refused" = "-9",
                                 "Don't know" = "-8",
                                 "No post-election Data" = "-7",
                                 "No post-election interview" = "-6",
                                 "Interview Breakoff" = "-5",
                                 "Rural" = "1",
                                 "Small town" = "2",
                                 "Suburb" = "3",
                                 "City" = "4"),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(ruralurban %in% c("Rural", "Small town", "Suburb", 
                           "City")) %>%
  group_by(ruralurban, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = ruralurban, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Rural/Urban Living",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  ) 

table(trump_bidendf$V202355)

######################### Liberal or Conservative
# Q: If you had to choose, would you consider yourself a liberal or a
#    conservative?

# -9. Refused
# -8. Don't know
# -4. Technical error
# -1. Inapplicable
# 1. Liberal
# 2. Conservative
# 3. Moderate {VOL, video/phone only}

trump_bidendf %>% 
  mutate(lib_conserv = fct_recode(factor(V201201),
                                  "Refused" = "-9",
                                  "Don't know" = "-8",
                                  "Technical error" = "-4",
                                  "Inapplicable" = "-1",
                                  "Liberal" = "1",
                                  "Conservative" = "2",
                                  "Moderate" = "3"),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(!(lib_conserv %in% c("Don't know", "Refused", "Technical error", 
                              "Moderate"))) %>%
  group_by(lib_conserv, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = lib_conserv, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2"))+
  labs(
    x = "Liberal/Conservative",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  ) 
##### Table
partycounts <- table(trump_bidendf$V201201)
print(partycounts)

############################ Present Religion
# Q: What is your present religion, if any?

# -9 . Refused
# -8.  Don't know
# 1.  Protestant
# 2.  Roman Catholic
# 3.  Orthodox Christian (such as Greek or Russian Orthodox)
# 4.  Latter-Day Saints (LDS)
# 5.  Jewish
# 6.  Muslim
# 7.  Buddhist
# 8.  Hindu
# 9.  Atheist
# 10. Agnostic
# 11. Something else
# 12. Nothing in particular
# Make new variable grouping the religions
trump_bidendf <- trump_bidendf %>%
  mutate(religion_grouped = case_when(
    V201435 %in% c("-9") ~ "Refused",
    V201435 %in% c("1") ~ "Protestant",
    V201435 %in% c("2") ~ "Roman Catholic",
    V201435 %in% c("3") ~ "Orthodox Christian",
    V201435 %in% c("4") ~ "LDS",
    V201435 %in% c("5") ~ "Jewish",
    V201435 %in% c("6") ~ "Muslim",
    V201435 %in% c("7") ~ "Buddhist",
    V201435 %in% c("8") ~ "Hindu",
    V201435 %in% c("9", "10", "11", "12") ~ "Non-Religious",
    TRUE ~ "Other"
  )) %>%
  filter(religion_grouped != "Don't know") 

trump_bidendf %>% 
  mutate(religion = fct_recode(factor(religion_grouped),
                               "Refused" = "Refused",
                               "Protest." = "Protestant",
                               "R. Cath" = "Roman Catholic",
                               "O. Christ" = "Orthodox Christian",
                               "LDS" = "LDS",
                               "Jewish" = "Jewish",
                               "Muslim" = "Muslim",
                               "Buddhist" = "Buddhist",
                               "Hindu" = "Hindu",
                               "Non-Rel" = "Non-Religious"),  
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  group_by(religion, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(!(religion %in% ("Other"))) %>%
  ggplot() +
  geom_col(aes(y = prop, x = religion, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Religion",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )

##### Table
religioncounts <- table(trump_bidendf$V201435)
print(religioncounts)

############################ Marital Status
# Q: Are you now married, widowed, divorced, separated or never married?
# 
# -9. Refused
# -8. Don't know
# 1. Married: spouse present
# 2. Married: spouse absent {VOL - video/phone only}
# 3. Widowed
# 4. Divorced
# 5. Separated
# 6. Never married

trump_bidendf %>% 
  mutate(marital = fct_recode(factor(V201508),
                              "Refused" = "-9",
                              "Don't know" = "-8",
                              "Married" = "1",
                              "Married: spouse absent" = "2",
                              "Widowed" = "3",
                              "Divorced" = "4",
                              "Separated" = "5",
                              "Never Married" = "6"),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(!(marital %in% c("Don't know", "Refused", "Married: spouse absent"))) %>%
  group_by(marital, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = marital, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Marital Status",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  ) 
#### table
counts <- table(trump_bidendf$V201508)
print(counts)

############################ Education
# Q: What is the highest level of school you have completed or the
# highest degree you have received?

# -9. Refused
# -8. Don't know
# -2. Missing, other specify not coded for preliminary release
#  1. Less than high school credential
#  2. High school credential
#  3. Some post-high school, no bachelor's degree
#  4. Bachelor's degree
#  5. Graduate degree

trump_bidendf %>% 
  mutate(education = fct_recode(factor(V201511x),
                                "Refused" = "-9",
                                "Don't know" = "-8",
                                "Missing" = "-2",
                                "Less HS" = "1",
                                "HS Cred." = "2",
                                "Post HS" = "3",
                                "Bachelor" = "4",
                                "Graduate" = "5"),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(education %in% c("Less HS", 
                          "HS Cred.", "Post HS", 
                          "Bachelor", "Graduate")) %>%
  group_by(education, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = education, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Education Level",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal()  +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )
table(trump_bidendf$V201511x)

###### Table
trump_bidendf %>%
  filter(V201511x == "2") %>%
  mutate(vote_post = case_when(
    V202073 == 1 ~ "Biden",
    V202073 == 2 ~ "Trump",
    TRUE ~ "Other response"
  )) %>%
  group_by(vote_post) %>%
  summarise(count = n())
T
############################ Total (Family) Income
# Q: total income?

# -9. Refused
# -5. Interview breakoff (sufficient partial IW)
# 1. Under $9,999
# 2. $10,000-14,999
# 3. $15,000-19,999
# 4. $20,000-24,999
# 5. $25,000-29,999
# 6. $30,000-34,999
# 7. $35,000-39,999
# 8. $40,000-44,999
# 9. $45,000-49,999
# 10. $50,000-59,999
# 11. $60,000-64,999
# 12. $65,000-69,999
# 13. $70,000-74,999
# 14. $75,000-79,999
# 15. $80,000-89,999
# 16. $90,000-99,999
# 17. $100,000-109,999
# 18. $110,000-124,999
# 19. $125,000-149,999
# 20. $150,000-174,999
# 21. $175,000-249,999
# 22. $250,000 or more

trump_bidendf <- trump_bidendf %>%
  mutate(Income_Group = case_when(
    V201617x %in% c("-9") ~ "Refused",
    V201617x %in% c("1") ~ "Under 10k",
    V201617x %in% c("2", "3", "4", "5") ~ "10k-30k",
    V201617x %in% c("6", "7", "8", "9", "10") ~ "30k-60k",
    V201617x %in% c("11", "12", "13", "14", "15", "16") ~ "60k-100k",
    V201617x %in% c("17", "18", "19") ~ "100k-150k",
    V201617x %in% c("20", "21") ~ "150k-250k",
    V201617x %in% c("22") ~ "250k+",
    TRUE ~ NA_character_
  ))
trump_bidendf$Income_Group <- factor(replace_na(as.character(trump_bidendf$Income_Group),
                                                replace = "Not Specified"))
table(trump_bidendf$Income_Group)
table(trump_bidendf$V201617x)

############# Doing income now from this
trump_bidendf %>% 
  mutate(income = fct_recode(factor(Income_Group),
                             "Refused" = "Refused",
                             "Not Specified" = "Not Specified",
                             "Under 10k" = "Under 10k",
                             "10k-30k" = "10k-30k",
                             "30k-60k" = "30k-60k",
                             "60k-100k" = "60k-100k",
                             "100k-150k" = "100k-150k",
                             "150k-250k" = "150k-250k",
                             "250k+" = "250k+"),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(income %in% c("Under 10k", "10k-30k", "30k-60k", "60k-100k", "100k-150k",
                       "150k-250k", "250k+")) %>%
  group_by(income, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = fct_relevel(income,
                                         c("Under 10k", "10k-30k", "30k-60k", "60k-100k",
                                           "100k-150k", "150k-250k", "250k+")),
               fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Total Income Level ($)",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal()  +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )

############################

trump_bidendfinc <- subset(trump_bidendf, V201617x != c('-5'))
#View(trump_bidendfinc)
trump_bidendfinc %>% 
  mutate(income_raw = as.numeric(factor(V201617x)),
         income = cut(income_raw,
                      breaks = c(-Inf, 1, 7, 12, 18, 24),
                      labels = c("Refused", "Under $30,000", "$30,000 - $59,999", 
                                 "$60,000 - $99,999", "$100,000 or more"),
                      include.lowest = TRUE),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(!is.na(income)) %>%
  filter(!(income %in% c("Refused"))) %>%
  group_by(income, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = income, fill = vote_post),show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Income Level",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )

############# trying something here with income to match modelling
trump_bidendfinc %>% 
  mutate(income_raw = as.numeric(factor(V201617x)),
         income = cut(income_raw,
                      breaks = c(-Inf, 1, 3, 7, 12, 18, 21, 23, 24),
                      labels = c("Refused", "Under 10k", "10k-30k", "30k-60k",
                                 "60k-100k", "100k-150k", "150k-250k", "250k+"),
                      include.lowest = TRUE),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(!is.na(income)) %>%
  filter(!(income %in% c("Refused"))) %>%
  group_by(income, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = income, fill = vote_post),show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Income Level",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )

######################### Defense Spending
# Q: Where would you place yourself on this scale, or haven't you
# thought much about this?


# -9. Refused
# -8. Don't know
# 1. Greatly decrease defense spending
# 2.
# 3.
# 4.
# 5.
# 6.
# 7. Greatly increase defense spending
# 99. Haven't thought much about this

trump_bidendf %>% 
  mutate(defense  = fct_recode(factor(V201249),
                               "Refused" = "-9",
                               "Don't know" = "-8",
                               "G. Decr" = "1",
                               "G. Incr" = "7",
                               "H.T." = "99"),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(!(defense %in% c("Refused", "Don't know"))) %>%
  group_by(defense, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = defense, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2")) +
  labs(
    x = "Defence Spending",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  ) 

###### table
defensecounts <- table(trump_bidendf$V201249)
print(defensecounts)
table(anes2020$V201249)

############################ Abortion
# Q: There has been some discussion about abortion during recent
# years. Which one of the opinions on this page best agrees with
# your view?
#   You can just tell me the number of the opinion you choose.

# -9. Refused
# -8. Don't know
# 1. By law, abortion should never be permitted
# 2. The law should permit abortion only in case of rape, incest, or
# when the woman's life is in danger
# 3. The law should permit abortion other than for
# rape/incest/danger to woman but only after need clearly
# established
# 4. By law, a woman should always be able to obtain an abortion
# as a matter of personal choice
# 5. Other {SPECIFY}

trump_bidendf %>%
  mutate(
    abortion = fct_recode(
      factor(V201336),
      "Refused" = "-9",
      "Don't know" = "-8",
      "Never" = "1",
      "R/I/D" = "2",
      "Need" = "3",
      "Choice" = "4",
      "Other" = "5"
    ),
    vote_post = ifelse(V202073 == 1, "Biden", "Trump")
  ) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post),
    abortion = fct_relevel(
      factor(abortion),  
      "Never", "R/I/D", 
      "Need", "Choice"
    )
  ) %>%
  filter(!(abortion %in% c("Don't know", "Refused", "Other"))) %>%
  group_by(abortion, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot() +
  geom_col(aes(y = prop, x = abortion, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2"))+
  labs(
    x = "Abortion",
    y = "Proportion",
    fill = "Vote Category"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )
######## table
counts <- table(trump_bidendf$V201336)
print(counts)

############################ Death penalty
# Q: Do you favor or oppose the death penalty for persons convicted
# of murder?
#   
# -9. Refused
# -8. Don't know
# 1. Favor
# 2. Oppose  

trump_bidendf %>% 
  mutate(Death_penalty = fct_recode(factor(V201343),
                                    "Refused" = "-9",
                                    "Don't know" = "-8",
                                    "Favour" = "1",
                                    "Oppose" = "2"),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(!(Death_penalty %in% c("Don't know", "Refused"))) %>%
  group_by(Death_penalty, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = Death_penalty, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2"))+
  labs(
    x = "Death Penalty",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal()  +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )
######## table
counts <- table(trump_bidendf$V201343)
print(counts)

############################ Immigration
# Q: PRE: US GOVERNMENT POLICY TOWARD UNAUTHORIZED IMMIGRANTS
# Which comes closest to your view about what government
# policy should be toward unauthorized immigrants now living in
# the United States?
#   You can just tell me the number of your choice.

# -9. Refused
# -8. Don't know
# 1. Make all unauthorized immigrants felons and send them back
# to their home country
# 2. Have a guest worker program that allows unauthorized
# immigrants to remain in US to work but only for limited time
# 3. Allow unauthorized immigrants to remain in US & eventually
# qualify for citizenship but only if they meet requirements
# 4. Allow unauthorized immigrants to remain in US & eventually
# qualify for citizenship without penalties

trump_bidendf %>% 
  mutate(immigration = fct_recode(factor(V201417),
                                  "Refused" = "-9",
                                  "Don't know" = "-8",
                                  "Deport" = "1",
                                  "Ltd. Time" = "2",
                                  "Qualify" = "3",
                                  "Allow" = "4"),
         vote_post = ifelse(V202073 == 1, "Biden", "Trump")) %>% 
  mutate(
    vote_post = fct_relevel(factor(vote_post), "Biden", "Trump"),
    vote_post = fct_rev(vote_post)) %>% 
  filter(immigration %in% c("Deport", "Ltd. Time", "Qualify", 
                            "Allow")) %>%
  group_by(immigration, vote_post) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  
  ggplot() +
  geom_col(aes(y = prop, x = immigration, fill = vote_post), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick2", "royalblue2"))+
  labs(
    x = "Immigration Policy",
    y = "Proportion",
    fill = "Vote Category") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  ) 

table(trump_bidendf$V201417)
######## Definitely gonna look at this






######################################### Modelling


###########################################################################
## Project: ANES data
## Script purpose: Attempting to group the other answers categories to other
## Date: 13/11/23
## Author: Rory O Sullivan
###########################################################################

library(randomForest)
library(tidyverse)
library(cowplot)
library(caret)
library(pROC)

##############################################################
# Making a New data frame with only certain columns
##############################################################

new_data <- anes2020 %>%
  select(V201600, V201033, V202073,  V201549x, V202352, V202355, V201201,
         V201249, V201433, V201435, V201508, V201511x, V201617x, V201014b, 
         V201366, V201343, V201507x, 
         V201417, V201312, V201416, V201262,
         V201258, V202337)
#View(new_data)

# Convert specified columns to factors
columns_to_convert <- c("V201600", "V201033", "V202073", "V201549x", 
                        "V202352", "V202355", "V201201", "V201249", 
                        "V201433", "V201435", "V201508", "V201511x",
                        "V201617x", "V201014b", "V201366", "V201343", "V201507x",
                        "V201417", "V201312", "V201416", "V201262",
                        "V201258", "V202337")
new_data <- new_data %>%
  mutate_at(vars(columns_to_convert), as.factor)


new_data <- new_data %>%
  mutate(V202073 = recode(V202073, "1" = "Biden", "2" = "Trump", .default = "Other"))


# Define the new names
new_column_names <- c("Gender", "Voting_Intention", "Actual_Vote", "Ethnicity", 
                      "Social_Class", "Rural_Urban", "Lib_Con", "Defense_Spend",
                      "Rel_imp", "Religion", "Marital", "Education", 
                      "Income", "State", "Abortion", "Death_penalty", "Age",
                      "Immigration", "Welfare","Gay_Marriage", "Business_env",
                      "Race_Relations", "Gun_Control")

# Assign the new names to the data frame
names(new_data) <- new_column_names
#View(new_data)

new_data$Age <- as.numeric(new_data$Age)

# Look at gender answers and then group
table(new_data$Gender)
new_data$Gender.f <- factor(new_data$Gender, levels = c("1", "2"), 
                            labels = c("Male", "Female"))
new_data$Gender.f <- factor(replace_na(as.character(new_data$Gender.f),
                                       replace = "Not Specified"))

# Look at ethnicity answers and then group
table(new_data$Ethnicity)
new_data$Ethnicity.f <- factor(new_data$Ethnicity, levels = c("1", "2", "3", 
                                                              "4", "5", "6"), 
                               labels = c("White", "Black", "Hispanic", "Asian", 
                                          "Native American", "Multiple"))
new_data$Ethnicity.f <- factor(replace_na(as.character(new_data$Ethnicity.f),
                                          replace = "Not Specified"))

# Look at Social Class answers and then group
table(new_data$Social_Class)
# 754 or 9.1% had no post election interview
new_data$Social.f <- factor(new_data$Social_Class, levels = c("1", "2", "3","4"), 
                            labels = c("Lower", "Working", "Middle", "Upper"))
new_data$Social.f <- factor(replace_na(as.character(new_data$Social.f),
                                       replace = "Not Specified"))

# Look at Rural/Urban Living answers and then group
table(new_data$Rural_Urban)
# Same num again as post question
new_data$Living.f <- factor(new_data$Rural_Urban, levels = c("1", "2", "3","4"), 
                            labels = c("Rural", "Small Town", "Suburb", "City"))

new_data$Living.f <- factor(replace_na(as.character(new_data$Living.f),
                                       replace = "Not Specified"))
# Look at Death Penalty answers and then group
table(new_data$Death_penalty)
new_data$Death.f <- factor(new_data$Death_penalty, levels = c("1", "2"), 
                           labels = c("Favour", "Oppose"))
new_data$Death.f <- factor(replace_na(as.character(new_data$Death.f),
                                      replace = "Not Specified"))

# Look at Abortion answers and then group
table(new_data$Abortion)
new_data$Abortion.f <- factor(new_data$Abortion, levels = c("1", "2", "3","4","5"), 
                              labels = c("Never Permitted", "OnlySomeCases",
                                         "OnlyNeedEstablished", "Always Permitted",
                                         "Other Opinion"))
new_data$Abortion.f <- factor(replace_na(as.character(new_data$Abortion.f),
                                         replace = "Not Specified"))

# Look at Defence Spending answers and then group
table(new_data$Defense_Spend)
new_data$Defense.f <- factor(new_data$Defense_Spend, levels = c("1", "2", "3",
                                                                "4","5","6","7",
                                                                "99"), 
                             labels = c("Greatly Decrease", "gdplus1", "gdplus2",
                                        "gdplus3", "gdplus4", "gdplus5", 
                                        "Greatly Increase", "Haven't Thought"))

new_data$Defense.f <- factor(replace_na(as.character(new_data$Defense.f),
                                        replace = "Not Specified"))

# Look at Religion answers and then group
table(new_data$Religion)
new_data$Religion.f <- factor(new_data$Religion, levels = c("1", "2", "3", "4", 
                                                            "5", "6", "7", "8",
                                                            "9", "10", "11", "12"), 
                              labels = c("Protest.", "R. Cath", "O. Christ",
                                         "LDS", "Jewish", "Muslim", 
                                         "Buddhist", "Hindu", "Atheist", "Agnostic", 
                                         "Something Else", "Nothing in Particular"))
new_data$Religion.f <- factor(replace_na(as.character(new_data$Religion.f),
                                         replace = "Not Specified"))

# Look at Liberal/Conservative Identity answers and then group
table(new_data$Lib_Con)
new_data$Lib_Con.f <- factor(new_data$Lib_Con, levels = c("-1", "1", "2"), 
                             labels = c("Inapplicable", "Liberal",
                                        "Conservative"))
new_data$Lib_Con.f <- factor(replace_na(as.character(new_data$Lib_Con.f),
                                        replace = "Not Specified"))

# Look at Marital Status answers and then group
table(new_data$Marital)
new_data$Marital.f <- factor(new_data$Marital, levels = c("1", "3",
                                                          "4","5","6"), 
                             labels = c("Married", "Widowed", "Divorced",
                                        "Separated", "Never Married"))

new_data$Marital.f <- factor(replace_na(as.character(new_data$Marital.f),
                                        replace = "Not Specified"))

# Look at Education answers and then group
table(new_data$Education)
new_data$Education.f <- factor(new_data$Education, levels = c("1", "2", "3",
                                                              "4", "5"), 
                               labels = c("Less HS", "HS Cred.", "Post HS",
                                          "Bach.", "Grad."))
new_data$Education.f <- factor(replace_na(as.character(new_data$Education.f),
                                          replace = "Not Specified"))

# Look at Religious Importance answers and then group
table(new_data$Rel_imp)
new_data$Rel_imp.f <- factor(new_data$Rel_imp, levels = c("1","2","3","4","5"), 
                             labels = c("EXtremely Important", "Very Important", 
                                        "Moderately Important",
                                        "A little Important", 
                                        "Not Important at all"))

new_data$Rel_imp.f <- factor(replace_na(as.character(new_data$Rel_imp.f),
                                        replace = "Not Specified"))
# Look at Income answers and then group
table(new_data$Income)
new_data$Income.f <- factor(new_data$Income, levels = c("-9", "1", "2", "3", "4", 
                                                        "5", "6", "7", "8",
                                                        "9", "10", "11", "12",
                                                        "13", "14", "15", "16", 
                                                        "17", "18", "19", "20",
                                                        "21", "22"), 
                            labels = c("Refused", "Under 10k", "10-15",
                                       "15-20", "20-25", "25-30", 
                                       "30-35", "35-40", "40-45", "45-50", 
                                       "50-60", "60-65", "65-70",
                                       "70-75", "75-80", "80-90", 
                                       "90-100", "100-110", "110-125", "125-150", 
                                       "150-175", "175-250", "250+"))
new_data$Income.f <- factor(replace_na(as.character(new_data$Income.f),
                                       replace = "Not Specified"))
# Look at Immigration answers and then group
table(new_data$Immigration)
new_data$Immigration.f <- factor(new_data$Immigration, levels = c("1", "2", "3", 
                                                                  "4"), 
                                 labels = c("DeportAll", "LimitedTime", "Qualify",
                                            "Allow"))
new_data$Immigration.f <- factor(replace_na(as.character(new_data$Immigration.f),
                                            replace = "Not Specified"))


##################### Grouping Religion.f into lesser categories
#View(new_data)
table(new_data$Religion.f)
# 6 levels of income plus one for refused
# Making new one with 8 levels
# Define the mapping of current levels to new groups
# Define the mapping of current levels to new numeric groups
new_data <- new_data %>%
  mutate(Religion_Group = case_when(
    Religion.f %in% c("Protest.") ~ "Protest.",
    Religion.f %in% c("R. Cath") ~ "R. Cath",
    Religion.f %in% c("O. Christ") ~ "O. Christ",
    Religion.f %in% c("LDS") ~ "LDS",
    Religion.f %in% c("Jewish") ~ "Jewish",
    Religion.f %in% c("Muslim") ~ "Muslim",
    Religion.f %in% c("Buddhist") ~ "Buddhist",
    Religion.f %in% c("Hindu") ~ "Hindu",
    Religion.f %in% c("Atheist", "Agnostic", "Something Else",
                      "Nothing in Particular") ~ "Non-Rel",
    TRUE ~ NA_character_
  ))
new_data$Religion_Group <- factor(replace_na(as.character(new_data$Religion_Group),
                                             replace = "Not Specified"))
### Checking
table(new_data$Religion.f)
table(new_data$Religion_Group)

##################### Grouping Education.f into lesser categories
#View(new_data)
table(new_data$Education.f)
# 6 levels of income plus one for refused
# Making new one with 8 levels
# Define the mapping of current levels to new groups
# Define the mapping of current levels to new numeric groups
new_data <- new_data %>%
  mutate(Education_Group = case_when(
    Education.f %in% c("Not Specified") ~ "Not Specified",
    Education.f %in% c("Less HS") ~ "Less HS",
    Education.f %in% c("HS Cred.") ~ "HS Cred.",
    Education.f %in% c("Post HS") ~ "Post HS",
    Education.f %in% c("Bach.", "Grad.") ~ "Uni Deg.",
    TRUE ~ NA_character_
  ))
new_data$Education_Group <- factor(replace_na(as.character(new_data$Education_Group),
                                              replace = "Not Specified"))
### Checking
table(new_data$Education.f)
table(new_data$Education_Group)
##################### Grouping Income.f into lesser categories
#View(new_data)
str(new_data$Income.f)
# 23 levels of income plus one for refused
# Making new one with 8 levels

new_data <- new_data %>%
  mutate(Income_Group = case_when(
    Income.f %in% c("Refused") ~ "Refused",
    Income.f %in% c("Under 10k") ~ "Under 10k",
    Income.f %in% c("10-15", "15-20", "20-25", "25-30") ~ "10k-30k",
    Income.f %in% c("30-35", "35-40", "40-45", "45-50", "50-60") ~ "30k-60k",
    Income.f %in% c("60-65", "65-70", "70-75", "75-80", "80-90", "90-100") ~ "60k-100k",
    Income.f %in% c("100-110", "110-125", "125-150") ~ "100k-150k",
    Income.f %in% c("150-175", "175-250") ~ "150k-250k",
    Income.f %in% c("250+") ~ "250k+",
    TRUE ~ NA_character_
  ))
new_data$Income_Group <- factor(replace_na(as.character(new_data$Income_Group),
                                           replace = "Not Specified"))
table(new_data$Income_Group)
################################ Setting the Reference Level

############### Trump Biden df
trump_biden_data <- new_data %>%
  filter(Actual_Vote %in% c("Biden", "Trump")) %>%
  mutate(Actual_Vote = ifelse(Actual_Vote == "Biden", 0, 1))
# Converting to factor
trump_biden_data$Actual_Vote <- factor(trump_biden_data$Actual_Vote,
                                       levels = c("0", "1"), 
                                       labels = c("Biden", "Trump"))
predictor_vars_limited <- trump_biden_data %>% 
  select(-c('Gender', 'Ethnicity', 'Social_Class', 'Rural_Urban',
            'Income', 'Defense_Spend', 'Abortion', 'Religion',
            'Rel_imp', 'Education', 'Lib_Con', 'Death_penalty', 'Marital',
            'Voting_Intention', 'Religion',  'Marital',  'Income.f',
            'Immigration'))
#View(predictor_vars_limited)

############################### New DataSets

original_df <- predictor_vars_limited %>%
  select(c("Actual_Vote", "Age", "Gender.f", "Ethnicity.f", "Social.f", 
           "Religion_Group", "Marital.f", "Lib_Con.f", "Living.f",
           "Abortion.f", "Death.f", "Defense.f","Immigration.f"))

rf_df <- predictor_vars_limited %>%
  select(c("Actual_Vote", "Age", "Gender.f", "Ethnicity.f", "Social.f", 
           "Religion_Group", "Marital.f", "Lib_Con.f", "Living.f",
           "Abortion.f", "Death.f", "Defense.f","Immigration.f",
           "Income_Group", "Education.f"))

###################### Reference Categories
#View(original_df)
original_df$Gender.f <- relevel(original_df$Gender.f, ref = "Female")
original_df$Ethnicity.f <- relevel(original_df$Ethnicity.f, ref = "White")

original_df$Social.f <- relevel(original_df$Social.f, ref = "Upper")
#original_df$Education.f <- relevel(original_df$Education.f, ref = "Grad.")
#original_df$Education_Group <- relevel(original_df$Education_Group, ref = "Uni Deg.")
#original_df$Income_Group <- relevel(original_df$Income_Group, ref = "100k-150k")
original_df$Religion_Group <- relevel(original_df$Religion_Group, ref = "Non-Rel")
original_df$Marital.f <- relevel(original_df$Marital.f, ref = "Never Married")
original_df$Lib_Con.f <- relevel(original_df$Lib_Con.f, ref = "Liberal")
original_df$Living.f <- relevel(original_df$Living.f, ref = "City")

original_df$Abortion.f <- relevel(original_df$Abortion.f, ref = "Always Permitted")
original_df$Death.f <- relevel(original_df$Death.f, ref = "Oppose")
original_df$Defense.f <- relevel(original_df$Defense.f, ref = "Greatly Decrease")
original_df$Immigration.f <- relevel(original_df$Immigration.f, ref = "Allow")

original_log <- glm(Actual_Vote ~., data = original_df,
                    family = "binomial")
summary(original_log)

############ RF Modelling

class_weights <- c(Biden = 1.75, Trump = 2.33)

original_model <- randomForest(Actual_Vote ~., data = rf_df,
                               ntree = 500, mtry =2, proximity = TRUE,
                               classwt = class_weights)
print(original_model)


#### Original Importance
importance_values <- importance(original_model)
sorted_importance <- importance_values[order(-importance_values[, 1]),]
print(sorted_importance)


### Plotting error of Original Model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(original_model$err.rate), times =3),
  Type = rep(c("OOB", "Trump", "Biden"), each=nrow(original_model$err.rate)),
  Error=c(original_model$err.rate[,"OOB"],
          original_model$err.rate[,"Trump"],
          original_model$err.rate[,"Biden"]))

# Define color palette
my_colors <- c("OOB" = "palegreen3", "Trump" = "firebrick2",
               "Biden" = "royalblue2")
# Leveling the legend
oob.error.data$Type <- factor(oob.error.data$Type,
                              levels = c("Trump", "OOB", "Biden"))

# Create plot
ggplot(data = oob.error.data, aes(x = Trees, y = Error, color = Type)) +
  geom_line() +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )

############################ K fold cross validation for Random Forest
# Define the training control
ctrl <- trainControl(method = "cv",  
                     number = 10)    

model <- train(Actual_Vote ~ .,                  
               data = original_df,            
               method = "rf",
               trControl = ctrl)       
print(model)
model$finalModel

model$resample
print(model$results)


# varImpPlot(original_model, sort = TRUE,)
# Gonna make a df instead so can get better customisation
# Create the data frame
importance_df <- data.frame(
  Variable <- c('Defence', 'Abortion', 'Immigration', 'Death',
                'Ethnicity', 'Lib_Con', 'Age', 'Income',
                'Religion', 'Education', 'Living', 'Marital',
                'Social', 'Sex'),
  
  Importance <- c(380.52756, 289.90590, 280.52826, 242.78167, 193.54862,
                  192.83689, 177.32658, 161.10505, 147.43335, 118.27167,
                  117.33010, 102.90685, 72.18129, 43.76327)
)

# Reorder the levels of Variable based on Importance
importance_df$Variable <- factor(importance_df$Variable, 
                                 levels =
                                   importance_df$Variable[order(importance_df$Importance, 
                                                                decreasing = FALSE)])
# Create the bar plot
ggplot(data = importance_df, aes(x = Importance, y = Variable)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),  
    axis.title.y = element_text(size = 18)
  ) +
  xlab("MDI") +
  ylab("Variable")

############ Logistic Modelling

original_log <- glm(Actual_Vote ~., data = original_df,
                    family = "binomial")
summary(original_log)

############### Logistic Importance
log_importance_df <- data.frame(
  Variable = c("Defense", "Abortion", "Immigration", "Death", "Lib_Con",
               "Ethnicity", "Age", "Income", "Religion", "Living",
               "Education", "Marital", "Social", "Sex"),
  Importance = c(313.6, 322.1, 439.2, 81.7, 243, 
                 282.7, 1.9, 1.4, 68.2, 9.6, 
                 1.9, 5.1, 2.6, 8.7)
)

# Reorder the levels of Variable based on Importance
log_importance_df$Variable <- factor(log_importance_df$Variable, 
                                     levels =
                                       log_importance_df$Variable[order(log_importance_df$Importance, 
                                                                        decreasing = FALSE)])
# Create the bar plot
ggplot(data = log_importance_df, aes(x = Importance, y = Variable)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),  
    axis.title.y = element_text(size = 18)
  ) +
  xlab("Change in AIC") +
  ylab("Variable")


############################ K fold cross validation for logistic
# Define the training control
ctrl <- trainControl(method = "cv",  
                     number = 10)    

model <- train(Actual_Vote ~ .,                  
               data = original_df,            
               method = "glm",
               trControl = ctrl)       
print(model)
model$finalModel

model$resample
print(model$results)


#########################  Looking at f1 score etc for Logistic

actual_labels <- original_df$Actual_Vote
predicted_probs <- predict(original_log, type = "response")


predicted_labels <- ifelse(predicted_probs >= 0.5, 1, 0)


conf_matrix <- table(Actual = actual_labels, Predicted = predicted_labels)


print(conf_matrix)
################# Analysing Conf Matrix
# Extract counts from the confusion matrix
TP <- conf_matrix[2, 2]  # True Positives
TN <- conf_matrix[1, 1]  # True Negatives
FP <- conf_matrix[1, 2]  # False Positives
FN <- conf_matrix[2, 1]  # False Negatives

# Calculate Precision
precision <- TP / (TP + FP)

# Calculate True Positive Rate (Recall or Sensitivity)
recall <- TP / (TP + FN)

# Calculate True Negative Rate (Specificity)
specificity <- TN / (TN + FP)

# Calculate False Positive Rate
fpr <- FP / (FP + TN)

# Calculate F1-Score
f1_score <- (2 * precision * recall) / (precision + recall)

# results
cat("Precision:", precision, "\n")
cat("Recall (True Positive Rate):", recall, "\n")
cat("Specificity (True Negative Rate):", specificity, "\n")
cat("False Positive Rate:", fpr, "\n")
cat("F1-Score:", f1_score, "\n")

################# Now doing ROC for Logisitc
# Calculate the ROC curve
roc_curvelog <- roc(actual_labels, predicted_probs)
# Calculate the AUC
auc_scorelog <- auc(roc_curvelog)
# Print the AUC score
cat("AUC:", auc_score, "\n")


plot(roc_curvelog, xlab = "False Positive Rate", ylab = "True Positive Rate")
legend(x = "bottomright", y = 0.95, legend = paste("AUC =", round(auc_scorelog, 2)),
       bty = "n")

############## ROC for Original Random Forest Model
# Calculate predicted probabilities
probabilities <- predict(original_model, type = "prob")
# Extract probabilities for the positive class
prob_pos <- probabilities[, "Trump"]
# Create ROC curve
roc_curverf <- roc(original_df$Actual_Vote, prob_pos)
# Plot ROC curve
plot(roc_curverf, xlab = "False Positive Rate", ylab = "True Positive Rate")

# Add AUC (Area Under the Curve) to plot
auc_scorerf <- auc(roc_curverf)
legend(x = "bottomright", y = 0.95, legend = paste("AUC =", round(auc_scorerf, 2)),
       bty = "n")

##################### Plotting ROC for Logsitic and RF on same plot

# Plot ROC curves for logistic regression and random forest
plot(roc_curvelog, col = "#69b3a2", lwd = 1.5, xlab = "False Positive Rate",
     ylab = "True Positive Rate")
lines(roc_curverf, col = "lightcoral", lwd = 1.5)


legend_text <- c(paste("Log AUC =", round(auc_scorelog, 2)),
                 paste("RF AUC =", round(auc_scorerf, 2)))
legend("bottomright", legend = legend_text, col = c("#69b3a2", "lightcoral"), lwd = 2, bty = "n")


#########################  Looking at f1 score etc for Random Forest
print(original_model)
conf_matrix <- original_model$confusion
conf_matrix <- conf_matrix[, -3]
conf_matrix

# Extract counts from the confusion matrix
TP <- conf_matrix[2, 2]  # True Positives
TN <- conf_matrix[1, 1]  # True Negatives
FP <- conf_matrix[1, 2]  # False Positives
FN <- conf_matrix[2, 1]  # False Negatives

# Calculate Precision
precision <- TP / (TP + FP)

# Calculate True Positive Rate (Recall or Sensitivity)
recall <- TP / (TP + FN)

# Calculate True Negative Rate (Specificity)
specificity <- TN / (TN + FP)

# Calculate False Positive Rate
fpr <- FP / (FP + TN)

# Calculate F1-Score
f1_score <- (2 * precision * recall) / (precision + recall)

# results
cat("Precision:", precision, "\n")
cat("Recall (True Positive Rate):", recall, "\n")
cat("Specificity (True Negative Rate):", specificity, "\n")
cat("False Positive Rate:", fpr, "\n")
cat("F1-Score:", f1_score, "\n")


############### Logistic Importance for final model
log_importance_df <- data.frame(Variable = c("Defence", "Abortion", "Immigration", "Death", "Lib_Con",
                                             "Ethnicity", "Age", "Religion", "Living",
                                             "Marital", "Social", "Sex"),
                                Importance = c(325.3, 333.4, 440.6, 86.9, 277.9, 
                                               244.7, 3.3, 67.1, 10,
                                               13.2, 6.2, 9.2)
                                
)

# Reorder the levels of Variable based on Importance
log_importance_df$Variable <- factor(log_importance_df$Variable, 
                                     levels =
                                       log_importance_df$Variable[order(log_importance_df$Importance, 
                                                                        decreasing = FALSE)])
# Create the bar plot
ggplot(data = log_importance_df, aes(x = Importance, y = Variable)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),  
    axis.title.y = element_text(size = 18)
  ) +
  xlab("Change in AIC") +
  ylab("Variable")


################################ Poster Work
############# General Importance

gen_imp_df <- data.frame(
  Variable = c("Defence", "Abortion", "Immigration", "Death_Pen", "Lib_Con",
               "Ethnicity", "Age", "Income", "Religion", "Living",
               "Education", "Marital", "Social", "Gender"),
  logImportance = c(313.6, 322.1, 439.2, 81.7, 243, 
                    282.7, 1.9, 1.4, 68.2, 9.6, 
                    1.9, 5.1, 2.6, 8.7),
  rfImportance = c(389.19135, 291.02478, 280.69150, 239.45669, 192.21719,
                   188.13726, 177.14470, 163.93628, 146.56324, 118.69432,
                   116.62206, 102.85605, 72.73725, 43.77173)
)
view(gen_imp_df)
# CHanging the order for the plot
desired_order <- c("Defence", "Abortion", "Immigration", "Death_Pen", "Lib_Con",
                   "Ethnicity", "Age", "Income", "Religion", "Living",
                   "Education", "Marital", "Social", "Gender")

# Reorder the levels of the Variable column based on the desired order
gen_imp_df$Variable <- factor(gen_imp_df$Variable, levels = desired_order)

# Plot
ggplot(data = gen_imp_df) +
  geom_bar(aes(x = logImportance, y = Variable, fill = "AIC"), stat = "identity") +
  geom_bar(aes(x = -rfImportance, y = Variable, fill = "MDI"), stat = "identity") +
  labs(x = "Importance", y = "Variable") +
  scale_fill_manual(values = c("AIC" = "#69b3a2", "MDI" = "#404080")) +  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),  
    axis.title.y = element_text(size = 14),
    legend.title = element_blank()  
  )

############### Logistic Importance
log_importance_df <- data.frame(Variable = c("Defence", "Abortion", "Immigration", "Death", "Lib_Con",
                                             "Ethnicity", "Age", "Income", "Religion", "Living",
                                             "Education", "Marital", "Social", "Gender"),
                                Importance = c(313.6, 322.1, 439.2, 81.7, 243, 
                                               282.7, 1.9, 1.4, 68.2, 9.6, 
                                               1.9, 5.1, 2.6, 8.7)
                                
)

# Reorder the levels of Variable based on Importance
log_importance_df$Variable <- factor(log_importance_df$Variable, 
                                     levels =
                                       log_importance_df$Variable[order(log_importance_df$Importance, 
                                                                        decreasing = FALSE)])
# Create the bar plot
ggplot(data = log_importance_df, aes(x = Importance, y = Variable)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)), 
    axis.title.y = element_text(size = 18)
  ) +
  xlab("Change in AIC") +
  ylab("Variable")

######################## Plotting RF Importance
importance_df <- data.frame(
  Variable = c("Defense", "Abortion", "Immigration", "Death", "Lib_Con",
               "Ethnicity", "Age", "Income", "Religion", "Living",
               "Education", "Marital", "Social", "Gender"),
  Importance = c(389.19135, 291.02478, 280.69150, 239.45669, 192.21719,
                 188.13726, 177.14470, 163.93628, 146.56324, 118.69432,
                 116.62206, 102.85605, 72.73725, 43.77173)
)

# Reorder the levels of Variable based on Importance
importance_df$Variable <- factor(importance_df$Variable, 
                                 levels =
                                   importance_df$Variable[order(importance_df$Importance, 
                                                                decreasing = FALSE)])
# Create the bar plot
ggplot(data = importance_df, aes(x = Importance, y = Variable)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),  
    axis.title.y = element_text(size = 18)
  ) +
  xlab("MDI") +
  ylab("Variable")

########################## Plotting error rate for poster without OOB
### Plotting error of Original Model

error.data <- data.frame(
  Trees=rep(1:nrow(original_model$err.rate), times =2),
  Type = rep(c("Trump", "Biden"), each=nrow(original_model$err.rate)),
  Error=c(original_model$err.rate[,"Trump"],
          original_model$err.rate[,"Biden"]))

# Define color palette
my_colors <- c("Trump" = "firebrick2", "Biden" = "royalblue2")
# Leveling the legend
error.data$Type <- factor(error.data$Type,
                          levels = c("Trump","Biden"))

# Create plot
ggplot(data = error.data, aes(x = Trees, y = Error, color = Type)) +
  geom_line() +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),    
  )

 