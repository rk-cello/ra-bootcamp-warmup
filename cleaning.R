##-- cleaning ----
#### semester data ####

library(tidyverse)

# import data 1
# rename column names
semester_dummy_1 <- read_csv("/Users/reinakishida/Desktop/warmup training package/01_data/raw/semester_dummy/semester_data_1.csv")
colnames(semester_dummy_1) <- semester_dummy_1[1,]
semester_dummy_1 <- semester_dummy_1[-1,]

# import data 2
semester_dummy_2 <- read_csv("/Users/reinakishida/Desktop/warmup training package/01_data/raw/semester_dummy/semester_data_2.csv")
# rename colnames
colnames(semester_dummy_2) <- colnames(semester_dummy_1)

# bind rows
selected_cols <- c("unitid", "semester","quarter","year","Y")
semester_dummy_1[selected_cols] <- apply(semester_dummy_1[selected_cols], 2, as.numeric)
semester_dummies <- bind_rows(semester_dummy_1, semester_dummy_2)
semester_dummies

# drop Y
semester_dummies_trunc <- semester_dummies %>% 
  select(-Y)

# create column : year of semester introduction

semester_dummies_semyear <- semester_dummies_trunc %>% 
  mutate(
    semyear = semester*year
  )

semester_dummies_semyear$semyear[semester_dummies_semyear$semyear == 0] <- 10000


semester_dummies_intro <- semester_dummies_semyear %>% 
  group_by(unitid) %>% 
  mutate(
    intro_year = min(semyear)
  )

# NA is no introduction of semester system
semester_dummies_semyear$semyear[semester_dummies_semyear$semyear == 10000] <- NA
semester_dummies_intro$intro_year[semester_dummies_intro$intro_year == 10000] <- NA
semester_dummies_intro$semyear[semester_dummies_intro$semyear == 10000] <- NA

# create post semester system introduction dummy
semester_dummies_post <- semester_dummies_intro %>% 
  mutate(
    post_intro = ifelse(intro_year <= year, 1, 0)
    )

#### graduate data ####
# import data
year_vec <- c(1991:1993, 1995:2016)
library(readxl)

file_paths <- paste0("/Users/reinakishida/Desktop/warmup training package/01_data/raw/outcome/", year_vec, ".xlsx")
data_list <- lapply(file_paths, read_excel)
combined_grad_data <- do.call(rbind, data_list)

# m_4yrgrads, totcohortsize chr to num
combined_grad_data_num <- combined_grad_data %>% 
  mutate(
    m_4yrgrads = as.numeric(m_4yrgrads),
    totcohortsize = as.numeric(totcohortsize)
  )

# women 4yrs grad rate to 0-1 scale
# men 4yrs grad rate
# total 4yrs grad rate
combined_grad_rate <- combined_grad_data_num %>% 
  mutate(
    women_gradrate_4yr = women_gradrate_4yr*0.01,
    men_gradrate_4yr = m_4yrgrads/m_cohortsize,
    tot_gradrate_4yr = tot4yrgrads/totcohortsize
  )
View(combined_grad_rate)

# round
combined_grad_round <- combined_grad_rate %>% 
  mutate(
    women_gradrate_4yr = round(women_gradrate_4yr, digits = 3),
    men_gradrate_4yr = round(men_gradrate_4yr, digits = 3),
    tot_gradrate_4yr = round(tot_gradrate_4yr, digits = 3)
  )

# 1991-2010 df
combined_grad_91to10 <- combined_grad_round %>% 
  filter(year <= 2010)


#### covar data ####

# import data
covars <- read_excel("/Users/reinakishida/Desktop/warmup training package/01_data/raw/covariates/covariates.xlsx")

# rename column
covars <- covars %>% 
  rename(unitid = university_id)

# erase aaaa
covars <- covars %>% 
  mutate(
    unitid = substr(unitid,1,6)
  )

# pivot wider
covars_wide <- covars %>% 
  pivot_wider(names_from = category, values_from = value)

# check unique values (years) in outcome and semester dummy
print(unique(semester_dummies_post$year))
print(unique(combined_grad_91to10$year))

# covar data: filter years
filtered_covars_wide <- covars_wide %>% 
  filter(1991 <= year, year <= 2010, year!=1994) %>% 
  mutate(
    unitid = as.numeric(unitid)
  )

# let covar data match with outcome data unitid
id_filtered_covars_wide <- semi_join(filtered_covars_wide, combined_grad_91to10, by = "unitid")
View(id_filtered_covars_wide)


#### master data ####
# rename data
semester_data <- semester_dummies_post
covariates_data <- id_filtered_covars_wide %>% 
  mutate(
    year = as.numeric(year)
  )
gradrate_data <- combined_grad_91to10

# left join data
sem_covar_data <- left_join(semester_data, covariates_data, join_by("unitid", "year"))
master_data <- left_join(sem_covar_data, gradrate_data, join_by("unitid", "year"))



setwd("/Users/reinakishida/Desktop/warmup training package/01_data/ra-bootcamp-warmup")
