##-- analysis ----
#### descriptive stats ####
# count NA
na_counts <- sapply(master_data, function(x) sum(is.na(x)))
print(na_counts)

# descriptive stats
install.packages("psych")
library(psych)
describe(master_data)

# 4 yrs grad rate per year
master_data_ave_gradrate <- master_data %>% 
  group_by(year) %>% 
  mutate(
    ave_gradrate = mean(tot_gradrate_4yr)
  )

ggplot(master_data_ave_gradrate, mapping = aes(x = year, y = ave_gradrate))+
  geom_line()+
  labs(title = "4-year graduation rate", x = "year", y = "4-year graduation rate")

# fraction of schools on semesters
master_data_semester_rate <- master_data_ave_gradrate %>% 
  group_by(year) %>% 
  mutate(
    semester_rate = mean(semester)
  )

ggplot(master_data_semester_rate, mapping = aes(x = year, y = semester_rate))+
  geom_line()+
  labs(title = "Fraction of schools on semesters", x = "year", y = "Fraction of schools on semesters")

#share of female / white students
master_data_fem_white_share <- master_data_semester_rate %>% 
  group_by(.drop = TRUE) %>% 
  mutate(
    white_cohortsize = as.numeric(white_cohortsize),
    instatetuition = as.numeric(instatetuition),
    women_share = w_cohortsize/totcohortsize,
    white_share = white_cohortsize/totcohortsize
  )

# create scatter plot function
scatter_plot <- function(z) {
  ggplot(master_data_fem_white_share)+
    aes(x = z, y = tot_gradrate_4yr)+
    geom_point(alpha = 0.3, color = "darkblue")
}

# by tuition
scatter_plot(master_data_fem_white_share$instatetuition)+
  labs(title = "4-year graduation rate by tuition", x = "tuition", y = "4-year graduation rate")

# by share of females
scatter_plot(master_data_fem_white_share$women_share)+
  labs(title = "4-year graduation rate by share of female students", x = "Share of female students", y = "4-year graduation rate")

# by share of whites
scatter_plot(master_data_fem_white_share$white_share)+
  labs(title = "4-year graduation rate by share of white students", x = "Share of white students", y = "4-year graduation rate")



#### regression ####
lm <- lm(master_data_fem_white_share$tot_gradrate_4yr ~ master_data_fem_white_share$post_intro)
summary(lm)

