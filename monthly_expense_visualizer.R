# install packages

install.packages("tidyverse")
library(tidyverse)
library(lubridate)
install.packages("readxl")
library(readxl)

# enter current cash value and run for current date

current_bank <- 5
current_date <- Sys.Date()

# enter dates and amounts of expenses

rent_dates <- ymd(c(20220801, 20220901, 20221001, 20221101, 20221201))
rent_amount <- 1050

rent_insur_dates <- ymd(c(20220702, 20220802, 20220902, 20221002, 20221102, 20221202))
rent_insur_amount <- 7.75

electric_dates <- ymd(c(20220730, 20220830, 20220930, 20221030, 20221130, 20221230))
electric_amount <- 35

gas_apt_dates <- ymd(c(20220706, 20220806, 20220906, 20221006, 20221106, 20221206))
gas_apt_amount <- 35

coursera_dates <- ymd(c(20220720))
coursera_amount <- 39

car_wash_dates <- ymd(c(20220714, 20220814, 20220914, 20221014, 20221114, 20221214))
car_wash_amount <- 15

internet_dates <- ymd(c(20220706, 20220806, 20220906, 20221006, 20221106, 20221206))
internet_amount <- 58

hulu_dates <- ymd(c(20220727, 20220827, 20220927, 20221027, 20221127, 20221227))
hulu_amount <- 23

phone_dates <- ymd(c(20220707, 20220807, 20220907, 20221007, 20221107, 20221207))
phone_amount <- 111

gym_dates <- ymd(c(20220730, 20220830, 20220930, 20221030, 20221130, 20221230))
gym_amount <- 15

dog_food_dates <- ymd(c(20220730, 20220830, 20220930, 20221030, 20221130, 20221230))
dog_food_amount <- 50

truck_payment_dates <- ymd(c(20220708, 20220808, 20220908, 20221008, 20221108, 20221208))
truck_payment_amount <- 350

truck_insurance_dates <- ymd(c(20220805, 20220905, 20221005, 20221105, 20221205))
truck_insurance_amount <- 120

gas_truck_dates <- ymd(c(20220703, 20220710, 20220717, 20220724, 20220731, 20220807,
                    20220814, 20220821, 20220828, 20220904, 20220911, 20220918,
                    20220925, 20221002, 20221009, 20221016, 20221023, 20221030,
                    20221106, 20221113, 20221120, 20221127, 20221204, 20221211,
                    20221218, 20221225))
gas_truck_amount <- 50

drugs_dates <- ymd(c(20220703, 20220710, 20220717, 20220724, 20220731, 20220807,
                     20220814, 20220821, 20220828, 20220904, 20220911, 20220918,
                     20220925, 20221002, 20221009, 20221016, 20221023, 20221030,
                     20221106, 20221113, 20221120, 20221127, 20221204, 20221211,
                     20221218, 20221225))
drugs_amount <- 35

food_dates <- ymd(c(20220703, 20220710, 20220717, 20220724, 20220731, 20220807,
                    20220814, 20220821, 20220828, 20220904, 20220911, 20220918,
                    20220925, 20221002, 20221009, 20221016, 20221023, 20221030,
                    20221106, 20221113, 20221120, 20221127, 20221204, 20221211,
                    20221218, 20221225))
food_amount <- 75

# Combine expenses into a data frame

# getting nowhere... rent <- tibble(rent_dates, rent_amount)

monthly_expenses <- data.frame(read_excel("Minor/Personal_stuff.xlsx", sheet="month"))

head(monthly_expenses)

monthly_expenses_1 <- rename(monthly_expenses, day_of_month=1)

head(monthly_expenses_1)

daily_expenses <- rowSums(select(monthly_expenses_1, - day_of_month), na.rm=TRUE)

mon_expenses <- mutate(monthly_expenses_1, daily_expenses)

head(mon_expenses)

# Summarize monthly expenses

days_with_expenses <- data.frame(mon_expenses) %>% 
  filter(daily_expenses > 0) %>% 
  group_by(day_of_month) %>% 
  arrange(day_of_month) %>% 
  relocate(daily_expenses, .after=day_of_month) %>% 

sum(daily_expenses)

head(days_with_expenses)  



