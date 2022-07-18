# install packages

install.packages("readxl")
library(readxl)
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)

# enter current cash value and run for current date

current_bank <- 8000
current_date <- Sys.Date()

# Import expenses dataframe

monthly_expenses <- read_excel("Month.xlsx", sheet="month")

monthly_expenses_1 <- rename(monthly_expenses, day_of_month=1)

daily_expenses <- rowSums(select(monthly_expenses_1, - day_of_month), na.rm=TRUE)

mon_expenses <- mutate(monthly_expenses_1, daily_expenses)

# Summarize monthly expenses

days_with_expenses <- mon_expenses %>% 
  filter(as.double(daily_expenses) > 0) %>% 
  group_by(day_of_month) %>% 
  arrange(day_of_month) %>% 
  relocate(daily_expenses, .after=day_of_month)

sum(daily_expenses)

View(days_with_expenses)  

# Future Analysis

date_expenses <- data.frame(date = c(current_date, current_date+1, current_date+2, 
                                     current_date+3, current_date+4, current_date+5, 
                                     current_date+6,current_date+7, current_date+8, 
                                     current_date+9, current_date+10, current_date+11, 
                                     current_date+12, current_date+13,current_date+14, 
                                     current_date+15, current_date+16, current_date+17, 
                                     current_date+18, current_date+19, current_date+20, 
                                     current_date+21, current_date+22, current_date+23, 
                                     current_date+24, current_date+25, current_date+26, 
                                     current_date+27, current_date+28, current_date+29, 
                                     current_date+30, current_date+31, current_date+32, 
                                     current_date+33, current_date+34)) %>% 
  mutate(day = as.double(format(date, format="%d")))

dates_with_expenses <- days_with_expenses %>% 
  select(day_of_month, daily_expenses) %>%  rename(day=day_of_month)

actual_date_expenses <- full_join(date_expenses, dates_with_expenses, by="day") 
actual_date_expenses[is.na(actual_date_expenses)] <-0

daily_summary <- actual_date_expenses %>% select(date, daily_expenses) %>% 
  arrange(date) %>% 
  mutate(bank_start_day=c(current_bank, current_bank-sum(daily_expenses[c(1)]), current_bank-sum(daily_expenses[c(1:2)]), 
                        current_bank-sum(daily_expenses[c(1:3)]), current_bank-sum(daily_expenses[c(1:4)]), 
                        current_bank-sum(daily_expenses[c(1:5)]), current_bank-sum(daily_expenses[c(1:6)]), 
                        current_bank-sum(daily_expenses[c(1:7)]), current_bank-sum(daily_expenses[c(1:8)]), 
                        current_bank-sum(daily_expenses[c(1:9)]), current_bank-sum(daily_expenses[c(1:10)]), 
                        current_bank-sum(daily_expenses[c(1:11)]), current_bank-sum(daily_expenses[c(1:12)]), 
                        current_bank-sum(daily_expenses[c(1:13)]), current_bank-sum(daily_expenses[c(1:14)]), 
                        current_bank-sum(daily_expenses[c(1:15)]), current_bank-sum(daily_expenses[c(1:16)]), 
                        current_bank-sum(daily_expenses[c(1:17)]), current_bank-sum(daily_expenses[c(1:18)]), 
                        current_bank-sum(daily_expenses[c(1:19)]), current_bank-sum(daily_expenses[c(1:20)]), 
                        current_bank-sum(daily_expenses[c(1:21)]), current_bank-sum(daily_expenses[c(1:22)]), 
                        current_bank-sum(daily_expenses[c(1:23)]), current_bank-sum(daily_expenses[c(1:24)]),
                        current_bank-sum(daily_expenses[c(1:25)]), current_bank-sum(daily_expenses[c(1:26)]), 
                        current_bank-sum(daily_expenses[c(1:27)]), current_bank-sum(daily_expenses[c(1:28)]), 
                        current_bank-sum(daily_expenses[c(1:29)]), current_bank-sum(daily_expenses[c(1:30)]),
                        current_bank-sum(daily_expenses[c(1:31)]), current_bank-sum(daily_expenses[c(1:32)]),
                        current_bank-sum(daily_expenses[c(1:33)]), current_bank-sum(daily_expenses[c(1:34)]))) %>% 
  mutate(bank_end_day=c(bank_start_day - daily_expenses))
  
ggplot(daily_summary, aes(x=date, y=bank_end_day, color="blue"))+
  geom_smooth(stat="identity") + ggtitle("Cash Available - Next 35 Days") +
  annotate("text", x=as.Date("2022-07-18"), y=7650, label="Start with $8,000") +
  annotate("text", x=as.Date("2022-08-15"), y=5700, label="End with $5,291 on 8/19")
