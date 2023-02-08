# Loading data set
library(tidyverse)
hiv_data <- read_csv("HIV AIDS Data set.csv")

# Renaming columns
hiv_data <- rename(hiv_data, "Woman_ID"="Woman ID", "Age_group"="Age group", 
                   "Marital_status"="Marital state", "Current_marital_status"="Current marital state",
                   "Highest_educational_qualification"="Highest education qualification", 
                   "Frequency_of_reading_newspapers"="Frequency of reading newspapper",
                   "Frequency_of_watching_television"="Frequency of watching televisio",
                   "Frequency_of_radio_listening"="Frequency of listening to radio",
                   "Frequency_of_all_media_combined"="Frequency of all media combined",
                   "Given_birth"="Have you ever given birth",  
                   "Current_pregnancy_status"="Current pregnancy status",
                   "Working_status"="Working status", "Wealth_index"="Welth index")

Factors <- c("Woman_ID", "Residence", "Region", "Religion", "Ethnicity", "Age_group",
             "Marital_status", "Current_marital_status", "Highest_educational_qualification",
             "Frequency_of_reading_newspapers", "Frequency_of_watching_television",
             "Frequency_of_radio_listening", "Frequency_of_all_media_combined",
             "Given_birth", "Current_pregnancy_status", "Working_status", "Wealth_index",
             "Y1")

# Converting variable data type as a factor
hiv_data[Factors] <- lapply(hiv_data[Factors], factor)

library(skimr)
# Taking summary
summary(hiv_data) # Before dropping NAs and don't know cases 18302
skim(hiv_data)

# Deleting don't know cases
hiv_data <- filter(hiv_data, Y1 %in% c(1,2))
summary(hiv_data)
skim(hiv_data)

# Dropping missing values in variable Frequency_of_watching_television
hiv_data <- drop_na(hiv_data, "Frequency_of_watching_television")
summary(hiv_data) # After dropping NAs in Frequency_of_watching_television 15121
skim(hiv_data)

# Assigning factor level names for data analysis
library(magrittr)
hdata <- hiv_data

Residence <- hdata$Residence %>% recode_factor("1" = "Urban", "2" = "Rural", 
                                               "3" = "Estate")

Region <- hdata$Region %>% recode_factor("1" = "Western", "2" = "Central", "3" = "Southern",
                                         "4" = "Northern", "5" = "Eastern", "6" = "North Eastern",
                                         "7" = "North Central", "8" = "Uva", "9" = "Sabaragamuwa")

Religion <- hdata$Religion %>% recode_factor("1" = "Buddhist", "2" = "Hindu", "3" = "Islam",
                                             "4" = "Roman Catholic", "5" = "Other Christian", "6" = "Other")

Ethnicity <- hdata$Ethnicity %>% recode_factor("1" = "Sinhala", "2" = "Sri Lanka Tamil", 
                                               "3" = "Indian Tamil",
                                               "4" = "Sri Lanka Moor/Muslim", 
                                               "5" = "Malay", "6" = "Burger",
                                               "7" = "Other")

Age_group <- hdata$Age_group %>% recode_factor("1" = "15-19", "2" = "20-24", "3" = "25-29",
                                               "4" = "30-34", "5" = "35-39", "6" = "40-44",
                                               "7" = "45-49")

Marital_status <- hdata$Marital_status %>% recode_factor("1" = "Married or Living together", "2" = "Divorced/Separated", 
                                                         "3" = "Widowed")

Current_marital_status <- hdata$Current_marital_status %>% 
  recode_factor("1" = "Currently married", "2" = "Living with a man", 
                "3" = "Not in union/Husband died/Divorced/Separated")


Highest_educational_qualification <- hdata$Highest_educational_qualification %>%
  recode_factor("1" = "No education", "2" = "Passed grade 1-5", "3" = "Passed grade 6-10",
                "4" = "Passed G.C.E.(O/L) or equivalent", 
                "5" = "Passed G.C.E.(A/L) or equivalent",
                "6" = "Degree and above")

Frequency_of_reading_newspapers <- hdata$Frequency_of_reading_newspapers %>%
  recode_factor("1" = "At least once a week", "2" = "Less than once a week", 
                "3" = "Not at all")

Frequency_of_watching_television <- hdata$Frequency_of_watching_television %>%
  recode_factor("1" = "At least once a week", "2" = "Less than once a week", 
                "3" = "Not at all")

Frequency_of_radio_listening <- hdata$Frequency_of_radio_listening %>%
  recode_factor("1" = "At least once a week", "2" = "Less than once a week", 
                "3" = "Not at all")

Frequency_of_all_media_combined <- hdata$Frequency_of_all_media_combined %>%
  recode_factor("1" = "At least once a week", "2" = "Less than once a week", 
                "3" = "Not at all")

Given_birth <- hdata$Given_birth %>% recode_factor("1" = "Yes", "2" = "No")

Current_pregnancy_status <- hdata$Current_pregnancy_status %>% 
  recode_factor("1" = "Yes", "2" = "No", "8" = "Don't know")

Working_status <- hdata$Working_status %>% recode_factor("1" = "Yes", "2" = "No")

Wealth_index <- hdata$Wealth_index %>% recode_factor("1" = "Lowest", "2" = "Second", "3" = "Middle",
                                                     "4" = "Fourth", "5" = "Highest")

Y1 <- hdata$Y1 %>% recode_factor("1" = "Right", "2" = "Wrong")

hdata <- tibble(Residence, Region, Religion, Ethnicity, Age_group, Marital_status, Current_marital_status,
                Highest_educational_qualification, Frequency_of_reading_newspapers,
                Frequency_of_watching_television, Frequency_of_radio_listening,
                Frequency_of_all_media_combined, Given_birth, Current_pregnancy_status,
                Working_status, Wealth_index, Y1)

# Data exploration
# One qualitative variable

library(forcats)
library(ggplot2)

ggplot(hdata, aes(x = fct_infreq(Residence))) + geom_bar(fill="#7FA6F8", width = 0.5) +
  xlab("Residence")

ggplot(hdata, aes(x = fct_infreq(Region))) + geom_bar(fill="#7FA6F8", width = 0.6) +
  xlab("Region")

ggplot(hdata, aes(x = fct_infreq(Religion))) + geom_bar(fill="#7FA6F8", width = 0.6) +
  xlab("Religion")

ggplot(hdata, aes(x = fct_infreq(Ethnicity))) + geom_bar(fill="#7FA6F8", width = 0.6) +
  xlab("Ethnicity") 

ggplot(hdata, aes(x = Age_group)) + geom_bar(fill="forest green", width = 0.6) +
  xlab("Age Group")

ggplot(hdata, aes(x = fct_infreq(Marital_status))) + geom_bar(fill="forest green", width = 0.4) +
  xlab("Marital Status")

ggplot(hdata, aes(x = fct_infreq(Current_marital_status))) + geom_bar(fill="forest green", width = 0.3) +
  xlab("Current Marital Status")

ggplot(hdata, aes(x = Highest_educational_qualification)) + geom_bar(fill="forest green", width = 0.6) +
  xlab("Highest Educational Qualification") + coord_flip()

ggplot(hdata, aes(x = Frequency_of_reading_newspapers)) + geom_bar(fill="forest green", width = 0.4) +
  xlab("Frequency of reading Newspapers")

ggplot(hdata, aes(x = Frequency_of_watching_television)) + geom_bar(fill="forest green", width = 0.4) +
  xlab("Frequency of watching Television")

ggplot(hdata, aes(x = Frequency_of_radio_listening)) + geom_bar(fill="forest green", width = 0.4) +
  xlab("Frequency of listening to Radio")

ggplot(hdata, aes(x = Frequency_of_all_media_combined)) + geom_bar(fill="forest green", width = 0.4) +
  xlab("Frequency of all media combined")

ggplot(hdata, aes(x = Given_birth)) + geom_bar(fill="#7FA6F8", width = 0.4) +
  xlab("Given Birth")

ggplot(hdata, aes(x = fct_infreq(Current_pregnancy_status))) +
  geom_bar(fill="#7FA6F8", width = 0.6)  +
  xlab("Current Pregnancy Status") 

ggplot(hdata, aes(x = Working_status)) + geom_bar(fill="forest green", width = 0.4) +
  xlab("Working Status")

ggplot(hdata, aes(x = Wealth_index)) + geom_bar(fill="forest green", width = 0.6) +
  xlab("Wealth Index")


# Analysis of response
ggplot(hdata, aes(x = Y1)) + geom_bar(fill="forest green", width = 0.4) +
  xlab("People can get HIV virus from mosquito bites")

# Covariates with response
ggplot(hdata) + geom_bar(aes(x = Residence, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  ylab("Percent")


ggplot(hdata) + geom_bar(aes(x = Region, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  ylab("Percent") 


ggplot(hdata) + geom_bar(aes(x = Religion, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  ylab("Percent")
ggplot(hdata) + geom_bar(aes(x = Y1, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.3, show.legend = FALSE) +
  ylab("Percent") + facet_wrap(vars(Religion), scales = "free_y")


ggplot(hdata) + geom_bar(aes(x = Ethnicity, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  ylab("Percent")
ggplot(hdata) + geom_bar(aes(x = Y1, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.3, show.legend = FALSE) +
  ylab("Percent") + facet_wrap(vars(Ethnicity), scales = "free_y")


ggplot(hdata) + geom_bar(aes(x = Age_group, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  ylab("Percent")


ggplot(hdata) + geom_bar(aes(x = Marital_status, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Marital Status")
ggplot(hdata) + geom_bar(aes(x = Y1, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.3, show.legend = FALSE) +
  ylab("Percent") + facet_wrap(vars(Marital_status), scales = "free_y") 


ggplot(hdata) + geom_bar(aes(x = Current_marital_status, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Current marital status")
ggplot(hdata) + geom_bar(aes(x = Y1, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.3, show.legend = FALSE) +
  ylab("Percent") + facet_wrap(vars(Current_marital_status), scales = "free_y") 


ggplot(hdata) + geom_bar(aes(x = Wealth_index, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Wealth index")


ggplot(hdata) + geom_bar(aes(x = Highest_educational_qualification, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Highest educational qualification")
ggplot(hdata) + geom_bar(aes(x = Y1, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.3, show.legend = FALSE) +
  ylab("Percent") + facet_wrap(vars(Highest_educational_qualification), scales = "free_y") 


ggplot(hdata) + geom_bar(aes(x = Frequency_of_radio_listening, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Frequency of radio listening")


ggplot(hdata) + geom_bar(aes(x = Frequency_of_reading_newspapers, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Frequency of reading newspapers")


ggplot(hdata) + geom_bar(aes(x = Frequency_of_watching_television, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Frequency of watching television")


ggplot(hdata) + geom_bar(aes(x = Frequency_of_all_media_combined, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Frequency of all media combined")


ggplot(hdata) + geom_bar(aes(x = Given_birth, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Given birth")


ggplot(hdata) + geom_bar(aes(x = Working_status, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Working status")


ggplot(hdata) + geom_bar(aes(x = Current_pregnancy_status, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.5, position = "dodge") +
  labs(y = "Percent", x = "Current pregnancy status")
ggplot(hdata) + geom_bar(aes(x = Y1, y = after_stat(100*count/sum(count)),
                             fill = Y1), width = 0.3, show.legend = FALSE) +
  ylab("Percent") + facet_wrap(vars(Current_pregnancy_status), scales = "free_y") 


#=======================================
ggplot(hdata) + geom_bar(aes(x = Religion, fill = Religion), width = 0.5, position = "dodge") +
  facet_wrap(vars(Ethnicity), scales = "free_y")

ggplot(hdata) + geom_bar(aes(x = Residence, fill = Residence), width = 0.5, position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(Region), scales = "free_y") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
  

ggplot(hdata) + geom_bar(aes(x = Religion, fill = Religion), position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(Ethnicity), scales = "free_y") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 

ggplot(hdata) + geom_bar(aes(x = Current_marital_status, fill = Current_marital_status), position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(Marital_status), scales = "free_y") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 

ggplot(hdata) + geom_bar(aes(x = Current_pregnancy_status, fill = Current_pregnancy_status), position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(Given_birth), scales = "free_y") + labs(subtitle = "Birth given", x = "Current pregnancy status")+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 

  