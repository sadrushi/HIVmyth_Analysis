# Loading data set
library(tidyverse)
hiv_data <- read_csv("HIV AIDS Data Set.csv")

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

Factors <- c( "Residence", "Region", "Religion", "Ethnicity", "Age_group",
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


contrasts(hiv_data$Y1)


# Fitting the model
hiv_data <- mutate(hiv_data, Id = row_number())
set.seed(123)
train_hiv_data <- sample_frac(hiv_data, 0.80)
dim(train_hiv_data)

test_hiv_data <- anti_join(hiv_data, train_hiv_data, by = "Id")
dim(test_hiv_data)

full <- glm(Y1~Residence+Region+Religion+Ethnicity+Age_group+Marital_status+
              Current_marital_status+Highest_educational_qualification+
              Frequency_of_reading_newspapers+Frequency_of_watching_television+
              Frequency_of_radio_listening+Frequency_of_all_media_combined+
              Given_birth+Current_pregnancy_status+Working_status+Wealth_index,
            data = train_hiv_data, family = binomial)
summary(full)

library(MASS)
backward <- stepAIC(full, direction = "backward", trace = FALSE)
backward$anova


fitted_model <- glm(Y1 ~ Region + Ethnicity + Age_group + Current_marital_status + 
                      Highest_educational_qualification + Frequency_of_reading_newspapers + 
                      Frequency_of_radio_listening + Wealth_index, data = train_hiv_data,
                    family = binomial)
summary(fitted_model)

# Goodness of fit of the model
library(ResourceSelection)

test_hiv_data$Y1 <- as.numeric(as.character(test_hiv_data$Y1))
test_hiv_data$Y1[test_hiv_data$Y1 == 1] <- 0
test_hiv_data$Y1[test_hiv_data$Y1 == 2] <- 1

model <- glm(Y1 ~ Region + Ethnicity + Age_group + Current_marital_status + 
               Highest_educational_qualification + Frequency_of_reading_newspapers + 
               Frequency_of_radio_listening + Wealth_index, data = test_hiv_data,
             family = binomial)

hoslem.test(test_hiv_data$Y1, fitted(model))









