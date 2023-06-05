library(tidyverse)
library(ggplot2)

# Import excel data and store in variables
clinical <- read_csv("clinical-study.csv")
protein <- read_csv("protein-levels.csv")


## Cleaning Data ##
clinical <-  rename(
  clinical,
  participant_id = subject_id,
  treatment = trt_grp,
  response = RESPONSE
)

protein <- rename(
  protein,
  concentration = protein_concentration
)

clinical <- filter(clinical, age>=18)
clinical <- distinct(clinical, participant_id, .keep_all = TRUE)

# Replace with mean so that it does not introduce bias to incumbent mean.
clinical <- replace_na(clinical, list(weight=91.39))
protein <- replace_na(protein, list(concentration=121.7))

clinical <- mutate(clinical, clinical, response = as_factor(response))
clinical <- mutate(clinical, clinical, BMI = weight/height**2)

clinical <- left_join(clinical, protein, by = "participant_id")


## Aggregating data ##

# Checking patient age/weight disparity  between the DRUG and CONTROL groups.
cmpr_demo <- clinical %>%
  group_by(treatment)
cmpr_demo %>% summarise(average_weight = mean(weight), average_age = mean(age))
#' Average weight is significantly high for control group at ~1.2% increase.
#' Average age is similar, with only .1 difference in mean.

# Comparing mean weight and age in responders vs non responders
cmpr_demo_response <- clinical %>%
  group_by(response)
cmpr_demo_response %>%
  summarise(average_weight = mean(weight), average_age = mean(age))
#'Average weight among responders is 3.3kg higher.

# Comparing response in treatment groups
cmpr_responses <- clinical %>%
  filter(response=="Y")%>%
  group_by(treatment)
cmpr_responses %>%  summarise(no_of_responses = n())
#' GSK's drug is twice as effective as the control drug, at a 69.35% increase.

# Comparing protein concentration between responders and non responders
cmpr_protein <- clinical %>%
  group_by(response) %>%
  summarise(avg_concentration = mean(concentration))
#' The average concentration of responders is much lower (22%) decrease compared
#' to non responders. This means that both drugs are effective when the protein
#' concentration is around 105.


## Plotting Graph##

# Age distribution among sexes
ggplot(cmpr_demo, aes(x=response, y=age, colour=treatment)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = FALSE)
#' Responders using GSK drug were slightly older on average compared to
#' responders using the control drug.
#' There is also a wider quartile age range for GSK drug responders.
#' Non Responders for both treatment groups have the same average age.

#Trend between BMI and Response
ggplot(cmpr_demo, aes(x=response, y=BMI, colour=treatment)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = FALSE)
#' Non responders had similar median BMI
#' GSK drug responders had a lower median and upper quartile than control group

# Trend between response and protein concentration
ggplot(cmpr_demo, aes(x=response, y=concentration, colour=treatment)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = FALSE)
#' Protein concentration among responders for both groups had the same median
#' and similar lower/upper quartile ranges.
#' Non responders for GSK drug had significantly higher lower/upper quartile 
#' range and median.


# OUTPUT #
# > source("/cloud/project/script.R", echo=TRUE)

# > library(tidyverse)

# > library(ggplot2)

# > # Import excel data and store in variables
# > clinical <- read_csv("clinical-study.csv")
# Rows: 772 Columns: 7                                                                                                                                
# ── Column specification ─────────────────────────────────────────────────────────────────────────────────────────
# Delimiter: ","
# chr (4): subject_id, sex, trt_grp, RESPONSE
# dbl (3): age, weight, height

# ℹ Use `spec()` to retrieve the full column specification for this data.
# ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# > protein <- read_csv("protein-levels.csv")
# Rows: 768 Columns: 2                                                                                                                                
# ── Column specification ─────────────────────────────────────────────────────────────────────────────────────────
# Delimiter: ","
# chr (1): participant_id
# dbl (1): protein_concentration

# ℹ Use `spec()` to retrieve the full column specification for this data.
# ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# > ## Cleaning Data ##
# > clinical <-  rename(
# +   clinical,
# +   participant_id = subject_id,
# +   treatment = trt_grp,
# +   response = RESPONSE
# + )

# > protein <- rename(
# +   protein,
# +   concentration = protein_concentration
# + )

# > clinical <- filter(clinical, age>=18)

# > clinical <- distinct(clinical, participant_id, .keep_all = TRUE)

# > # Replace with mean so that it does not introduce bias to incumbent mean.
# > clinical <- replace_na(clinical, list(weight=91.39))

# > protein <- replace_na(protein, list(concentration=121.7))

# > clinical <- mutate(clinical, clinical, response = as_factor(response))

# > clinical <- mutate(clinical, clinical, BMI = weight/height**2)

# > clinical <- left_join(clinical, protein, by = "participant_id")

# > ## Aggregating data ##
# > 
# > # Checking patient age/weight disparity  between the DRUG and CONTROL groups.
# > cmpr_demo <- clinical %>%
# +   group_by(t .... [TRUNCATED] 

# > cmpr_demo %>% summarise(average_weight = mean(weight), average_age = mean(age))
# # A tibble: 2 × 3
#   treatment average_weight average_age
#   <chr>              <dbl>       <dbl>
# 1 CONTROL             91.9        61.9
# 2 DRUG                90.8        61.8

# > #' Average weight is significantly high for control group at ~1.2% increase.
# > #' Average age is similar, with only .1 difference in mean.
# > 
# > # Co .... [TRUNCATED] 

# > cmpr_demo_response %>%
# +   summarise(average_weight = mean(weight), average_age = mean(age))
# # A tibble: 2 × 3
#   response average_weight average_age
#   <fct>             <dbl>       <dbl>
# 1 N                  90.0        61.7
# 2 Y                  93.3        61.9

# > #'Average weight among responders is 3.3kg higher.
# > 
# > # Comparing response in treatment groups
# > cmpr_responses <- clinical %>%
# +   filter(respons .... [TRUNCATED] 

# > cmpr_responses %>%  summarise(no_of_responses = n())
# # A tibble: 2 × 2
#   treatment no_of_responses
#   <chr>               <int>
# 1 CONTROL               124
# 2 DRUG                  210

# > #' GSK's drug is twice as effective as the control drug, at a 69.35% increase.
# > 
# > # Comparing protein concentration between responders and non res .... [TRUNCATED] 

# > #' The average concentration of responders is much lower (22%) decrease compared
# > #' to non responders. This means that both drugs are effective wh .... [TRUNCATED] 
# `geom_smooth()` using formula = 'y ~ x'

# > #' Responders using GSK drug were slightly older on average compared to
# > #' responders using the control drug.
# > #' There is also a wider quartile  .... [TRUNCATED] 
# `geom_smooth()` using formula = 'y ~ x'

# > #' Non responders had similar median BMI
# > #' GSK drug responders had a lower median and upper quartile than control group
# > 
# > # Trend between resp .... [TRUNCATED] 
# `geom_smooth()` using formula = 'y ~ x'

# > #' Protein concentration among responders for both groups had the same median
# > #' and similar lower/upper quartile ranges.
# > #' Non responders for  .... [TRUNCATED] 