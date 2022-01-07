library(here)
library(readr)
library(dplyr)
library(skimr)

library(pdftools)

library(stringr)
library(magrittr)

library(purrr)

library(tibble)

library(tidyr)
library(ggplot2)
library(ggpubr)
library(forcats)
library(lme4)
library(lmerTest)
library(car)
library(ggiraph)
library(ggforce)
library(viridis)
library(cowplot)

#Data Import
diet_data <- readr::read_csv(here("data", "raw", "dietary_risk_exposure_all_ages_2017.csv"))
sep_age_diet_data <- read_csv(here("data", "raw", "dietary_risk_exposure_sep_ages_2017.csv"))

#Data exploration
dplyr::glimpse(diet_data)
dplyr::glimpse(sep_age_diet_data)

#summary statistics for categorical and numerical data
skimr::skim(diet_data)
skim(sep_age_diet_data)

#Data wrangling
diet_data <- dplyr::rename(diet_data, dietary_risk = rei_name)
sep_age_diet_data <- dplyr::rename(sep_age_diet_data, dietary_risk = rei_name)

glimpse(diet_data)
glimpse(sep_age_diet_data)

dplyr::distinct(diet_data, dietary_risk)
# or 
diet_data %>% 
  distinct(dietary_risk)

#to see if  distinct values of this column are the same in both dataframes
dplyr::setequal(
  distinct(diet_data, location_name),
  distinct(sep_age_diet_data, location_name)
)

sep_age_diet_data %>%
  distinct(location_name) %>%
  pull()

diet_data %>%
  dplyr::arrange(mean) %>%
  glimpse()

sep_age_diet_data %>%
  dplyr:: count(age_group_name)

sep_age_diet_data %>%
  count(location_name, sex, age_group_name)

sep_age_diet_data %>%
  dplyr::filter(
    sex == "Female",
    age_group_name == "25 to 29",
    location_name == "Afghanistan"
  )

#importing data from the pdf paper
paper <- pdftools::pdf_text(here("data", "raw", "Afshin_et_al_2019.pdf"))

#saving as an rda file - R data file
save(diet_data, sep_age_diet_data, paper, file = here::here("data", "imported", "imported_data.rda"))

#now at any point we could come back and we could load our imported data like so:
load(here::here("data", "imported", "imported_data.rda"))

summary(paper)
#each page is a character, so we select the third page

pdf_table <- paper[3]
summary(pdf_table)
glimpse(pdf_table,nchar.max=800)

pdf_table %<>%
  stringr::str_split(pattern = "Diet")

glimpse(pdf_table,nchar.max=800)

pdf_table %<>%
  unlist()

summary(pdf_table)
dplyr:: first(pdf_table)
nth(pdf_table,2)
last(pdf_table)
nth(pdf_table,-1) #-1 is last
nth(pdf_table,-2) #-2 is second to last
 
pdf_table %<>%
  str_subset(pattern="high in|low in")
first(pdf_table)
last(pdf_table)

pdf_table %<>%
  stringr::str_replace_all(
    pattern = "·",
    replacement = "."
  )
last(pdf_table)

#\\s is interpreted as a space as the \\ indicates that the s should be interpreted as a special character and not simply the letter s. The {2,} indicates two or more spaces, while {2} would indicate exactly two spaces.

table_split <- str_split(
  string = pdf_table,
  pattern = "\\s{2,}"
)
glimpse(table_split)

pdf_table %>%
  str_which(pattern = "seafood|sugar")

pdf_table %>%
  str_subset(pattern = "seafood|sugar") #same as pdf_table[str_which(pdf_table, pattern = "seafood|sugar")]

pdf_table[str_which(pdf_table,
                    pattern =
                      "seafood|sugar"
)] <- str_replace(
  string = pdf_table[str_which(pdf_table,
                               pattern =
                                 "seafood|sugar"
  )],
  pattern = "Mean",
  replacement = " Mean"
)

table_split <- str_split(pdf_table, pattern = "\\s{2,}")

glimpse(table_split)

category <- map(table_split,1)
amount <- purrr::map(table_split,3)
glimpse(category)
glimpse(amount)

class(category)
class(amount)

category %<>% unlist()
amount %<>% unlist()
class(category)

#Now we will create a tibble, which is an important data frame structure in the tidyverse which allows us to use other packages in the tidyverse with our data.
guidelines <- tibble::tibble(
  category = category,
  amount = amount
)
guidelines


guidelines %<>%
  tidyr::separate(amount,
                  c("optimal", "lower", "upper","extra"),
                  sep = "\\(|–|\\)"
  )

guidelines

guidelines %<>%
  separate(category, c("direction", "food"), sep = " in ")
guidelines

guidelines %<>%
  separate(optimal,
           into = c("optimal", "unit"),
           sep = " |%",
           remove = FALSE
  )
guidelines

guidelines %<>%
  na_if("") %<>%
  replace_na(list(unit="%"))
guidelines

guidelines %<>% 
  purrr::modify_at("extra", ~NULL)

guidelines %<>%
  relocate(unit, .after = last_col())
guidelines
guidelines %<>%
  mutate(across(lower:upper, as.numeric))


guidelines %<>%
  mutate_at(
    vars("food"),
    ~str_replace_all(
      string= .,
      pattern = "sugar-sweetened",
      replacement = "sugar-sweetened beverages"
    )
  )

guidelines %<>%
  mutate_at(
    vars(food),
    ~ str_replace(
      string = .,
      pattern = "polyunsaturated",
      replacement = "polyunsaturated fatty acids"
    )
  )

guidelines

guidelines %<>%
  mutate_at(vars(direction), str_trim)

guidelines

glimpse(diet_data)    
diet_data

distinct(diet_data, dietary_risk)
diet_data %<>%
  separate(dietary_risk,
           c("direction","food"),
           sep=" in "
  )

distinct(diet_data,dietary_risk)

diet_data %<>%
  mutate_at(vars(dietary_risk), str_trim)

sep_age_diet_data %<>%
  separate(dietary_risk,
           c("direction","food"),
           sep=" in "
  )

setequal(
  distinct(diet_data, dietary_risk),
  distinct(sep_age_diet_data, dietary_risk)
)

dplyr::intersect(
  names(diet_data),
  names(guidelines)
)
guidelines %<>%
  rename(
    upper_optimal = upper,
    lower_optimal = lower,
    unit_optimal = unit
  )

guidelines

dplyr::bind_cols(
  count(diet_data, unit, food),
  count(sep_age_diet_data, unit, food),
  count(guidelines, unit_optimal, food)
)

guidelines %<>%
  mutate(lower_optimal = dplyr::if_else(
    condition = unit_optimal == "mg",
    true = lower_optimal / 1000,
    false = lower_optimal
  ))

# "optimal" variable
guidelines %<>%
  mutate(optimal = if_else(condition = unit_optimal == "mg",
                           true = optimal / 1000,
                           false = optimal
  ))

# "upper_optimal" variable
guidelines %<>%
  mutate(upper_optimal = if_else(condition = unit_optimal == "mg",
                                 true = upper_optimal / 1000,
                                 false = upper_optimal
  ))


# replace "mg" with "g" in the "unit_optimal" variable
guidelines %<>%
  mutate(unit_optimal = if_else(condition = unit_optimal == "mg",
                                true = "g",
                                false = unit_optimal
  ))

guidelines

diet_and_guidelines <- diet_data %>%
  dplyr::full_join(guidelines, by = "food")

all_age_diet_and_guidelines <- sep_age_diet_data %>%
  full_join(guidelines, by = "food")

glimpse(diet_and_guidelines)

arrange(guidelines, food)

diet_and_guidelines %<>%
  mutate(Relative_Percent = (mean / optimal) * 100)

all_age_diet_and_guidelines %<>%
  mutate(Relative_Percent = (mean / optimal) * 100)
diet_and_guidelines %<>%
  mutate(range_percent = case_when(
    direction == "high" ~ (mean / upper_optimal) * 100,
    direction == "low" ~ (mean / lower_optimal) * 100
  ))

all_age_diet_and_guidelines %<>%
  mutate(range_percent = case_when(
    direction == "high" ~ (mean / upper_optimal) * 100,
    direction == "low" ~ (mean / lower_optimal) * 100
  ))


diet_and_guidelines %<>%
  mutate(percent_over_under = case_when(
    direction == "high" & mean > upper_optimal ~
      ((mean - upper_optimal) / upper_optimal) * 100,
    direction == "high" & mean <= upper_optimal ~ 0,
    direction == "low" & mean >= lower_optimal ~ 0,
    direction == "low" & mean < lower_optimal ~
      ((lower_optimal - mean) / lower_optimal) * -100
  ))


all_age_diet_and_guidelines %<>%
  mutate(percent_over_under = case_when(
    direction == "high" & mean > upper_optimal ~
      ((mean - upper_optimal) / upper_optimal) * 100,
    direction == "high" & mean <= upper_optimal ~ 0,
    direction == "low" & mean >= lower_optimal ~ 0,
    direction == "low" & mean < lower_optimal ~
      ((lower_optimal - mean) / lower_optimal) * -100
  ))

#Another option is to create a binary outcome indicating whether optimal consumption was achieved or not.

diet_and_guidelines %<>%
  mutate(opt_achieved = if_else(
    condition = direction == "low" & mean > lower_optimal |
      direction == "high" & mean < upper_optimal,
    true = "Yes",
    false = "No"
  ))

all_age_diet_and_guidelines %<>%
  mutate(opt_achieved = if_else(
    condition = direction == "low" & mean > lower_optimal |
      direction == "high" & mean < upper_optimal,
    true = "Yes",
    false = "No"
  ))

glimpse(diet_and_guidelines)

diet_and_guidelines_long <- diet_and_guidelines %>%
  pivot_longer(
    cols = contains("percent"),
    names_to = "percent_type",
    values_to = "percent"
  )

all_age_diet_and_guidelines_long <- all_age_diet_and_guidelines %>%
  pivot_longer(
    cols = contains("percent"),
    names_to = "percent_type",
    values_to = "percent"
  )

save(all_age_diet_and_guidelines, all_age_diet_and_guidelines_long, diet_and_guidelines, sep_age_diet_data, 
     file = here::here("data", "wrangled", "wrangled_data.rda"))

write_csv(all_age_diet_and_guidelines, file = here::here("data", "wrangled", "all_age_diet_and_guidelines.csv"))
write_csv(all_age_diet_and_guidelines_long, file = here::here("data", "wrangled", "all_age_diet_and_guidelines_long.csv"))
write_csv(diet_and_guidelines, file = here::here("data", "wrangled", "diet_and_guidelines.csv"))
write_csv(sep_age_diet_data, file = here::here("data", "wrangled", "sep_age_diet_data.csv"))

load(here::here("data", "wrangled", "wrangled_data.rda"))

diet_and_guidelines %>%
  filter(location_name=="Global") %>%
  arrange(Relative_Percent) %>%
  print(1000)

diet_and_guidelines %>%
  select(Relative_Percent) %>%
  summary()

diet_and_guidelines %>%
  arrange(-Relative_Percent) %>%
  glimpse()

diet_and_guidelines %>%
  select(contains("percent")) %>%
  summary()

diet_and_guidelines %>%
  glimpse()

diet_and_guidelines %>%
  count(location_name, sex, opt_achieved) %>%
  arrange(-n) %>%
  # this allows us to show the full output
  print(n = 1e3)

