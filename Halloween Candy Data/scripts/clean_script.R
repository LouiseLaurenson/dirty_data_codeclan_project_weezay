library(tidyverse)
library(readxl)
library(janitor)

boing_candy_2015 <-
  read_excel("candy_ranking_raw_data/boing-boing-candy-2015.xlsx")
boing_candy_2016 <-
  read_excel("candy_ranking_raw_data/boing-boing-candy-2016.xlsx")
boing_candy_2017 <-
  read_excel("candy_ranking_raw_data/boing-boing-candy-2017.xlsx")


boing_candy_2015 <- boing_candy_2015 %>%
  clean_names()
boing_candy_2016 <- boing_candy_2016 %>%
  clean_names()
boing_candy_2017 <- boing_candy_2017 %>%
  clean_names()

#cleaning 2015 ---------------------------------------------------------

#first clean as massive chunk at the end not needed
candy_2015 <- boing_candy_2015 %>%
  select(1:96, 115)

# further clean by column name
candy_2015_clean <- candy_2015 %>%
  select(-12, -16, -17, -18, -23, -26, -27,
         -28, -33, -34, -38, -41, -45, -56, -88, -82, -93, -94, -95)


#rename starting columns to make a standered across all three

candy_2015_clean <- candy_2015_clean %>%
  rename(age = how_old_are_you,
         going_trick_or_treating =
           are_you_going_actually_going_trick_or_treating_yourself)

#rename some columns to read better

candy_2015_clean <- candy_2015_clean %>%
  rename(
    penut_butter_kisses = anonymous_brown_globs_that_come_in_black_and_orange_wrappers,
    box_of_raisins = box_o_raisins,
    now_and_laters = nown_laters,
    tolberone = tolberone_something_or_other,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    circus_peanut = those_odd_marshmallow_circus_peanut_things
  )

#clean age column 
#making in to int and removing after . point AND 
#removing ages over 120 and under 2

candy_2015_clean_age <- candy_2015_clean %>%
  mutate(age = str_remove(age, pattern = "[\\.][0-9]|[>]")) %>%
  mutate(age = as.integer(age)) %>%  
  filter(!age > 120) %>%
  filter(!age < 2)

#changing time stamp to just year column 

candy_2015_clean_year <- candy_2015_clean_age %>% 
  mutate(timestamp = str_sub(timestamp, start = 1, end = 4)) %>% 
  rename(year = timestamp)


#clean 2016 -------------------------------------------------------------

#first clean as massive chunk at the end not needed
candy_2016 <- boing_candy_2016 %>%
  select(1:106)


#anything thats is just not a sweet
candy_2016_clean <- candy_2016 %>%
  select(-6, -12, -15, -19, -21, -22, -26,
         -27, -31, -32, -38, -43, -49, -69, -79, -90, -92, -101, -102,
         -103, -104, -105)

#rename starting columns to make a standered across all three
candy_2016_clean <- candy_2016_clean %>%
  rename(
    age = how_old_are_you,
    going_trick_or_treating =
      are_you_going_actually_going_trick_or_treating_yourself,
    gender = your_gender,
    country = which_country_do_you_live_in
  )

#renaming anything that's typed wrong 
candy_2016_clean <- candy_2016_clean %>%
  rename(
    penut_butter_kisses = anonymous_brown_globs_that_come_in_black_and_orange_wrappers,
    bonkers = bonkers_the_candy,
    box_of_raisins = boxo_raisins,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    now_and_laters = nown_laters,
    sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature,
    sweetums = sweetums_a_friend_to_diabetes,
    circus_peanut = those_odd_marshmallow_circus_peanut_things,
    tolberone = tolberone_something_or_other
  )

#clean age column

#making in to int and removing after . point
#removing ages over 120 and under 2

candy_2016_clean_age <- candy_2016_clean %>%
  mutate(age = str_remove(age, pattern = "[\\.][0-9]|[>]")) %>%
  mutate(age = as.integer(age)) %>% 
  filter(!age > 120) %>%
  filter(!age < 2)


#clean gender
candy_2016_clean_gender <-  candy_2016_clean_age %>%
  mutate(gender = str_replace(gender, 
                                   pattern = "I'd rather not say", "Other"))


#clean country -----------

#making all uppercase and remvoving !
candy_2016_clean_country <- candy_2016_clean_gender %>%
  mutate(country = str_to_upper(country)) %>%
  mutate(country = str_remove_all(country,
                                  pattern = "!"))


#grouping counrtys
uk_pattern <- "UK|ENGLAND|UNITED K+|U.K|SCOTLAND|IRELAND"
usa_ <-"USA|UNITED S+|U\\.*S|MURICA|USSA|AMERICA|THE YOO ESS OF AAAYYYYYY|MERICA|NORTH CAROLINA|'MERICA|PITTSBURGH|NEW YORK|NEW JERSEY|MURRIKA|ALASKA"
canada_pattern <- "CANADA"
eu_pattern <-"FRANCE|BELGIUM|CROATIA|PORTUGAL|ESPAÑA|HUNGARY|AUSTRIA|GERMANY|NETHERLANDS|FINLAND|EUROPE|GREECE|ICELAND|DENMARK|THE NETHERLANDS|SPAIN|SWEDEN"
asia_pattern <-"JAPAN|KOREA|SOUTH KOREA|PHILIPPINES|CHINA|KENYA|UAE|INDONESIA|SINGAPORE|TAIWAN|HONG KONG                                                           "



candy_2016_clean_country <- candy_2016_clean_country %>%
  mutate(
    country = case_when(
      str_detect(country, pattern = usa_) ~ "USA",
      str_detect(country, pattern = uk_pattern) ~ "UK",
      str_detect(country, pattern = eu_pattern) ~ "EU",
      str_detect(country, pattern = asia_pattern) ~ "ASIA",
      str_detect(country, pattern = canada_pattern) ~ "CANADA",
      .default = "Other"
      
    )
  )

#changing time stamp to just year column 

candy_2016_year <- candy_2016_clean_country %>% 
  mutate(timestamp = str_sub(timestamp, start = 1, end = 4)) %>% 
  rename(year = timestamp)


#cleaning 2017 -------------------------------------------------------------

#cleaning columns
#rid of end chuck
candy_2017 <- boing_candy_2017 %>% 
  select(1:109)

candy_2017_clean <- candy_2017 %>%
  select(-6, -12, -15, -19, -21, -22, -26, -27, -31,
         -32, -34, - 38, -43, -49, -69, -70, -81, -86, -92, -94, -104,
         -105,-106, -107, -108)

#removing Q

candy_2017_clean <- candy_2017_clean %>%
  rename_with(.cols = everything(), .fn = ~ str_remove((.x), pattern = "q[0-9]_"))
#making standernames

candy_2017_clean <- candy_2017_clean %>%
  rename(going_trick_or_treating = going_out)

candy_2017_clean <- candy_2017_clean %>%
  rename(
    penut_butter_kisses = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes,
    bonkers = bonkers_the_candy,
    box_of_raisins = boxo_raisins,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    now_and_laters = nown_laters,
    sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature,
    sweetums = sweetums_a_friend_to_diabetes,
    circus_peanut = those_odd_marshmallow_circus_peanut_things,
    tolberone = tolberone_something_or_other
  )

#making age an int
#making in to int and removing after . point
#removing ages over 120 and under 2

candy_2017_clean_age <- candy_2017_clean %>%
  mutate(age = str_remove(age, pattern = "[\\.][0-9]|[>]")) %>%
  mutate(age = as.integer(age)) %>% 
  filter(!age > 120) %>%
  filter(!age < 2)

#clean gender
candy_2017_clean_gender <-  candy_2017_clean_age %>%
  mutate(gender = str_replace(gender, pattern = "I'd rather not say", "Other"))

#clean country 

candy_2017_country <- candy_2017_clean_gender %>%
  mutate(country = str_to_upper(country)) %>% 
  mutate(country = str_remove_all(country,
                                  pattern = "!"))


candy_2017_country <- candy_2017_country %>%
  mutate(
    country = case_when(
      str_detect(country, pattern = usa_) ~ "USA",
      str_detect(country, pattern = uk_pattern) ~ "UK",
      str_detect(country, pattern = eu_pattern) ~ "EU",
      str_detect(country, pattern = asia_pattern) ~ "ASIA",
      str_detect(country, pattern = canada_pattern) ~ "CANADA",
      .default = "Other"
      
    )
  )

#adding year column 

candy_2017_clean_id <- candy_2017_country %>% 
  mutate(internal_id = str_replace_all(internal_id, pattern = "[0-9]+", "2017")) %>% 
  rename(year = internal_id)

#joining data 

joining_2015_2016 <- full_join(candy_2015_clean_year, candy_2016_year)
joining_all_years <- full_join(joining_2015_2016, candy_2017_clean_id)


organise_joinin_all_data <- joining_all_years %>% 
  relocate(country, .before = butterfinger) %>% 
  relocate(gender, .before = butterfinger)


#making data longer 

id_and_data <- organise_joinin_all_data %>% 
  mutate(id_number = 1:n()) %>% 
  relocate(id_number, .before = year)
  

fully_clean_candy_data <- id_and_data  %>% 
  pivot_longer(cols = -id_number & -year & -age & -going_trick_or_treating & -country & -gender, 
               names_to = "candy",
               values_to = "rating")



write_csv(fully_clean_candy_data , "clean_data/candy_all_data.csv")
