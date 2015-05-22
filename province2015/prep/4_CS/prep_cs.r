# prep_cs.r

# 1. explore CS data. Decide to save 2 files, one for status and one for trend
# 2. status: save CS extent data with max year only for status
# 3. trend: calculate trend using all CS extent years

## setup ----

library(dplyr)
library(readr)
library(tools) # to remove file extention

dir_scenario = '~/github/chn/province2015'
setwd(file.path(dir_scenario, '/prep/4_CS'))

# read in extent data
f_in = 'cs_extent_chn2015_HHM.csv'
ex = read_csv(f_in); head(ex); summary(ex)

## 1. Explore how variable is habitat area (= extent) by year ----
# i.e. what does the year column mean?

extent = ex %>%
  arrange(rgn_id, habitat, year) %>%
  group_by(rgn_id, habitat) %>%
  mutate(yr_count = n()) %>%
  ungroup()

extent
# rgn_id  year  habitat	hectare	yr_count
# 1	2007	saltmarshes	1188600.000	1
# 1	2008	seagrasses	100.000	2
# 1	2012	seagrasses	100.000	2          # <-- 2 years of data for seagrasses, extents the same
# 2	2007	saltmarshes	81551.000	1
# 3	2007	saltmarshes	76840.000	1
# 4	2007	saltmarshes	721275.000	1
# 4	2010	seagrasses	289.000	1
# 5	2007	saltmarshes	363979.000	1
# 6	2007	saltmarshes	98275.000	2
# 6	2008	saltmarshes	18314.800	2      # <-- 2 years of data for seagrasses, extents different
# ...
#

## 2. status: save CS extent data with max year only for status ----

extent_status = extent %>%
  group_by(rgn_id, habitat) %>%
  filter(year == max(year)) %>%
  select(-yr_count)

f_out = paste0(file_path_sans_ext(f_in), '_maxyr.csv')
write_csv(extent_status, file.path(dir_scenario, 'layers', f_out))
# now register this in layers.csv as cs_extent


## 3. trend: calculate trend using all CS extent years ----

extent_trend =
  bind_cols(
    extent %>%
      group_by(rgn_id, habitat) %>%
      filter(yr_count != 1) %>%
      summarize(hectare_min_yr = first(hectare)),
    extent %>%
      group_by(rgn_id, habitat) %>%
      filter(yr_count != 1) %>%
      summarize(hectare_max_yr = last(hectare)) %>%
      ungroup() %>%
      select(hectare_max_yr)) %>%
  mutate(trend.score = hectare_max_yr - hectare_min_yr) %>%
  select(rgn_id, habitat, trend.score) %>%
  bind_rows(
    extent %>%
      group_by(rgn_id, habitat) %>%
      filter(yr_count == 1) %>%
      mutate(trend.score = -0.1) %>%         # penalize for having only one year of data -- need to ask CHN about this
      select(rgn_id, habitat, trend.score)) %>%
  arrange(rgn_id, habitat); head(extent_trend)


f_out = paste0(tools::file_path_sans_ext(f_in), '_trend.csv')
write_csv(extent_trend, file.path(dir_scenario, 'layers', f_out))
# now register this in layers.csv as cs_extent_trend

