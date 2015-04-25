# data_explore.r to explore CS data

## setup ----

library(dplyr)
library(readr)
library(tools) # to remove file extention

dir_scenario = '~/github/chn/province2015'
setwd(file.path(dir_scenario, '/prep/4_CS'))

# read in extent data
f_in = 'cs_extent_chn2015_HHM.csv'
ex = read_csv(f_in); head(extent); summary(extent)

## Explore how variable is habitat area (= extent) by year ----
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

## Next steps ---
# 1. Ask China team how to interpret their habitat data through time
# 2. Meanwhile, create cs_extent_chn2015_HHM_maxyr.csv that is only the max year per rgn_id-habitat combo
# 3. Save in layers folder, register in layers.csv, use to create goal model in functions.r

extent_save = extent %>%
  group_by(rgn_id, habitat) %>%
  filter(year == max(year)) %>%
  select(-yr_count)

f_out = paste0(file_path_sans_ext(f_in), '_maxyr.csv')
write_csv(extent_save, file.path(dir_scenario, 'layers', f_out))

