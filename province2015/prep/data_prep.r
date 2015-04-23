# data_prep.r

dir_chn_prep = '~/github/chn/province2015/prep'
setwd(dir_chn_prep)

# setup ----
library(readxl)  # install.packages('readxl')
library(readr)   # install.packages('readr')
library(stringr) # install.packages('stringr')
library(tools)
library(dplyr)  # install.packages('dplyr')
source(file.path(dir_chn_prep,'prep_functions.r'))

# paths: '~/' means '/Users/julialowndes/' for Julie
dir_raw = '~/Google Drive/1 OHI+ Countries:Regions:Territories/China/OHI China 2015/model_data/'

chn_file_list = list.files(dir_raw)

# CS data ----

# Julie finish
# cs_file_list = str_match(chn_file_list, 'cs_')

cs_file_list = c('cs_contribtion_chn2015_HHM.csv.xlsx',
                 'cs_condition_chn2015_HHM.xlsx',
                 'cs_extent_chn2015_HHM.xlsx')

for (f_orig in cs_file_list) {  # f_orig = 'cs_contribtion_chn2015_HHM.csv.xlsx'

  dir_f = file.path(dir_chn_prep, '4_CS')

  # read in data
  d = read_excel(file.path(dir_raw, f_orig)); head(d); summary(d)

  # typo correction
  if ('habit' %in% names(d)) {
    d = d %>%
      rename(habitat = habit); head(d)
  }

  # add rgn_id from prep_functions.r
  dn = add_rgn_id(d, fld_name = 'rgn_ID') %>%
    filter(!is.na(rgn_id)); head(dn)

  # work with file name
  f_new = file_path_sans_ext(file_path_sans_ext(f_orig))
  f_new = str_replace(f_new, ' ', '')

  # save as csv
  write_csv(dn, file.path(dir_f, paste0(f_new, '.csv')))

  # typo correction
  if (f_new == 'cs_contribtion_chn2015_HHM') {
    file.rename(file.path(dir_f, paste0(f_new, '.csv')),
                file.path(dir_f, 'cs_contribution_chn2015_HHM.csv'))
  }

}

# NP data ----

# Julie finish
# np_file_list = str_match(chn_file_list, 'cs_')

np_file_list = c('np _weight_chn2015_HHM.csv.xlsx',
                 'np_harvest_chn2015_HHM.csv.xlsx',
                 'np_exposure_chn2015_HHM.csv.xlsx',
                 'np_risk_chn2015_HHM.csv.xlsx')

for (f_orig in np_file_list) {  # f_orig = 'np_harvest_chn2015_HHM.csv.xlsx'

  dir_f = file.path(dir_chn_prep, '3_NP')

  d = read_excel(file.path(dir_raw, f_orig)); head(d); summary(d)
  f_new = file_path_sans_ext(file_path_sans_ext(f_orig))
  f_new = str_replace(f_new, ' ', '')

  # typo correction
  if ('tones' %in% names(d)) {
    d = d %>%
      rename(tonnes = tones); head(d)
  }

  # add rgn_id from prep_functions.r
  dn = add_rgn_id(d, fld_name = 'rgn_id'); head(dn)

  # save as csv
  write_csv(dn, file.path(dir_f, paste0(f_new, '.csv')))

}
