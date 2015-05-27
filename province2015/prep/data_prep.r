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
dir_layers = '~/github/chn/province2015/layers'

##changed raw file path on Ning's computer:
dir_raw = '~/Google Drive/OHI China 2015/model_data/'

chn_file_list = list.files(dir_raw)

# CS data ----

# Julie finish
# cs_file_list = str_match(chn_file_list, 'cs_')

cs_file_list = c('cs_contribtion_chn2015_HHM.csv.xlsx',
                 'cs_condition_chn2015_HHM.xlsx',
                 'cs_extent_chn2015_HHM.xlsx')


  # add rgn_id from prep_functions.r - add_rgn_id()
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

for (f_orig in np_file_list) {  # f_orig =  'np _weight_chn2015_HHM.csv.xlsx'

  dir_f = file.path(dir_chn_prep, '3_NP')

  d = read_excel(file.path(dir_raw, f_orig)); head(d); summary(d)
  f_new = file_path_sans_ext(file_path_sans_ext(f_orig))
  f_new = str_replace(f_new, ' ', '')

  # typo correction for column header (needs an if statement)
  if ('tones' %in% names(d)) {
    d = d %>%
      rename(tonnes = tones); head(d)
  }

  # typo correction for product names (str_replace doesn't need and if statement)
  d = d %>%
    mutate(product = str_replace_all(product, 'Sea medicianProduct', 'sea_medicine'),
           product = str_replace_all(product, 'Seasalt',             'seasalt'),
           product = str_replace_all(product, 'ChemProduct',         'sea_chemicals'))


  # add rgn_id from prep_functions.r
  dn = add_rgn_id(d, fld_name = 'rgn_id'); head(dn); summary(dn)

  # np_weight can't have NAs. This is not the most elegant fix to this problem.
  if (f_orig == 'np _weight_chn2015_HHM.csv.xlsx') {
    dn = dn %>%
      filter(rgn_id != 6) %>%
      bind_rows(data.frame(
        rgn_id = 6,
        product = c('sea_medicine', 'seasalt', 'sea_chemicals'),
        weight = c(0.2, 0.4, 0.4))) %>%
      arrange(product, rgn_id)
  }

  # save as csv
  write_csv(dn, file.path(dir_f, paste0(f_new, '.csv'))) # save a copy in prep
  write_csv(dn, file.path(dir_layers, paste0(f_new, '.csv'))) # save in layers folder

}

# CP data

cp_file_list=c("cp_condition_chn2015_zb.csv",
               "cp_extent_chn2015_zb.csv")

for (f_orig in cp_file_list) {
  dir_f = file.path(dir_chn_prep, "5_CP")
  d = read.csv(file.path(dir_raw, f_orig)); head(d); summary(d)
  dn = add_rgn_id(d, fld_name = 'rgn_ID')

  write_csv(dn, file.path(dir_f, f_orig))
  write_csv(dn, file.path(dir_layers, f_orig))
}

# FP data
##FIS data
fis_file_list = c("6A_fis_ft.xlsx",
                 "6A_fis_mmsy.xlsx",
                 "6A_fis_tc.xlsx",
                 "6A_fis_ut.xlsx",
                 "6A_fp_w.xlsx")

for (f_orig in fis_file_list) {
  dir_f = file.path(dir_chn_prep, "1.1_FIS")
  d = read_excel(file.path(dir_raw, f_orig)); head(d); summary(d)
  dn = add_rgn_id(d, fld_name = "province_id")

  f_new = file_path_sans_ext(f_orig)
  f_new = str_replace(f_new, "6A_", "")

  write_csv(dn, file.path(dir_f, paste0(f_new, "_chn2015_LZH.csv")))
  write_csv(dn, file.path(dir_layers, paste0(f_new, "_chn2015_LZH.csv")))
}

## MAR
mar_file_list = c("6A_mar_ac.xlsx",
                  "6A_mar_smk.xlsx",
                  "6A_mar_yc.xlsx",
                  "6A_mar_yk.xlsx")

for (f_orig in mar_file_list) {
  dir_f = file.path(dir_chn_prep, "1.2_MAR")
  d = read_excel(file.path(dir_raw, f_orig)); head(d); summary(d)
  dn = add_rgn_id(d, fld_name = "province_id") ## need to revisit.
  ## Error in if (max(unique(dn$rgn_id)) != dim(lk_tbl)[1]) { :
  ## missing value where TRUE/FALSE needed
  ### but i counted, all the provinces are there for each data set...
}
