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
## to make a universal pathway, should we just upload raw files into prep
## folder, and prep, and then save the cleaned data to layers folder? right now
## we have the same cleaned files in both prep and layers
## then: dir_raw = '~/github/chn/province2015/prep/subfolder' # ForOmar: dir_raw = '~/github/Google_Drive/OHI_China_2015/model_data'

chn_file_list = list.files(dir_raw)
## seems like we never used this line of code

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

for (f_orig in np_file_list) {  # f_orig =  'np_risk_chn2015_HHM.csv.xlsx'

  dir_f = file.path(dir_chn_prep, '3_NP')

  d = read_excel(file.path(dir_raw, f_orig)); head(d); summary(d)
  f_new = file_path_sans_ext(file_path_sans_ext(f_orig))
  f_new = str_replace(f_new, ' ', '')

  # typo correction for column header (needs an if statement)
  if ('tones' %in% names(d)) {
    d = d %>%
      rename(tonnes = tones); head(d)
  }

  # typo correction for product names (str_replace doesn't need an if statement)
  d = d %>%
    mutate(product = str_replace_all(product, 'Sea medicianProduct', 'sea_medicine'),
           product = str_replace_all(product, 'Seasalt',             'sea_salt'),
           product = str_replace_all(product, 'seasalt',             'sea_salt'),
           product = str_replace_all(product, 'ChemProduct',         'sea_chemicals'))


  # add rgn_id from prep_functions.r
  dn = add_rgn_id(d, fld_name = 'rgn_id'); head(dn)

  # save as csv
  write_csv(dn, file.path(dir_f, paste0(f_new, '.csv'))) # save a copy in prep
  write_csv(dn, file.path(dir_layers, paste0(f_new, '.csv'))) # save in layers folder

}

# CP data ====

cp_file_list=c("cp_condition_chn2015_zb.csv",
               "cp_extent_chn2015_zb.csv")

for (f_orig in cp_file_list) {
  dir_f = file.path(dir_chn_prep, "5_CP")
  d = read.csv(file.path(dir_raw, f_orig)); head(d); summary(d)
  dn = add_rgn_id(d, fld_name = 'rgn_ID')

  if ('habit' %in% names(dn)) {
    dn = rename(dn, habitat = habit); head(dn)
  }

  write_csv(dn, file.path(dir_f, f_orig))
  write_csv(dn, file.path(dir_layers, f_orig))
}

# FP data ====
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

## MAR ====
mar_file_list = c("6A_mar_ac.xlsx",
                  "6A_mar_smk.xlsx",
                  "6A_mar_yc.xlsx",
                  "6A_mar_yk.xlsx")

for (f_orig in mar_file_list) {
  dir_f = file.path(dir_chn_prep, "1.2_MAR")
  d = read_excel(file.path(dir_raw, f_orig)); head(d); summary(d)
  dn = add_rgn_id(d, fld_name = "province_id") ## worked, but showed an error message:
  ## Error in if (max(unique(dn$rgn_id)) != dim(lk_tbl)[1]) { :
  ## missing value where TRUE/FALSE needed

  f_new = file_path_sans_ext(f_orig) %>%
    str_replace("6A_", "")

  write_csv(dn, file.path(dir_f, paste0(f_new, "_chn2015_LZH.csv")))
  write_csv(dn, file.path(dir_layers, paste0(f_new, "_chn2015_LZH.csv")))

}

#AO ====
ao_file_list = c("6B_ao_pp.xlsx",
                 "6B_ao_oao.xlsx",
                 "6B_ao_du.xlsx")

for (f_orig in ao_file_list) {
  dir_f = file.path(dir_chn_prep, "2_AO")
  d = read_excel(file.path(dir_raw, f_orig)); head(d); summary(d)

  dn = add_rgn_id(d, fld_name = "province_id")

  f_new = file_path_sans_ext(f_orig) %>%
    str_replace("6B_", "")

  write_csv(dn, file.path(dir_f, paste0(f_new, "_chn2015_LZH.csv")))
  write_csv(dn, file.path(dir_layers, paste0(f_new, "_chn2015_LZH.csv")))
}

# TR ====
tr_file_list = c("6G_tr_marinearea_chn2015_YWW.csv",
                 "6G_tr_tourist_chn2015_YWW.csv")

for (f_orig in tr_file_list) {
  dir_f = file.path(dir_chn_prep, "7_TR")
  d = read.csv(file.path(dir_raw, f_orig)); head(d); summary(d)

  f_new = str_replace(f_orig, "6G_", "")

  write_csv(d, file.path(dir_f, f_new))
  write_csv(d, file.path(dir_layers, f_new))
}

# ICO ====
ico_file_list = c("6H_ico_species_chn2015_YWW.csv")

for (f_orig in ico_file_list) {
  dir_f = file.path(dir_chn_prep, "9.1_ICO")
  d = read.csv(file.path(dir_raw, f_orig)); head(d); summary(d)

  if ("region_id" %in% names(d)) {
  d = rename(d, rgn_id = region_id); head(d)
  }

  f_new = str_replace(f_orig, "6H_", "")

  write_csv(d, file.path(dir_f, f_new))
  write_csv(d, file.path(dir_layers, f_new))

}

# LSP ====
lsp_file_list = c("6H_lsp_cmpa_chn2015_YWW.csv",
                  "6H_lsp_marinearea_chn2015_YWW.csv")

for (f_orig in lsp_file_list) {
  dir_f = file.path(dir_chn_prep, "9.2_LSP")
  d = read.csv(file.path(dir_raw, f_orig)); head(d); summary(d)

  f_new = str_replace(f_orig, "6H_", "")

  write_csv(d, file.path(dir_f, f_new))
  write_csv(d, file.path(dir_layers, f_new))

}
# LIV_ECO ====
# LIV
liv_file_list = c("le_livjob_chn2015_zb.csv",
                 "le_livwage_chn2015_zb.csv")

for (f_orig in liv_file_list) {
  dir_f = file.path(dir_chn_prep, "6.1_LIV")
  d = read.csv(file.path(dir_raw, f_orig)); head(d); summary(d)

  dn = add_rgn_id(d, fld_name = "province")

  write_csv(dn, file.path(dir_f, f_orig))
  write_csv(dn, file.path(dir_layers, f_orig))
}

# ECO
eco_file_list = c("le_eco_chn2015_zb.csv")

for (f_orig in eco_file_list) {
  dir_f = file.path(dir_chn_prep, "6.2_ECO")
  d = read.csv(file.path(dir_raw, f_orig)); head(d); summary(d)

  dn = add_rgn_id(d, fld_name = "province")

  write_csv(dn, file.path(dir_f, f_orig))
  write_csv(dn, file.path(dir_layers, f_orig))
}
