# data_prep.r

#设置路径 (dir: directory)
dir_chn_prep = '~/github/chn/province2015/prep'
setwd(dir_chn_prep)

# setup ---- 叫出需要的工具包
library(readxl)  # install.packages('readxl')
library(readr)   # install.packages('readr')
library(stringr) # install.packages('stringr')
library(tools)
library(dplyr)  # install.packages('dplyr')
source(file.path(dir_chn_prep,'prep_functions.r'))

# dir_layers: 设置 layers 文件夹
dir_layers = '~/github/chn/province2015/layers'

# dir_raw: 最原始文件存放在这里
# paths: '~/' means '/Users/julialowndes/' for Julie
dir_raw = '~/github/chn/province2015/pre-proc'

# dir_raw = c('mendes'='~/Google Drive/OHI China 2015/model_data/',
#             'ningningj'='~/Google Drive/OHI China 2015/model_data/',
#             'julialowndes'='~/Google Drive/1 OHI+ Countries:Regions:Territories/China/OHI China 2015/model_data/',
#             'jstewart'    ='~/Google Drive/1 OHI+ Countries:Regions:Territories/China/OHI China 2015/model_data/')[Sys.info()["user"]]

chn_file_list = list.files(dir_raw)

# CS data ----

cs_file_list = c('cs_contribtion_chn2015_HHM.csv.xlsx',
                 'cs_condition_chn2015_HHM.xlsx',
                 'cs_extent_chn2015_HHM.xlsx')

# f: file, orig: original， d: data
for (f_orig in cs_file_list) { #f_orig = 'cs_contribtion_chn2015_HHM.csv.xlsx'

  dir_f = file.path(dir_chn_prep, '4_CS')

  d = read_excel(file.path(dir_raw, '4_CS', f_orig)); head(d); summary(d) #读取数据
  f_new = file_path_sans_ext(file_path_sans_ext(f_orig)) #去除原始文件扩展名

  if ('habit' %in% names(d)) {
    d = d %>%
      rename(habitat = habit); head(d)
  }

  # add rgn_id from prep_functions.r - add_rgn_id() #省份名字变为数字
  dn = add_rgn_id(d, fld_name = 'rgn_ID') %>%
    filter(!is.na(rgn_id)); head(dn)

  # work with file name
  f_new = file_path_sans_ext(file_path_sans_ext(f_orig))
  f_new = str_replace(f_new, ' ', '')

  # save as csv
  write_csv(dn, file.path(dir_f, paste0(f_new, '.csv')))
  write_csv(dn, file.path(dir_layers, paste0(f_new, '.csv')))

  # typo correction
  if (f_new == 'cs_contribtion_chn2015_HHM') {
    file.rename(file.path(dir_f, paste0(f_new, '.csv')),
                file.path(dir_f, 'cs_contribution_chn2015_HHM.csv'))

    file.rename(file.path(dir_layers, paste0(f_new, '.csv')),
                file.path(dir_layers, 'cs_contribution_chn2015_HHM.csv'))
  }
}

# NP data ----

# Julie finish
# np_file_list = str_match(chn_file_list, 'cs_')

np_file_list = c('np_weight_chn2015_HHM.csv',
                 'np_harvest_chn2015_HHM.csv',
                 'np_exposure_chn2015_HHM.csv',
                 'np_risk_chn2015_HHM.csv')

for (f_orig in np_file_list) {  #f_orig = 'np_harvest_chn2015_HHM.csv'

  dir_f = file.path(dir_chn_prep, '3_NP')

  d = read.csv(file.path(dir_raw, '3_NP', f_orig)); head(d); summary(d)

  # typo correction for product names (str_replace doesn't need an if statement)
  dn = d %>%
    mutate(product = str_replace_all(product, 'Sea medicianProduct', 'sea_medicine'),
           product = str_replace_all(product, 'Seasalt',             'seasalt'),
           product = str_replace_all(product, 'seasalt',             'seasalt'),
           product = str_replace_all(product, 'ChemProduct',         'sea_chemicals'))


  # add rgn_id from prep_functions.r
  if(f_orig == 'np_harvest_chn2015_HHM.csv'){
    dn = add_rgn_id(dn, fld_name = 'rgn_id')

    # correct typos in heading
    dn = dn %>%
      rename(tonnes = tones); head(dn)
  }

#   # np_weight can't have NAs. This is not the most elegant fix to this problem.
#   if (f_orig == 'np _weight_chn2015_HHM.csv.xlsx') {
#     dn = dn %>%
#       filter(rgn_id != 6) %>%
#       bind_rows(data.frame(
#         rgn_id = 6,
#         product = c('sea_medicine', 'seasalt', 'sea_chemicals'),
#         weight = c(0.2, 0.4, 0.4))) %>%
#       arrange(product, rgn_id)
#   }

  write_csv(dn, file.path(dir_f, f_orig)) # save a copy in prep
  write_csv(dn, file.path(dir_layers,f_orig)) # save in layers folder

}


# CP data ----

cp_file_list=c("cp_condition_chn2015_zb.csv",
               "cp_extent_chn2015_zb.csv")

for (f_orig in cp_file_list) {
  dir_f = file.path(dir_chn_prep, "5_CP")
  d = read.csv(file.path(dir_raw, "5_CP", f_orig)); head(d); summary(d)
  dn = add_rgn_id(d, fld_name = 'rgn_ID')

  if ('habit' %in% names(dn)) {
    dn = rename(dn, habitat = habit); head(dn)
  }

  write_csv(dn, file.path(dir_f, f_orig))
  write_csv(dn, file.path(dir_layers, f_orig))
}


# FP data ----


##FIS data
fis_file_list = c("6A_fis_ft.xlsx",
                 "6A_fis_mmsy.xlsx",
                 "6A_fis_tc.xlsx",
                 "6A_fis_ct.xlsx")

for (f_orig in fis_file_list) {
  dir_f = file.path(dir_chn_prep, "1.1_FIS")
  d = read_excel(file.path(dir_raw, "1.1_FIS", f_orig)); head(d); summary(d)
  dn = add_rgn_id(d, fld_name = "province_id")

  f_new = file_path_sans_ext(f_orig)
  f_new = str_replace(f_new, "6A_", "")

  write_csv(dn, file.path(dir_f, paste0(f_new, "_chn2015_LZH.csv")))
  write_csv(dn, file.path(dir_layers, paste0(f_new, "_chn2015_LZH.csv")))
}

## MAR ====
mar_file_list = c("6A_mar_ac.xlsx",
                  "6A_mar_smk.xlsx",
                  "6A_mar_yk.xlsx")

for (f_orig in mar_file_list) { # f_orig = "6A_mar_yk.xlsx"
  dir_f = file.path(dir_chn_prep, "1.2_MAR")
  d = read_excel(file.path(dir_raw, "1.2_MAR", f_orig)); head(d); summary(d)
  dn = add_rgn_id(d, fld_name = "province_id")

  f_new = file_path_sans_ext(f_orig) %>%
    str_replace("6A_", "")

  write_csv(dn, file.path(dir_f, paste0(f_new, "_chn2015_LZH.csv")))
  write_csv(dn, file.path(dir_layers, paste0(f_new, "_chn2015_LZH.csv")))

}


#AO ----

ao_file_list = c("6B_ao_gc.xlsx",
                 "6B_ao_ic.xlsx",
                 "6B_ao_afc.xlsx",
                 "6B_ao_apc.xlsx")

for (f_orig in ao_file_list) { # f_orig = ao_file_list[5]
  dir_f = file.path(dir_chn_prep, "2_AO")

  # ao_afc (number of fishermen) last col was set as "date". Need to change to "numeric" for calculation:
  d = if (f_orig == "6B_ao_afc.xlsx") {
    read_excel(file.path(dir_raw, "2_AO", f_orig), col_types = c('text', 'numeric', 'numeric'))
  } else {read_excel(file.path(dir_raw, "2_AO", f_orig))} ; head(d) ; summary(d)

  # change the ao_aec and ao_aer (gas price) column name to rmb_l; R can't deal with /
  names(d)[names(d) == "rmb/l"] = 'rmb_l'

  # change ao_apc and ao_apr (port) column name to count
  if ("NO." %in% names(d)) {
    d = rename(d, count = NO.)
  }

  dn = add_rgn_id(d, fld_name = "province_id")

  f_new = file_path_sans_ext(f_orig) %>%
    str_replace("6B_", "")

  write_csv(dn, file.path(dir_f, paste0(f_new, "_chn2015_LZH.csv")))
  write_csv(dn, file.path(dir_layers, paste0(f_new, "_chn2015_LZH.csv")))
}

# TR ----

tr_file_list = c("6G_tr_tourist_chn2015_YWW.csv",
                 "6G_tr_s_chn2015_YWW.csv",
                 "6G_tr_coastalline_chn2015_YWW.csv",
                 # "6G_tr_coastalwaterarea_chn2015_YWW.csv",
                 # "6G_tr_marinearea_chn2015_YWW.csv"
                 )

for (f_orig in tr_file_list) {
  dir_f = file.path(dir_chn_prep, "7_TR")
  d = read.csv(file.path(dir_raw, "7_TR", f_orig)); head(d); summary(d)
  d = filter(d,!is.na(rgn_id)); head(d);
  f_new = str_replace(f_orig, "6G_", "")

  write_csv(d, file.path(dir_f, f_new))
  write_csv(d, file.path(dir_layers, f_new))
}


# ICO ----

ico_file_list = c("6H_ico_species_chn2015_YWW.csv")

for (f_orig in ico_file_list) {
  dir_f = file.path(dir_chn_prep, "9.1_ICO")
  d = read.csv(file.path(dir_raw, "9.1_ICO", f_orig)); head(d); summary(d)

  if ("region_id" %in% names(d)) {
  d = rename(d, rgn_id = region_id); head(d)
  }

  #to remove NA columns: count of NA in a column does not equal to the number of rows.
  #there may be a better way to do this
  d = d[, colSums(is.na(d)) !=nrow(d)]

  f_new = str_replace(f_orig, "6H_", "")

  write_csv(d, file.path(dir_f, f_new))
  write_csv(d, file.path(dir_layers, f_new))

}

# LSP ----

lsp_file_list = c("6H_lsp_cmpa_chn2015_YWW.csv",
                  "6H_lsp_marinearea_chn2015_YWW.csv")

for (f_orig in lsp_file_list) {
  dir_f = file.path(dir_chn_prep, "9.2_LSP")
  d = read.csv(file.path(dir_raw, "9.2_LSP", f_orig)); head(d); summary(d)
  d = filter(d, !is.na(rgn_id))

  f_new = str_replace(f_orig, "6H_", "")

  write_csv(d, file.path(dir_f, f_new))
  write_csv(d, file.path(dir_layers, f_new))

}

# LIV ----

liv_file_list = c( # all data layers updated on 9/15/2015 on new data from CHN
                 "le_livjobindustry_chn2015_zb.csv",
                 "le_livjobprovince_chn2015_zb.csv",
                 "le_livwagetown_chn2015_zb.csv",
                 "le_livwagevillage_chn2015_zb.csv")

for (f_orig in liv_file_list){ #f_orig = "le_livjobindustry_chn2015_zb.csv"
  dir_f = file.path(dir_chn_prep, "6.1_LIV")
  d = read.csv(file.path(dir_raw, "6.1_LIV", f_orig)); head(d); summary(d)

  d = d[, colSums(is.na(d)) !=nrow(d)] #remove NA columns
  d = d[!is.na(d[1,]),] #remove NA rows


  if(f_orig == "le_livjobindustry_chn2015_zb.csv") {
    dn = d %>%
    mutate(category = str_replace_all(category, "beach placer industry", 'mining'),
           category = str_replace_all(category, "coastal tourism", 'tourism'),
           category = str_replace_all(category, "marein engineering architecture", 'egineering_arch'),
           category = str_replace_all(category, "marine biomedicine", 'biomedicine'),
           category = str_replace_all(category, "marine chemical industry", 'chemical'),
           category = str_replace_all(category, "marine communication and trasportation industry", 'comm_transport'),
           category = str_replace_all(category, "maren electric power and seawater utilization industry", 'electric'),
           category = str_replace_all(category, "marine fishery and the related industries", 'fishing'),
           category = str_replace_all(category, "marine shipbuilting industry", 'ship_building'),
           category = str_replace_all(category, "offshore oil and natural gas industry", 'oil_gas'),
           category = str_replace_all(category, "sea salt industry", 'seasalt'))
    }

  if('province' %in% names(d)){
  dn = add_rgn_id(d, fld_name = "province")
}
  write_csv(dn, file.path(dir_f, f_orig))
  write_csv(dn, file.path(dir_layers, f_orig))
}

# ECO ----
eco_file_list = c("LE_eco_chn2015_zb.csv") #updated on 9/9/2015 on new data from CHN

for (f_orig in eco_file_list) {
  dir_f = file.path(dir_chn_prep, "6.2_ECO")
  d = read.csv(file.path(dir_raw, "6.2_ECO", f_orig)); head(d); summary(d)

  dn = add_rgn_id(d, fld_name = "province") %>%
  mutate(value = str_replace(value, ',',''))

  write_csv(dn, file.path(dir_f, f_orig))
  write_csv(dn, file.path(dir_layers, f_orig))
}


# SPP ----

# gl_spp_trend = read.csv('/Volumes/data_edit/git-annex/globalprep/SpeciesDiversity/v2015/intermediate/spp_all_cleaned.csv'); head(gl_spp_trend)
# write.csv(gl_spp_trend, '10.1_SPP/gl_spp_all.csv', row.names=F)
gl_spp_trend = read.csv(file.path(dir_chn_prep, '10.1_SPP/gl_spp_all.csv')); head(gl_spp_trend)

china_spp = read.csv(file.path(dir_raw, '10.1_SPP', 'spp_species_chn2015_LM.csv')); head(china_spp)

d = china_spp %>%
  rename(province_id = rgn_id,
         sciname     = species_latin) %>%
  select(-species_common) %>%
  add_rgn_id(fld_name = "province_id")

dir_f = file.path(dir_chn_prep, "10.1_SPP")
write_csv(d, file.path(dir_f, 'spp_species_chn2015_LM.csv'))
write_csv(d, file.path(dir_layers, 'spp_species_chn2015_LM.csv'))

d2 = d %>%
  filter(IUCN_class != '') %>%
  left_join(gl_spp_trend %>%
              select(sciname,
                     popn_category,
                     popn_trend,
                     trend_score), by='sciname') %>%
  filter(!is.na(trend_score)) %>%
  select(rgn_id, sciname, #risk.wt = value, IUCN_class, popn_trend,
         trend_score)

dir_f = file.path(dir_chn_prep, "10.1_SPP")
write_csv(d2, file.path(dir_f, 'spp_iucn_trends_chn2015.csv'))
write_csv(d2, file.path(dir_layers, 'spp_iucn_trends_chn2015.csv'))

# only 11 of these in 6 provinces have a trend score.

#   rgn_id                    sciname trend_score
# 1       1 Balaenoptera acutorostrata           0
# 2       1       Haliaeetus pelagicus        -0.5
# 3       1         Egretta eulophotes        -0.5
# 4       1            Larus saundersi        -0.5
# 5       1             Platalea minor           0
# 6       3             Larus relictus        -0.5
# 7       4         Egretta eulophotes        -0.5
# 8       5         Egretta eulophotes        -0.5
# 9       8          Pelecanus crispus        -0.5
# 10     11         Montipora stellata        -0.5
# 11     11            Holothuria atra           0
# 12      2                         NA          NA
# 13      6                         NA          NA
# 14      7                         NA          NA
# 15      9                         NA          NA
# 16     10                         NA          NA


# CW ----
## in original data set, all pollutants are in one file. I have already separated each pollutant into
## an individual data set and saved as a .csv

cw_file_list = c('cw_phosphate_chn2015_LM.csv',
                 'cw_nitrogen_chn2015_LM.csv',
                 'cw_cod_chn2015_LM.csv',
                 'cw_oil_chn2015_LM.csv')

for (f_orig in cw_file_list) {
dir_f = file.path(dir_chn_prep, "8_CW")
d = read.csv(file.path(dir_raw, "8_CW", f_orig)); head(d); summary(d)

dn = add_rgn_id(d, fld_name = 'rgn_id')

write_csv(dn, file.path(dir_f, f_orig))
write_csv(dn, file.path(dir_layers, f_orig)) }


# PRESSURE & RESILIENCE ----
# data layers used in config.R ＃在 config.R 中用到的文件
# first load layers from calculate_scores.R ＃先做完 calculate_scores.R 中 layers = Layers('layers.csv', 'layers') 这一步

#＃ cs_habitat_extent = Habitat extent * rank, per Carbon Storage habitats
#＃                   = extent * contribution

extent = layers$data[['cs_extent']] %>%
  select(rgn_id, habitat, extent = hectare)

contribution = layers$data[['cs_contribution']] %>%
  select(rgn_id, habitat, contribution = value)

result = full_join(extent, contribution, by = c('rgn_id', 'habitat')) %>%
  mutate(hectare = extent*contribution) %>%
  select(rgn_id, habitat, hectare)

write_csv(result, file.path(dir_layers, 'cs_habitat_extent_chn2015.csv'))

#＃ cp_habitat_extent_rank = Habitat extent * rank, per Coastal Protection habitats
#＃                        = extent * weight

habitat.wt = c('saltmarshes' = 3,
               'mangroves' = 4,
               'seagrasses' = 1,
               'coral reef' = 4)

m = layers$data[['cp_extent']] %>%
  group_by(rgn_id, habitat) %>%
  filter(year == max(year)) %>% #choose the most recent year's data
  select(-layer,
         -year,
         extent = hectare) %>%
  mutate(weight = habitat.wt[habitat],
         extent_rank = extent * weight) %>%
  select(rgn_id, habitat, extent_rank)

write_csv(m, file.path(dir_layers, 'cp_habitat_extent_rank_chn2015.csv'))

#＃ hab_presence: 1 for presence, 0 for absence

m = layers$data[['cp_extent']] %>%
  group_by(rgn_id, habitat) %>%
  filter(year == max(year)) %>% #choose the most recent year's data
  select(-layer,
         -year,
         extent = hectare) %>%
  mutate(boolean = 1) %>%
  select(rgn_id, habitat, boolean)

write_csv(m, file.path(dir_layers, 'hab_presence_chn2015.csv'))





