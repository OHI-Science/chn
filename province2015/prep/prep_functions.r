add_rgn_id = function(d, fld_name = 'rgn_ID') {

  library(readr)

  # join data to lookup table
  d2 = d %>%
    rename_(.dots = setNames(fld_name, "rgn_code")) %>%
    left_join(
    read_csv('~/github/chn/province2015/prep/lookup_rgn_code.csv'),
      by = c('rgn_code')) %>%
    select(-rgn_code); head(d2); summary(d2)

  dn = bind_cols(d2 %>% select(rgn_id),
                 d2 %>% select(-rgn_id))

  # return data.frame
  return(as.data.frame(dn))
}


fill_nas = function() {
 Julie to do
}
