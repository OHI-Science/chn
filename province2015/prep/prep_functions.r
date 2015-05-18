add_rgn_id = function(d, fld_name = 'rgn_ID') {

  library(readr)
  lk_tbl = read_csv('~/github/chn/province2015/prep/lookup_rgn_code.csv')

  # join data to lookup table
  d2 = d %>%
    rename_(.dots = setNames(fld_name, "rgn_code")) %>%
#     group_by
  full_join(
    lk_tbl, by = c('rgn_code')) %>%
    select(-rgn_code); head(d2); summary(d2)

  dn = bind_cols(d2 %>% select(rgn_id),
                 d2 %>% select(-rgn_id))

  ## fill missing regions as NAs --Julie come back here
  # if there are missing rgn_ids, fill with NAs
  if (max(unique(dn$rgn_id)) != dim(lk_tbl)[1]){ # this won't work, will need a group_by here, or above

    if (!'year' %in% names(dn)) {
      dl = dn %>%
        group_by(rgn_id) %>%
        mutate(n = count())
    } else {
      dl = dn %>%
        group_by(rgn_id, year) %>%
        mutate(n = count())
    }
  }
  # return data.frame
  return(as.data.frame(dn))
}
