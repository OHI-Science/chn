FIS = function(layers, status_year){
  # layers used: fis_meancatch, fis_b_bmsy, fis_proparea_saup2rgn

  # catch data
  c = SelectLayersData(layers, layers='fis_meancatch', narrow=T) %>%
    select(
      fao_saup_id    = id_chr,
      taxon_name_key = category,
      year,
      catch          = val_num)

  # separate out the region ids:
  c$fao_id    <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[1]))
  c$saup_id   <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[2]))
  c$TaxonName <- sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[1])
  c$TaxonKey  <- as.numeric(sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[2]))
  c$catch     <- as.numeric(c$catch)
  c$year      <- as.numeric(as.character(c$year))
  #Create Identifier for linking assessed stocks with country-level catches
  c$stock_id <- paste(as.character(c$TaxonName),
                      as.character(c$fao_id), sep="_")

  # b_bmsy data
  b = SelectLayersData(layers, layer='fis_b_bmsy', narrow=T) %>%
    select(
      fao_id         = id_num,
      TaxonName      = category,
      year,
      bmsy           = val_num)
  # Identifier taxa/fao region:
  b$stock_id <- paste(b$TaxonName, b$fao_id, sep="_")
  b$bmsy     <- as.numeric(b$bmsy)
  b$fao_id   <- as.numeric(as.character(b$fao_id))
  b$year     <- as.numeric(as.character(b$year))


  # area data for saup to rgn conversion
  a = layers$data[['fis_proparea_saup2rgn']] %>%
    select(saup_id, rgn_id, prop_area)
  a$prop_area <- as.numeric(a$prop_area)
  a$saup_id   <- as.numeric(as.character(a$saup_id))
  a$rgn_id    <- as.numeric(as.character(a$rgn_id))

  # ------------------------------------------------------------------------
  # STEP 1. Merge the species status data with catch data
  #     AssessedCAtches: only taxa with catch status data
  # -----------------------------------------------------------------------
  AssessedCatches <- join(b, c,
                          by=c("stock_id", "year"), type="inner")

  # b,c by stock_id

  # include only taxa with species-level data
  AssessedCatches <- AssessedCatches[as.numeric(AssessedCatches$TaxonKey)>=600000, ]
  AssessedCatches$penalty <- 1

  # ------------------------------------------------------------------------
  # STEP 2. Estimate status data for catch taxa without species status
  #     UnAssessedCatches: taxa with catch status data
  # -----------------------------------------------------------------------
  UnAssessedCatches <- c[!(c$year %in% AssessedCatches$year &
                             c$stock_id %in% AssessedCatches$stock_id), ]

  # 2a.  Join UnAssessedCatches data to the b_bmsy summaries for each FAO/Year

  # Average status data for assessed stocks by FAO region for each year.
  # This is used as the starting estimate for unassesed stocks
  # Here, the Median b_bmsy was chosen for TaxonKey >= 600000
  # and Min b_bmsy for TaxonKey < 600000
  #  *************NOTE *****************************
  #  Using the minimum B/BMSY score as an starting point
  #  for the estimate of B/BMSY for unassessed taxa not
  #  identified to species level is very conservative.
  #  This is a parameter that can be changed.
  #  ***********************************************
  b_summary <- ddply(b, .(fao_id, year), summarize,
                     Medianb_bmsy=quantile(as.numeric(bmsy), probs=c(0.5)),
                     Minb_bmsy=min(as.numeric(bmsy)))

  UnAssessedCatches <- join(UnAssessedCatches, b_summary, by=c("fao_id", "year"),
                            type="left", match="all")

  #  UnAssessedCatches <- UnAssessedCatches[!(is.na(UnAssessedCatches$Medianb_bmsy)), ] #added 8/21/2014 due to changes in b/bmsy data created NAs here

  #   ## Troubleshooting:
  #   head(UnAssessedCatches[is.na(UnAssessedCatches$Medianb_bmsy), ])
  #   tmp <- UnAssessedCatches[is.na(UnAssessedCatches$Medianb_bmsy), ]
  #   unique(tmp$fao_id)
  #   saups <- unique(tmp$saup_id)
  #   unique(tmp$stock_id)
  #   a[a$saup_id %in% saups,]
  #
  #   ggplot(tmp, aes(x=year, y=catch, group=saup_id, color=saup_id)) +
  #     geom_point() +
  #     geom_line() +
  #     facet_wrap( ~ stock_id, ncol=9, scale="free")
  #
  # 2b.  Create a penalty variable based on taxa level:
  UnAssessedCatches$TaxonPenaltyCode <- substring(UnAssessedCatches$TaxonKey,1,1)

  # 2c. Create a penalty table for taxa not identified to species level
  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  penaltyTable <- data.frame(TaxonPenaltyCode=1:6,
                             penalty=c(0.01, 0.25, 0.5, 0.8, 0.9, 1))
  # 2d.Merge with data
  UnAssessedCatches <- join(UnAssessedCatches, penaltyTable, by="TaxonPenaltyCode")

  # ------------------------------------------------------------------------
  # STEP 3. Calculate score for all taxa based on status (b/bmsy) and taxa
  # -----------------------------------------------------------------------

  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05

  ## Function to calculate score for different scenarios:
  score <- function(data, variable){
    #data <- AssessedCatches
    #variable <- "bmsy"
    ifelse(data[ ,variable]*data[, "penalty"]<lowerBuffer,
           data[ ,variable]*data[, "penalty"],
           ifelse(data[ ,variable]*data[, "penalty"]>upperBuffer,
                  ifelse(1-alpha*(data[ ,variable]*data[, "penalty"]
                                  -upperBuffer)>beta,
                         1-alpha*(data[ ,variable]*data[, "penalty"]-upperBuffer),beta),
                  1))
  }

  AssessedCatches$score <- score(data=AssessedCatches, variable="bmsy")

  # Median is used to calculate score for species with Taxon 6 coding
  UnAssessedCatchesT6 <- subset(UnAssessedCatches, penalty==1)
  UnAssessedCatchesT6$score <- score(UnAssessedCatchesT6, "Medianb_bmsy")

  UnAssessedCatches <- subset(UnAssessedCatches, penalty!=1)
  UnAssessedCatches$score <- score(UnAssessedCatches, "Medianb_bmsy")

  AllScores <- rbind(AssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                     UnAssessedCatchesT6[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                     UnAssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")])

  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each saup_id region
  # -----------------------------------------------------------------------

  # 4a. To calculate the weight (i.e, the relative catch of each stock per saup_id),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region r, which is calculated as:

  smc <- ddply(.data = AllScores, .(year, saup_id), summarize,
               SumCatch = sum(catch))
  AllScores<-join(AllScores,smc,by=c("year","saup_id"))
  AllScores$wprop<-AllScores$catch/AllScores$SumCatch


  #  4b. The "score" and "weight" values per taxon per SAUP region are used to
  #    calculate a geometric weighted mean across taxa for each saup_id region
  geomMean <- ddply(.data = AllScores, .(saup_id, year), summarize, status_saup = prod(score^wprop))

  # ------------------------------------------------------------------------
  # STEP 5. Convert status from saup spatial scale to OHI spatial scale
  # -----------------------------------------------------------------------
  # In many cases the ohi reporting regions are comprised of multiple saup regions.
  # To correct for this, the proportion of each saup area of the total area of the
  # OHI region was calculated. This was used to calculate Status from the Status_saup.
  # This type of adjustment is omitted if the data were collected at the same spatial
  # scale as the collecting region.

  # Join region names/ids to Geom data
  geomMean <- join(a, geomMean, type="inner", by="saup_id") # merge km2 of shelf area with status results

  # weighted mean scores
  StatusData <- ddply(.data = geomMean, .(rgn_id, year), summarize, Status = sum(status_saup*prop_area))

  # 2013 status is based on 2011 data (most recent data)
  status = StatusData %>%
    filter(year==status_year) %>%
    mutate(
      score     = round(Status*100),
      dimension = 'status') %>%
    select(region_id=rgn_id, dimension, score)

  # ------------------------------------------------------------------------
  # STEP 6. Calculate trend
  # -----------------------------------------------------------------------
  trend = ddply(StatusData, .(rgn_id), function(x){
    mdl = lm(Status ~ year, data=x)
    data.frame(
      score     = round(coef(mdl)[['year']] * 5, 2),
      dimension = 'trend')}) %>%
    select(region_id=rgn_id, dimension, score)
  # %>% semi_join(status, by='rgn_id')

  # assemble dimensions
  scores = rbind(status, trend) %>% mutate(goal='FIS')
  return(scores)
}

MAR = function(layers, status_years){
  # layers used: mar_harvest_tonnes, mar_harvest_species, mar_sustainability_score, mar_coastalpopn_inland25km, mar_trend_years
  harvest_tonnes = rename(
    SelectLayersData(layers, layers='mar_harvest_tonnes', narrow=T),
    c('id_num'='rgn_id', 'category'='species_code', 'year'='year', 'val_num'='tonnes'))
  harvest_species = rename(
    SelectLayersData(layers, layers='mar_harvest_species', narrow=T),
    c('category'='species_code', 'val_chr'='species'))
  sustainability_score = rename(
    SelectLayersData(layers, layers='mar_sustainability_score', narrow=T),
    c('id_num'='rgn_id', 'category'='species', 'val_num'='sust_coeff'))
  popn_inland25mi = rename(
    SelectLayersData(layers, layers='mar_coastalpopn_inland25km', narrow=T),
    c('id_num'='rgn_id', 'year'='year', 'val_num'='popsum'))
  trend_years = rename(
    SelectLayersData(layers, layers='mar_trend_years', narrow=T),
    c('id_num'='rgn_id', 'val_chr'='trend_yrs'))

  rky = harvest_tonnes %>%
    merge(harvest_species     , all.x=TRUE, by='species_code') %>%
    merge(sustainability_score, all.x=TRUE, by=c('rgn_id', 'species')) %>%
    dcast(rgn_id + species + species_code + sust_coeff ~ year, value.var='tonnes', mean, na.rm=T) %>%
    arrange(rgn_id, species)

  # x = csv_compare(rky, '1-rky')

  # smooth each species-country time-series using a running mean with 4-year window, excluding NAs from the 4-year mean calculation
  # TODO: simplify below with dplyr::group_by()
  yrs_smooth <- names(rky)[!names(rky) %in% c('rgn_id','species','species_code','sust_coeff')]
  rky_smooth = zoo::rollapply(t(rky[,yrs_smooth]), 4, mean, na.rm = TRUE, partial=T)
  rownames(rky_smooth) = as.character(yrs_smooth)
  rky_smooth = t(rky_smooth)
  rky = as.data.frame(cbind(rky[, c('rgn_id','species','species_code','sust_coeff')], rky_smooth)); head(rky)
  #  x = csv_compare(rky, '2-rky-smooth')  # DEBUG

  # melt
  m = melt(rky,
           id=c('rgn_id', 'species', 'species_code', 'sust_coeff'),
           variable.name='year', value.name='sm_tonnes'); head(m)
  #   m <- m %>%
  #     arrange(rgn_id, species)
  #   x = csv_compare(m, '3-m-melt')  # DEBUG
  # "Component “year”: 'current' is not a factor"

  # for each species-country-year, smooth mariculture harvest times the sustainability coefficient
  m = within(m, {
    sust_tonnes = sust_coeff * sm_tonnes
    year        = as.numeric(as.character(m$year))
  })
  #   m <- m %>%
  #     arrange(rgn_id, species)
  #   x = csv_compare(m, '4-m-within')  # DEBUG

  # merge the MAR and coastal human population data
  m = merge(m, popn_inland25mi, by=c('rgn_id','year'), all.x=T)
  #   m <- m %>%
  #     arrange(rgn_id, species, species_code)
  #   m_a = csv_compare(m, '5-m-merge')  # DEBUG

  # must first aggregate all weighted timeseries per region, before dividing by total population
  #   ry = ddply(m, .(rgn_id, year, popsum), summarize,
  #              sust_tonnes_sum = sum(sust_tonnes),
  #              mar_pop         = sum(sust_tonnes) / popsum[1]) # <-- PROBLEM using popsum[1] with ddply!!!

  # aggregate all weighted timeseries per region, and divide by coastal human population
  ry = m %>%
    group_by(rgn_id, year) %>%
    summarize(
      sust_tonnes_sum = sum(sust_tonnes)) %>%
    merge(
      popn_inland25mi, by=c('rgn_id','year'), all.x=T) %>%
    mutate(
      mar_pop         = sust_tonnes_sum / popsum) %>%
    select(rgn_id, year, popsum, sust_tonnes_sum, mar_pop)
  #  ry_b = csv_compare(ry, '6-ry-ddply')  # RIGHT
  #   ry_a = ry
  #   eq = all.equal(ry_a, ry_b)
  #   if (class(eq) == 'character') browser()


  # get reference quantile based on argument years
  ref_95pct = quantile(subset(ry, year <= max(status_years), mar_pop, drop=T), 0.95, na.rm=T)
  #  x = csv_compare(ref_95pct, '7-ref95pct-quantile')  # DEBUG

  # identify reference rgn_id
  ry_ref = ry %>%
    filter(year <=max(status_years)) %>%
    arrange(mar_pop) %>%
    filter(mar_pop >= ref_95pct)
  cat(sprintf('95th percentile rgn_id for MAR ref pt is: %s\n', ry_ref$rgn_id[1])) # rgn_id 25 = Thailand

  ry = within(ry, {
    status = ifelse(mar_pop / ref_95pct > 1,
                    1,
                    mar_pop / ref_95pct)})
  status <- subset(ry, year == max(status_years), c('rgn_id', 'status'))
  status$status <- round(status$status*100, 2)
  #   x = csv_compare(ry, '8-ry-within')  # DEBUG

  # get list where trend is only to be calculated up to second-to-last-year
  # species where the last year of the time-series was 2010, and the same value was copied over to 2011
  # i.e. it was gapfilled using the previous year

  # get MAR trend
  ry = merge(ry, trend_years, all.x=T)
  yr_max = max(status_years)
  trend = ddply(ry, .(rgn_id), function(x){  # x = subset(ry, rgn_id==5)
    yrs = ifelse(x$trend_yrs=='4_yr',
                 (yr_max-5):(yr_max-1), # 4_yr
                 (yr_max-5):(yr_max))   # 5_yr
    y = subset(x, year %in% yrs)
    return(data.frame(
      trend = round(max(min(lm(status ~ year, data=y)$coefficients[['year']] * 5, 1), -1), 2)))
  })

  # return scores
  scores = status %>%
    select(region_id = rgn_id,
           score     = status) %>%
    mutate(dimension='status') %>%
    rbind(
      trend %>%
        select(region_id = rgn_id,
               score     = trend) %>%
        mutate(dimension = 'trend')) %>%
    mutate(goal='MAR')

  return(scores)
  # NOTE: some differences to www2013 are due to 4_yr species only previously getting trend calculated to 4 years (instead of 5)
}

FP = function(layers, scores){
  # weights
  w = rename(SelectLayersData(layers, layers='fp_wildcaught_weight', narrow=T),
             c('id_num'='region_id', 'val_num'='w_FIS')); head(w)

  # scores
  s = dcast(scores, region_id + dimension ~ goal, value.var='score', subset=.(goal %in% c('FIS','MAR') & !dimension %in% c('pressures','resilience'))); head(s)

  # combine
  d = merge(s, w)
  d$w_MAR = 1 - d$w_FIS
  d$score = apply(d[,c('FIS','MAR','w_FIS', 'w_MAR')], 1, function(x){ weighted.mean(x[1:2], x[3:4], na.rm=TRUE) })
  d$goal = 'FP'

  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}


AO = function(layers,
              year_max,
              year_min=max(min(layers_data$year, na.rm=T), year_max - 10),
              Sustainability=1.0){

  # cast data
  layers_data = SelectLayersData(layers, targets='AO')

  ry = rename(dcast(layers_data, id_num + year ~ layer, value.var='val_num',
                    subset = .(layer %in% c('ao_need'))),
              c('id_num'='region_id', 'ao_need'='need')); head(ry); summary(ry)

  r = na.omit(rename(dcast(layers_data, id_num ~ layer, value.var='val_num',
                           subset = .(layer %in% c('ao_access'))),
                     c('id_num'='region_id', 'ao_access'='access'))); head(r); summary(r)

  ry = merge(ry, r); head(r); summary(r); dim(r)

  # model
  ry = within(ry,{
    Du = (1.0 - need) * (1.0 - access)
    statusData = ((1.0 - Du) * Sustainability)
  })

  # status
  r.status <- ry %>%
    filter(year==year_max) %>%
    select(region_id, statusData) %>%
    mutate(status=statusData*100)
  summary(r.status); dim(r.status)

  # trend
  r.trend = ddply(subset(ry, year >= year_min), .(region_id), function(x)
  {
    if (length(na.omit(x$statusData))>1) {
      # use only last valid 5 years worth of status data since year_min
      d = data.frame(statusData=x$statusData, year=x$year)[tail(which(!is.na(x$statusData)), 5),]
      trend = coef(lm(statusData ~ year, d))[['year']]*5
    } else {
      trend = NA
    }
    return(data.frame(trend=trend))
  })

  # return scores
  scores = r.status %>%
    select(region_id, score=status) %>%
    mutate(dimension='status') %>%
    rbind(
      r.trend %>%
        select(region_id, score=trend) %>%
        mutate(dimension='trend')) %>%
    mutate(goal='AO') # dlply(scores, .(dimension), summary)
  return(scores)
}

NP <- function(layers){

  # temporary libraries to load while testing
  #     library(dplyr)
  #     library(tidyr)

  # load appropriate layers
  np_exposure = layers$data[['np_exposure']]
  np_harvest  = layers$data[['np_harvest_tonnes']]
  np_risk     = layers$data[['np_risk']]
  np_weight   = layers$data[['np_harvest_weight']]


  # Calculate sustainability
  # sustainability = 1 - (exposure + risk) / 2

  np_sust <- np_exposure %>%
    select(rgn_id,
           product,
           exposure = value) %>%     # different approach, same result to just below
    full_join(
      np_risk %>%
        select(rgn_id,
               product,
               risk = value),          # different approach, same result to just above
      by = c('rgn_id', 'product')) %>%
    rowwise() %>%              # otherwise will operate on the whole dataframe
    mutate(sustainability = 1 - mean(c(exposure, risk), na.rm = TRUE))
  # sapply(np_sust, class); way to check the class of each variable within the dataframe


  # Calculate relative harvest using the mean as the reference point
  # relative harvest = tonnes / mean(tonnes) for each region-product
  np_harvest_rel <- np_harvest %>%
    select(-layer) %>%
    group_by(rgn_id, product) %>%
    mutate(tonnes_mean = mean(tonnes),
           tonnes_rel = tonnes / tonnes_mean) %>%
    rowwise() %>%
    mutate(tonnes_rel_capped = min(tonnes_rel, 1)); head(np_harvest_rel)
  hist(np_harvest_rel$tonnes_rel_capped)

  # Question for OHI-China Team:
  # 1. Currently, the reference point for NP is the mean tonnes for each product-region.
  #    Please review the reference point and the variable 'tonnes_real_capped';
  #    is this the behavior you expect? Should so many scores be 1.0?

  #    rgn_id year product  tonnes tonnes_mean tonnes_rel tonnes_rel_capped
  #         1 2007 seasalt  135788     1079230  0.1258194         0.1258194
  #         1 2008 seasalt  145690     1079230  0.1349944         0.1349944
  #         1 2009 seasalt 2223300     1079230  2.0600805         1.0000000
  #         1 2010 seasalt 1460500     1079230  1.3532801         1.0000000
  #         1 2011 seasalt 1336200     1079230  1.2381053         1.0000000
  #         1 2012 seasalt 1173900     1079230  1.0877203         1.0000000


  # Calculate status for each product (status of each natural product per region)
  # xp = Hp * Sp

  xp = np_harvest_rel %>%
    left_join(np_sust,
              by = c('rgn_id', 'product')) %>%
    mutate(product_status = tonnes_rel_capped * sustainability)
  hist(xp$product_status)

  # Calculate Xp (status of each region)
  ### Calculates NP status for all production years for each region, based
  ### upon weighted mean of all products produced.
  ### From this, reports the most recent year as the NP status.
  ### Calculates NP trend for each region, based upon slope of a linear
  ### model over the past six years inclusive (five one-year intervals).
  ### Returns data frame with status and trend by region:
  ### [goal   dimension   region_id   score]
  #########################################.

  ### Calculate status, trends
  ### aggregate across products to rgn-year status, weighting with np_weight
  np_status_all <- xp %>%
    left_join(np_weight,
              by = c('rgn_id', 'product')) %>%
    select(rgn_id, year, product, product_status, weight) %>%
    group_by(rgn_id, year) %>%
    summarize(status = weighted.mean(product_status, weight)) %>%
    filter(!is.na(status)) %>% # 1/0 produces NaN
    ungroup()

  # Question for OHI-China team:
  # 2. Should the np_weight be based on production value? What do these weights mean?

  ### get current status
  np_status_current <- np_status_all %>%
    filter(year == max(year) & !is.na(status)) %>%
    mutate(
      dimension = 'status',
      score     = round(status,4) * 100) %>%
    select(rgn_id, dimension, score)
  stopifnot(
    min(np_status_current$score, na.rm = TRUE) >= 0,
    max(np_status_current$score, na.rm = TRUE) <= 100)

  ### trend based on 5 intervals (6 years of data)
  np_trend <- np_status_all %>%
    filter(year <= max(year) & year > (max(year) - 5) & !is.na(status)) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(status ~ year, data=.)) %>%
    summarize(
      rgn_id    = rgn_id,
      dimension = 'trend',
      score     = max(-1, min(1, coef(mdl)[['year']] * 5)))
  stopifnot(min(np_trend$score) >= -1, max(np_trend$score) <= 1)

  ### return scores
  np_scores <- np_status_current %>%
    full_join(np_trend) %>%
    mutate(goal = 'NP') %>%
    select(goal, dimension, region_id=rgn_id, score) %>%
    arrange(goal, dimension, region_id)

  return(np_scores)
}


CS = function(layers){

  # temporary libraries to load while testing
#     library(dplyr)
#     library(tidyr)

  # identify and select layers
  lyrs = c('cs_condition',
           'cs_contribution',
           'cs_extent',
           'cs_extent_trend')
  D = SelectLayersData(layers, layers=lyrs); head(D); summary(D)


  # spread data so layers are columns
  rk = D %>%
    select(region_id = id_num,
           layer,
           habitat = category,
           val_num) %>%
    tidyr::spread(layer, val_num) %>%   #spread(key=variable to become the column headings, value=data)
    dplyr::rename(contribution = cs_contribution,
                  condition    = cs_condition,
                  extent       = cs_extent,
                  extent_trend = cs_extent_trend); head(rk)

#    region_id     habitat contribution condition  extent extent_trend
#            1 saltmarshes          1.0       0.8 1188600         -0.1
#            1  seagrasses          0.5       0.8     100          0.0
#            2 saltmarshes          1.0       0.8   81551         -0.1
#            3 saltmarshes          1.0       0.8   76840         -0.1
#            4 saltmarshes          1.0       0.8  721275         -0.1
#            4  seagrasses          0.5       0.8     289         -0.1


  # limit to CS habitats (since only some habitats contribute to CS, but all are included in BD) -> not really needed
  # here b/c only these three habitats are included in the dataset. But good to pay attention to it, and if needed,
  # use filter as such:
  rk = rk %>%
    filter(habitat %in% c('mangroves','saltmarshes','seagrasses'))


  ## status model calculations
  #  for each region, for each habitat, it's the (sum of extent*condition*contribution)/sum(extent)
  #  xCS = sum(ck           * Cc/Cr     * Ak) / At
  #  xCS = sum(contribution * condition * extent_per_habitat) / total_extent_all_habitats

  xCS = rk %>%
    mutate(c_c_a = contribution * condition * extent) %>%  #calculate for each region, each habitat
    group_by(region_id) %>%
    summarize(sum_c_c_a  = sum(c_c_a),          # summarize will act based on group_by; aggregate for each region
              total_extent = sum(extent)) %>%   # compare by substituting 'mutate' in place of 'summarize'; summarize
                                                # gives one aggregated sum_c_c_a to each region, while mutate would simply add
                                                # one new column and give one sum_c_c_a to each region and habitat
    ungroup() %>% #always a good practice to ungroup before next operation
    mutate(xCS_calc = sum_c_c_a/total_extent,
           score = min(1,xCS_calc) * 100); head(xCS) #score can't exceed 100

  # format to combine with other goals **variable must be called r.status with the proper formatting**
    r.status = xCS %>%
    mutate(goal      = 'CS',
           dimension = 'status') %>%
    select(region_id,
           goal,
           dimension,
           score); head(r.status)

  # r.status formatting
  #   region_id goal dimension    score
  #            1   CS    status 68.25393
  #            2   CS    status 68.25393
  #            3   CS    status 68.25393


  # trend calculations
  trendCS = rk %>%
    group_by(region_id) %>%
    summarize(trend_raw = sum(extent * extent_trend) / sum(extent),
              score = max(min(trend_raw, 1), -1))

  # format to combine with other goals **variable must be called r.trend with the following formatting**
  r.trend = trendCS %>%
    mutate(goal      = 'CS',
           dimension = 'trend') %>%
    select(region_id,
           goal,
           dimension,
           score)

  # r.trend formatting
  #    region_id goal dimension     score
  #            1   CS     trend 0.8743067
  #            2   CS     trend 0.8743067
  #            3   CS     trend 0.8743067


  # return scores
  scores = rbind(r.status, r.trend)
  return(scores)
}


CP = function(layers){

  # select data, combine cp_condition and cs_extent (ie. most rencent year)

  m = layers$data[['cp_condition']] %>%
    select(rgn_id,
           habitat,
           condition=value) %>%
    full_join(layers$data[['cs_extent']] %>%
                select(-layer,
                       -year,
                       extent = hectare) ) %>% #join by rgn_id, habitat
     group_by(rgn_id) #?? didn't change the table. why? see below

  # add habitat weight
  habitat.wt = c('saltmarshes' = 3,
                 'mangroves' = 4,
                 'seagrasses' = 1)
  m = m %>%
    mutate(weight = habitat.wt[habitat])

#       rgn_id     habitat condition     extent weight
#   1       1 saltmarshes       0.5 1188600.00      4
#   2       2 saltmarshes       0.5   81551.00      4
#   3       3 saltmarshes       0.5   76840.00      4
#   4       4 saltmarshes       0.5  721275.00      4
#   5       5 saltmarshes       0.5  363979.00      4
#   6       6 saltmarshes       0.5   18314.80      4
#   7       7 saltmarshes       0.5  164270.00      4


  # Calculate CP status based on the most recent year's data
  # China model:
  # x = sum [ (Cc / Cr)  * (Wk / Wmax) * (Ak / Atotal) ]
  # x = sum [condition   * (Wk / Wmax) * (Extent_k / Total_extent))]

  xCP = m %>%
    group_by(rgn_id) %>%
    summarize(total_extent = sum(extent), ## ?? did not work...
              hab_score = condition * weight/4 * extent/A_total,
              rgn_score = sum(hab_score))

#################### below are the gl2014 codes #############################


  # join layer data
  d =
    join_all(
      list(
        layers$data[['hab_health']] %>%
          select(rgn_id, habitat, health),

        layers$data[['hab_trend']] %>%
          select(rgn_id, habitat, trend),

        # for habitat extent
        rbind_list(

          # do not use all mangrove
          layers$data[['hab_extent']] %>%
            filter(!habitat %in% c('mangrove','mangrove_inland1km','mangrove_offshore1km')) %>%
            select(rgn_id, habitat, km2),

          # just use inland1km and offshore1km
          m)),

      by=c('rgn_id','habitat'), type='full') %>%
    select(rgn_id, habitat, km2, health, trend)

  # limit to CP habitats and add rank
  habitat.rank = c('coral'            = 4,
                   'mangrove'         = 4,
                   'saltmarsh'        = 3,
                   'seagrass'         = 1,
                   'seaice_shoreline' = 4)

  d = d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(km2==0, NA, km2))

  if (nrow(d) > 0){
    scores_CP = rbind_list(
      # status
      d %>%
        filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
        group_by(rgn_id) %>%
        summarize(
          score = pmin(1, sum(rank * health * extent) / (sum(extent) * max(rank)) ) * 100,
          dimension = 'status'),
      # trend
      d %>%
        filter(!is.na(rank) & !is.na(trend) & !is.na(extent)) %>%
        group_by(rgn_id) %>%
        summarize(
          score = sum(rank * trend * extent) / (sum(extent)* max(rank)),
          dimension = 'trend')) %>%
      mutate(
        goal = 'CP') %>%
      select(region_id=rgn_id, goal, dimension, score)
  } else {
    scores_CP = data.frame(
      goal      = character(0),
      dimension = character(0),
      region_id = integer(0),
      score     = numeric())
  }

  # return scores
  return(scores_CP)
}


TR = function(layers, year_max, debug=FALSE, pct_ref=90){

  ## China model:
  # X =  log [(At/Vt * S) + 1]

  # At = number of tourists in year t (million)
  # Vt = area of sea whitin jurisdiction (km2)
  # S = 0.787, sustainability coefficient

  #library(dplyr)
  #library(tidyr)
  # merge tourist + area into one frame
  S = 0.787
  d = layers$data[['tr_tourist']] %>%
    left_join(layers$data[["tr_marinearea"]], by="rgn_id") %>%
    select(rgn_id,
           year,
           tourist = million,
           area = km2) %>%
    mutate(tour_per_area = tourist*1000000/area,
           tour_per_area_S = tour_per_area * S,
           tour_per_area_S_1 = tour_per_area_S +1,
           log = log10(tour_per_area_S_1)); head(d); summary(d) ## really strange outcome. are the log supposed to be status score?

# rgn_id year tourist    area tour_per_area tour_per_area_S tour_per_area_S_1          log
#   1      1 2008 6487.79 2000000   0.003243895     0.002552945          1.002553 0.0011073172
#   2      1 2009 4223.38 2000000   0.002111690     0.001661900          1.001662 0.0007211549
#   3      1 2010 5503.80 2000000   0.002751900     0.002165745          1.002166 0.0009395542
#   4      1 2011 6775.49 2000000   0.003387745     0.002666155          1.002666 0.0011563557
#   5      2 2008  568.93  129300   0.004400077     0.003462861          1.003463 0.0015013035
#   6      2 2009  630.67  129300   0.004877572     0.003838649          1.003839 0.0016639124

  # PREVIOUS:
  # formula:
  #   E = Ed / (L - (L*U))
  #   Sr = (S-1)/5
  #   Xtr = E * Sr
  #
  # Ed = Direct employment in tourism (tr_jobs_tourism): ** this has not been gapfilled. We thought it would make more sense to do at the status level.
  # L = Total labor force (tr_jobs_total)
  # U = Unemployment (tr_unemployment) 2013: max(year)=2011; 2012: max(year)=2010
  # so E is tourism  / employed
  # S = Sustainability index (tr_sustainability)
  #
  # based on model/GL-NCEAS-TR_v2013a: TRgapfill.R, TRcalc.R...
  # spatial gapfill simply avg, not weighted by total jobs or country population?
  # scenario='eez2013'; year_max = c(eez2012=2010, eez2013=2011, eez2014=2012)[scenario]; setwd(sprintf('~/github/ohi-global/%s', scenario))

  # get regions
  rgns = layers$data[[conf$config$layer_region_labels]] %>%
    select(rgn_id, rgn_label = label)

  # merge layers and calculate score
  d = layers$data[['tr_jobs_tourism']] %>%
    select(rgn_id, year, Ed=count) %>%
    arrange(rgn_id, year) %>%
    merge(
      layers$data[['tr_jobs_total']] %>%
        select(rgn_id, year, L=count),
      by=c('rgn_id','year'), all=T) %>%
    merge(
      layers$data[['tr_unemployment']] %>%
        select(rgn_id, year, U=percent) %>%
        mutate(U = U/100),
      by=c('rgn_id','year'), all=T) %>%
    merge(
      layers$data[['tr_sustainability']] %>%
        select(rgn_id, S_score=score),
      by=c('rgn_id'), all=T)  %>%
    mutate(
      E     = Ed / (L - (L * U)),
      S     = (S_score - 1) / 5,
      Xtr   = E * S ) %>%
    merge(rgns, by='rgn_id') %>%
    select(rgn_id, rgn_label, year, Ed, L, U, S, E, Xtr)

  # feed NA for subcountry regions without sufficient data (vs global analysis)
  if (conf$config$layer_region_labels!='rgn_global' & sum(!is.na(d$Xtr))==0) {
    scores_TR = rbind_list(
      rgns %>%
        select(region_id = rgn_id) %>%
        mutate(
          goal      = 'TR',
          dimension = 'status',
          score     = NA),
      rgns %>%
        select(region_id = rgn_id) %>%
        mutate(
          goal      = 'TR',
          dimension = 'trend',
          score     = NA))
    return(scores_TR)
  }

  #   if (debug){
  #     # compare with pre-gapfilled data
  #     if (!file.exists('temp')) dir.create('temp', recursive=T)
  #
  #     # cast to wide format (rows:rgn, cols:year, vals: Xtr) similar to original
  #     d_c = d %>%
  #       filter(year %in% (year_max-5):year_max) %>%
  #       dcast(rgn_id ~ year, value.var='Xtr')
  #     write.csv(d_c, sprintf('temp/%s_TR_0-pregap_wide.csv', basename(getwd())), row.names=F, na='')
  #
  #     o = read.csv(file.path(dir_neptune_data, '/model/GL-NCEAS-TR_v2013a/raw/TR_status_pregap_Sept23.csv'), na.strings='') %>%
  #       melt(id='rgn_id', variable.name='year', value.name='Xtr_o') %>%
  #       mutate(year = as.integer(sub('x_TR_','', year, fixed=T))) %>%
  #       arrange(rgn_id, year)
  #
  #     vs = o %>%
  #       merge(
  #         expand.grid(list(
  #           rgn_id = rgns$rgn_id,
  #           year   = 2006:2011)),
  #         by=c('rgn_id', 'year'), all=T) %>%
  #       merge(d, by=c('rgn_id','year')) %>%
  #       mutate(Xtr_dif = Xtr - Xtr_o) %>%
  #       select(rgn_id, rgn_label, year, Xtr_o, Xtr, Xtr_dif, E, Ed, L, U, S) %>%
  #       arrange(rgn_id, year)
  #     write.csv(vs, sprintf('temp/%s_TR_0-pregap-vs_details.csv', basename(getwd())), row.names=F, na='')
  #
  #     vs_rgn = vs %>%
  #       group_by(rgn_id) %>%
  #       summarize(
  #         n_notna_o   = sum(!is.na(Xtr_o)),
  #         n_notna     = sum(!is.na(Xtr)),
  #         dif_avg     = mean(Xtr, na.rm=T) - mean(Xtr_o, na.rm=T),
  #         Xtr_2011_o  = last(Xtr_o),
  #         Xtr_2011    = last(Xtr),
  #         dif_2011    = Xtr_2011 - Xtr_2011_o) %>%
  #       filter(n_notna_o !=0 | n_notna!=0) %>%
  #       arrange(desc(abs(dif_2011)), Xtr_2011, Xtr_2011_o)
  #     write.csv(vs_rgn, sprintf('temp/%s_TR_0-pregap-vs_summary.csv', basename(getwd())), row.names=F, na='')
  #   }

  # get georegions for gapfilling
  georegions = layers$data[['rgn_georegions']] %>%
    dcast(rgn_id ~ level, value.var='georgn_id')
  georegion_labels =  layers$data[['rgn_georegion_labels']] %>%
    mutate(level_label = sprintf('%s_label', level)) %>%
    dcast(rgn_id ~ level_label, value.var='label') %>%
    left_join(
      layers$data[['rgn_labels']] %>%
        select(rgn_id, v_label=label),
      by='rgn_id')

  # setup data for georegional gapfilling (remove Antarctica rgn_id=213)
  if (!file.exists('temp')) dir.create('temp', recursive=T)
  csv = sprintf('temp/%s_TR_1-gapfill-georegions.csv', basename(getwd()))
  if (conf$config$layer_region_labels=='rgn_global'){
    d_g = gapfill_georegions(
      data              = d %>%
        filter(rgn_id!=213) %>%
        select(rgn_id, year, Xtr),
      fld_id            = 'rgn_id',
      fld_value         = 'Xtr',
      fld_weight        = NULL,
      georegions        = georegions,
      ratio_weights     = FALSE,
      georegion_labels  = georegion_labels,
      r0_to_NA          = TRUE,
      attributes_csv    = csv)

    # regions with Travel Warnings at http://travel.state.gov/content/passports/english/alertswarnings.html
    rgn_travel_warnings = c('Djibouti'=46, 'Eritrea'=45, 'Somalia'=44, 'Mauritania'=64)
    # TODO: check if regions with travel warnings are gapfilled (manually checked for 2013)
    d_g = rbind_list(
      d_g %>%
        filter(!rgn_id %in% rgn_travel_warnings),
      d_g %>%
        filter(rgn_id %in% rgn_travel_warnings) %>%
        mutate(
          Xtr = 0.1 * Xtr))
  } else {
    d_g = d
  }

  # filter: limit to 5 intervals (6 years worth of data)
  #   NOTE: original 2012 only used 2006:2010 whereas now we're using 2005:2010
  d_g_f = d_g %>%
    filter(year %in% (year_max - 5):year_max)

  # rescale for
  #   status: 95 percentile value across all regions and filtered years
  #   trend: use the value divided by max bc that way it's rescaled but not capped to a lower percentile (otherwise the trend calculated for regions with capped scores, i.e. those at or above the percentile value, would be spurious)

  d_q_yr  =
    d_g_f %>%
    group_by(year) %>%
    summarize(
      Xtr_q = quantile(Xtr, probs=pct_ref/100, na.rm=T))
  # year     Xtr_q
  # 2006 0.06103857
  # 2007 0.06001672
  # 2008 0.06222823
  # 2009 0.05563864
  # 2010 0.05811622
  # 2011 0.05893174

  Xtr_max = max(d_g_f$Xtr, na.rm=T)

  # print the reference point--incomplete
  #   d_g_f_ref = d_g_f_r %>%
  #     filter(Xtr >= Xtr_max)
  #   cat(sprintf('the %f percentile for TR is for rgn_id=%f', pct_ref,

  d_g_f_r = d_g_f %>%
    left_join(d_q_yr, by='year') %>%
    mutate(
      Xtr_rq  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q), # rescale to qth percentile, cap at 1
      Xtr_rmax = Xtr / Xtr_max )                         # rescale to max value
  if (debug){
    write.csv(d_g_f_r, sprintf('temp/%s_TR_2-filtered-rescaled.csv', basename(getwd())), row.names=F, na='')
  }

  # calculate trend
  d_t = d_g_f_r %>%
    filter(!is.na(Xtr_rmax)) %>%
    arrange(year, rgn_id) %>%
    group_by(rgn_id) %>%
    do(mod = lm(Xtr_rmax ~ year, data = .)) %>%
    do(data.frame(
      rgn_id = .$rgn_id,
      dimension = 'trend',
      score = max(min(coef(.$mod)[['year']] * 5, 1), -1)))

  # get status (as last year's value)
  d_s = d_g_f_r %>%
    arrange(year, rgn_id) %>%
    group_by(rgn_id) %>%
    summarize(
      dimension = 'status',
      score = last(Xtr_rq) * 100)

  # bind rows
  d_b = rbind(d_t, d_s) %>%
    mutate(goal = 'TR')

  if (conf$config$layer_region_labels=='rgn_global'){
    # assign NA for uninhabitated islands
    unpopulated = layers$data[['le_popn']] %>%
      group_by(rgn_id) %>%
      filter(count==0) %>%
      select(rgn_id)
    d_b$score = ifelse(d_b$rgn_id %in% unpopulated$rgn_id, NA, d_b$score)

    # replace North Korea value with 0
    d_b$score[d_b$rgn_id == 21] = 0
  }

  # final scores
  scores = d_b %>%
    select(region_id=rgn_id, goal, dimension, score)

  if (debug){

    # compare with original scores
    csv_o = file.path(dir_neptune_data, 'git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores.Global2013.www2013_2013-10-09.csv')
    o = read.csv(csv_o, na.strings='NA', row.names=1) %>%
      filter(goal %in% c('TR') & dimension %in% c('status','trend') & region_id!=0) %>%
      select(goal, dimension, region_id, score_o=score)

    vs = scores %>%
      merge(o, all=T, by=c('goal','dimension','region_id')) %>%
      merge(
        rgns %>%
          select(region_id=rgn_id, region_label=rgn_label),
        all.x=T) %>%
      mutate(
        score_dif    = score - score_o,
        score_notna  =  is.na(score)!=is.na(score_o)) %>%
      #filter(abs(score_dif) > 0.01 | score_notna == T) %>%
      arrange(desc(dimension), desc(abs(score_dif))) %>%
      select(dimension, region_id, region_label, score_o, score, score_dif)

    # output comparison
    write.csv(vs, sprintf('temp/%s_TR_3-scores-vs.csv', basename(getwd())), row.names=F, na='')

  }

  return(scores)
}

LIV_ECO = function(layers, subgoal){

  ## read in all data:

  # gdp, wages, jobs and workforce_size data
  le_gdp   = SelectLayersData(layers, layers='le_gdp')  %>%
    select(rgn_id = id_num, year, gdp_usd = val_num)

  le_wages = SelectLayersData(layers, layers='le_wage_sector_year') %>%
    select(rgn_id = id_num, year, sector = category, wage_usd = val_num)

  le_jobs  = SelectLayersData(layers, layers='le_jobs_sector_year') %>%
    select(rgn_id = id_num, year, sector = category, jobs = val_num)

  le_workforce_size = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
    select(rgn_id = id_num, year, jobs_all = val_num)

  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    select(rgn_id = id_num, year, pct_unemployed = val_num)

  # debug
  #     le_gdp            = read.csv('eez2014/layers/le_gdp.csv')
  #     le_wages          = read.csv('eez2014/layers/le_wage_sector_year.csv')
  #     le_jobs           = read.csv('eez2014/layers/le_jobs_sector_year.csv')
  #     le_workforce_size = read.csv('eez2014/layers/le_workforcesize_adj.csv')
  #     le_unemployment   = read.csv('eez2014/layers/le_unemployment.csv')

  # multipliers from Table S10 (Halpern et al 2012 SOM)
  multipliers_jobs = data.frame('sector' = c('tour','cf', 'mmw', 'wte','mar'),
                                'multiplier' = c(1, 1.582, 1.915, 1.88, 2.7)) # no multiplers for tour (=1)
  # multipliers_rev  = data.frame('sector' = c('mar', 'tour'), 'multiplier' = c(1.59, 1)) # not used because GDP data is not by sector


  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  # reworded from SOM p.26-27
  #reference point for wages is the reference region (r) with the highest average wages across all sectors.
  #Reference points for jobs (j) and revenue (e) employ a moving baseline. The two metrics (j, e) are calculated
  #as relative values: the value in the current year (or most recent year), c, relative to the value in a recent
  #moving reference period, r, defined as 5 years prior to c. This reflects an implicit goal of maintaining coastal
  #livelihoods and economies (L&E) on short time scales, allowing for decadal or generational shifts in what people
  #want and expect for coastal L&E. The most recent year c must be 2000 or later in order for the data to be included.

  liv =
    # adjust jobs
    le_jobs %>%
    left_join(multipliers_jobs, by = 'sector') %>%
    mutate(jobs_mult = jobs * multiplier) %>%  # adjust jobs by multipliers
    left_join(le_employed, by= c('rgn_id', 'year')) %>%
    mutate(jobs_adj = jobs_mult * proportion_employed) %>% # adjust jobs by proportion employed
    left_join(le_wages, by=c('rgn_id','year','sector')) %>%
    arrange(year, sector, rgn_id)

  # LIV calculations ----

  # LIV status
  liv_status = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wage_usd))
  # aia/subcountry2014 crashing b/c no concurrent wage data, so adding this check
  if (nrow(liv_status)==0){
    liv_status = liv %>%
      select(region_id=rgn_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'status',
        score     = NA)
    liv_trend = liv %>%
      select(region_id=rgn_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'trend',
        score     = NA)
  } else {
    liv_status = liv_status %>%
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      arrange(rgn_id, year, sector) %>%
      # summarize across sectors
      group_by(rgn_id, year) %>%
      summarize(
        # across sectors, jobs are summed
        jobs_sum  = sum(jobs_adj, na.rm=T),
        # across sectors, wages are averaged
        wages_avg = mean(wage_usd, na.rm=T)) %>%
      group_by(rgn_id) %>%
      arrange(rgn_id, year) %>%
      mutate(
        # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        jobs_sum_first  = first(jobs_sum),                     # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
        # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
        # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        wages_avg_first = first(wages_avg)) %>% # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
      # calculate final scores
      ungroup() %>%
      mutate(
        x_jobs  = pmax(-1, pmin(1,  jobs_sum / jobs_sum_first)),
        x_wages = pmax(-1, pmin(1, wages_avg / wages_avg_first)),
        score   = mean(c(x_jobs, x_wages), na.rm=T) * 100) %>%
      # filter for most recent year
      filter(year == max(year, na.rm=T)) %>%
      # format
      select(
        region_id = rgn_id,
        score) %>%
      mutate(
        goal      = 'LIV',
        dimension = 'status')

    # LIV trend
    # From SOM p. 29: trend was calculated as the slope in the individual sector values (not summed sectors)
    # over the most recent five years...
    # with the average weighted by the number of jobs in each sector
    # ... averaging slopes across sectors weighted by the revenue in each sector

    # get trend across years as slope of individual sectors for jobs and wages
    liv_trend = liv %>%
      filter(!is.na(jobs_adj) & !is.na(wage_usd)) %>%
      # TODO: consider "5 year time spans" as having 5 [(max(year)-4):max(year)] or 6 [(max(year)-5):max(year)] member years
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      # get sector weight as total jobs across years for given region
      arrange(rgn_id, year, sector) %>%
      group_by(rgn_id, sector) %>%
      mutate(
        weight = sum(jobs_adj, na.rm=T)) %>%
      # reshape into jobs and wages columns into single metric to get slope of both with one do() call
      reshape2::melt(id=c('rgn_id','year','sector','weight'), variable='metric', value.name='value') %>%
      mutate(
        sector = as.character(sector),
        metric = as.character(metric)) %>%
      # get linear model coefficient per metric
      group_by(metric, rgn_id, sector, weight) %>%
      do(mdl = lm(value ~ year, data=.)) %>%
      summarize(
        metric = metric,
        weight = weight,
        rgn_id = rgn_id,
        sector = sector,
        # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
        sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
      arrange(rgn_id, metric, sector) %>%
      # get weighted mean across sectors per region-metric
      group_by(metric, rgn_id) %>%
      summarize(
        metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
      # get mean trend across metrics (jobs, wages) per region
      group_by(rgn_id) %>%
      summarize(
        score = mean(metric_trend, na.rm=T)) %>%
      # format
      mutate(
        goal      = 'LIV',
        dimension = 'trend') %>%
      select(
        goal, dimension,
        region_id = rgn_id,
        score)
  }


  # ECO calculations ----
  eco = le_gdp %>%
    mutate(
      rev_adj = gdp_usd,
      sector = 'gdp') %>%
    # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
    select(rgn_id, year, sector, rev_adj)

  # ECO status
  eco_status = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # across sectors, revenue is summed
    group_by(rgn_id, year) %>%
    summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    select(
      goal, dimension,
      region_id = rgn_id,
      score)

  # ECO trend
  eco_trend = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    # get sector weight as total revenue across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(rgn_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    summarize(
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    # get weighted mean across sectors per region
    group_by(rgn_id) %>%
    summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    select(
      goal, dimension,
      region_id = rgn_id,
      score)

  # report LIV and ECO scores separately
  if (subgoal=='LIV'){
    d = rbind(liv_status, liv_trend)
  } else if (subgoal=='ECO'){
    d = rbind(eco_status, eco_trend)
  } else {
    stop('LIV_ECO function only handles subgoal of "LIV" or "ECO"')
  }
  return(d)

}


LE = function(scores, layers){

  # calculate LE scores
  scores.LE = scores %.%
    filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %.%
    dcast(region_id + dimension ~ goal, value.var='score') %.%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm=T)) %.%
    select(region_id, dimension, score) %.%
    mutate(goal  = 'LE')

  # rbind to all scores
  scores = scores %.%
    rbind(scores.LE)

  # return scores
  return(scores)
}


ICO = function(layers){

  # layers
  lyrs = c('ico_spp_extinction_status' = 'risk_category',
           'ico_spp_popn_trend'        = 'popn_trend')

  # cast data ----
  layers_data = SelectLayersData(layers, layers=names(lyrs))
  rk = rename(dcast(layers_data, id_num + category ~ layer, value.var='val_chr'),
              c('id_num'='region_id', 'category'='sciname', lyrs))

  # lookup for weights status
  w.risk_category = c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)

  # lookup for population trend
  w.popn_trend = c('Decreasing' = -0.5,
                   'Stable'     =  0,
                   'Increasing' =  0.5)

  # status
  r.status = rename(ddply(rk, .(region_id), function(x){
    mean(1 - w.risk_category[x$risk_category], na.rm=T) * 100 }),
    c('V1'='score'))

  # trend
  r.trend = rename(ddply(rk, .(region_id), function(x){
    mean(w.popn_trend[x$popn_trend], na.rm=T) }),
    c('V1'='score'))

  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='ICO'))
  return(scores)

}


LSP = function(layers, ref_pct_cmpa=30, ref_pct_cp=30, status_year, trend_years){

  lyrs = list('lsp_cmpa',
              'lsp_marinearea')

  # cast data ----
  cmpa = SelectLayersData(layers, layers='lsp_cmpa')
  marinearea = SelectLayersData(layers, layers='lsp_marinearea')

  cmpa = cmpa %>%
    select(rgn_id = id_num,
           year,
           cmpa = val_num); head(cmpa)

  marinearea = marinearea %>%
    select(rgn_id = id_num,
           marinearea = val_num); head(marinearea)

  # Calculate status of each year in each province
  d = cmpa %>%
    left_join(marinearea)%>% #head(d)
    mutate(reference = marinearea*0.3)%>%
    mutate(pct_cmpa = cmpa/marinearea*100)%>%
    mutate(status = pmin(pct_cmpa/30 *100, 100))

 # select status: year = 2012
  status = filter(d, year == 2012)%>%
   select(rgn_id, status)

 # calculate trend: not working. lm wrong: example: rgn 7: 12-14 --> coefficient too large
  #  coef(lm(pct_cmpa ~ year, data=D))
  #  (Intercept)          year
  #  -1165.6904933     0.5864533

  rgn_id = c(1:11)

 for (i in 1:11) {
   D = filter(d, rgn_id == i)
   trend = min(1, max(0, 4 * coef(lm(pct_cmpa ~ year, data=D))[['year']]))
   trend2 = data.frame(rgn_id, trend)
   return(trend2)
 } ## 0 for all provinces, eventhough for region 4,5, 7, and 8 show slight increase in cmpa; it is a rounding issue.


#   r  = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['r']]))),
#               c('id_num'='region_id', lyrs[['r']]))
#   ry = rename(dcast(d, id_num + year ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['ry']]))),
#               c('id_num'='region_id', lyrs[['ry']]))

  # fill in time series from first year specific region_id up to max year for all regions and generate cumulative sum
#   yr.max = max(max(ry$year), status_year)
#   r.yrs = ddply(ry, .(region_id), function(x){
#     data.frame(region_id=x$region_id[1],
#                year=min(x$year):yr.max)
#   })
#   r.yrs = merge(r.yrs, ry, all.x=T)
#   r.yrs$cp[is.na(r.yrs$cp)]     = 0
#   r.yrs$cmpa[is.na(r.yrs$cmpa)] = 0
#   r.yrs = within(r.yrs, {
#     cp_cumsum    = ave(cp  , region_id, FUN=cumsum)
#     cmpa_cumsum  = ave(cmpa, region_id, FUN=cumsum)
#     pa_cumsum    = cp_cumsum + cmpa_cumsum
#   })

  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
#   r.yrs = merge(r.yrs, r, all.x=T); head(r.yrs)
#   r.yrs = within(r.yrs,{
#     pct_cp    = pmin(cp_cumsum   / area_inland1km   * 100, 100)
#     pct_cmpa  = pmin(cmpa_cumsum / area_offshore3nm * 100, 100)
#     prop_protected    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2
#   })
#
#   # extract status based on specified year
#   r.status = r.yrs %>%
#     filter(year==status_year) %>%
#     select(region_id, status=prop_protected) %>%
#     mutate(status=status*100)
#   head(r.status)

  # calculate trend
  r.trend = ddply(subset(r.yrs, year %in% trend_years), .(region_id), function(x){
    data.frame(
      trend = min(1, max(0, 5 * coef(lm(prop_protected ~ year, data=x))[['year']])))})

  # return scores
  scores = rbind.fill(
    within(r.status, {
      goal      = 'LSP'
      dimension = 'status'
      score     = status}),
    within(r.trend, {
      goal      = 'LSP'
      dimension = 'trend'
      score     = trend}))
  return(scores[,c('region_id','goal','dimension','score')])
}

SP = function(scores){

  d = within(
    dcast(
      scores,
      region_id + dimension ~ goal, value.var='score',
      subset=.(goal %in% c('ICO','LSP') & !dimension %in% c('pressures','resilience')))
    , {
      goal = 'SP'
      score = rowMeans(cbind(ICO, LSP), na.rm=T)})


  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}


CW = function(layers){

  # layers
  lyrs = c('po_pathogens' = 'a',
           'po_nutrients' = 'u',
           'po_chemicals' = 'l',
           'po_trash'     = 'd',
           'cw_pesticide_trend'   = 'pest_trend',
           'cw_fertilizer_trend'  = 'fert_trend',
           'cw_coastalpopn_trend' = 'popn_trend',
           'cw_pathogen_trend'    = 'path_trend')

  # cast data
  d = SelectLayersData(layers, layers=names(lyrs))
  r = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs))),
             c('id_num'='region_id', lyrs)); head(r); summary(r)

  # invert pressures
  r$a = 1 - r$a
  r$u = 1 - r$u
  r$l = 1 - r$l
  r$d = 1 - r$d

  # invert trends for CW
  r$popn_trend = -1 * r$popn_trend
  r$path_trend = -1 * r$path_trend
  r$pest_trend = -1 * r$pest_trend
  r$fert_trend = -1 * r$fert_trend

  # status
  r$status = psych::geometric.mean(t(r[,c('a','u','l','d')]), na.rm=T) * 100

  # trend
  r$trend = rowMeans(r[,c('pest_trend','fert_trend','popn_trend','path_trend')], na.rm=T)

  # return scores
  scores = rbind(
    within(r, {
      goal      = 'CW'
      dimension = 'status'
      score     = status}),
    within(r, {
      goal      = 'CW'
      dimension = 'trend'
      score     = trend}))[,c('region_id','goal','dimension','score')]
  return(scores)
}


HAB = function(layers){

  # get layer data
  d =
    join_all(
      list(

        layers$data[['hab_health']] %>%
          select(rgn_id, habitat, health),

        layers$data[['hab_trend']] %>%
          select(rgn_id, habitat, trend),

        layers$data[['hab_extent']] %>%
          select(rgn_id, habitat, extent=km2)),

      by=c('rgn_id','habitat'), type='full') %>%
    select(rgn_id, habitat, extent, health, trend)

  # limit to habitats used for HAB, create extent presence as weight
  d = d %>%
    filter(habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom')) %>%
    mutate(
      w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    filter(!is.na(w)) %>%
    group_by(rgn_id)

  # calculate scores
  scores_HAB = rbind_list(
    # status
    d %>%
      filter(!is.na(health)) %>%
      summarize(
        score = pmin(1, sum(w * health) / sum(w)) * 100,
        dimension = 'status'),
    # trend
    d %>%
      filter(!is.na(trend)) %>%
      summarize(
        score =  sum(w * trend) / sum(w),
        dimension = 'trend')) %>%
    mutate(
      goal = 'HAB') %>%
    select(region_id=rgn_id, goal, dimension, score)

  # return scores
  return(scores_HAB)
}


SPP = function(layers){

  # scores
  scores = cbind(rename(SelectLayersData(layers, layers=c('spp_status'='status','spp_trend'='trend'), narrow=T),
                        c(id_num='region_id', layer='dimension', val_num='score')),
                 data.frame('goal'='SPP'))
  scores = mutate(scores, score=ifelse(dimension=='status', score*100, score))
  return(scores)
}

BD = function(scores){

  d = within(
    dcast(
      scores,
      region_id + dimension ~ goal, value.var='score',
      subset=.(goal %in% c('HAB','SPP') & !dimension %in% c('pressures','resilience'))),
{
  goal = 'BD'
  score = rowMeans(cbind(HAB, SPP), na.rm=T)})

# return all scores
return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}

PreGlobalScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)

  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns[,'id_num'], 0))

  # apply NA to Antarctica
  id_ant = subset(rgns, val_chr=='Antarctica', id_num, drop=T)
  scores[scores$region_id==id_ant, 'score'] = NA

  return(scores)
}

FinalizeScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)

  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors=F); head(d)
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) &
               !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))
  scores = merge(scores, d, all=T)[,c('goal','dimension','region_id','score')]

  # order
  scores = arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
}
