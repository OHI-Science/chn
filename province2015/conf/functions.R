FIS = function(layers){

# cast data
  ft = layers$data[['fis_ft']] %>% # fishing efforts
    select(-layer,
           ft = kilowatt)

  mmsy = layers$data[['fis_mmsy']] %>% #maximum sustainable yield
    select(rgn_id,
           mmsy = tonnes)

  tc = layers$data[['fis_tc']] %>% #normalizing factor
    select(rgn_id,
           tc = score)

  ct = layers$data[['fis_ct']] %>% # total catch
    select(rgn_id,
           year,
           ct = tonnes)

## Status model:
  # Bt = ut/q

  # delta_Bt:    0,              if mmsy_ref-Bt<0.05*mmsy_ref
  #              |mmsy_ref-Bt|,  if  |mmsy_ref-Bt|<mmsy_ref
  #              mmsy_ref,       otherwise.
  # xFIS = (1 - delta_Bt/mmsy_ref) * Tc

    # before calculating status, we need to obtain r, K, and q:
    # first, calcualte Ut = Ct/ft, and Ut+1
    # then, run a multiple linear regression in the form of:
    # Ut+1 - Ut - 1 = r - [r/(Kq)]*Ut - q*ft
    # obtain r, K, q from linear model coefficients

  ### To Mian: Real calculation of status and trend (40-102). but results are all 0's, which screwed up trend calcualtion in the next step.
  ### So I made a place holder status data set after this session (line 101 -). You should check on this.
  ### 李冕，40-102 是正确的计算方式。但现状（status) 计算结果很奇怪，全是0，因为中间计算的的 r, K, q, mmsy 结果奇怪，有很多
  ### 不该有的负数值。导致趋势 (trend) 计算不能做。这里你该仔细看一看。为了FP计算，我暂时在101- 我暂时做了占位符 （r.status, r.trend)。

  # combining all data
D1 = ft %>%
  left_join(ct, by = c("rgn_id", "year")) %>%
  mutate(ut = ct/ft) %>%
  group_by(rgn_id) %>%
  mutate(ut_plus1 = c(ut[-1], NA), # lag: start from year 1, last year 2013 set as NA.
                                   # Q: lost the most recent year's info
         y = ut_plus1/ut - 1) %>%
  filter(!year=='2013')

D2 = D1 %>%
  do(dlm = lm(y ~ ut + ft, data = .)) %>%
  # calculating r, q, and K from multiple linear regression
  mutate(r = coef(dlm)[["(Intercept)"]],
         r_Kq = -coef(dlm)[['ut']], # = r/Kq
         q = -coef(dlm)[['ft']],
         K = r/r_Kq/q,
         mmsy = r*K/4,
         mmsy_r = mmsy*0.75)  %>%
  select(-dlm) %>%
  ungroup; head(D2); summary(D2)

## status:
fis.status.all.years = D2 %>%
  select(rgn_id, q) %>%
  full_join(
    select(D1, rgn_id, year, ut), by = 'rgn_id') %>%
  mutate(Bt = ut/q) %>%
  full_join(
    select(D2, rgn_id, mmsy_r), by = 'rgn_id') %>%
  mutate(abs = abs(mmsy_r - Bt), #absolute value of (mmsy_r - Bt)

         d_Bt = if (abs< 0.05*mmsy_r) {
           0
         } else if ( abs > 0.05*mmsy_r | abs < mmsy_r) {
                       abs
         } else mmsy_r ) %>%
  full_join (tc, by = 'rgn_id') %>%
  mutate(x.fis = max(0, min(1, (1 - d_Bt/mmsy_r)*tc))*100)

# # current status 最近一年现状
# r.status = fis.status.all.years %>%
#   filter(year==max(year)) %>% # year = 2012
#   mutate(goal = "FIS",
#          dimension = 'status') %>%
#   select(goal,
#          dimension,
#          region_id = rgn_id,
#          score = x.fis)

  ###  Real calculation of trend when FIS status question answered (ie. status = 0)
  ### 趋势计算
  # r.trend = status.all.years %>%
  #   group_by(rgn_id) %>%
  #   filter(year == (max(year) - 4) : max(year)) %>% # most recent 5 years of data
  #   do(dml = lm(x.fis ~ year, data =.)) %>%
  #   mutate(trend = coef(dml)[['year']]*4,  # can't calculate now b/c all status are 0's
  #          goal = 'FIS',
  #          dimension = 'trend') %>%
  #   select(goal,
  #          dimension,
  #          region_id = rgn_id,
  #          score = trend)


  ### Status Place holder
r.status = data.frame(goal = 'FIS',
                        dimension = 'status',
                        region_id = c(1:11),
                        score = 1)


 ### Trend placeholder
r.trend = data.frame(goal = 'FIS',
                     dimension = 'trend',
                     region_id = c(1:11),
                     score = 0)



fis_scores = rbind(r.status, r.trend)
return(scores)
}

MAR = function(layers, Debug=T){
  # CHN model: Yc = sum(harvest * msi) / area
  #        status = log10(Yc+1)

  # cast data 取数据
  mar_msi = layers$data[['mar_smk']]
  mar_area = layers$data[['mar_ac']] #1994-2013
  mar_harvest = layers$data[['mar_yk']] #1994-2013

D = full_join(mar_msi, mar_harvest, by=c("rgn_id", 'species' )) %>% # 合并数据
  select(rgn_id,
         species,
         year,
         harvest = tonnes,
         msi = score) %>%
  left_join(mar_area, by=c("rgn_id", "year")) %>%
  select(-layer,
         area = km2)

# status - using all years from word document equations
# aggregate all weighted timeseries per province (group by province and year), and divide by area
mar.status.all.years =
    D %>%
      filter(!area == 0) %>% #  exclude cases where no harvest and no allowable area (rgn_id 6)
                             #  should be filter(!(area == 0 & harvest == 0))
      group_by(rgn_id, year) %>%
      summarize(yc = sum(harvest*msi/area),
                yc.log = log10(yc+1)) %>%
      ungroup(); head(mar.status.all.years); summary(mar.status.all.years); sapply(mar.status.all.years, class)

# set reference point the highest region in the most recent (max) year
# 参考点的取值，暂时取用最近一年最高 yc.log 值为参考点； 在文件叙述中没有表明。需要讨论。

ref_data = mar.status.all.years %>%
  filter(year == 2013); ref_data #选最近一年

ref_pt_info <- ref_data %>% filter(yc.log == max(yc.log, na.rm=TRUE)) #选最高yc.log 为参考点
ref_pt <- ref_pt_info$yc.log
cat(sprintf('reference point for MAR is: %s (region %s)\n', ref_pt, ref_pt_info$rgn_id))
# display reference point
# 直接显示／打印标题内容 cat(sprintf('.... %s ...%s..'), 取代第一个%s, 取代第二个%s)

mar.status.all.years = mar.status.all.years %>%
  mutate(x.mar = ifelse(yc.log / ref_pt > 1,
                        1,
                        yc.log / ref_pt)); head(mar.status.all.years); summary(mar.status.all.years)

# current status
r.status = mar.status.all.years %>%
  filter(year == max(year)) %>%
  mutate(score = round(x.mar*100,2)) %>%
  select(rgn_id,
         score) %>%
  rbind(data.frame(rgn_id = 6, #SH[6] has no MAR from 2007-2013, social preference to have no more mariculture. Add back as NA
                   score  = NA)) %>%
  arrange(rgn_id) %>%
  mutate(dimension = 'status',
         goal = 'MAR') %>%
  select(goal,
         dimension,
         region_id = rgn_id,
         score)


# trend
r.trend = mar.status.all.years %>%
  select(rgn_id, year, x.mar) %>%
  group_by(rgn_id) %>%
  filter(year == (max(year)-4):max(year)) %>% #the most recent 5 years of data
  do(dml = lm(x.mar ~ year, data=.)) %>% # lm 线性方程
  mutate(score = pmax(-1, pmin(1, coef(dml)[['year']]*4))) %>% # 4 intervals 4个间隔；year的系数
                                                               # pmin(1, ...): 最大不超过1
                                                               # pmax(-1, ...): 最小不超过－1
  select(region_id = rgn_id,
         score) %>%
  rbind(data.frame(region_id = 6, score = NA)) %>% # rgn 6 (SH), again doesn't have a trend as there is no more MAR
  mutate(dimension = 'trend',
         goal = 'MAR') %>%
  select(goal,
         dimension,
         region_id,
         score) %>%
  arrange(region_id)


# if (Debug == TRUE) mar_scores = rbind(r.status, r.trend)

mar_scores = rbind(r.status, r.trend)
return(scores)

}

FP = function(layers, scores, debug=T){
  # CHN model: xFP = w * xFIS + (1-w) * xMAR
  #
  #            w = 1,                      if xMAR = No data
  #                0.5,                    if xFIS = 0.25 * Tc
  #                Bt/(Bt + sum(Yk)),      otherwise

# cast data needed for w calculation: Bt, Yk, Tc from FIS and MAR data layers and calculations
  mar_yk = layers$data[['mar_yk']] %>%
    group_by(rgn_id) %>%
    filter(year == max(year)) %>% # mar: status of 2013； MAR 现状用2013
    summarize(sum.yk = sum(tonnes)) %>%
    rename(region_id = rgn_id)

  fis_Bt = fis.status.all.years %>% # calculated in FIS status
    group_by(rgn_id) %>%
    filter(year == max(year)) %>% # fis: status of 2012； FIS 现状用2012
    select(region_id = rgn_id, Bt)

  fis_Tc = layers$data[['fis_tc']] %>%
    rename(region_id = rgn_id,
           Tc = score)

### 计算现状， 239-243 在正式计算所有得分(通过calculate_scores.R)并储存在scores.csv 之后，用来代替 247-248
#   s = scores %>%
#     filter(goal %in% c('MAR', 'FIS'),
#            !dimension %in% c('pressures','resilience')) %>%
#     tidyr::spread(goal, score) %>%
#     select(region_id, dimension, FIS, MAR)

  # combine fis and mar scores
  s = rbind(fis_scores, mar_scores) %>%
    spread(goal, score)

  # calcualte w
  w = s %>%
    filter(dimension == 'status') %>%
    left_join(mar_yk, by = 'region_id') %>%
    left_join(fis_Bt, by = 'region_id') %>%
    left_join(fis_Tc, by = 'region_id') %>%
    mutate(w = ifelse (is.na(MAR), 1,
                       ifelse(FIS == 0.25 * fis_Tc, 0.5, Bt/(Bt+sum.yk)))) %>%
    select(region_id, w)

  scores_FP = s %>%
    full_join(w, by = 'region_id') %>%
    mutate(score = w*FIS + (1-w)*MAR,
           goal = 'FP') %>%
    select(goal,
           dimension,
           region_id,
           score)

  return(scores_FP) }



AO = function(layers){

  # status
  # xAO = (APc/APr + AFc/AFr + AEc/AEr) / 3

  # cast data
  lyrs = c(#'ao_port',
           #'ao_port_ref', # no year. couldn't join port data with the men and gas data properly
          'ao_men',       #2003-2013
          'ao_men_ref',
          'ao_gas',       #2010-2014
          'ao_gas_ref')
  d = SelectLayersData(layers, layers=lyrs); head(D); summary(D)

  D = d %>%
    select(rgn_id = id_num,
           year,
           val_num,
           layer) %>%
    spread(layer, val_num) %>%
    select(rgn_id,
           year,
           gas = ao_gas,
           gas_ref = ao_gas_ref,
           fishermen = ao_men,
           fishermen_ref = ao_men_ref) %>%
    full_join(layers$data[['ao_port']] %>%
                select(rgn_id, port = count), by = 'rgn_id') %>% # join with ao_port and ao_port_ref
    full_join(layers$data[['ao_port_ref']] %>%
                select(rgn_id, port_ref = count), by = 'rgn_id')

  # status
  status.all.years = D %>%
    group_by(rgn_id) %>%
    filter(!is.na(gas) & !is.na(fishermen)) %>% # NA prevents further calculations; 去掉NA，因为无法计算
    mutate(x.ao = max(0, min(1, (port/port_ref + fishermen/fishermen_ref + gas/gas_ref)/3))*100 )
  ## Q for CHN: only 2010-2013 have data in all three categories (port, fishermen, gas), and thus only those
  ## years have status scores. do you want to see score for 2014, using only gas and port data?
  ## 问题：只有2010-2013 有所有数据（port, fishermen, gas)， 所以只有这几年有现状得分。2014 只有gas 和port
  ## 数据，如果想得分，只能忽略port。可以吗？

  ## Ref point: ref points for each year in provided data. shouldn'd we use one ref point for each region
  ## (ie. highest number of fishermen in rgn 1 across all years)?
  ## 参考点问题： 目前每个变量每年都有个参考值（port_ref, fishermen_ref, gas_ref) 。
  ## 但在计算得分时，我们需要一个总的参考点?

  # current status: 2013
  r.status = status.all.years %>%
    filter(year == 2013) %>%
    mutate(goal = 'AO',
           dimension = 'status') %>%
    select(goal,
           dimension,
           region_id = rgn_id,
           score = x.ao)

  # trend calculation: 2010-2013
  r.trend = status.all.years %>%
    group_by(rgn_id) %>%
    do(dml = lm(x.ao ~ year, data =.)) %>%
    mutate(trend = coef(dml)[['year']]*3,  # 4 years, 3 intervals
           goal = "AO",
           dimension = "trend") %>%
    select(goal,
           dimension,
           region_id = rgn_id,
           score = trend)


  scores = rbind(r.status, r.trend)
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
  #    Please review the reference point and the variable 'tonnes_rel_capped';
  #    is this the behavior you expect? Should so many scores be 100?
  # 目前，参考点是产值平均值-请查对‘tonnes_rel_capped'.这是你们想要的结果吗？这样做的结果是很多地区得分是100

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

  # temporary libraries to load while testing （工具包）
#     library(dplyr)
#     library(tidyr)

  # identify and select layers 调出所需文件, 文件名字用 layers.csv 中设置的短名字
  lyrs = c('cs_condition',
           'cs_contribution',
           'cs_extent',
           'cs_extent_trend')
  D = SelectLayersData(layers, layers=lyrs); head(D); summary(D)
  # SelectLayerData 从数据层中选所需文件
  # head: 头6横行数据
  # summary: 总结

  # spread data so layers are columns
  rk = D %>% # %>% 是 dplyr 工具包中的一个operator. 方便连续使用多个命令，不必每次都写文件名字
    select(region_id = id_num,
           layer,
           habitat = category,
           val_num) %>%
    tidyr::spread(layer, val_num) %>%   #spread(key=variable to become the column headings, value=data) 展开
    dplyr::rename(contribution = cs_contribution, #另命名
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


  # limit to CS habitats (since only some habitats contribute to CS, but all are included in BD)
  rk = rk %>%
    filter(habitat %in% c('mangroves','saltmarshes','seagrasses')) # 过滤，只选想要的数据横行


  ## status model calculations 现状
  #  for each region, for each habitat, it's the (sum of extent*condition*contribution)/sum(extent)
  #  xCS = sum(ck           * Cc/Cr     * Ak) / At
  #  xCS = sum(contribution * condition * extent_per_habitat) / total_extent_all_habitats

  xCS = rk %>%
    mutate(c_c_a = contribution * condition * extent) %>%  # 另加 calculate for each region, each habitat
    group_by(region_id) %>%
    summarize(sum_c_c_a  = sum(c_c_a),          # summarize will act based on group_by; aggregate for each region
              total_extent = sum(extent)) %>%   # compare by substituting 'mutate' in place of 'summarize'; summarize
                                                # gives one aggregated sum_c_c_a to each region, while mutate would simply add
                                                # one new column and give one sum_c_c_a to each region and habitat
                                                # 摘要，和另加相似，但会聚集一组数据，结果指给一个
    ungroup() %>% #always a good practice to ungroup before next operation
    mutate(xCS_calc = sum_c_c_a/total_extent,
           score = min(1,xCS_calc) * 100); head(xCS) #score can't exceed 100

  # format to combine with other goals **variable must be called r.status with the proper formatting**
  # 一定要取名：r.status (和r.trend)
    r.status = xCS %>%
    mutate(goal      = 'CS',
           dimension = 'status') %>%
    select(goal, # 选择,只选需要的竖行
           dimension,
           region_id,
           score); head(r.status) #一定要是这个顺序

  # r.status formatting
  #   region_id goal dimension    score
  #            1   CS    status 68.25393
  #            2   CS    status 68.25393
  #            3   CS    status 68.25393


  # trend calculations 趋势计算
  trendCS = rk %>%
    group_by(region_id) %>%
    summarize(trend_raw = sum(extent * extent_trend) / sum(extent),
              score = max(min(trend_raw, 1), -1))

  # format to combine with other goals **variable must be called r.trend with the following formatting**
  r.trend = trendCS %>%
    mutate(goal      = 'CS',
           dimension = 'trend') %>%
    select(goal,
           dimension,
           region_id,
           score)

  # r.trend formatting
  #    region_id goal dimension     score
  #            1   CS     trend 0.8743067
  #            2   CS     trend 0.8743067
  #            3   CS     trend 0.8743067


  # return scores
  scores = rbind(r.status, r.trend) #合并所有横行
  return(scores)
}


CP = function(layers){

  # select data, combine cp_condition, cp_extent (chose the most rencent year b/c data are very sparse and scattered.
  # most habitats in each province has only 1 year of data, and few have up to 3), and cs_extent_trend for trend calculation b/c there were very few and uneven years
  # of data for each region.
  # 结合 cp_condition, cp_extent (最近年的面积)， cs_extent_trend

  m = layers$data[['cp_condition']] %>%
    select(rgn_id,
           habitat,
           condition=value) %>%
    full_join(layers$data[['cp_extent']] %>%
                group_by(rgn_id, habitat) %>%
                filter(year==max(year)) %>% #choose the most recent year's data
                select(-layer,
                       -year,
                       extent = hectare),
              by = c('rgn_id', 'habitat')) %>% #join by rgn_id, habitat
    full_join(layers$data[['cs_extent_trend']] %>%
                select(-layer,
                       trend = trend.score),
              by = c('rgn_id', 'habitat'))

  # add habitat weight
  habitat.wt = c('saltmarshes' = 3,
                 'mangroves' = 4,
                 'seagrasses' = 1,
                 'coral reef' = 4)
  m = m %>%
    mutate(weight = habitat.wt[habitat])

#     rgn_id     habitat condition    extent    trend weight
#   1      1 saltmarshes       0.5 1188600.0     -0.1      4
#   2      2 saltmarshes       0.5   81551.0     -0.1      4
#   3      3 saltmarshes       0.5   76840.0     -0.1      4
#   4      4 saltmarshes       0.5  721275.0     -0.1      4
#   5      5 saltmarshes       0.5  363979.0     -0.1      4
#   6      6 saltmarshes       0.5   18314.8 -79960.2      4


  # Current CP status
  # China model:
  # x = sum [ (Cc / Cr)  * (Wk / Wmax) * (Ak / Atotal) ]
  # x = sum [condition   * (Wk / Wmax) * (Extent_k / Total_extent))]

  r.status = m %>%
    group_by(rgn_id) %>%
    summarize(score = pmin(1, sum(condition* weight/4*extent/sum(extent)) ) * 100,
              dimension ='status',
              goal = 'CP') %>%
    rename(region_id = rgn_id) ; head(r.status)


# Trend
r.trend = m %>%
  filter(!habitat=='coral reef') %>% # coral reef has no trend score... NA throws off calculation, therefore removed now
  group_by(rgn_id) %>%
  summarize(trend_raw = sum(weight * extent * trend) / (sum(extent) * max(weight)),
            score = max(min(trend_raw, 1), -1),
            dimension = 'trend',
            goal = 'CP') %>%
  select(region_id = rgn_id,
         score,
         dimension,
         goal) ; head(r.trend)

#combine status and trend
scores_CP = rbind(r.status, r.trend)

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
  # Select data; calculate status score for each year in each region
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
           log = log10(tour_per_area_S_1),
           ref_point = max(log), #assume ref point is maximum log(tour_per_area_S_1)
                                 # 参考点使用 log(tour_per_area_S_1) 跨省最大值
           xTR = log/ref_point*100) %>%
    round(2); head(d); summary(d)

#     rgn_id year tourist    area tour_per_area tour_per_area_S tour_per_area_S_1  log ref_point   xTR
#   1      1 2008 6487.79 2000000       3243.89         2552.95           2553.95 3.41      6.76 50.38
#   2      1 2009 4223.38 2000000       2111.69         1661.90           1662.90 3.22      6.76 47.63
#   3      1 2010 5503.80 2000000       2751.90         2165.75           2166.75 3.34      6.76 49.33
#   4      1 2011 6775.49 2000000       3387.74         2666.16           2667.16 3.43      6.76 50.66

 # current TR status
r.status = d %>%
  filter(year == 2011) %>%
   mutate(goal = 'TR',
         dimension = 'status') %>%   #format
   select(region_id = rgn_id,
          goal,
          dimension,
          score = xTR); head(r.status)

 # Trend: 4 years of data, 3 intervals
r.trend = d %>%
  group_by(rgn_id) %>%
  do(dml = lm(xTR ~ year, data =.)) %>%
  summarize(region_id = rgn_id,
            dimension = 'trend',
            goal = 'TR',
            score = max(min(coef(dml)[['year']] * 3, 1), -1))

scores_TR = rbind(r.status, r.trend)
return(scores_TR)
}

LIV_ECO = function(layers, subgoal){

# select data

 jobs = layers$data[['le_livjob']] %>%
   select(rgn_id, sector = datalayer, jobs = value, year); head(jobs)

 wage = layers$data[['le_livwage']] %>%
   select(rgn_id, wage = value, year); head(wage)

 income = layers$data[['le_eco']] %>%
   select(rgn_id, income = value, year); head(income)

  # China model:
  # xLIV = ( sum(jobs) / sum(jobs_ref) + wage / wage_ref) /2
  # xECO = income / income_ref
  # xLE = (xLIV + xECO)/2

 #LIV status:

 # calculate job and wage score. find reference points: "from model description: the maximum quantity in each category has been
 # used as the reference point".

 # jobs multiplier placeholders were added (original multipliers are found in Table S10 from Halpern et al 2012 SOM)
 # all set to be 1 for now, to be updated in the future
 jobs_multiplier = c('beach_placer' = 1,
                      'tourism' = 1,
                      'egineering_arch' = 1,
                      'biomedicine' = 1,
                      'chemical' = 1,
                      'comm_transport' = 1,
                      'electric' = 1,
                      'fishing' = 1,
                      'ship_building' = 1,
                      'oil_gas' = 1,
                      'seasalt' = 1)

  jobs = jobs %>%
  mutate(multiplier = jobs_multiplier[sector],
         jobs_adj = jobs * multiplier)

 # calculate jobs, wages scores, and then status of all years
  jobs_score = jobs %>%
   group_by(sector) %>%
   mutate(jobs_ref = max(jobs_adj)) %>% #find reference point for each industry, across all regions and all years (2007-2011)
   ungroup() %>%
   group_by(rgn_id, year) %>%
   summarize(jobs_score = sum(jobs_adj)/sum(jobs_ref)); head(jobs_score)

 wage_score = wage %>%
 mutate(wage_ref = max(wage),
        wage_score = wage/wage_ref)

 xLIV_all_years = full_join(jobs_score, #calculate status scores for each year
                  select(wage_score, rgn_id, year, wage_score)) %>%
                  mutate(xLIV = (jobs_score + wage_score)/2*100 )
 # current status
 LIV.status = xLIV_all_years %>%
   filter(year == max(year)) %>%
   select(region_id = rgn_id,
          score = xLIV) %>%
   mutate(dimension = 'status',
          goal = 'LIV') ; head(LIV.status)

#      region_id    score dimension goal
#  1          1 46.94071    status  LIV
#  2          2 29.34375    status  LIV
#  3          3 47.84660    status  LIV

# LIV trend
# From SOM p. 29: trend was calculated as the slope in the individual sector values (not summed sectors)
# over the most recent five years...
# with the average weighted by the number of jobs in each sector
# ... averaging slopes across sectors weighted by the revenue in each sector

LIV.trend = left_join(jobs, wage) %>%
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
  filter(metric == 'jobs_adj' | metric == 'wage') %>%
  # get linear model coefficient per metric
  group_by(metric, rgn_id, sector, weight) %>%
  do(mdl = lm(value ~ year, data=.)) %>%
  summarize(
    metric = metric,
    weight = weight,
    rgn_id = rgn_id,
    sector = sector,
    sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 4))) %>% # 2007 - 2011: 4 intervals
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
  select(region_id = rgn_id,
         score,
         dimension,
         goal)

#    region_id score dimension goal
# 1          1  0.75     trend  LIV
# 2          2  0.75     trend  LIV
# 3          3  0.75     trend  LIV
# 4          4  0.75     trend  LIV
# 5          5  0.75     trend  LIV
# 6          6  0.75     trend  LIV
# 7          7  0.75     trend  LIV
# 8          8  0.75     trend  LIV
# 9          9  0.75     trend  LIV
# 10        10  0.75     trend  LIV
# 11        11  0.75     trend  LIV

# ECO status calculation
xECO_all_years = income %>%
  mutate (eco_ref = max(income),
          xECO = income/eco_ref*100); head(xECO_all_years)

ECO.status = xECO_all_years %>%
  filter(year == max(year)) %>%
  select(region_id = rgn_id,
         score = xECO) %>%
  mutate(dimension = 'status',
         goal = 'ECO')  %>%
  arrange(region_id) ; head(ECO.status)

#   region_id    score dimension goal
# 1         1 32.28161    status  ECO
# 2         2 15.43792    status  ECO
# 3         3 37.49262    status  ECO
# 4         4 85.39489    status  ECO

# ECO trend

ECO.trend = xECO_all_years %>%
  group_by(rgn_id) %>%
  do(lmd = lm(xECO ~ year, data =.)) %>%
  summarize(region_id = rgn_id,
            score = pmax(pmin(coef(lmd)[['year']] *4, 1) ,-1),
            dimension = 'trend',
            goal = 'ECO'); head(ECO.trend)

#    region_id score dimension goal
# 1         1     1     trend  ECO
# 2         2     1     trend  ECO
# 3         3     1     trend  ECO

scores_LIV_ECO = rbind(LIV.status, LIV.trend, ECO.status, ECO.trend)
return(scores_LIV_ECO)

}


LE = function(scores, layers){

## To replace 1128-1129 after the testing phase
#   scores_LE = scores %>%
#     filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
#     spread(goal, score)

 scores_LE = scores_LIV_ECO %>%
   spread(goal, score) %>%
   mutate(score = rowMeans(cbind(ECO, LIV, na.rm=T))) %>%
   select(region_id, score, dimension) %>%
   mutate(goal = 'LE')

 return(scores_LE)
}

ICO = function(layers){

  #cast data:
  d = layers$data[['ico_species']] %>%
    select(-layer) %>%
    rename(count = value)

  # lookup for weights status
  w.risk_category = c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)
  d = d %>%
    mutate(risk.wt = w.risk_category[category]) ; head(d)

#     rgn_id category count risk.wt
#   1      1       VU     6     0.8
#   2      1       NT     1     0.6
#   3      1       LC     5     0.4
#

  #CHN model: xICO = sum(Si * Wi)/sum(Si)
  #                = sum(count of species * weight.risk )/sum(count of species)

  r.status = d %>%
  mutate(count_wt = count * risk.wt) %>%
  group_by(rgn_id) %>%
  summarize(score = sum(count_wt)/sum(count)*100,
            dimension = 'status',
            goal = 'ICO') %>%
  select(region_id = rgn_id, #format
         score,
         dimension,
         goal)

#   region_id    score dimension goal
# 1          1 44.00000    status  ICO
# 2          2 48.57143    status  ICO
# 3          3 41.66667    status  ICO

# Trend: the same as SPP trend. Data from gl2014. only contains 9 species in 10 provinces.
# the other provinces will be given NA for now.
d2 = layers$data[['spp_iucn_trends']] %>%
  select(rgn_id, trend_score)

spp.trend = d2 %>%
  group_by(rgn_id) %>%
  summarize(score = mean(trend_score))

NA.trend = data.frame(rgn_id = '2', score = 'NA') ## assign NA to the rest of the provinces

r.trend = rbind(spp.trend, NA.trend) %>%
  arrange(rgn_id) %>%
  rename(region_id = rgn_id) %>%
  mutate(dimension = 'trend',
         goal = 'ICO')

scores_ICO = rbind(r.status, r.trend)
return(scores_ICO) }


LSP = function(layers, ref_pct_cmpa=30, ref_pct_cp=30, status_year, trend_years){

  # CHN model:
  # xLSP = %cmpa / reference%
  #      = (cmpa/total_marine_area) / 30%

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
    left_join(marinearea, by = 'rgn_id') %>% #head(d)
    mutate(reference = marinearea*0.05)%>% # ref is 5% of jurisdictional marine area
    mutate(pct_cmpa = cmpa/marinearea*100)%>%
    mutate(status = pmin(pct_cmpa/5 *100, 100))

 # Current status: year = 2012
  r.status = filter(d, year == 2012)%>%
   mutate(dimension = 'status',
          goal = "LSP") %>%
   select(region_id = rgn_id,
          score = status,
          dimension,
          goal) ; head(r.status)

#     region_id      score dimension goal
#  1          1   4.275067    status  LSP
#  2          2  30.852024    status  LSP
#  3          3   5.959905    status  LSP
#  4          4   4.595564    status  LSP
#  5          5   3.440244    status  LSP
#  6          6  31.273667    status  LSP

 #trend (2009 - 20112)
 r.trend = d %>%
   group_by(rgn_id) %>%
   do(dlm = lm(status ~ year, data=.)) %>%
   summarize(region_id = rgn_id,
             score = max(min(coef(dlm)[['year']]*3, 1) -1),
             dimension = 'trend',
             goal = 'LSP') ; head(r.trend) # all 0's

#     region_id score dimension goal
# 1          1     0     trend  LSP
# 2          2     0     trend  LSP
# 3          3     0     trend  LSP
# 4          4     0     trend  LSP
# 5          5     0     trend  LSP
# 6          6     0     trend  LSP

scores_LSP = rbind(r.status, r.trend)
return(scores_LSP)
}

SP = function(scores){

  scores = rbind(scores_ICO, scores_LSP) %>%
    spread(goal, score) %>%
    filter(!dimension %in% c('pressures', 'resilience')) %>%
    mutate(score = rowMeans(cbind(as.numeric(ICO), as.numeric(LSP))),
           goal = 'SP') %>%
    select(goal, dimension, region_id, score)

return(scores)
}


CW = function(layers){

  # cast data: 8/14: CHN manual showed 2010-2014 calculations. but I only have 2011 - 2013. Have emailed
  # Mian for more data.
 lyrs = c('cw_phosphate', 'cw_nitrogen', 'cw_cod', 'cw_oil')
 d = SelectLayersData(layers, layers = lyrs); head(d) ; summary(d)

 D = d%>%
   select(rgn_id = id_num,
          year,
          val_num,
          layer) %>%
   spread(layer, val_num) %>% #head(D)
   rename(phosphate = cw_phosphate,
          nitrogen = cw_nitrogen,
          cod = cw_cod,
          oil = cw_oil)

 # status
 # model = 4throot (mean(pollutant_scores))

 cw.status.all.years = D %>%
   group_by(rgn_id, year) %>%
   mutate(cw = (sum(phosphate, nitrogen, cod, oil)/4)^(1/4)) %>%
   ungroup %>%
   mutate(cw.ref = max(cw[year==max(year)])) %>% # reference point: not specified in OHI manual yet. choose the highest cw.score of all regions
                                                   # in most recent year as a ref point for now
   mutate(x.cw = cw/cw.ref*100)

 # current status
 r.status = cw.status.all.years %>%
   mutate(goal = 'CW',
          dimension = 'status') %>%
   filter(year == max(year)) %>%
   select(goal,
          dimension,
          region_id = rgn_id,
          score = x.cw); head(r.status)

  # trend
 r.trend = cw.status.all.years %>%
   group_by(rgn_id) %>%
   do(dml = lm(x.cw ~ year, data = .)) %>%
   summarize(region_id = rgn_id,
             trend = max(-1, min(1, coef(dml)[['year']]))) %>%
   mutate(goal = 'CW',
          dimension = 'trend') %>%
   select(goal,
          dimension,
          region_id,
          score = trend)

 scores = rbind(r.status, r.trend)




###################### gl 2014 #################################
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

  #cast data
  lyrs = c('cs_condition',
           'cs_extent',
           'cs_extent_trend')

  d = SelectLayersData(layers, layers = lyrs) %>%
    select(region_id = id_num,
           layer,
           habitat = category,
           val_num) %>%
    tidyr::spread(layer, val_num) %>%   #spread(key=variable to become the column headings, value=data)
    dplyr::rename(condition    = cs_condition,
                  extent       = cs_extent,
                  extent_trend = cs_extent_trend) ; head(d)

  # status = 1/3(Csg + Csm + Cmg)

  r.status = d %>%
    group_by(region_id) %>%
    summarize(score = mean(condition) * 100,
              dimension = 'status',
              goal = ' HAB') %>%
    select(region_id,
           score,
           dimension,
           goal) ; head(r.status)

#     region_id score dimension goal
#   1         1    80    status  HAB
#   2         2    80    status  HAB
#   3         3    80    status  HAB
#   4         4    80    status  HAB
#   5         5    80    status  HAB
#   6         6    80    status  HAB

  # trend = sum(extent * extent_trend) / sum(extent)
  r.trend = d %>%
    group_by(region_id) %>%
    summarize(trend_raw = sum(extent * extent_trend) / sum(extent),
              score = max(min(trend_raw, 1), -1) * 100,
              dimension = 'trend',
              goal = 'HAB') %>%
    select(region_id,
           score,
           dimension,
           goal) ; head(r.trend)

  #   region_id       score dimension goal
  # 1         1   -9.999159     trend  HAB
  # 2         2  -10.000000     trend  HAB
  # 3         3  -10.000000     trend  HAB

  scores_HAB = cbind(r.status, r.trend)
  return(scores_HAB)

  ############################## gl2014 model ##################################
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
  # cast data
  species = layers$data[['spp_species']] %>%
    select(rgn_id, risk.wt = value)

  ## iucn_trends created by NCEAS from global SPP trend data. But only 11 species in 6 provinces have trend score.
  ## used for now. will need updates later. See data_prep.r --> SPP for how to obtain the trend scores.
  trend = layers$data[['spp_iucn_trends']] %>%
    select(rgn_id, trend_score)

  # status = 1 - sum(risk.wt)/number of species = 1 - mean(risk.wt)
  r.status = species %>%
    group_by(rgn_id) %>%
    summarize(score = (1- mean(risk.wt)) *100) %>%
    rename(region_id = rgn_id) %>%
    mutate(dimension = 'status',
           goal = 'SPP')

  # Trend: the same as SPP trend. Data from gl2014. only contains 11 species in 6 provinces.
  # the other provinces will be given NA for now.

  spp.trend = trend %>%
    group_by(rgn_id) %>%
    summarize(score = mean(trend_score))

  NA.trend = data.frame(rgn_id = c(2, 6, 7, 9, 10), score = 'NA') ## assign NA to provinces without trend data

  r.trend = rbind(spp.trend, NA.trend) %>%
    arrange(rgn_id) %>%
    rename(region_id = rgn_id) %>%
    mutate(dimension = 'trend',
           goal = 'SPP')

  # combine status and trend scores
  scores_SPP = rbind(r.status, r.trend)
  return(scores_SPP)

  #################### gl2014 model ###############################
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
