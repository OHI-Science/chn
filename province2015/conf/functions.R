FIS = function(layers){

# cast data
  ft = layers$data[['fis_ft']] %>% # fishing efforts
    select(-layer,
           ft = kilowatt)

  mmsy = layers$data[['fis_mmsy']] %>% #maximum sustainable yield -> not used in calcuation b/c it was calculated in D2 (below)
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

  # delta_Ct:    0,              if mmsy_ref-Bt<0.05*mmsy_ref
  #              |mmsy_ref-Bt|,  if  |mmsy_ref-Bt|<mmsy_ref
  #              mmsy_ref,       otherwise.
  # xFIS = (1 - delta_Ct/mmsy_ref) * Tc

  # before calculating status, we need to obtain r, K, and q:
  # first, calcualte Ut = Ct/ft, and Ut+1
  # then, run a multiple linear regression in the form of:
  # Ut+1 - Ut - 1 = r - [r/(Kq)]*Ut - q*ft
  # obtain r, K, q from linear model coefficients

  ### To Mian: Real calculation of status and trend (40-108). But status results are all 0's, which screwed up trend calcualtion next.
  ### Intermediate steps (r, K, q, mmsy) have many unwanted negative numbers. my mmsy results are different from provided mmsy data (except for region 1 and 4)
  ### Checked with CHN team, the error is a result of the raw data. There might have been reporting errors.

  # calculate needed variables (r, q, K, mmsy) from linear model (independent variables: ut, ft)
  D1 = ft %>%
    left_join(ct, by = c("rgn_id", "year")) %>%
    mutate(ut = ct/ft) %>%
    group_by(rgn_id) %>%
    mutate(ut_plus1 = c(ut[-1], NA), # lag: start from year 1, last year 2013 set as NA.
           # Q: lost the most recent year's (2013) info. r, q, k, and mmsy can also be calculated up to 2012, and therefore status
           # will only be calculated up to 2012.
           y = ut_plus1/ut - 1) %>%
    filter(!year=='2013') %>%
    ungroup()

  D2 = D1 %>%
    group_by(rgn_id) %>%
    do(dlm = lm(y ~ ut + ft, data = .)) %>%
    # calculating r, q, and K from multiple linear regression
    mutate(r = coef(dlm)[["(Intercept)"]],
           r_Kq = -coef(dlm)[['ut']], # = r/Kq
           q = -coef(dlm)[['ft']],
           K = r/r_Kq/q,
           mmsy = r*K/4,
           mmsy_r = mmsy*0.75)  %>%
    select(-dlm) %>%
    ungroup(); head(D2); summary(D2)
  ### NOTE: take a look at D2, some mmsy values are negative.

  ## status:
  status.all.years = D2 %>%
    select(rgn_id, mmsy_r) %>%
    left_join(ct, by='rgn_id') %>%
    filter(!year == max(year)) %>% # take out most recent year. See reasoning in D1.
    mutate(abs = abs(mmsy_r - ct)) %>%
    group_by(rgn_id, year) %>%
    mutate(d_Ct = if (abs< 0.05*mmsy_r) {
      0
    } else if ( abs > 0.05*mmsy_r & abs < mmsy_r) {
      abs
    } else mmsy_r ) %>%
    ungroup() %>%
    left_join(tc, by = 'rgn_id') %>%
    mutate(x.fis = pmax(-1, pmin(1, (1-(d_Ct/mmsy_r))*tc)) *100)

  # # current status 最近一年现状 2012
  r.status = status.all.years %>%
    filter(year==max(year)) %>% # year = 2012
    mutate(goal = "FIS",
           dimension = 'status') %>%
    select(goal,
           dimension,
           region_id = rgn_id,
           score = x.fis)

  ###  trend
  ### 趋势计算
  r.trend = status.all.years %>%
    select(rgn_id, year, x.fis) %>%
    filter(year > (max(year)-5)) %>% # most recent 5 years of data
    group_by(rgn_id) %>%
    do(dml = lm(x.fis ~ year, data =.)) %>%
    mutate(trend = round(max(-1, min(1, coef(dml)[['year']] * 0.05)), 1),
           goal = 'FIS',
           dimension = 'trend') %>%
    select(goal,
           dimension,
           region_id = rgn_id,
           score = trend) %>%
    ungroup()

  scores_FIS = rbind(r.status, r.trend)
  return(scores_FIS)
}

MAR = function(layers){
  # CHN model: Yc = sum(harvest * msi) / area
  #        status = log10(Yc + 1)

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

  mar.status.all.years =
    D %>%
    filter(!area == 0) %>% #  exclude cases where no harvest and no allowable area (rgn_id 6)
    #  should be filter(!(area == 0 & harvest == 0))
    group_by(rgn_id, year) %>%
    summarize(yc = sum(harvest*msi/area),
              yc.log = log10(yc+1)) %>%
    ungroup() %>%
    # set reference point, to CHN's request on 10.16.2015: Guangdong, most recent year -> rgn 9, year 2013
    mutate(ref = yc.log[rgn_id == 9 & year == 2013],
           x.mar = pmax(0, pmin(1, yc.log/ref)) * 100)

  # current status
  r.status = mar.status.all.years %>%
    filter(year == 2012) %>% # choose 2012 to match status year of FIS
    mutate(score = round(x.mar,2)) %>%
    select(rgn_id,
           score) %>%
    rbind(data.frame(rgn_id = as.integer(6), #SH[6] has no MAR from 2007-2013, social preference to have no more mariculture. Add back as NA
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
    filter(year > (max(year)-5) & !rgn_id== 6) %>% #the most recent 5 years of data; ignore region 6 b/c no harvest in past 5 years
    do(dml = lm(x.mar ~ year, data=.)) %>% # lm 线性方程
    mutate(score = pmax(-1, pmin(1, coef(dml)[['year']]*0.05))) %>%
    select(region_id = rgn_id,
           score) %>%
    rbind(data.frame(region_id = as.integer(6), score = NA)) %>% # rgn 6 (SH), again doesn't have a trend as there is no more MAR
    mutate(dimension = 'trend',
           goal = 'MAR') %>%
    select(goal,
           dimension,
           region_id,
           score) %>%
    arrange(region_id) %>%
    ungroup()

  scores_MAR = rbind(r.status, r.trend)
  return(scores_MAR)
}

FP = function(layers, scores, debug=T){
  # CHN model: xFP = w * xFIS + (1-w) * xMAR
  #
  #            w = 1,                      if xMAR = No data
  #                0.5,                    if xFIS = 0.25 * Tc
  #                Ct/(Ct + sum(Yk)),      otherwise

# cast data needed for w calculation: Ct, Yk, Tc from FIS and MAR data layers and calculations
  mar_yk = layers$data[['mar_yk']] %>%
    group_by(rgn_id) %>%
    filter(year == 2012) %>%
    summarize(sum.yk = sum(tonnes)) %>%
    dplyr::rename(region_id = rgn_id) %>%
    ungroup()

  fis_ct = layers$data[['fis_ct']] %>%
    filter(year == 2012) %>%
    select(region_id = rgn_id,
           ct = tonnes)

  fis_Tc = layers$data[['fis_tc']] %>%
    select(region_id = rgn_id,
           Tc = score)

## status and trend years are uneven among goals, eg. FIS 2012, MAR 2013, which were used for FP, which combines these two goals.
## is it okay?
## 问题：FIS用2012， MAR2013。暂时用这个数据合并来计算FP得分。可以吗？

# combine fis and mar scores during testing individual goals
#   s = rbind(scores_FIS, scores_MAR) %>%
#     spread(goal, score)

### 计算现状
  s = scores %>%
    filter(goal %in% c('MAR', 'FIS'),
           !dimension %in% c('pressures','resilience')) %>%
    tidyr::spread(goal, score)




  # calcualte w
  w = s %>%
    filter(dimension == 'status') %>%
    left_join(mar_yk, by = 'region_id') %>%
    left_join(fis_ct, by = 'region_id') %>%
    left_join(fis_Tc, by = 'region_id') %>%
    mutate(w = ifelse (is.na(MAR), 1,
                       ifelse(FIS == 0.25 * fis_Tc, 0.5, ct/(ct+sum.yk)))) %>%
    select(region_id, w)

  scores_FP = s %>%
    full_join(w, by = 'region_id') %>%
    rowwise() %>%
    mutate(score = sum(w*FIS, (1-w)*MAR, na.rm=T),
           goal = 'FP') %>%
    select(goal,
           dimension,
           region_id,
           score)

  return(rbind(scores,scores_FP)) }



AO = function(layers){

  # status
  # xAO = (APc/APr + AFc/AFr ＋ AEi) / 3
  # AEi = Gr/Ir - Gc/Ic

  # cast data
  lyrs = c(#'ao_port',
           #'ao_port_ref',  # no year. couldn't join port data with the men and gas data properly
          'ao_men',         #2003-2013
          'ao_diesel',       #2010-2013
          'ao_income')       #2009-2013
  d = SelectLayersData(layers, layers=lyrs); head(d); summary(d)

  D = d %>%
    select(rgn_id = id_num,
           year,
           val_num,
           layer) %>%
    spread(layer, val_num) %>%
    select(rgn_id,
           year,
           diesel = ao_diesel,
           income = ao_income,
           fishermen = ao_men) %>%
    full_join(layers$data[['ao_port']] %>%
                select(rgn_id, port = count), by = 'rgn_id') %>% # join with ao_port
    filter(!is.na(rgn_id)) %>%
    mutate(ae_ref = max(diesel/income, na.rm = T),  # AE_reference point = maximum (diesel/income) across year and provinces
           ae = ae_ref - diesel/income, # calculate AEi
           fishermen_ref = max(fishermen), # new fishermen ref point, max across years. #CHN: 所有的参考点的取值，都取跨年的最高值，也就是历史的最高值
           port_ref = max(port))

  # status 2010 - 2013
  status.all.years = D %>%
    filter(!is.na(fishermen) & !is.na(diesel) & !is.na(income)) %>% # NA prevents further calculations; 去掉NA，因为无法计算
 #   rowwise %>%
    group_by(rgn_id, year) %>%
    mutate(x.ao = max(0, min(1, (port/port_ref + fishermen/fishermen + ae)/3))*100) %>%
    ungroup()
  ## Q for CHN: only 2010-2013 have data in all three categories (port, fishermen, gas), and thus only those
  ## years have status scores. do you want to see score for 2014, using only gas and port data?
  ## 问题：只有2010-2013 有所有数据（port, fishermen, gas)， 所以只有这几年有现状得分。2014 只有gas 和port
  ## 数据，如果想得分，只能忽略port。可以吗？

# Q for CHN team on Reference: ref points for each type of data are provided for
# each year in provided data (port: apr, fishermen: gas, aer). Fishermen and gas
# reference points are the highest number across all provinces in that year,
# which is a moving target. Generally we set the reference point to be the
# highest number across all years as an aspiration point for provinces to
# achieve （ie. the same ref number for all years)。
# 问题：目前每个变量每年都有个参考值（港口: apr, 渔民: afr, 油价：aer).
# 渔民和油价参考点是每年各个省份的最高值（每年都有个不同的参考值）。但在计算得分时，
# 我们通常需要一个总的参考点（每年都该是一样的)，比如跨年份的最高值，而不是每年的最高值。
# CHN answer: 所有的参考点的取值，都取跨年的最高值，也就是历史的最高值

  # current status: 2013
  r.status = status.all.years %>%
    group_by(rgn_id) %>%
    filter(year == max(year)) %>%
    mutate(goal = 'AO',
           dimension = 'status') %>%
    select(goal,
           dimension,
           region_id = rgn_id,
           score = x.ao) %>%
    ungroup()

  # trend calculation: 2010-2013
  r.trend = status.all.years %>%
    group_by(rgn_id) %>%
    do(dml = lm(x.ao ~ year, data =.)) %>%
    #mutate(trend = coef(dml)[['year']]*5)
    mutate(trend = max(-1, min(1, coef(dml)[['year']]*0.05)),
           goal = "AO",
           dimension = "trend") %>%
    select(goal,
           dimension,
           region_id = rgn_id,
           score = trend) %>%
    ungroup()


  scores_AO = rbind(r.status, r.trend)
  return(scores_AO)
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
  # did not receive np_harvest_relative; 没收到 Hp： “单个自然产品相对于所有产品总产值的权重” 的数据, 如下计算


  # Calculate sustainability (Sp)
  # sustainability (Sp) = 1 - mean(exposure + risk)

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
  # 没收到 Hp： “单个自然产品相对于所有产品总产值的权重” 的数据, 暂时如下计算。需改进：
  # relative harvest = tonnes / mean(tonnes) for each region-product
  np_harvest_rel <- np_harvest %>%
    select(-layer) %>%
    group_by(rgn_id, product) %>%
    mutate(tonnes_mean = mean(tonnes),
           tonnes_rel = tonnes / tonnes_mean) %>%
    rowwise() %>%
    mutate(tonnes_rel_capped = min(tonnes_rel, 1)) %>%
    ungroup(); head(np_harvest_rel)
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
  # xp = Hp * Sp = harvest_rel *

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
  ### model over the past 5 years inclusive (4 one-year intervals).
  ### Returns data frame with status and trend by region:
  ### [goal   dimension   region_id   score]
  #########################################.

  ### Calculate status, trends
  ### aggregate across products to rgn-year status, weighting with np_weight
  np_status_all = xp %>%
    left_join(np_weight,
              by = c('rgn_id', 'product')) %>%
    filter(!rgn_id == 6) %>% #rgn 6 has no harvest; if status  = NA, 1/0 produces NaN
    select(rgn_id, year, product, product_status, weight) %>%
    group_by(rgn_id, year) %>%
    summarize(status = weighted.mean(product_status, weight)) %>%
    ungroup()

  ### get current status
  r.status = np_status_all %>%
    filter(year == max(year) & !is.na(status)) %>%
    mutate(score = pmax(-1,pmin(1, round(status,4))) * 100) %>%
    select(rgn_id, score) %>%
    rbind(data.frame(rgn_id = as.integer(6), score = NA)) %>%
    arrange(rgn_id) %>%
    mutate(goal = 'NP',
           dimension = 'status') %>%
    select(goal, dimension, region_id = rgn_id, score)
  # good check to have in case the results exceeded 0-100 boundary
  stopifnot(
    min(r.status$score, na.rm = TRUE) >= 0,
    max(r.status$score, na.rm = TRUE) <= 100)

  ### trend based on 4 intervals (5 years of data)
  r.trend = np_status_all %>%
    filter(year > (max(year) - 5) & !is.na(status)) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(status ~ year, data=.)) %>%
    summarize(region_id = rgn_id,
              score = max(-1, min(1, coef(mdl)[['year']] * 0.05))) %>%
    rbind(data.frame(region_id = as.integer(6), score = NA)) %>%
    arrange(region_id) %>%
    mutate(goal = 'NP', dimension = 'trend') %>%
    select(goal, dimension, region_id, score) %>%
    ungroup()

  ### return scores
  scores_NP = rbind(r.status, r.trend)
  return(scores_NP)
}


CS = function(layers){

  # temporary libraries to load while testing （工具包）
#     library(dplyr)
#     library(tidyr)

  # identify and select layers 调出所需文件, 文件名字用 layers.csv 中设置的短名字
  lyrs = c('cs_condition',
           'cs_contribution',
           'cs_extent',
           'cs_extent_trend') # a file that NCEAS/Julie calculated and saved in layers folder （prep/4_CS/prep_cs.r)
                              # 我们计算并添加的数据层(看怎样计算：prep/4_CS/prep_cs.r)
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
  #  xCS = sum(ck           * Cc/Cr     *                 Ak / At)
  #      = sum(contribution * condition * extent_per_habitat / total_extent_all_habitats)

  xCS = rk %>%
    group_by(region_id) %>%
    mutate(total_extent = sum(extent),
           extent_ratio = extent/total_extent) %>%
    summarize(xCS = sum(contribution * condition * extent_ratio),
              score = pmax(-1, pmin(1, xCS))*100) %>%
    ungroup()


  # format to combine with other goals **variable must be called r.status with the proper formatting**
  # 一定要取名：r.status (和r.trend)
    r.status = xCS %>%
      select(region_id,
             score)  %>%
    mutate(goal      = 'CS',
           dimension = 'status'); head(r.status)


  # trend calculations 趋势计算
  trendCS = rk %>%
    group_by(region_id) %>%
    summarize(trend_raw = sum(extent * extent_trend) / sum(extent),
              score = max(min(trend_raw, 1), -1)) %>%
    ungroup()

  # format to combine with other goals **variable must be called r.trend with the following formatting**
  r.trend = trendCS %>%
    select(region_id,
           score) %>%
    mutate(goal      = 'CS',
           dimension = 'trend')

  # r.trend formatting
  #    region_id goal dimension     score
  #            1   CS     trend 0.8743067
  #            2   CS     trend 0.8743067
  #            3   CS     trend 0.8743067


  # return scores
  scores_CS = rbind(r.status, r.trend) #合并所有横行
  return(scores_CS)
}


CP = function(layers){

  # select data, combine cp_condition, cp_extent (chose the most rencent year b/c data are very sparse and scattered.
  # most habitats in each province has only 1 year of data, and few have up to 3), and cs_extent_trend for trend calculation b/c there were very few and uneven years
  # of data for each region.
  # Question for CHN team: cp_conndition, cp_extent include Coral Reef data in region 11, but cs_extent_trend does not. How to reconcile?
  # For now, in trend calculation, region 11 coral reef is just ignored.

  # 结合 cp_condition, cp_extent (选最近年的面积, 因为数据很少，大部分生境只有1年数据，少部分有2-3年)， cs_extent_trend （碳汇趋势数据）
  # 问题： cp_conndition, cp_extent 有新加 Coral Reef 数据在 region 11, 但 cs_extent_trend 没有。暂时在计算趋势时，省略了 region 11 的
  # Coral Reef. 中国团队想怎样处理？

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

# Status year different for each habitat and province, which is not explicit in the status scores.
# See description above previous question.
# 问题： 现状计算，每个生境和省份年份都不同。原因看上个问题之上的数据描述。

  r.status = m %>%
    group_by(rgn_id) %>%
    summarize(score = pmax(-1, pmin(1, sum(condition* weight/4*extent/sum(extent)) )) * 100,
              dimension ='status',
              goal = 'CP') %>%
    select(goal, dimension, region_id = rgn_id, score)%>%
    ungroup(); head(r.status)


# Trend
r.trend = m %>%
  filter(!habitat=='coral reef') %>% # coral reef has no trend score... NA throws off calculation, therefore removed now
  group_by(rgn_id) %>%
  summarize(trend_raw = sum(weight * extent * trend) / (sum(extent) * max(weight)),
            score = max(min(trend_raw, 1), -1),
            dimension = 'trend',
            goal = 'CP') %>%
  select(goal, dimension, region_id = rgn_id,
         score) %>%
  ungroup() ; head(r.trend)

#combine status and trend
scores_CP = rbind(r.status, r.trend)

return(scores_CP)
}


TR = function(layers, year_max, debug = FALSE, pct_ref=90){

  ## China model:
  # X =  log [(At/Vt * S) + 1]

  # At = number of tourists in year t (million)
  # Vt = coastline length (km)
  # S = 0.787, sustainability coefficient

  #library(dplyr)
  #library(tidyr)
  # Select data; calculate status score for each year in each region

  d = layers$data[['tr_tourist']] %>%
    left_join(layers$data[["tr_coastline"]], by="rgn_id") %>%
    select(rgn_id = as.integer(rgn_id),
           year,
           tourist = million,
           coastline = km) %>%
    left_join(select(layers$data[["tr_sustainability"]], - layer, S = value), by = "rgn_id") %>%
    mutate(tour_per_km = tourist*1000000/coastline,
           tour_per_km_S = tour_per_km * S,
           tour_per_km_S_1 = tour_per_km_S +1,
           log = log(tour_per_km_S_1),
           ref_point = max(log), #assume ref point is maximum log(tour_per_area_S_1) （2010 SH）
                                 # 参考点使用 log(tour_per_area_S_1) 跨省最大值 （2010 上海）
           xTR = log/ref_point*100); head(d); summary(d)

 # current TR status
r.status = d %>%
  filter(year == 2015) %>%
   mutate(goal = 'TR',
         dimension = 'status') %>%   #format
   select(goal,
          dimension,
          region_id = as.integer(rgn_id),
          score = xTR); head(r.status)

 # Trend: 2 years of data
r.trend = d %>%
  group_by(rgn_id) %>%
  do(dml = lm(xTR ~ year, data =.)) %>%
  summarize(goal = 'TR',
            dimension = 'trend',
            region_id = as.integer(rgn_id),
            score = max(min(coef(dml)[['year']] *0.05, 1), -1)) %>%
  ungroup

scores_TR = rbind(r.status, r.trend)
return(scores_TR)
}

LIV = function(layers){

  # CHN model:
  # x.LIV = ( sum(jobs_c)/sum(jobs_ref) + wage_c/wage_r )/2

# select data

#  jobs = layers$data[['le_livjob']] %>%
#    select(rgn_id, sector = datalayer, jobs = value, year); head(jobs)

 jobs_industry = layers$data[['liv_job_industry']] %>%
   select(industry = category,
          year,
          jobs_ind = value)

 jobs_province = layers$data[['liv_job_province']] %>%
   select(rgn_id,
          year,
          jobs_prov = value)

 wage_urban = layers$data[['liv_wage_urban']] %>%
   select(rgn_id,
          year,
          wage_urban = value)

 wage_rural = layers$data[['liv_wage_rural']] %>%
   select(rgn_id,
          year,
          wage_rural = value)

#testing:
 wage.lyr = SelectLayersData(layers, c('le_livwagetown_chn2015_zb.csv', 'le_livewagevillage_chn2015_zb.csv'))

 #LIV status:

 # calcualte jobs in each industry in each province:
 # j = N *(x/sum(x)) = jobs_industry * (jobs_province / jobs_all_provinces)

 jobs = jobs_province %>%
   group_by(year) %>%
   mutate(jobs_all_provinces = sum(jobs_prov),
          proportion = jobs_prov/jobs_all_provinces) %>%
   ungroup %>%
   full_join(jobs_industry, by = 'year') %>%
   mutate(jobs = jobs_ind * proportion) %>%
   select(rgn_id,
          year,
          industry,
          jobs) %>%
   filter(!is.na(year))



 # calculate job and wage score. find reference points: "from model description: the maximum quantity in each category has been
 # used as the reference point".

 # jobs multiplier placeholders were added (original multipliers are found in Table S10 from Halpern et al 2012 SOM)
 # all set to be 1, so that it doesn't affect the results. To be updated in the future
 jobs_multiplier = data.frame(
   industry  = c('mining', 'tourism', 'egineering_arch', 'biomedicine', 'chemical', 'comm_transport', 'electric', 'fishing', 'ship_building', 'oil_gas', 'seasalt'),
   multiplier =c(1,         1,         1,                 1,             1,          1,                1,          1,         1,               1,         1))

  jobs = jobs %>%
    left_join(jobs_multiplier, by = c('industry')) %>%
    mutate(jobs_adj = jobs * multiplier) %>%
    group_by(industry) %>%
    mutate(jobs_ref = max(jobs_adj)) %>%    # find reference point for each industry, across all regions and all years (2007-2013)。 no info on
                                            # on coasta line length and therefore couldn't calculate max quantity per unit coast line
                                            # 每个行业的参考点为跨省跨年度的最大值 （2007-2013）
                                            # 取总量最大值为参考点，没有海岸线长度资料，无法计算单位海岸线最大值。
    ungroup

 # status: calculate jobs, wages scores, and status of all years
  # Jobs score
  jobs_score = jobs %>%
   group_by(rgn_id, year) %>%
   summarize(jobs_score = sum(jobs_adj)/sum(jobs_ref)) %>%
    ungroup(); head(jobs_score)

  # Wage score
 wage = left_join(wage_urban, wage_rural, by = c('rgn_id', 'year')) %>%
    mutate(wage = (wage_urban + wage_rural)/2)

 wage_score = wage %>%
 mutate(wage_ref = max(wage), # reference point: max wage across all regions, all years; no info on
                              # on coasta line length and therefore couldn't calculate max quantity per unit coast line
                              # 取总量最大值为参考点，没有海岸线长度资料，无法计算单位海岸线最大值。
        wage_score = wage/wage_ref)

 # status all years
 xLIV_all_years = full_join(jobs_score, #calculate status scores for each year
                  select(wage_score, rgn_id, year, wage_score), by = c('rgn_id','year')) %>%
                  mutate(xLIV = (jobs_score + wage_score)/2*100 )

 # current status
 r.status = xLIV_all_years %>%
   filter(year == max(year)) %>% #2011
   mutate(dimension = 'status',
          goal = 'LIV') %>%
   select(goal,
          dimension,
          region_id = rgn_id,
          score = xLIV); head(r.status)

# LIV trend
# From SOM p. 29: trend was calculated as the slope in the individual sector/industry values (not summed sectors)
# over the most recent five years...
# with the average weighted by the number of jobs in each sector
# ... averaging slopes across sectors weighted by the revenue in each sector

r.trend = left_join(jobs, wage, by=c('rgn_id', 'year')) %>%
  # get sector weight as total jobs across years for given region
  arrange(rgn_id, year, industry) %>%
  group_by(rgn_id, industry) %>%
  mutate(weight = sum(jobs_adj, na.rm=T)) %>%
  # reshape into jobs and wages columns into single metric to get slope of both with one do() call
  reshape2::melt(id=c('rgn_id','year','industry','weight'), variable='metric', value.name='value') %>%
  mutate(
    industry = as.character(industry),
    metric = as.character(metric)) %>%
  filter(metric == 'jobs_adj' | metric == 'wage') %>%
  # get linear model coefficient per metric
  group_by(metric, rgn_id, industry, weight) %>%
  do(mdl = lm(value ~ year, data=.)) %>%
  summarize(
    metric = metric,
    weight = weight,
    rgn_id = rgn_id,
    industry = industry,
    industry_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 0.05))) %>%
  arrange(rgn_id, metric, industry) %>%
  # get weighted mean across sectors per region-metric
  group_by(metric, rgn_id) %>%
  summarize(
    metric_trend = weighted.mean(industry_trend, weight, na.rm=T)) %>%
  # get mean trend across metrics (jobs, wages) per region
  group_by(rgn_id) %>%
  summarize(
    score = mean(metric_trend, na.rm=T)) %>%
  # format
  mutate(
    goal      = 'LIV',
    dimension = 'trend') %>%
  select(goal, dimension, region_id = rgn_id, score) %>%
  ungroup(); head(r.trend)

scores_LIV = rbind(r.status, r.trend)
return(scores_LIV)
}

ECO = function(layers){
# xECO = income / income_ref

# cast data
income = layers$data[['eco_eco']] %>%
  select(rgn_id, income = value, year); head(income)

# ECO status calculation
xECO_all_years = income %>%
  mutate (eco_ref = max(income),
          xECO = income/eco_ref*100); head(xECO_all_years)

r.status = xECO_all_years %>%
  filter(year == max(year)) %>%
  mutate(dimension = 'status',
         goal = 'ECO')  %>%
  select(goal,
         dimension,
         region_id = rgn_id,
         score = xECO) %>%
  arrange(region_id) ; head(r.status)

# ECO trend

r.trend = xECO_all_years %>%
  group_by(rgn_id) %>%
  do(lmd = lm(xECO ~ year, data =.)) %>%
  summarize(region_id = rgn_id,
            score = pmax(pmin(coef(lmd)[['year']] * 0.05, 1) ,-1),
            dimension = 'trend',
            goal = 'ECO') %>%
  select(goal, dimension, region_id, score)%>%
  ungroup(); head(r.trend)

scores_ECO = rbind(r.status, r.trend)
return(scores_ECO)
}


LE = function(scores, layers){

  # xLE = (xLIV + xECO)/2

  #  During testing-individual-goal phase, run this line instead of the first two lines of code:
  # 在单独查看LE目标时，用这个line 代替第一，二行程序
  # scores_LE = rbind(scores_LIV, scores_ECO) %>%

  scores_LE = scores %>%
    filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
    spread(goal, score) %>%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm=TRUE)) %>%
    mutate(goal = 'LE') %>%
   select(goal, dimension, region_id, score)


 return(rbind(scores, scores_LE))
}

ICO = function(layers){

  #cast data:
  d = layers$data[['ico_species']] %>%
    select(rgn_id,
           category,
           count = value)

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

  #CHN model: xICO = 1 - sum(Si * Wi)/sum(Si)
  #                = 1 - sum(count of species * weight.risk )/sum(count of species)

  r.status = d %>%
  mutate(count_wt = count * risk.wt) %>%
  group_by(rgn_id) %>%
  summarize(score = (1 - sum(count_wt)/sum(count)) * 100,
            dimension = 'status',
            goal = 'ICO') %>%
  select( goal, dimension, region_id = rgn_id, score) %>%
    ungroup()

#   goal dimension region_id    score
#   (chr)     (chr)     (int)    (dbl)
#   1   ICO    status         1 54.73684
#   2   ICO    status         2 56.66667
#   3   ICO    status         3 53.33333

# Trend: the same as SPP trend. Data from gl2014. only contains 10 species in 10 provinces.

# 加入了我们根据2014全球SPP趋势计算的 spp_iucn_trend。 只有10个省份，9个物种有趋势值。region 2 暂时设为NA。
# 在 province2015/prep/data_prep.r/SPP 中查看我们怎样从全球评估中取出中国所需的值。 －－》需要和goal keeper 讲

d2 = layers$data[['spp_iucn_trends']] %>%
  select(rgn_id, trend_score)

spp.trend = d2 %>%
  group_by(rgn_id) %>%
  summarize(score = mean(trend_score)) %>%
  ungroup()

NA.trend = data.frame(rgn_id = as.integer(2), score = NA) ## assign NA to the rest of the provinces

r.trend = rbind(spp.trend, NA.trend) %>%
  mutate(dimension = 'trend',
         goal = 'ICO') %>%
  select(goal,
         dimension,
         region_id = rgn_id,
         score) %>%
  arrange(region_id)

scores_ICO = rbind(r.status, r.trend)
return(scores_ICO)

}


LSP = function(layers){

  # CHN model:
  # xLSP = %cmpa * St
  #      = (cmpa/total_marine_water_area) / cultural_impact_factor

  # cast data ----
  cmpa = SelectLayersData(layers, layers='lsp_cmpa') %>% #2009 - 2012
  select(rgn_id = id_num,
         year,
         cmpa = val_num)

  marine_area = SelectLayersData(layers, layers='lsp_marinearea') %>%
    select(rgn_id = id_num,
           area = val_num)

  cul_factor = SelectLayersData(layers, layers='lsp_cultural_impact') %>%
    select(rgn_id = id_num,
           cul_value = val_num)

  # Calculate status of each year in each province
  status.all.years = cmpa %>%
    left_join(marine_area, by = 'rgn_id') %>% #head(d)
    left_join(cul_factor, by = 'rgn_id') %>%
    mutate(pct_cmpa = cmpa/area*cul_value,
           ref_point = max(pct_cmpa),
          status = pmin(pct_cmpa/ref_point *100, 100))

 # Current status: year = 2012
  r.status = filter(status.all.years, year == max(year))%>%
   mutate(dimension = 'status',
          goal = "LSP") %>%
   select(goal, dimension, region_id = rgn_id, score = status) ; head(r.status)

 #trend (2009 - 20112)
 r.trend = status.all.years %>%
   group_by(rgn_id) %>%
   do(dlm = lm(status ~ year, data=.)) %>%
   summarize( goal = 'LSP',
              dimension = 'trend',
              region_id = rgn_id,
             score = max(min(coef(dlm)[['year']]*0.05, 1) -1)) %>%
   ungroup() ; head(r.trend)

scores_LSP = rbind(r.status, r.trend)
return(scores_LSP)
}

SP = function(scores){
  #  During testing-individual-goal phase, run this line instead of the first two lines of code:
  # 在单独查看LE目标时，用这个line 代替第一，二行程序
  # scores_SP = rbind(scores_ICO, scores_LSP) %>%

  scores_SP = scores %>%
        filter(goal %in% c('ICO','LSP') & dimension %in% c('status','trend','score','future')) %>%
    spread(goal, score) %>%
    mutate(score = rowMeans(cbind(as.numeric(ICO), as.numeric(LSP)), na.rm=TRUE),
           goal = 'SP') %>%
    select(goal, dimension, region_id, score); head(scores_SP)

return(rbind(scores, scores_SP))

}


CW = function(layers){

  # cast data: 2010-2014

 lyrs = c('cw_phosphate', 'cw_nitrogen', 'cw_cod', 'cw_oil')
 d = SelectLayersData(layers, layers = lyrs); head(d) ; summary(d)

 D = d %>%
   select(rgn_id = id_num,
          year,
          val_num,
          layer) %>%
   spread(layer, val_num) %>% #head(D)
   select(rgn_id,
          year,
          phosphate = cw_phosphate,
          nitrogen = cw_nitrogen,
          cod = cw_cod,
          oil = cw_oil)

 # status
 # model = 4th.root (phosphate*nitrogen*cod*oil)

 cw.status.all.years = D %>%
   group_by(rgn_id, year) %>%
   mutate(x.cw = max(-1, (min(1, phosphate*nitrogen*cod*oil)^(1/4)))*100) %>%
   ungroup


 # current status
 r.status = cw.status.all.years %>%
   mutate(goal = 'CW',
          dimension = 'status') %>%
   group_by(rgn_id) %>%
   filter(year == max(year)) %>%
   select(goal,
          dimension,
          region_id = rgn_id,
          score = x.cw) %>%
   ungroup()

  # trend
 r.trend = cw.status.all.years %>%
   group_by(rgn_id) %>%
   do(dml = lm(x.cw ~ year, data = .)) %>%
   summarize(region_id = rgn_id,
             trend = max(-1, min(1, coef(dml)[['year']]*0.05))) %>%
   mutate(goal = 'CW',
          dimension = 'trend') %>%
   select(goal,
          dimension,
          region_id,
          score = trend) %>%
   ungroup()

 scores_CW = rbind(r.status, r.trend)
 return(scores_CW)
}


HAB = function(layers){

#   #cast data

  # select data, combine cp_condition, cp_extent (chose the most rencent year b/c data are very sparse and scattered.
  # most habitats in each province has only 1 year of data, and few have up to 3), and cs_extent_trend for trend calculation b/c there were very few and uneven years
  # of data for each region.
  # Question for CHN team: cp_conndition, cp_extent include Coral Reef data in region 11, but cs_extent_trend does not. How to reconcile?
  # For now, in trend calculation, region 11 coral reef is just ignored.

  # 结合 cp_condition, cp_extent (选最近年的面积, 因为数据很少，大部分生境只有1年数据，少部分有2-3年)， cs_extent_trend （碳汇趋势数据）
  # 问题： cp_conndition, cp_extent 有新加 Coral Reef 数据在 region 11, 但 cs_extent_trend 没有。暂时在计算趋势时，省略了 region 11 的
  # Coral Reef. 中国团队想怎样处理？

  d = layers$data[['cp_condition']] %>%
    select(rgn_id,
           habitat,
           condition=value) %>%
    full_join(layers$data[['cp_extent']] %>%
                group_by(rgn_id, habitat) %>%
                filter(year==max(year)) %>% #choose the most recent year's data
                select(-layer,
                       -year,
                       extent = hectare),
              by = c('rgn_id', 'habitat')) %>%
    arrange(rgn_id) %>% #join by rgn_id, habitat
    right_join(layers$data[['cs_extent_trend']] %>%
                select(-layer,
                       trend = trend.score),
              by = c('rgn_id', 'habitat'))

  # status = 1/3(Csg + Csm + Cmg)
  # Status year different for each habitat and province, which is not explicit in the status scores.
  # See description above previous question.
  # 问题： 现状计算，每个生境和省份年份都不同。原因看上个问题之上的数据描述。
  r.status = d %>%
    group_by(rgn_id) %>%
    summarize(score = mean(condition) * 100,
              dimension = 'status',
              goal = 'HAB') %>%
    select(goal,
           dimension,
           region_id = rgn_id,
           score) %>%
    ungroup(); head(r.status)

  # trend = sum(extent * extent_trend) / sum(extent)
  r.trend = d %>%
    filter(!habitat == 'coral reef') %>%
    group_by(rgn_id) %>%
    summarize(trend_raw = sum(extent * trend) / sum(extent),
              score = max(min(trend_raw, 1), -1),
              dimension = 'trend',
              goal = 'HAB') %>%
    select(goal,
           dimension,
           region_id = rgn_id,
           score) %>%
    ungroup(); head(r.trend)

  #   region_id       score dimension goal
  # 1         1   -9.999159     trend  HAB
  # 2         2  -10.000000     trend  HAB
  # 3         3  -10.000000     trend  HAB

  scores_HAB = rbind(r.status, r.trend)
  return(scores_HAB)
}


SPP = function(layers){
  # cast data
  species = layers$data[['spp_species']] %>%
    select(rgn_id, risk.wt = value)

  ## iucn_trends created by NCEAS from global SPP trend data. But only 9 species in 10 provinces have trend score.
  ## used for now. will need updates later. See data_prep.r --> SPP for how to obtain the trend scores.
  ## 加入了我们根据2014全球SPP趋势计算的 spp_iucn_trend。 只有10个省份，9个物种有趋势值。region 2 暂时设为NA。
  ## 在 province2015/prep/data_prep.r/SPP 中查看我们怎样从全球评估中取出中国所需的值。 －－》需要和goal keeper 讲

  trend.data = layers$data[['spp_iucn_trends']] %>%
    select(rgn_id, trend_score)

  # status = 1 - sum(risk.wt)/number of species = 1 - mean(risk.wt)
  r.status = species %>%
    group_by(rgn_id) %>%
    summarize(score = (1- mean(risk.wt)) *100,
              dimension = 'status',
              goal = 'SPP') %>%
   select(goal, dimension, region_id = rgn_id, score) %>%
    ungroup()

  # Trend: the same as SPP trend. Data from gl2014
  # region 2 will be given NA for now.

  spp.trend = trend.data %>%
    group_by(rgn_id) %>%
    summarize(score = mean(trend_score)) %>%
    ungroup()

  NA.trend = data.frame(rgn_id = as.integer(2), score = NA) ## assign NA to province without trend data

  r.trend = rbind(spp.trend, NA.trend) %>%
    arrange(rgn_id) %>%
    mutate(dimension = 'trend',
           goal = 'SPP') %>%
    select(goal, dimension, region_id = rgn_id, score)


  # combine status and trend scores
  scores_SPP = rbind(r.status, r.trend)
  return(scores_SPP)
}

BD = function(scores){
  #  During testing-individual-goal phase, run this line instead of the first two lines of code:
  # 在单独查看LE目标时，用这个line 代替第一,二行程序
  # scores_BD = rbind(scores_HAB, scores_SPP) %>%

  scores_BD = scores %>%
      filter(goal %in% c('HAB','SPP') & dimension %in% c('status','trend','score','future')) %>%
    spread(goal, score) %>%
    mutate(score = rowMeans(cbind(as.numeric(HAB), as.numeric(SPP)), na.rm=TRUE),
           goal = 'BD') %>%
    select(goal,
           dimension,
           region_id,
           score)

  return(rbind(scores, scores_BD))

}

##### during testing phase only.
##### Combining all the scores into one data frame for CHN team for reference
# CHN.scores = rbind(scores_AO, scores_BD, scores_CP, scores_CS, scores_CW, scores_FIS, scores_FP,
#                    scores_HAB, scores_ICO, scores_LE, scores_LIV_ECO, scores_LSP,
#                    scores_MAR, scores_NP, scores_SP, scores_TR)
# library(readr) # contains write_csv function
# dir_layers = '~/github/chn/province2015/tmp' #save results to temporary folder
# write_csv(CHN.scores, file.path(dir_layers, 'china.final.scores.temp.csv')) # saved on 8.21.2015

# combined goals only:
# comb.scores = rbind(scores_FP, scores_LE, scores_SP, scores_BD)
# dir_layers = '~/github/chn/province2015/tmp'
# write_csv(comb.scores, file.path(dir_layers, 'comb.scores.csv'))
#########

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
