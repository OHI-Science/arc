library(sf) #install.packages("sf")
library(tidyverse)
library(datalimited)
install.packages("devtools")
devtools::install_github("datalimited/datalimited")
################# Load Catch Data###########

catch<- read.csv('circle2016/prep/FIS/reg/noba/spatial_catch_prebbmsy_fmsy1.csv')%>%
  rename(common = Common_Name)

fis_dir<- 'circle2016/prep/FIS'
####Catch MSY#####

cmsy_fits <- plyr::dlply(catch, c("stock_id", "common"), function(x) {

  #make sure the data is ordered from 1950 to 2014
  x <- arrange(x,year)
  out <- cmsy(ct = x$tons, yr = x$year,  start_r = resilience(x$Resilience[1]),
              reps = 2e4)
  out$year <- x$year
  out
}, .parallel = TRUE)
saveRDS(cmsy_fits, file = file.path(fis_dir,"reg/noba/catch_model_bmsy_noba/cmsy-fits.rds"))
fake_data <- data.frame(bbmsy_q2.5 = NA, bbmsy_q25 = NA, bbmsy_q50 = NA,
                        bbmsy_q75 = NA, bbmsy_q97.5 = NA)

cmsy_bbmsy <- plyr::ldply(cmsy_fits, function(x) {
  bbmsy_cmsy <- x$biomass[, -1] / x$bmsy
  bbmsy_out <- tryCatch({
    bbmsy_out <- summarize_bbmsy(bbmsy_cmsy)
    bbmsy_out$year <- x$year
    bbmsy_out}, error = function(e) fake_data)
})
cmsy_bbmsy$model <- "CMSY"
<<<<<<< HEAD
write.csv(cmsy_bbmsy, "circle2016/prep/FIS/reg/catch_model_bmsy_reg/cmsy_bbmsy_reg.csv", row.names=FALSE)
=======
write.csv(cmsy_bbmsy, "circle2016/prep/FIS/reg/noba/catch_model_bmsy_noba/cmsy_bbmsy_noba.csv", row.names=FALSE)
>>>>>>> 0a274ebb0905145c89a89fad84fe28856d4f05ae

###Format CMSY data for toolbox######
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)

<<<<<<< HEAD
cmsy <- read.csv('circle2016/prep/FIS/reg/catch_model_bmsy_reg/cmsy_bbmsy_reg.csv') %>%
=======
cmsy <- read.csv('circle2016/prep/FIS/reg/noba/catch_model_bmsy_noba/cmsy_bbmsy_noba.csv') %>%
>>>>>>> 0a274ebb0905145c89a89fad84fe28856d4f05ae
  mutate(prior = 'constrained') %>%
  filter(!is.na(bbmsy_mean))

#comsir <- read.csv('prep/FIS/catch_model_bmsy/comsir_bbmsy.csv') %>%
#mutate(prior = 'NA') %>%
#filter(!is.na(bbmsy_mean))

new_b_bmsy <- function(b_bmsy=constrained, method = "cmsy"){
  b_bmsy <- b_bmsy %>%
    dplyr::select(stock_id, year, bbmsy_mean, prior, model) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
<<<<<<< HEAD
    mutate(mean_5year = rollmean(bbmsy_mean, 5, align="right", fill=NA))
  write.csv(b_bmsy, sprintf('circle2016/prep/FIS/noba/meanbmsy/%s_b_bmsy_%s_mean5yrs_reg.csv', method, unique(b_bmsy$prior)), row.names=FALSE)
}

new_b_bmsy(cmsy, method="cmsy")
=======
    mutate(mean_5year = zoo::rollmean(bbmsy_mean, 5, align="right", fill=NA))
  write.csv(b_bmsy, sprintf('circle2016/prep/FIS/reg/noba/meanbmsy/%s_b_bmsy_%s_mean5yrs_noba.csv', method, unique(b_bmsy$prior)), row.names=FALSE)
}

new_b_bmsy(cmsy, method="cmsy")

###### Final formatting

cmsy <- read.csv('circle2016/prep/FIS/reg/noba/meanbmsy/cmsy_b_bmsy_constrained_mean5yrs_noba.csv') %>%
  dplyr::select(stock_id, year, cmsy_bbmsy=mean_5year)


#comsir <- read.csv('prep/FIS/reg/noba/meanbmsy/comsir_b_bmsy_NA_mean5yrs.csv') %>%
#dplyr::select(stock_id, year, comsir_bbmsy=mean_5year)

## Mean catch data created in "meanCatch.R"
mean_catch <- read.csv("circle2016/prep/FIS/reg/noba/fmsy1_meancatch.csv") %>%
  mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
  mutate(taxon_key = str_sub(stock_id_taxonkey, -6, -1)) %>%
  mutate(stock_id = substr(stock_id_taxonkey, 1, nchar(stock_id_taxonkey)-7))

## combine data
setdiff(cmsy$stock_id, mean_catch$stock_id)
setdiff(mean_catch$stock_id, cmsy$stock_id)
intersect(mean_catch$stock_id, cmsy$stock_id) #946
mean_catch<- rename(mean_catch, year=Year)
mean_catch<- rename(mean_catch, mean_catch=meancatch)



###CMSY join with mean catch
data <- mean_catch %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  left_join(cmsy, by=c("stock_id", "year")) %>%
  ungroup()%>%
  mutate(bbmsy = cmsy_bbmsy) %>%
  dplyr::select(rgn_id, stock_id, taxon_key, year, bbmsy, mean_catch) %>%
  #filter(year >= 2001) %>%
  unique()%>%
  dplyr::select(rgn_id, stock_id, year, bbmsy) %>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(data, file='circle2016/prep/FIS/reg/noba/fis_cmsy_bbmsy_noRAM_noba.csv', row.names=FALSE)




#### TESTING OUT SCCORES########
FIS = function(layers, status_year){

  #catch data
  c<- read.csv('circle2016/prep/FIS/reg/noba/fmsy1_meancatch.csv') %>%
    dplyr::select(
      rgn_id,
      stock_id_taxonkey,
      year,
      catch          = mean_catch)
  # b_bmsy data
  b<- read.csv('circle2016/prep/FIS/reg/noba/fis_cmsy_bbmsy_noRAM_noba.csv') %>%
    dplyr::select(
      rgn_id,
      stock_id,
      year,
      bmsy           = bbmsy)

  #comsir data
  #f = SelectLayersData(layers, layer='fis_comsir_bmsy_arc2016', narrow = TRUE) %>%
  #dplyr::select(
  #rgn_id         = id_num,
  #stock_id      = category,
  #year,
  #bmsy           = val_num)

  # The following stocks are fished in multiple regions and have high b/bmsy values
  # Due to the underfishing penalty, this actually penalizes the regions that have the highest
  # proportion of catch of these stocks.  The following corrects this problem:
  #filter(b, stock_id %in% c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47'))

  high_bmsy <- c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47', 'Sardinella_aurita-34', 'Scomberomorus_cavalla-31')

  b <- b %>%
    mutate(bmsy = ifelse(stock_id %in% high_bmsy, 1, bmsy))


  # separate out the stock_id and taxonkey:
  c <- c %>%
    mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
    mutate(taxon_key = stringr::str_sub(stock_id_taxonkey, -6, -1)) %>%
    mutate(stock_id = substr(stock_id_taxonkey, 1, nchar(stock_id_taxonkey)-7)) %>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(taxon_key = as.numeric(as.character(taxon_key))) %>%
    dplyr::select(rgn_id, year, stock_id, taxon_key, catch)

  # general formatting:
  b <- b %>%
    mutate(bmsy = as.numeric(bmsy)) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(stock_id = as.character(stock_id))


  # ------------------------------------------------------------------------
  # STEP 1. Calculate scores for Bbmsy values
  # -----------------------------------------------------------------------
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05

  b$score = ifelse(b$bmsy < lowerBuffer, b$bmsy,
                   ifelse (b$bmsy >= lowerBuffer & b$bmsy <= upperBuffer, 1, NA))
  b$score = ifelse(!is.na(b$score), b$score,
                   ifelse(1 - alpha*(b$bmsy - upperBuffer) > beta,
                          1 - alpha*(b$bmsy - upperBuffer),
                          beta))
  b$score = ifelse(b$rgn_id == "1" & b$bmsy >1, 1, b$score)


  # ------------------------------------------------------------------------
  # STEP 1. Merge the b/bmsy data with catch data
  # -----------------------------------------------------------------------
  data_fis <- c %>%
    left_join(b, by=c('rgn_id', 'stock_id', 'year')) %>%
    dplyr::select(rgn_id, stock_id, year, taxon_key, catch, bmsy, score)


  # ------------------------------------------------------------------------
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Mean score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  # -----------------------------------------------------------------------

  ## this takes the mean score within each region
  data_fis_gf <- data_fis %>%
    group_by(rgn_id, year) %>%
    mutate(Mean_score = mean(score, na.rm=TRUE)) %>%
    ungroup()

  ## this takes the median score across all regions (when no stocks have scores within a region)
  #data_fis_gf <- data_fis_gf %>%
  #group_by(year) %>%
  #mutate(Median_score_global = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
  #ungroup() %>%
  #mutate(Median_score = ifelse(is.na(Median_score), Median_score_global, Median_score)) %>%
  #dplyr::select(-Median_score_global)

  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  status_year=2015
  penaltyTable <- data.frame(TaxonPenaltyCode=1:6,
                             penalty=c(0.1, 0.25, 0.5, 0.8, 0.9, 1))

  data_fis_gf <- data_fis_gf %>%
    mutate(TaxonPenaltyCode = as.numeric(substring(taxon_key, 1, 1))) %>%
    left_join(penaltyTable, by='TaxonPenaltyCode') %>%
    mutate(score_gf = Mean_score * penalty) %>%
    mutate(score_gapfilled = ifelse(is.na(score), "Mean gapfilled", "none")) %>%
    mutate(score = ifelse(is.na(score), score_gf, score))


  gap_fill_data <- data_fis_gf %>%
    mutate(gap_fill = ifelse(is.na(bmsy), "mean", "none")) %>%
    dplyr::select(rgn_id, stock_id, taxon_key, year, catch, score, gap_fill) %>%
    filter(year == status_year)
  write.csv(gap_fill_data, 'circle2016/temp/FIS_summary_gf_reg.csv', row.names=FALSE)
  write.csv(data_fis_gf, 'circle2016/temp/FIS_summary_gf2_reg.csv', row.names=FALSE)

  status_data <- data_fis_gf %>%
    dplyr::select(rgn_id, stock_id, year, catch, score)


  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each region
  # -----------------------------------------------------------------------

  # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region/year

  status_data <- status_data %>%
    group_by(year, rgn_id) %>%
    mutate(SumCatch = sum(catch)) %>%
    ungroup() %>%
    mutate(wprop = catch/SumCatch)

  status_data <- status_data %>%
    group_by(rgn_id, year) %>%
    summarize(status = weighted.mean(score, wprop)) %>%
    ungroup()

  # ------------------------------------------------------------------------
  # STEP 5. Get yearly status and trend
  # -----------------------------------------------------------------------

  status <-  status_data %>%
    group_by(rgn_id) %>% #group by rgn_id and add max year in to get status
    filter(year >= max(year, na.rm=T)) %>%
    mutate(
      score     = round(status*100, 1),
      dimension = 'status') %>%
    dplyr::select(region_id=rgn_id, score, dimension)

  status_year<- 2014
  trend_years <- status_year:(status_year-4)
  first_trend_year <- min(trend_years)

  trend <- status_data %>%
    filter(year %in% trend_years) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == first_trend_year]) %>%
    summarize(region_id = rgn_id,
              score = round(coef(mdl)['year']/adjust_trend * 5, 4),
              dimension = 'trend') %>%
    ungroup() %>%
    mutate(score = ifelse(score > 1, 1, score)) %>%
    mutate(score = ifelse(score < (-1), (-1), score))

  # return scores
  scores= full_join(status, trend)%>%
    mutate(goal='FIS')%>%
    data.frame()



  write.csv(scores, 'circle2016/prep/FIS/reg/reg_scores.csv')

  return(scores)
}
>>>>>>> 0a274ebb0905145c89a89fad84fe28856d4f05ae
