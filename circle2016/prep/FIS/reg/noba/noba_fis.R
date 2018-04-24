library(sf) #install.packages("sf")
library(tidyverse)
library(datalimited)
install.packages("devtools")
devtools::install_github("datalimited/datalimited")
################# Load Catch Data###########

catch<- read.csv('circle2016/prep/FIS/reg/noba/spatial_catch_prebbmsy_.csv')%>%
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
write.csv(cmsy_bbmsy, "circle2016/prep/FIS/reg/catch_model_bmsy_reg/cmsy_bbmsy_reg.csv", row.names=FALSE)
