##############################################################################
## Read in market pentration data */
##############################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)


## Read in monthly files, append to yearly file, fill in missing info, and collapse down to yearly file
for (y in 2010:2015) {
    ## Pull market penetration data by contract/month
    ma.path=paste0("data/input/monthly-ma-state-and-county-penetration/State_County_Penetration_MA_",y,"_01.csv")
    pene.data=read_csv(ma.path,skip=1,
                       col_names=c("state","county","fips_state","fips_cnty","fips",
                                   "ssa_state","ssa_cnty","ssa","eligibles","enrolled",
                                   "penetration"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         fips_state = col_integer(),
                         fips_cnty = col_integer(),
                         fips = col_double(),
                         ssa_state = col_integer(),
                         ssa_cnty = col_integer(),
                         ssa = col_double(),
                         eligibles = col_number(),
                         enrolled = col_number(),
                         penetration = col_number()
                       ), na="*") %>%
      mutate(year=y)
      
  ## Fill in missing fips codes (by state and county)
  pene.data = pene.data %>%
    group_by(state, county) %>%
    fill(fips)

  ## Collapse to yearly data
  ma.penetration = pene.data %>%
    group_by(fips,state,county) %>%
    dplyr::summarize(avg_eligibles=mean(eligibles),sd_eligibles=sd(eligibles),
              min_eligibles=min(eligibles),max_eligibles=max(eligibles),
              first_eligibles=first(eligibles),last_eligibles=last(eligibles),
              avg_enrolled=mean(enrolled),sd_enrolled=sd(enrolled),
              min_enrolled=min(enrolled),max_enrolled=max(enrolled),
              first_enrolled=first(enrolled),last_enrolled=last(enrolled),              
              year=last(year),ssa=first(ssa))
  
  assign(paste0("ma.pene.",y),ma.penetration) 
}

ma.penetration.data=rbind(ma.pene.2010,ma.pene.2011,ma.pene.2012,
                          ma.pene.2013,ma.pene.2014,ma.pene.2015)
write_rds(ma.penetration.data,"data/output/ma_penetration.rds")
