##############################################################################
## Read in Average Fee-for-Service Costs per County */
##############################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

## Assign yearly file paths

ffs.path.2010=paste0("data/input/ffs-costs/aged10.csv")
ffs.path.2011=paste0("data/input/ffs-costs/aged11.csv")
ffs.path.2012=paste0("data/input/ffs-costs/aged12.csv")
ffs.path.2013=paste0("data/input/ffs-costs/aged13.csv")
ffs.path.2014=paste0("data/input/ffs-costs/aged14.csv")
ffs.path.2015=paste0("data/input/ffs-costs/FFS15.xlsx")

drops <- array(dim = c(6, 2))
drops[,1] <- 2010:2015
drops[,2] <- c(2, 2, 2, 2, 2, 2)

# Loop through 2010–2014 (CSV files)
for (y in 2010:2014) {
  d <- drops[which(drops[,1] == y), 2]
  
  ffs.data <- read_csv(get(paste0("ffs.path.", y)),
                       skip = d,
                       col_names = FALSE, na = "*")
  
  ffs.data <- ffs.data[, 1:15]
  names(ffs.data) <- c("ssa", "state", "county_name", "parta_enroll",
                       "parta_reimb", "parta_percap", "parta_reimb_unadj",
                       "parta_percap_unadj", "parta_ime", "parta_dsh",
                       "parta_gme", "partb_enroll", "partb_reimb",
                       "partb_percap", "mean_risk")
  
  ffs.costs <- ffs.data %>%
    select(ssa, state, county_name, parta_enroll, parta_reimb,
           partb_enroll, partb_reimb, mean_risk) %>%
    mutate(year = y,
           ssa = as.numeric(ssa)) %>%
    mutate(across(c(parta_enroll, parta_reimb, partb_enroll, partb_reimb, mean_risk),
                  ~as.numeric(str_replace_all(., ",", ""))))
  
  assign(paste0("ffs.costs.", y), ffs.costs)
}

# Read in 2015 Excel file
d <- drops[which(drops[,1] == 2015), 2]
ffs.data <- read_xlsx(ffs.path.2015,
                      skip = d,
                      col_names = c("ssa", "state", "county_name", "parta_enroll",
                                    "parta_reimb", "parta_percap", "parta_reimb_unadj",
                                    "parta_percap_unadj", "parta_ime", "parta_dsh",
                                    "parta_gme", "partb_enroll", "partb_reimb",
                                    "partb_percap", "mean_risk"),
                      na = "*")

ffs.costs <- ffs.data %>%
  select(ssa, state, county_name, parta_enroll, parta_reimb,
         partb_enroll, partb_reimb, mean_risk) %>%
  mutate(year = 2015,
         ssa = as.numeric(ssa)) %>%
  mutate(across(c(parta_enroll, parta_reimb, partb_enroll, partb_reimb, mean_risk),
                ~as.numeric(str_replace_all(., ",", ""))))

assign("ffs.costs.2015", ffs.costs)

# Combine all years into one dataset
ffs.costs.final <- bind_rows(ffs.costs.2010, ffs.costs.2011, ffs.costs.2012,
                             ffs.costs.2013, ffs.costs.2014, ffs.costs.2015)

# Save to output
write_rds(ffs.costs.final, "data/output/ffs_costs.rds")
