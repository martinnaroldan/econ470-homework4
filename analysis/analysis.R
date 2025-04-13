if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, lubridate, stringr, readxl, data.table, gdata, tidyverse, dplyr, scales, knitr, broom, kableExtra, gridExtra, stargazer, lmtest, sandwich)


# import and clean data 
final.data <- read_rds("data/output/final_ma_data.rds")

final.clean.data <- final.data %>%
  filter((year %in% 2010:2015) & !is.na(partc_score))  %>%
  distinct(contractid, planid, county, .keep_all = TRUE)



# Remove all SNPs, 800-series plans, and prescription drug only plans (i.e., plans that do not offer Part C benefits). Provide a box and whisker plot showing the distribution of plan counts by county over time. Do you think that the number of plans is sufficient, too few, or too many?
final.clean.data <- final.clean.data %>%
  filter(
    snp == "No",
    !(planid >= 800 & planid < 900),
    !(partd == "Y" & plan_type == "PDP")
  )


county_plan_counts <- final.clean.data %>%
  group_by(year, state, county) %>%
  summarise(plan_count = n(), .groups = "drop")

county_plan_boxplot <- ggplot(county_plan_counts, aes(x = factor(year), y = plan_count)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    x = "Year",
    y = "Plans per County"
  ) +
  theme_minimal()

print(county_plan_boxplot)




# Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. How has this distribution changed over time?

star_distribution <- final.clean.data %>%
  filter(year %in% c(2010, 2012, 2015) & !is.na(Star_Rating)) %>%
  count(year, Star_Rating, name = "plan_count")

star_plot <- ggplot(star_distribution, aes(x = factor(Star_Rating), y = plan_count, fill = factor(year))) +
  geom_col(position = "dodge") +
  labs(
    x = "Star Rating",
    y = "Number of Plans",
    fill = "Year"
  ) +
  theme_minimal()

print(star_plot)


# 3Plot the average benchmark payment over time from 2010 through 2015. How much has the average benchmark payment risen over the years?

avg.benchmark <- final.clean.data %>%
  group_by(ssa, year) %>%
  ggplot(aes(x = as.factor(year), y = ma_rate, group = 1)) +
  stat_summary(fun = "mean", geom = "line", na.rm = TRUE) +
  labs(
    x = "Year",
    y = "Benchmark Payments ($)"
  ) +
  scale_y_continuous(labels = comma, limits = c(600, 900)) +
  theme_bw()
avg.benchmark


# Plot the average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015. Has Medicare Advantage increased or decreased in popularity? How does this share correlate with benchmark payments?
ma.mkt.data <- final.clean.data %>%
  group_by(fips, year) %>%
  summarize(
    enroll = first(avg_enrolled),
    medicare = first(avg_eligibles),
    bench = mean(ma_rate, na.rm = TRUE)
  ) %>%
  mutate(mkt_share = enroll / medicare)

ma.share <- ma.mkt.data %>%
  ggplot(aes(x = as.factor(year), y = mkt_share, group = 1)) +
  stat_summary(fun = "mean", geom = "line", na.rm = TRUE) +
  labs(
    x = "Year",
    y = "MA Market Share"
  ) +
  theme_bw()

share.reg <- lm(mkt_share ~ bench, data = ma.mkt.data)
ma.share

#Estimate ATEs: For the rest of the assignment, we’ll use a regression discontinuity design to estimate the average treatment effect from receiving a marginally higher rating. We’ll focus only on 2010
data_2010 <- final.data %>%
             filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score)) %>%
  distinct(contractid, planid, county, .keep_all = TRUE)


# 5. Calculate the running variable underlying the star rating. Provide a table showing the number of plans that are rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.

#Part 1
data_2010 <- data_2010 %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen, rectalcancer_screen, cv_diab_cholscreen, glaucoma_test,
          monitoring, flu_vaccine, pn_vaccine, physical_health, mental_health,
          osteo_test, physical_monitor, primaryaccess, osteo_manage,
          diab_healthy, bloodpressure, ra_manage, copd_test, bladder,
          falling, nodelays, doctor_communicate, carequickly, customer_service,                    
          overallrating_care, overallrating_plan, complaints_plan, appeals_timely,
          appeals_review, leave_plan, audit_problems, hold_times, info_accuracy,
          ttyt_available),
    na.rm=T)) %>%
    select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, partd, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, plan_type) %>% 
    mutate(mkt_share = avg_enrollment/avg_eligibles, 
          HMO=str_detect(plan_type, "HMO"))


#Part 2
data_2010_rounded <- data_2010 %>%
  mutate(rounded_30=ifelse(raw_rating>=2.75 & raw_rating<3.00 & Star_Rating==3.0,1,0), 
         rounded_35=ifelse(raw_rating>=3.25 & raw_rating<3.50 & Star_Rating==3.5,1,0),
         rounded_40=ifelse(raw_rating>=3.75 & raw_rating<4.00 & Star_Rating==4.0,1,0),
         rounded_45=ifelse(raw_rating>=4.25 & raw_rating<4.50 & Star_Rating==4.5,1,0), 
         rounded_50=ifelse(raw_rating>=4.75 & raw_rating<5.00 & Star_Rating==5.0,1,0)) %>%
  group_by(Star_Rating) %>% 
  filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5)) %>% 
  summarize(count_30=sum(rounded_30), 
            count_35=sum(rounded_35), 
            count_40=sum(rounded_40), 
            count_45=sum(rounded_45),
            count_50=sum(rounded_50))%>% 
  mutate(rounded_up=count_30 + count_35 + count_40 + count_45 + count_50) %>% 
  select(Star_Rating, rounded_up)


kable(data_2010_rounded, caption="Number of ratings that were rounded up")



# Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. 
star30 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.125), 
                         raw_rating<=(2.75+0.125), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(star30)

# Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.
star35 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.125), 
                         raw_rating<=(3.25+0.125), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(star35)


tidy_30 <- tidy(star30)
tidy_35 <- tidy(star35)


table_6 <- full_join(
  tidy_30 %>% select(term, estimate, std.error) %>% rename(Estimate_3 = estimate, SE_3 = std.error),
  tidy_35 %>% select(term, estimate, std.error) %>% rename(Estimate_3.5 = estimate, SE_3.5 = std.error),
  by = "term"
)


table_6 %>%
  mutate(
    Estimate_3 = sprintf("%.4f", Estimate_3),
    SE_3 = sprintf("(%.4f)", SE_3),
    Estimate_3.5 = sprintf("%.4f", Estimate_3.5),
    SE_3.5 = sprintf("(%.4f)", SE_3.5)
  ) %>%
  select(term, Estimate_3, SE_3, Estimate_3.5, SE_3.5) %>%
  kable(col.names = c("", "3 Star", "", "3.5 Star", ""),
        caption = "Table 6: RD Estimates by Star Rating",
        align = "lcccc") %>%
  kable_styling(full_width = FALSE, position = "left")



# Repeat your results for bandwidhts of 0.1, 0.12, 0.13, 0.14, and 0.15 (again for 3 and 3.5 stars). Show all of the results in a graph. How sensitive are your findings to the choice of bandwidth?
star30_1 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.1), 
                         raw_rating<=(2.75+0.1), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(star30_1)

star30_12 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.12), 
                         raw_rating<=(2.75+0.12), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(star30_12)

star30_13 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.13), 
                         raw_rating<=(2.75+0.13), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(star30_13)

star30_14 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.14), 
                         raw_rating<=(2.75+0.14), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(star30_14)

star30_15 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.15), 
                         raw_rating<=(2.75+0.15), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(star30_15)



star35_1 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.1), 
                         raw_rating<=(3.25+0.1), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(star35_1)

star35_12 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.12), 
                         raw_rating<=(3.25+0.12), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(star35_12)

star35_13 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.13), 
                         raw_rating<=(3.25+0.13), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(star35_13)

star35_14 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.14), 
                         raw_rating<=(3.25+0.14), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(star35_14)

star35_15 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.15), 
                         raw_rating<=(3.25+0.15), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(star35_15) 


results_df <- tibble(
  Cutoff = rep(c("3 vs 2.5 Stars", "3.5 vs 3 Stars"), each = 5),
  Bandwidth = rep(c(0.10, 0.12, 0.13, 0.14, 0.15), 2),
  Estimate = c(
    tidy(star30_1)$estimate[2],
    tidy(star30_12)$estimate[2],
    tidy(star30_13)$estimate[2],
    tidy(star30_14)$estimate[2],
    tidy(star30_15)$estimate[2],
    tidy(star35_1)$estimate[2],
    tidy(star35_12)$estimate[2],
    tidy(star35_13)$estimate[2],
    tidy(star35_14)$estimate[2],
    tidy(star35_15)$estimate[2]
  ),
  SE = c(
    tidy(star30_1)$std.error[2],
    tidy(star30_12)$std.error[2],
    tidy(star30_13)$std.error[2],
    tidy(star30_14)$std.error[2],
    tidy(star30_15)$std.error[2],
    tidy(star35_1)$std.error[2],
    tidy(star35_12)$std.error[2],
    tidy(star35_13)$std.error[2],
    tidy(star35_14)$std.error[2],
    tidy(star35_15)$std.error[2]
  )
)

fig7 <- ggplot(results_df, aes(x = Bandwidth, y = Estimate, color = Cutoff)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE,
                    ymax = Estimate + 1.96 * SE), width = 0.005) +
  labs(
    title = "RDD Treatment Effects Across Bandwidths",
    x = "Bandwidth (+/-)",
    y = "Estimated Treatment Effect on Market Share"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("3 vs 2.5 Stars" = "darkblue", "3.5 vs 3 Stars" = "firebrick"))

print(fig7) 


# Examine (graphically) whether contracts appear to manipulate the running variable. In other words, look at the distribution of the running variable before and after the relevent threshold values. What do you find?
library(gridExtra) 


subset_3.0 <- data_2010 %>% filter(raw_rating >= 2.75 & raw_rating < 3.25)
subset_3.5 <- data_2010 %>% filter(raw_rating >= 3.25 & raw_rating < 3.75)

density_3.0 <- ggplot(subset_3.0, aes(x = raw_rating)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  geom_vline(xintercept = 3.0, linetype = "dashed") +
  labs(title = "(a) Distribution Around 3.0", x = "Raw Rating", y = "Density") +
  theme_minimal()

density_3.5 <- ggplot(subset_3.5, aes(x = raw_rating)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  labs(title = "(b) Distribution Around 3.5", x = "Raw Rating", y = "Density") +
  theme_minimal()

dist_plot <- grid.arrange(density_3.0, density_3.5, ncol = 2, top = "Running Variable Distributions")
print(dist_plot)


# Similar to question 4, examine whether plans just above the threshold values have different characteristics than contracts just below the threshold values. Use HMO and Part D status as your plan characteristics.

data_2010 <- data_2010 %>%
  mutate(
    HMO_binary = as.numeric(HMO),
    PartD_binary = as.numeric(partd == "Y")
  )

get_covariate_balance <- function(df, center, low, high, covars) {
  balanced_data <- df %>%
    filter(raw_rating >= (center - 0.125) & raw_rating <= (center + 0.125)) %>%
    filter(Star_Rating %in% c(low, high)) %>%
    mutate(treat = as.numeric(Star_Rating == high))

  map_dfr(covars, function(var) {
    group_means <- balanced_data %>%
      group_by(treat) %>%
      summarize(mean = mean(.data[[var]], na.rm = TRUE), .groups = "drop")

    tibble(
      variable = var,
      mean_diff = group_means$mean[2] - group_means$mean[1]
    )
  }) %>%
    mutate(variable = recode(variable, HMO_binary = "HMO", PartD_binary = "Part D"))
}

covariates <- c("HMO_binary", "PartD_binary")
balance_data_3.0 <- get_covariate_balance(data_2010, 2.75, 2.5, 3.0, covariates)
balance_data_3.5 <- get_covariate_balance(data_2010, 3.25, 3.0, 3.5, covariates)

balance_plot_3.0 <- ggplot(balance_data_3.0, aes(x = mean_diff, y = variable)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(title = "(a) Covariate Balance Near 3.0", x = "Difference in Means", y = NULL) +
  xlim(-0.4, 0.4) +
  theme_minimal(base_size = 14)

balance_plot_3.5 <- ggplot(balance_data_3.5, aes(x = mean_diff, y = variable)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(title = "(b) Covariate Balance Near 3.5", x = "Difference in Means", y = NULL) +
  xlim(-0.4, 0.4) +
  theme_minimal(base_size = 14)

char_plot <- grid.arrange(balance_plot_3.0, balance_plot_3.5, ncol = 2, top = "Plan Characteristics Around Cutoffs")

rm(list = setdiff(ls(), c("plan_counts_plot", "star_dist_plot", "bench_plt", "adv_share_plt", "data_2010_round", "table_6", "q7_fig", "dist_plot", "density_3.0", "density_3.5", "char_plot", "balance_plot_3.0", "balance_plot_3.5")))

save.image("results/hwk4_workspace.RData")