

# Libraries -------

library(tidyverse)
library(readxl)
library(lubridate)
library(fixest)
library(broom)
library(did)
library(stringr)
library(haven)
library(foreign)
library(cowplot)
library(stringi)
library(ggthemes)
library(ggsci)
library(MetBrewer)
library(scales)
library(broom)
library(ggthemes)
library(sf)


# 1. Data -------

combined <- read_rds("~/BurkeLab Dropbox/Carlos Gould/Ecuador Electricity/ecuador_electricity_combined.rds")
xwalk <- read_csv("~/Downloads/icd_xwalk.csv")


df_ec <- 
  combined %>% 
  mutate(
    log_total_rate = log(total_rate),
    customers_percent_pec_1 = customers_percent_pec * 100
  ) %>% 
  filter(!is.na(log_total_rate) &
           !is.na(customers_percent_pec_1) &
           !is.na(canton_id) &
           !is.na(month_year) &
           !is.na(customers_percent_pec_1) &
           !is.na(bdh_int) &
           !is.na(epobreza_int) &
           !is.na(correa_votes_percent_int) &
           !is.na(facilities_docnurse5_int_rate) &
           !is.na(doctors_nurses_int_rate)) %>% 
  filter(month_year<ymd("2020-03-01")) %>% 
  dplyr::select(canton_id, month_year, log_total_rate, customers_percent_pec_1, population_int,
                bdh_int, epobreza_int, correa_votes_percent_int, facilities_docnurse5_int_rate, doctors_nurses_int_rate) %>% 
  arrange(canton_id, month_year) %>% 
  ungroup() %>% 
  mutate(canton_id = as.character(canton_id), 
         month_year = as.character(month_year)) %>% 
  group_by(canton_id, month_year) %>% 
  summarize(
    log_total_rate = mean(log_total_rate, na.rm=T), 
    customers_percent_pec_1 = mean(customers_percent_pec_1, na.rm=T), 
    population_int = mean(population_int, na.rm=T), 
    bdh_int = mean(bdh_int, na.rm=T), 
    epobreza_int = mean(epobreza_int, na.rm=T), 
    correa_votes_percent_int = mean(correa_votes_percent_int, na.rm=T), 
    facilities_docnurse5_int_rate = mean(facilities_docnurse5_int_rate, na.rm=T), 
    doctors_nurses_int_rate = mean(doctors_nurses_int_rate, na.rm=T)
  ) %>% 
  ungroup() 

# For each outcome w/ some threshold of missingness: ------

main_lr_unadjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate), 
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate),
      log(ab_rate),
      log(cancer_rate),
      log(f_rate),
      log(g_rate),
      log(circulatory_rate),
      log(k_rate),
      log(n_rate),
      log(o_rate),
      log(p_rate),
      log(st_rate)
    ) ~
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_lr_adjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate), 
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate),
      log(ab_rate),
      log(cancer_rate),
      log(f_rate),
      log(g_rate),
      log(circulatory_rate),
      log(k_rate),
      log(n_rate),
      log(o_rate),
      log(p_rate),
      log(st_rate)
    ) ~
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int +
      pm25 |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int,
    cluster = NULL
  )

main_lr_adjusted$`lhs: log(total_rate)`

main_r_unadjusted <- 
  feols(
    c(total_rate, 
      j_rate, 
      copd_rate,
      flu_rate,
      asthma_rate,
      ab_rate,
      cancer_rate,
      f_rate,
      g_rate,
      circulatory_rate,
      k_rate,
      n_rate,
      o_rate,
      p_rate,
      st_rate) ~
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_r_adjusted <- 
  feols(
    c(total_rate, 
      j_rate, 
      copd_rate,
      flu_rate,
      asthma_rate,
      ab_rate,
      cancer_rate,
      f_rate,
      g_rate,
      circulatory_rate,
      k_rate,
      n_rate,
      o_rate,
      p_rate,
      st_rate
    ) ~
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )


main_count_unadjusted <- 
  fepois(
    c(total, 
      j_icd10, 
      copd_icd10,
      flu_icd10,
      asthma,
      ab_icd10,
      cancer_icd10,
      f_icd10,
      g_icd10,
      circulatory_icd10,
      k_icd10,
      n_icd10,
      o_icd10,
      p_icd10,
      st_icd10
    ) ~
      offset(log(population_int)) +
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )


main_count_adjusted <- 
  fepois(
    c(total, 
      j_icd10, 
      copd_icd10,
      flu_icd10,
      asthma,
      ab_icd10,
      cancer_icd10,
      f_icd10,
      g_icd10,
      circulatory_icd10,
      k_icd10,
      n_icd10,
      o_icd10,
      p_icd10,
      st_icd10
    ) ~
      offset(log(population_int)) +
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate +
      pm25 |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

outcomes <- c("total", 
              "j_icd10", 
              "copd_icd10",
              "flu_icd10",
              "asthma",
              "ab_icd10",
              "cancer_icd10",
              "f_icd10",
              "g_icd10",
              "circulatory_icd10",
              "k_icd10",
              "n_icd10",
              "o_icd10",
              "p_icd10",
              "st_icd10")

main_count_adjusted_combined <- 
  tidy(main_count_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_count_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_count_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_count_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_count_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_count_adjusted[[6]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_adjusted[[7]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_adjusted[[8]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_adjusted[[9]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_adjusted[[10]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_adjusted[[11]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_adjusted[[12]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_adjusted[[13]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_adjusted[[14]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_adjusted[[15]], conf.int=T, conf.level = .995)[1,]
  ) %>% 
  mutate(icd_code = outcomes) %>% 
  left_join(xwalk) %>% 
  mutate(
    outcome_type = "Count",
    adjusted = "Adjusted",
    sample = "Full",
  )

main_count_unadjusted_combined <- 
  tidy(main_count_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_count_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_count_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_count_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_count_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_count_unadjusted[[6]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_unadjusted[[7]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_unadjusted[[8]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_unadjusted[[9]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_unadjusted[[10]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_unadjusted[[11]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_unadjusted[[12]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_unadjusted[[13]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_unadjusted[[14]], conf.int=T, conf.level = .995)[1,],
    tidy(main_count_unadjusted[[15]], conf.int=T, conf.level = .995)[1,]
  ) %>% 
  mutate(icd_code = outcomes) %>% 
  left_join(xwalk) %>% 
  mutate(
    outcome_type = "Count",
    adjusted = "Unadjusted",
    sample = "Full",
  )

main_r_adjusted_combined <- 
  tidy(main_r_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_r_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[6]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_adjusted[[7]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_adjusted[[8]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_adjusted[[9]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_adjusted[[10]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_adjusted[[11]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_adjusted[[12]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_adjusted[[13]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_adjusted[[14]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_adjusted[[15]], conf.int=T, conf.level = .995)[1,]
  ) %>% 
  mutate(icd_code = outcomes) %>% 
  left_join(xwalk) %>% 
  mutate(
    outcome_type = "Rate",
    adjusted = "Adjusted",
    sample = "Full",
  )

main_r_unadjusted_combined <- 
  tidy(main_r_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_r_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[6]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_unadjusted[[7]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_unadjusted[[8]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_unadjusted[[9]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_unadjusted[[10]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_unadjusted[[11]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_unadjusted[[12]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_unadjusted[[13]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_unadjusted[[14]], conf.int=T, conf.level = .995)[1,],
    tidy(main_r_unadjusted[[15]], conf.int=T, conf.level = .995)[1,]
  ) %>% 
  mutate(icd_code = outcomes) %>% 
  left_join(xwalk) %>% 
  mutate(
    outcome_type = "Rate",
    adjusted = "Unadjusted",
    sample = "Full",
  )


main_lr_adjusted_combined <- 
  tidy(main_lr_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_lr_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[6]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_adjusted[[7]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_adjusted[[8]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_adjusted[[9]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_adjusted[[10]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_adjusted[[11]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_adjusted[[12]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_adjusted[[13]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_adjusted[[14]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_adjusted[[15]], conf.int=T, conf.level = .995)[1,]
  ) %>% 
  mutate(icd_code = outcomes) %>% 
  left_join(xwalk) %>% 
  mutate(
    outcome_type = "log Rate",
    adjusted = "Adjusted",
    sample = "Full",
  )

main_lr_unadjusted_combined <- 
  tidy(main_lr_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_lr_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[6]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_unadjusted[[7]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_unadjusted[[8]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_unadjusted[[9]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_unadjusted[[10]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_unadjusted[[11]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_unadjusted[[12]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_unadjusted[[13]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_unadjusted[[14]], conf.int=T, conf.level = .995)[1,],
    tidy(main_lr_unadjusted[[15]], conf.int=T, conf.level = .995)[1,]
  ) %>% 
  mutate(icd_code = outcomes) %>% 
  left_join(xwalk) %>% 
  mutate(
    outcome_type = "log Rate",
    adjusted = "Unadjusted",
    sample = "Full",
  )

main_full_combined <- 
  main_count_adjusted_combined %>% 
  bind_rows(
    main_count_unadjusted_combined,
    main_r_adjusted_combined,
    main_r_unadjusted_combined,
    main_lr_adjusted_combined,
    main_lr_unadjusted_combined
  )

outcome_limits <- main_lr_unadjusted_combined$outcome

main_rate_fig <- 
  ggplot(
  main_full_combined %>% 
    filter(outcome_type=="Rate"),
  aes(y=outcome, x=estimate, xmin=conf.low, xmax=conf.high,
      shape=adjusted) 
) + 
  geom_pointrange(position=position_dodge(width=0.5)) +
  scale_y_discrete(limits=rev(outcome_limits)) + 
  scale_x_continuous(
    trans=scales::pseudo_log_trans(),
    breaks=c(-6, -4, -2, -1, -.5,  0)) + 
  geom_vline(xintercept=0) + 
  coord_cartesian(xlim=c(-4, 1)) + 
  theme_classic() + 
  ylab("") + 
  xlab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment\n(per 100,000 population)") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 


main_lograte_fig <- 
  ggplot(
  main_full_combined %>% 
    filter(outcome_type=="log Rate"),
  aes(y=outcome, x=estimate, xmin=conf.low, xmax=conf.high,
      shape=adjusted) 
) + 
  geom_pointrange(position=position_dodge(width=0.5)) +
  scale_y_discrete(limits=rev(outcome_limits)) + 
  scale_x_continuous(
    trans=scales::pseudo_log_trans(),
    breaks=c(-.025, -.01, -.005, 0,  .005, .01, .025),
    labels=scales::percent_format()
    ) + 
  geom_vline(xintercept=0) + 
  coord_cartesian(xlim=c(-.028, .015)) +
  theme_classic() + 
  ylab("") + 
  xlab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment") + 
  theme(
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

main_count_fig <- 
  ggplot(
  main_full_combined %>% 
    filter(outcome_type=="Count"),
  aes(y=outcome, x=exp(estimate), 
      xmin=exp(conf.low), xmax=exp(conf.high),
      shape=adjusted) 
) + 
  geom_pointrange(position=position_dodge(width=0.5)) +
  scale_y_discrete(limits=rev(outcome_limits)) + 
  scale_x_continuous(trans="log", breaks=c(0.95, 0.96, .97, .98, .99, 1, 1.01, 1.02, 1.03)) +
  coord_cartesian(xlim=c(0.95, 1.03)) + 
  geom_vline(xintercept=1) + 
  # coord_cartesian(xlim=c(-.028, .015)) +
  theme_classic() + 
  ylab("") + 
  xlab("Incidence rate ratio per 1 p.p. increase in PEC enrollment") + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

main_hosp_fig <- 
  plot_grid(
  main_lograte_fig,
  NULL,
  main_rate_fig,
  NULL,
  main_count_fig,
  nrow=5,
  rel_heights=c(1, 0.05, 1, 0.05, 1.1)
)

# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/main_hosp_fig.pdf",
#   plot = main_hosp_fig,
#   dpi = 300,
#   height = 250,
#   width = 250,
#   unit = "mm"
# )


# Bootstrap -------

# Full, main, unadjusted ------

combined_canton_province <- combined %>% 
  dplyr::select(canton_id, province, province.x, province.y) %>% 
  distinct() %>% 
  mutate(province_1 = ifelse(is.na(province.x), province.y, province.x)) %>% 
  mutate(province_2 = ifelse(is.na(province_1), province, province_1)) %>% 
  mutate(province_3 = ifelse(is.na(province_2), str_extract(canton_id, "^[^,]+"), province_2)) %>% 
  dplyr::select(-province, -province.x, -province.y, -province_1, -province_2) %>% 
  dplyr::rename(province = province_3) %>% 
  distinct(canton_id, province)

combined <- combined %>% 
  dplyr::select(-province, -province.x, -province.y) %>% 
  left_join(combined_canton_province) %>% 
  mutate(province_yr = paste0(province, "_", yr))

# combined$xunit = combined$province_yr
# cc <- unique(combined$xunit)

combined$xunit = combined$canton_id
cc <- unique(combined$xunit)

main_lr_unadjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate),
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate),
      log(ab_rate),
      log(cancer_rate),
      log(f_rate),
      log(g_rate),
      log(circulatory_rate),
      log(k_rate),
      log(n_rate),
      log(o_rate),
      log(p_rate),
      log(st_rate)
    ) ~
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_lr_unadjusted_combined <- 
  tidy(main_lr_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_lr_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[15]], conf.int=T, conf.level = .95)[1,]
  ) 

main_lr_unadjusted_combined_full <- main_lr_unadjusted_combined %>% mutate(model="Full")

set.seed(8)

for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T)) 
  subdata <- inner_join(combined, samp, by="xunit")
  
  main_lr_unadjusted <- 
    feols(
      c(log(total_rate), 
        log(j_rate),
        log(copd_rate),
        log(flu_rate),
        log(asthma_rate),
        log(ab_rate),
        log(cancer_rate),
        log(f_rate),
        log(g_rate),
        log(circulatory_rate),
        log(k_rate),
        log(n_rate),
        log(o_rate),
        log(p_rate),
        log(st_rate)
  ) ~
    customers_percent_pec_1  |
    canton_id + 
    month_year,
  subdata %>% 
    mutate(customers_percent_pec_1 = customers_percent_pec*100),
  weights=subdata$population_int
  )

  main_lr_unadjusted_combined <- 
  tidy(main_lr_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_lr_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[15]], conf.int=T, conf.level = .95)[1,]
  )  %>% 
    mutate(model = as.character(i))
  
  main_lr_unadjusted_combined_full <-
    main_lr_unadjusted_combined_full %>% 
    bind_rows(main_lr_unadjusted_combined)

  if (round(i,-1)==i) {print(i)}  #print every 10
}


# Full, main, adjusted --------

main_lr_adjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate),
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate),
      log(ab_rate),
      log(cancer_rate),
      log(f_rate),
      log(g_rate),
      log(circulatory_rate),
      log(k_rate),
      log(n_rate),
      log(o_rate),
      log(p_rate),
      log(st_rate)
    ) ~
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int +
      pm25 |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_lr_adjusted_combined <- 
  tidy(main_lr_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_lr_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[15]], conf.int=T, conf.level = .95)[1,]
  )  

main_lr_adjusted_combined_full <-main_lr_adjusted_combined %>% mutate(model="Full")


for (i in 1:1000) {
  
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))
  
  subdata <- inner_join(combined, samp, by="xunit")
  
  main_lr_adjusted <- 
    feols(
      c(log(total_rate), 
        log(j_rate),
        log(copd_rate),
        log(flu_rate),
        log(asthma_rate),
        log(ab_rate),
        log(cancer_rate),
        log(f_rate),
        log(g_rate),
        log(circulatory_rate),
        log(k_rate),
        log(n_rate),
        log(o_rate),
        log(p_rate),
        log(st_rate)
      ) ~
        customers_percent_pec_1 +
        bdh_int + 
        epobreza_int + 
        correa_votes_percent_int + 
        facilities_docnurse5_int_rate + 
        doctors_nurses_int_rate + 
        population_int +
        pm25 |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  main_lr_adjusted_combined <- 
    tidy(main_lr_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
    bind_rows(
      tidy(main_lr_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[6]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[7]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[8]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[9]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[10]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[11]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[12]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[13]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[14]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[15]], conf.int=T, conf.level = .95)[1,]
    )  %>% 
    mutate(model = as.character(i))
  
  main_lr_adjusted_combined_full <-
    main_lr_adjusted_combined_full %>% 
    bind_rows(main_lr_adjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# Combining bootstrapped estimates into figures -------

outcomes <- c("total_rate", 
              "j_rate",
              "copd_rate",
              "flu_rate",
              "asthma_rate",
              "ab_rate",
              "cancer_rate",
              "f_rate",
              "g_rate",
              "circulatory_rate",
              "k_rate",
              "n_rate",
              "o_rate",
              "p_rate",
              "st_rate"
)

xwalk[15,1] <- "j_rate"

main_lr_combined_full <- 
  main_lr_unadjusted_combined_full  %>% 
  mutate(adjusted = "Unadjusted")%>% 
  mutate(icd_rate = rep(outcomes,1001)) %>% 
  bind_rows(main_lr_adjusted_combined_full %>% 
              mutate(adjusted = "Adjusted")%>% 
              mutate(icd_rate = rep(outcomes,1001))) %>% 
  left_join(xwalk) %>% 
  mutate(name = plyr::mapvalues(
    outcome,
    from=c("Total hospitalizations",
           "Respiratory system",
           "Influenza and pneumonia",
           "COPD",
           "Asthma"),
    to=c("All hospitalizations",
         "Respiratory",
         "Influenza and pneumonia",
         "COPD",
         "Asthma")
  ))


main_lr_medians <- 
  main_lr_combined_full %>% 
  filter(!is.na(outcome)) %>% 
  filter(model!="Full") %>% 
  group_by(outcome,adjusted) %>% 
  summarize(median = median(estimate, na.rm=T))

coef_fig <- 
  ggplot(main_lr_combined_full %>% 
         filter(!is.na(outcome)) %>% 
           filter(model!="Full"), 
       aes(y=name, x=estimate, fill=adjusted)) + 
  ggdist::stat_slab(
    position = position_dodge(width=.8),
    alpha=0.7
  ) +
  geom_point(
    data=main_lr_combined_full %>% 
      filter(!is.na(outcome)) %>% 
      filter(model=="Full"),
    aes(y=name, x=estimate, group=adjusted),
    position=position_dodge(width=0.8)
  ) +
  scale_fill_manual(values=c(met.brewer("Cassatt1")[2], met.brewer("Cassatt1")[6])) + 
  
  annotate("text", y= 5.3, x=-0.025, label="Unadjusted", size=3, color=met.brewer("Cassatt1")[6]) +
  annotate("text", y= 4.9, x=-0.025, label="Adjusted", size=3, color=met.brewer("Cassatt1")[2]) +
  
  scale_x_continuous(labels=scales::percent_format(),
                     breaks=c(-0.03,-.025,-0.02, -0.015, -0.01, -0.005, 0, 0.005))  + 
  scale_y_discrete(limits=
                     rev(c("All hospitalizations",
                           "Respiratory", 
                           "Influenza and pneumonia",
                           "COPD",
                           "Asthma"))) +
  coord_cartesian(xlim=c(-.031, 0.006)) + 
  geom_vline(xintercept=0) + 
  theme_classic() + 
  ylab("") + 
  xlab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

coef_fig

main_lr_combined_full  %>% 
  filter(!is.na(outcome)) %>% 
  group_by(outcome, adjusted) %>% 
  summarize(p5 = mean(p.value<.05))

pvalue_df <- main_lr_combined_full %>% 
  filter(!is.na(outcome)) %>% 
  mutate(p_value_cat = ifelse(adjusted=="Adjusted" & p.value<=.05, "1",
                              ifelse(adjusted=="Adjusted" & p.value>.05, "2",
                                     ifelse(adjusted=="Unadjusted" & p.value<=.05, "3",
                                            ifelse(adjusted=="Unadjusted" & p.value>.05, "4", NA))))) %>% 
  ungroup()

pvalue_summary_df <- main_lr_combined_full %>% 
  group_by(icd_code, adjusted) %>% 
  summarize(
    p05 = mean(p.value<.05, na.rm=T)
  )

main_lr_combined_full_pvalues <-
  main_lr_combined_full %>% 
  group_by(outcome, adjusted) %>% 
  summarise(
    p05 = mean(p.value<0.05, na.rm=T),
    p01 = mean(p.value<0.01, na.rm=T),
    p005 = mean(p.value<0.005, na.rm=T),
  )


total_events_rate_df <- combined %>% 
  filter(month_year>=ymd("2012-01-01") &
           month_year<=ymd("2020-02-01")) %>% 
  filter(!is.na(customers_percent_pec) &
           !is.na(canton_id)) %>% 
  dplyr::select(total, j_icd10, asthma, copd_icd10, flu_icd10, population_int) %>% 
  summarize(
    total = sum(total, na.rm=T),
    j_icd10 = sum(j_icd10, na.rm=T),
    asthma = sum(asthma, na.rm=T),
    copd_icd10 = sum(copd_icd10, na.rm=T),
    flu_icd10 = sum(flu_icd10, na.rm=T),
    population = sum(population_int, na.rm=T)
  ) %>% 
  pivot_longer(-population) %>% 
  mutate(
    rate = value / population * 100000,
    events = value
  ) %>% 
  mutate(
    rate = round(rate, digits=2)
  ) %>% 
  mutate(
    name = plyr::mapvalues(
      name,
      from=c("total", "j_icd10", "asthma", "copd_icd10", "flu_icd10"),
      to=c("All hospitalizations","Respiratory", "Asthma", "COPD","Influenza and pneumonia")
    )
  )

hosp_regression_events_fig <- 
  ggplot(
    total_events_rate_df, 
    aes(y=name, x="Total events", label=value)
  ) + 
  geom_text(size=3) +
  scale_y_discrete(limits=
                     rev(c("All hospitalizations",
                           "Respiratory", 
                           "Influenza and pneumonia",
                           "COPD",
                           "Asthma"))) +
  xlab("Total events") + 
  theme_void() +
  theme(
    axis.text.y = element_text(size=8, color="black",hjust =1),
    axis.title.x.top = element_text(size=7, color="black", face="bold")
  )


hosp_regression_rate_fig <- 
  ggplot(
    total_events_rate_df, 
    aes(y=name, x="Rate per 100k population", label=rate)
  ) + 
  geom_text(size=3) +
  scale_y_discrete(limits=
                     rev(c("All hospitalizations",
                           "Respiratory", 
                           "Influenza and pneumonia",
                           "COPD",
                           "Asthma"))) +
  theme_void() +
  xlab("Rate per 100k population") + 
  theme(axis.title.x.top = element_text(size=7, color="black", face="bold"))


main_lr_combined_full_estimates <-
  main_lr_combined_full %>% 
  dplyr::group_by(outcome, adjusted) %>% 
  dplyr::summarise(
    q25 = quantile(estimate, probs=.025, na.rm=T)*100,
    q50 = quantile(estimate, probs=.50, na.rm=T)*100,
    q75 = quantile(estimate, probs=.975, na.rm=T)*100,
  ) %>% 
  mutate(
    effect_size = paste0(round(q50, digits=2), "% (", 
                         round(q25, digits=2),"%, ", 
                         round(q75, digits=2),"%)")
  ) %>% 
  filter(outcome=="Total hospitalizations" |
           outcome=="Respiratory system" | 
           outcome=="Influenza and pneumonia" |
           outcome=="COPD" |
           outcome=="Asthma") %>% 
  mutate(name = plyr::mapvalues(
    outcome,
    from=c("Total hospitalizations",
             "Respiratory system",
             "Influenza and pneumonia",
             "COPD",
             "Asthma"),
    to=c("All hospitalizations",
                 "Respiratory",
                 "Influenza and pneumonia",
                 "COPD",
                 "Asthma")
  ))


hosp_regression_effectsize_fig <- 
  ggplot(
    main_lr_combined_full_estimates %>% 
      ungroup() %>% 
      dplyr::mutate(label_group = paste0(name, adjusted),
             number = row_number()), 
    aes(y=number, x="Effect size", label=effect_size)
  ) + 
  geom_text(size=3,nudge_y = .2) +
  theme_void() +
  xlab("Effect size") + 
  theme(axis.title.x.top = element_text(size=7, color="black", face="bold"))


coef_fig_comb <- plot_grid(
  hosp_regression_events_fig,
  hosp_regression_rate_fig,
  coef_fig,
  hosp_regression_effectsize_fig,
  align="h",
  nrow=1,
  rel_widths=c(0.4, 0.2, 1, 0.5)
)


coef_fig_comb_lab <- plot_grid(
  NULL,
  coef_fig_comb,
  nrow=2,
  rel_heights=c(0.05, 1)
) +
  annotate("text", x=0.08, y=0.95, label="Outcome", size=3, fontface="bold") +
  annotate("text", x=0.155, y=0.95, label="Total events", size=3, fontface="bold") +
  annotate("text", x=0.25, y=0.95, label="Rate per 100k", size=3, fontface="bold") +
  annotate("text", x=0.875, y=0.95, label="Median effect size (2.5%tile, 97.5%tile)", size=3, fontface="bold") 

# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/coef_fig_comb_prov_v_June7_2.pdf",
#   plot = coef_fig_comb_lab,
#   dpi = 300,
#   height = 120,
#   width = 300,
#   unit = "mm"
# )


# Full outcomes bootstrapped fig ------

main_lr_combined_full <- 
  main_lr_combined_full %>% 
  dplyr::select(-icd_code) %>%
  # dplyr::select(-outcome.x, -icd_code.x, -icd_code.y, -outcome.y, -outcome) %>% 
  rename(icd_code = icd_rate) %>%
  left_join(xwalk, by=c("icd_code"))


outcomes_main <- 
  c("Total hospitalizations", "Respiratory system", "Influenza and pneumonia", "COPD", "Asthma")

coef_fig_full <- 
  ggplot(main_lr_combined_full %>% 
           filter(model!="Full") %>% 
           # filter(outcome.x %in% outcomes_main) %>% 
           rename(outcome = outcome.x), 
         aes(y=outcome, x=estimate, fill=adjusted)) + 
  ggdist::stat_slab(
    position = position_dodge(width=.8),
    alpha=0.7
  ) +
  geom_point(
    data=main_lr_combined_full %>% 
      filter(model=="Full")  %>% 
      # filter(outcome.x %in% outcomes_main) %>% 
      rename(outcome = outcome.x),
    aes(y=outcome, x=estimate, group=adjusted),
    position=position_dodge(width=0.8)
  ) +
  scale_fill_manual(values=c(met.brewer("Cassatt1")[2], met.brewer("Cassatt1")[6])) + 
  scale_x_continuous(labels=scales::percent_format(),
                     breaks=c(-0.03,-.025,-0.02, -0.015, -0.01, -0.005, 0, 0.005))  + 
  coord_cartesian(xlim=c(-.031, 0.02)) +
  geom_vline(xintercept=0) + 
  theme_classic() + 
  ylab("") + 
  xlab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

coef_fig_full



total_events_rate_df <- combined %>% 
  filter(month_year>=ymd("2012-01-01") &
           month_year<=ymd("2020-02-01")) %>% 
  filter(!is.na(customers_percent_pec) &
           !is.na(canton_id)) %>% 
  summarize(
    ab_icd10 = sum(ab_icd10, na.rm=T),
    asthma = sum(asthma, na.rm=T),
    cancer_icd10 = sum(cancer_icd10, na.rm=T),
    circulatory_icd10 = sum(circulatory_icd10, na.rm=T),
    copd_icd10 = sum(copd_icd10, na.rm=T),
    f_icd10 = sum(f_icd10, na.rm=T),
    flu_icd10 = sum(flu_icd10, na.rm=T),
    g_icd10 = sum(g_icd10, na.rm=T),
    j_icd10 = sum(j_icd10, na.rm=T),
    k_icd10 = sum(k_icd10, na.rm=T),
    n_icd10 = sum(n_icd10, na.rm=T),
    o_icd10 = sum(o_icd10, na.rm=T),
    p_icd10 = sum(p_icd10, na.rm=T),
    st_icd10 = sum(st_icd10, na.rm=T),
    total = sum(total, na.rm=T),
    
    population = sum(population_int, na.rm=T)
  ) %>% 
  pivot_longer(-population) %>% 
  mutate(
    rate = value / population * 100000,
    events = value
  ) %>% 
  mutate(
    rate = round(rate, digits=2)
  ) %>% 
  rename(icd_code = name) %>% 
  left_join(xwalk)


hosp_regression_events_fig_full <- 
  ggplot(
    total_events_rate_df  %>% 
      filter(!is.na(outcome)), 
    aes(y=outcome, x="Total events", label=value)
  ) + 
  geom_text(size=3) +
  xlab("Total events") + 
  theme_void() +
  theme(
    axis.text.y = element_text(size=8, color="black",hjust =1),
    axis.title.x.top = element_text(size=7, color="black", face="bold")
  )


hosp_regression_rate_fig_full <- 
  ggplot(
    total_events_rate_df  %>% 
      filter(!is.na(outcome)), 
    aes(y=outcome, x="Rate per 100k population", label=rate)
  ) + 
  geom_text(size=3) +
  theme_void() +
  xlab("Rate per 100k population") + 
  theme(axis.title.x.top = element_text(size=7, color="black", face="bold"))


main_lr_combined_full_estimates <-
  main_lr_combined_full %>% 
  group_by(outcome.x, adjusted) %>% 
  summarise(
    q25 = quantile(estimate, probs=.025, na.rm=T)*100,
    q50 = quantile(estimate, probs=.50, na.rm=T)*100,
    q75 = quantile(estimate, probs=.975, na.rm=T)*100,
  ) %>% 
  mutate(
    effect_size = paste0(round(q50, digits=2), "% (", 
                         round(q25, digits=2),"%, ", 
                         round(q75, digits=2),"%)")
  ) 


hosp_regression_effectsize_fig_full <- 
  ggplot(
    main_lr_combined_full_estimates %>% 
      filter(!is.na(outcome.x)) %>% 
      ungroup() %>% 
      mutate(label_group = paste0(outcome.x, adjusted),
             number = row_number()), 
    aes(y=number, x="Effect size", label=effect_size)
  ) + 
  geom_text(size=3,nudge_y = .2) +
  theme_void() +
  xlab("Effect size") + 
  theme(axis.title.x.top = element_text(size=7, color="black", face="bold"))



coef_pvalue_fig_full <- 
  plot_grid(
    hosp_regression_events_fig_full,
    hosp_regression_rate_fig_full,
    coef_fig_full,
    hosp_regression_effectsize_fig_full,
    align="h",
    nrow=1,
    rel_widths=c(0.6, 0.3, 1, 0.4)
  ) 


coef_pvalue_fig_full_lab <- plot_grid(
  NULL,
  coef_pvalue_fig_full,
  nrow=2,
  rel_heights=c(0.05, 1)
) +
  annotate("text", x=0.18, y=0.95, label="Outcome", size=3, fontface="bold") +
  annotate("text", x=0.24, y=0.95, label="Total events", size=3, fontface="bold") +
  annotate("text", x=0.34, y=0.95, label="Rate per 100k", size=3, fontface="bold") +
  annotate("text", x=0.90, y=0.95, label="Median effect size (2.5%tile, 97.5%tile)", size=3, fontface="bold") 


# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/coef_fig_full_1_June_v3.pdf",
#   plot = coef_pvalue_fig_full_lab,
#   dpi = 300,
#   height = 275,
#   width = 400,
#   unit = "mm"
# )

# Rates -----

# unadjusted ------
combined$xunit = combined$canton_id
cc <- unique(combined$xunit)

main_r_unadjusted <- 
  feols(
    c(total_rate, 
      j_rate,
      copd_rate,
      flu_rate,
      asthma_rate,
      ab_rate,
      cancer_rate,
      f_rate,
      g_rate,
      circulatory_rate,
      k_rate,
      n_rate,
      o_rate,
      p_rate,
      st_rate
    ) ~
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_r_unadjusted_combined <- 
  tidy(main_r_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_r_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_unadjusted[[15]], conf.int=T, conf.level = .95)[1,]
  ) 

main_r_unadjusted_combined_full <- main_r_unadjusted_combined %>% mutate(model="Full")


for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  main_r_unadjusted <- 
    feols(
      c(total_rate, 
        j_rate,
        copd_rate,
        flu_rate,
        asthma_rate,
        ab_rate,
        cancer_rate,
        f_rate,
        g_rate,
        circulatory_rate,
        k_rate,
        n_rate,
        o_rate,
        p_rate,
        st_rate
      ) ~
        customers_percent_pec_1  |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  main_r_unadjusted_combined <- 
    tidy(main_r_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
    bind_rows(
      tidy(main_r_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[6]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[7]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[8]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[9]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[10]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[11]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[12]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[13]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[14]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_unadjusted[[15]], conf.int=T, conf.level = .95)[1,]
    )  %>% 
    mutate(model = as.character(i))
  
  main_r_unadjusted_combined_full <-
    main_r_unadjusted_combined_full %>% 
    bind_rows(main_r_unadjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}


# adjusted --------

main_r_adjusted <- 
  feols(
    c(total_rate, 
      j_rate,
      copd_rate,
      flu_rate,
      asthma_rate,
      ab_rate,
      cancer_rate,
      f_rate,
      g_rate,
      circulatory_rate,
      k_rate,
      n_rate,
      o_rate,
      p_rate,
      st_rate
    ) ~
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int +
      pm25 |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_r_adjusted_combined <- 
  tidy(main_r_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_r_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(main_r_adjusted[[15]], conf.int=T, conf.level = .95)[1,]
  )  

main_r_adjusted_combined_full <- main_r_adjusted_combined %>% mutate(model="Full")


for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  main_r_adjusted <- 
    feols(
      c(total_rate, 
        j_rate,
        copd_rate,
        flu_rate,
        asthma_rate,
        ab_rate,
        cancer_rate,
        f_rate,
        g_rate,
        circulatory_rate,
        k_rate,
        n_rate,
        o_rate,
        p_rate,
        st_rate
      ) ~
        customers_percent_pec_1 +
        bdh_int + 
        epobreza_int + 
        correa_votes_percent_int + 
        facilities_docnurse5_int_rate + 
        doctors_nurses_int_rate + 
        population_int +
        pm25 |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  main_r_adjusted_combined <- 
    tidy(main_r_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
    bind_rows(
      tidy(main_r_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[6]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[7]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[8]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[9]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[10]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[11]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[12]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[13]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[14]], conf.int=T, conf.level = .95)[1,],
      tidy(main_r_adjusted[[15]], conf.int=T, conf.level = .95)[1,]
    )  %>% 
    mutate(model = as.character(i))
  
  main_r_adjusted_combined_full <-
    main_r_adjusted_combined_full %>% 
    bind_rows(main_r_adjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# combining r fig ------

main_r_combined_full <- 
  main_r_adjusted_combined_full %>% 
  mutate(icd_code = rep(c("total", 
                          "j_icd10",
                          "copd_icd10",
                          "flu_icd10",
                          "asthma",
                          "ab_icd10",
                          "cancer_icd10",
                          "f_icd10",
                          "g_icd10",
                          "circulatory_icd10",
                          "k_icd10",
                          "n_icd10",
                          "o_icd10",
                          "p_icd10",
                          "st_icd10"), 1001),
         adjusted = "Adjusted") %>% 
  bind_rows(
    main_r_unadjusted_combined_full %>% 
      mutate(icd_code = rep(c("total", 
                              "j_icd10",
                              "copd_icd10",
                              "flu_icd10",
                              "asthma",
                              "ab_icd10",
                              "cancer_icd10",
                              "f_icd10",
                              "g_icd10",
                              "circulatory_icd10",
                              "k_icd10",
                              "n_icd10",
                              "o_icd10",
                              "p_icd10",
                              "st_icd10"),1001),
             adjusted = "Unadjusted") 
  ) %>% 
  left_join(xwalk, by=c("icd_code"))

r_coef_fig_full <- 
  ggplot(main_r_combined_full %>% 
           filter(model!="Full") %>% 
           filter(!is.na(outcome)) %>% 
           left_join(total_events_rate_df) %>% 
           mutate(estimate_percent = estimate / rate), 
         aes(y=outcome, x=estimate_percent, fill=adjusted)) + 
  ggdist::stat_slab(
    position = position_dodge(width=.8),
    alpha=0.7
  ) +
  geom_point(
    data=main_r_combined_full %>% 
      filter(model=="Full") %>% 
      filter(!is.na(outcome))%>% 
      left_join(total_events_rate_df) %>% 
      mutate(estimate_percent = estimate / rate),
    aes(y=outcome, x=estimate_percent, group=adjusted),
    position=position_dodge(width=0.8)
  ) +
  scale_fill_manual(values=c(met.brewer("Cassatt1")[2], met.brewer("Cassatt1")[6])) + 
  scale_x_continuous(labels=scales::percent_format(),
                     breaks=c(-.04,-.035,-0.03,-.025,-0.02, -0.015, -0.01, -0.005, 0, 0.005, .01))  +
  coord_cartesian(xlim=c(-.035, 0.015)) +
  geom_vline(xintercept=0) + 
  theme_classic() + 
  ylab("") + 
  xlab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  )


main_r_combined_full_estimates <-
  main_r_combined_full %>% 
  left_join(total_events_rate_df) %>% 
  mutate(estimate_percent = estimate / rate) %>% 
  group_by(outcome, adjusted) %>% 
  summarise(
    q25 = quantile(estimate_percent, probs=.025, na.rm=T)*100,
    q50 = quantile(estimate_percent, probs=.50, na.rm=T)*100,
    q75 = quantile(estimate_percent, probs=.975, na.rm=T)*100,
  ) %>% 
  mutate(
    effect_size = paste0(round(q50, digits=2), "% (", 
                         round(q25, digits=2),"%, ", 
                         round(q75, digits=2),"%)")
  ) 


r_hosp_regression_effectsize_fig_full <- 
  ggplot(
    main_r_combined_full_estimates %>% 
      filter(!is.na(outcome)) %>% 
      ungroup() %>% 
      mutate(label_group = paste0(outcome, adjusted),
             number = row_number()), 
    aes(y=number, x="Effect size", label=effect_size)
  ) + 
  geom_text(size=3,nudge_y = .2) +
  theme_void() +
  xlab("Effect size") + 
  theme(axis.title.x.top = element_text(size=7, color="black", face="bold"))



r_coef_fig_full_comb <- 
  plot_grid(
    r_coef_fig_full,
    r_hosp_regression_effectsize_fig_full,
    align="h",
    nrow=1,
    rel_widths=c(1, 0.4)
  ) +
  annotate("text", x=0.86, y=0.99, label="Median effect size (95% CI)", size=3, fontface="bold") 


# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/r_coef_fig_full_june_v2.pdf",
#   plot = r_coef_fig_full_comb,
#   # dpi = 300,
#   height = 250,
#   width = 350,
#   unit = "mm"
# )


 # Full, Poisson, unadjusted ------

combined$xunit = combined$canton_id
cc <- unique(combined$xunit)

main_p_unadjusted <- 
  fepois(
    c(total, 
      j_icd10,
      copd_icd10,
      flu_icd10,
      asthma,
      ab_icd10,
      cancer_icd10,
      f_icd10,
      g_icd10,
      circulatory_icd10,
      k_icd10,
      n_icd10,
      o_icd10,
      p_icd10,
      st_icd10
    ) ~
      offset(log(population_int)) + 
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_p_unadjusted_combined <- 
  tidy(main_p_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_p_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_unadjusted[[15]], conf.int=T, conf.level = .95)[1,]
  ) 

main_p_unadjusted_combined_full <- main_p_unadjusted_combined %>% mutate(model="Full")


for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  main_p_unadjusted <- 
    fepois(
      c(total, 
        j_icd10,
        copd_icd10,
        flu_icd10,
        asthma,
        ab_icd10,
        cancer_icd10,
        f_icd10,
        g_icd10,
        circulatory_icd10,
        k_icd10,
        n_icd10,
        o_icd10,
        p_icd10,
        st_icd10
      ) ~
        offset(log(population_int)) + 
        customers_percent_pec_1  |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  main_p_unadjusted_combined <- 
    tidy(main_p_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
    bind_rows(
      tidy(main_p_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[6]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[7]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[8]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[9]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[10]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[11]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[12]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[13]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[14]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_unadjusted[[15]], conf.int=T, conf.level = .95)[1,]
    ) %>% 
    mutate(model = as.character(i))
  
  main_p_unadjusted_combined_full <-
    main_p_unadjusted_combined_full %>% 
    bind_rows(main_p_unadjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}


# Full, Poisson, adjusted --------

main_p_adjusted <- 
  fepois(
    c(total, 
      j_icd10,
      copd_icd10,
      flu_icd10,
      asthma,
      ab_icd10,
      cancer_icd10,
      f_icd10,
      g_icd10,
      circulatory_icd10,
      k_icd10,
      n_icd10,
      o_icd10,
      p_icd10,
      st_icd10
    ) ~
      offset(log(population_int)) + 
      customers_percent_pec_1  +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int +
      pm25 |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_p_adjusted_combined <- 
  tidy(main_p_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_p_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(main_p_adjusted[[15]], conf.int=T, conf.level = .95)[1,]
  )  

main_p_adjusted_combined_full <-main_p_adjusted_combined %>% mutate(model="Full")


for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  main_p_adjusted <- 
    fepois(
      c(total, 
        j_icd10,
        copd_icd10,
        flu_icd10,
        asthma,
        ab_icd10,
        cancer_icd10,
        f_icd10,
        g_icd10,
        circulatory_icd10,
        k_icd10,
        n_icd10,
        o_icd10,
        p_icd10,
        st_icd10
      ) ~
        offset(log(population_int)) + 
        customers_percent_pec_1  +
        bdh_int + 
        epobreza_int + 
        correa_votes_percent_int + 
        facilities_docnurse5_int_rate + 
        doctors_nurses_int_rate |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  main_p_adjusted_combined <- 
    tidy(main_p_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
    bind_rows(
      tidy(main_p_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[6]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[7]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[8]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[9]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[10]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[11]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[12]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[13]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[14]], conf.int=T, conf.level = .95)[1,],
      tidy(main_p_adjusted[[15]], conf.int=T, conf.level = .95)[1,]
    )  %>% 
    mutate(model = as.character(i))
  
  main_p_adjusted_combined_full <-
    main_p_adjusted_combined_full %>% 
    bind_rows(main_p_adjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# Combining estimates from Poisson draws --------

main_p_combined_full <- 
  main_p_adjusted_combined_full %>% 
  mutate(icd_code = rep(c("total", 
         "j_icd10",
         "copd_icd10",
         "flu_icd10",
         "asthma",
         "ab_icd10",
         "cancer_icd10",
         "f_icd10",
         "g_icd10",
         "circulatory_icd10",
         "k_icd10",
         "n_icd10",
         "o_icd10",
         "p_icd10",
         "st_icd10"), 1001),
         adjusted = "Adjusted") %>% 
  bind_rows(
    main_p_unadjusted_combined_full %>% 
      mutate(icd_code = rep(c("total", 
                          "j_icd10",
                          "copd_icd10",
                          "flu_icd10",
                          "asthma",
                          "ab_icd10",
                          "cancer_icd10",
                          "f_icd10",
                          "g_icd10",
                          "circulatory_icd10",
                          "k_icd10",
                          "n_icd10",
                          "o_icd10",
                          "p_icd10",
                          "st_icd10"),1001),
             adjusted = "Unadjusted") 
  ) %>% 
  left_join(xwalk, by=c("icd_code"))


coef_p_fig_full <- 
  ggplot(main_p_combined_full %>% 
           filter(model!="Full") %>% 
           filter(!is.na(outcome)), 
         aes(y=outcome, x=exp(estimate), fill=adjusted)) + 
  ggdist::stat_slab(
    position = position_dodge(width=.8),
    alpha=0.7
  ) +
  geom_point(
    data=main_p_combined_full %>% 
      filter(model=="Full"),
    aes(y=outcome, x=exp(estimate), group=adjusted),
    position=position_dodge(width=0.8)
  ) +
  scale_fill_manual(values=c(met.brewer("Cassatt1")[2], met.brewer("Cassatt1")[6])) + 
  scale_x_log10(breaks=c(0.96, 0.97, 0.98, 0.99, 1.00, 1.01, 1.02))  +
  coord_cartesian(xlim=c(0.95, 1.025)) +
  geom_vline(xintercept=1) + 
  theme_classic() + 
  ylab("") + 
  xlab("Incidence rate ratio per 1 p.p. increase in PEC enrollment") +
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    # axis.text.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 



main_p_combined_full_estimates <-
  main_p_combined_full %>% 
  group_by(outcome, adjusted) %>% 
  summarise(
    q25 = quantile(estimate, probs=.025, na.rm=T),
    q50 = quantile(estimate, probs=.50, na.rm=T),
    q75 = quantile(estimate, probs=.975, na.rm=T),
  ) %>% 
  mutate(
    effect_size = paste0(round(exp(q50), digits=3), " (", 
                         round(exp(q25), digits=3),", ", 
                         round(exp(q75), digits=3),")")
  ) 


p_effectsize_fig_full <- 
  ggplot(
    main_p_combined_full_estimates %>% 
      filter(!is.na(outcome)) %>% 
      ungroup() %>% 
      mutate(label_group = paste0(outcome, adjusted),
             number = row_number()), 
    aes(y=number, x="Effect size", label=effect_size)
  ) + 
  geom_text(size=3, nudge_y = .2) +
  theme_void() +
  xlab("Effect size") + 
  theme(axis.title.x.top = element_text(size=7, color="black", face="bold"))


coef_p_fig_full_comb <- 
  plot_grid(
    coef_p_fig_full,
    p_effectsize_fig_full,
    # align="hv",
    nrow=1,
    rel_widths=c(1, 0.5)
    ) +
  annotate("text", x=0.835, y=.99, label="Median (95% CI)")

# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/coef_p_fig_full_june_v2.pdf",
#   plot = coef_p_fig_full_comb,
#   height = 250,
#   width = 300,
#   unit = "mm"
# )

# Rerun analysis at two month aggregates --------


combined_2mo <- 
  combined %>% 
  mutate(month = month(month_year),
         year = year(month_year)) %>% 
  mutate(
    quarter = ifelse(month>=1 & month<=2, "Q1",
                     ifelse(month>=3 & month<=4, "Q2",
                            ifelse(month>=5 & month<=6, "Q3",
                                   ifelse(month>=7 & month<=8, "Q4",
                                          ifelse(month>=9 & month<=10, "Q5",
                                                 ifelse(month>=11 & month<=12, "Q6", NA)))))),
    quarter_year = paste0(quarter, " ", year)
  ) %>% 
  group_by(canton_id, quarter_year) %>% 
  summarize(
    total = sum(total, na.rm=T),
    asthma = sum(asthma, na.rm=T),
    copd_icd10 = sum(copd_icd10, na.rm=T),
    flu_icd10 = sum(flu_icd10, na.rm=T),
    ab_icd10 = sum(ab_icd10, na.rm=T),
    cancer_icd10 = sum(cancer_icd10, na.rm=T),
    blood_icd10 = sum(blood_icd10, na.rm=T),
    e_icd10 = sum(e_icd10, na.rm=T),
    f_icd10 = sum(f_icd10, na.rm=T),
    g_icd10 = sum(g_icd10, na.rm=T),
    eye_icd10 = sum(eye_icd10, na.rm=T),
    ear_icd10 = sum(ear_icd10, na.rm=T),
    circulatory_icd10 = sum(circulatory_icd10, na.rm=T),
    j_icd10 = sum(j_icd10, na.rm=T),
    k_icd10 = sum(k_icd10, na.rm=T),
    l_icd10 = sum(l_icd10, na.rm=T),
    m_icd10 = sum(m_icd10, na.rm=T),
    n_icd10 = sum(n_icd10, na.rm=T),
    o_icd10 = sum(o_icd10, na.rm=T),
    p_icd10 = sum(p_icd10, na.rm=T),
    q_icd10 = sum(q_icd10, na.rm=T),
    r_icd10 = sum(r_icd10, na.rm=T),
    st_icd10 = sum(st_icd10, na.rm=T),
    
    customers_percent_pec = mean(customers_percent_pec, na.rm=T),
    population_int = sum(population_int, na.rm=T),
    
    bdh_int = mean(bdh_int, na.rm=T),
    epobreza_int  = mean(epobreza_int, na.rm=T),
    correa_votes_percent_int = mean(correa_votes_percent_int, na.rm=T),
    facilities_docnurse5_int_rate  = mean(facilities_docnurse5_int_rate, na.rm=T),
    doctors_nurses_int_rate = mean(doctors_nurses_int_rate, na.rm=T),
    pm25 = mean(pm25, na.rm=T)
  ) %>% 
  mutate(
    total_rate = total / population_int * 100000,
    asthma_rate = asthma / population_int * 100000,
    copd_rate = copd_icd10 / population_int * 100000,
    flu_rate = flu_icd10 / population_int * 100000,
    ab_rate = ab_icd10 / population_int * 100000,
    cancer_rate = cancer_icd10 / population_int * 100000,
    blood_rate = blood_icd10 / population_int * 100000,
    e_rate = e_icd10 / population_int * 100000,
    f_rate = f_icd10 / population_int * 100000,
    g_rate = g_icd10 / population_int * 100000,
    eye_rate = eye_icd10 / population_int * 100000,
    ear_rate = ear_icd10 / population_int * 100000,
    circulatory_rate = circulatory_icd10 / population_int * 100000,
    j_rate = j_icd10 / population_int * 100000,
    k_rate = k_icd10 / population_int * 100000,
    l_rate = l_icd10 / population_int * 100000,
    m_rate = m_icd10 / population_int * 100000,
    n_rate = n_icd10 / population_int * 100000,
    o_rate = o_icd10 / population_int * 100000,
    p_rate = p_icd10 / population_int * 100000,
    q_rate = q_icd10 / population_int * 100000,
    r_rate = r_icd10 / population_int * 100000,
    st_rate = st_icd10 / population_int * 100000,
  )


twomonth_main_count_adjusted <- 
  fepois(
    c(total, 
      j_icd10, 
      copd_icd10,
      flu_icd10,
      asthma,
      ab_icd10,
      cancer_icd10,
      f_icd10,
      g_icd10,
      circulatory_icd10,
      k_icd10,
      n_icd10,
      o_icd10,
      p_icd10,
      st_icd10
    ) ~
      offset(log(population_int)) +
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate +
      population_int +
      pm25 |
      canton_id + 
      quarter_year,
    combined_2mo %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_2mo$population_int
  )


twomonth_main_count_unadjusted <- 
  fepois(
    c(total, 
      j_icd10, 
      copd_icd10,
      flu_icd10,
      asthma,
      ab_icd10,
      cancer_icd10,
      f_icd10,
      g_icd10,
      circulatory_icd10,
      k_icd10,
      n_icd10,
      o_icd10,
      p_icd10,
      st_icd10
    ) ~
      offset(log(population_int)) +
      customers_percent_pec_1 |
      canton_id + 
      quarter_year,
    combined_2mo %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_2mo$population_int
  )

twomonth_main_lr_adjusted <- 
  feols(
    c(log(total_rate),
      log(j_rate),
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate),
      log(ab_rate),
      log(cancer_rate),
      log(f_rate),
      log(g_rate),
      log(circulatory_rate),
      log(k_rate),
      log(n_rate),
      log(o_rate),
      log(p_rate), 
      log(st_rate)
    ) ~
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate +
      population_int +
      pm25 |
      canton_id + 
      quarter_year,
    combined_2mo %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_2mo$population_int
  )


twomonth_main_lr_unadjusted <- 
  feols(
    c(log(total_rate),
      log(j_rate),
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate),
      log(ab_rate),
      log(cancer_rate),
      log(f_rate),
      log(g_rate),
      log(circulatory_rate),
      log(k_rate),
      log(n_rate),
      log(o_rate),
      log(p_rate), 
      log(st_rate)
    ) ~
      customers_percent_pec_1 |
      canton_id + 
      quarter_year,
    combined_2mo %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_2mo$population_int
  )

twomonth_main_count_adjusted_combined <- 
  tidy(twomonth_main_count_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(twomonth_main_count_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_count_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_count_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_count_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_count_adjusted[[6]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_adjusted[[7]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_adjusted[[8]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_adjusted[[9]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_adjusted[[10]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_adjusted[[11]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_adjusted[[12]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_adjusted[[13]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_adjusted[[14]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_adjusted[[15]], conf.int=T, conf.level = .995)[1,]
  ) %>% 
  mutate(icd_rate = outcomes) %>%
  left_join(xwalk %>% dplyr::select(icd_rate, outcome), by=c("icd_rate")) %>% 
  mutate(
    outcome_type = "Count",
    adjusted = "Adjusted",
    sample = "Full",
  )

twomonth_main_count_unadjusted_combined <- 
  tidy(twomonth_main_count_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(twomonth_main_count_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_count_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_count_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_count_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_count_unadjusted[[6]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_unadjusted[[7]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_unadjusted[[8]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_unadjusted[[9]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_unadjusted[[10]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_unadjusted[[11]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_unadjusted[[12]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_unadjusted[[13]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_unadjusted[[14]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_count_unadjusted[[15]], conf.int=T, conf.level = .995)[1,]
  ) %>% 
  mutate(icd_rate = outcomes) %>% 
  left_join(xwalk %>% dplyr::select(icd_rate, outcome), by=c("icd_rate")) %>% 
  mutate(
    outcome_type = "Count",
    adjusted = "Unadjusted",
    sample = "Full",
  )

twomonth_main_full_combined <- 
  twomonth_main_count_adjusted_combined %>% 
  bind_rows(
    twomonth_main_count_unadjusted_combined
  )

twomonth_count_fig <- ggplot(
  twomonth_main_full_combined,
  aes(y=outcome, x=exp(estimate), 
      xmin=exp(conf.low), xmax=exp(conf.high),
      shape=adjusted) 
) + 
  geom_pointrange(position=position_dodge(width=0.5)) +
  # scale_y_discrete(limits=rev(outcome_limits)) + 
  scale_x_continuous(trans="log", breaks=c(0.95, 0.96, .97, .98, .99, 1, 1.01, 1.02, 1.03)) +
  coord_cartesian(xlim=c(0.95, 1.03)) + 
  geom_vline(xintercept=1) + 
  # coord_cartesian(xlim=c(-.028, .015)) +
  theme_classic() + 
  ylab("") + 
  xlab("Incidence rate ratio per 1 p.p. increase in PEC enrollment") + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

twomonth_main_lr_adjusted_combined <- 
  tidy(twomonth_main_lr_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(twomonth_main_lr_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_lr_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_lr_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_lr_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_lr_adjusted[[6]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_adjusted[[7]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_adjusted[[8]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_adjusted[[9]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_adjusted[[10]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_adjusted[[11]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_adjusted[[12]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_adjusted[[13]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_adjusted[[14]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_adjusted[[15]], conf.int=T, conf.level = .995)[1,]
  ) %>% 
  mutate(icd_rate = outcomes) %>% 
  left_join(xwalk %>% dplyr::select(icd_rate, outcome), by=c("icd_rate")) %>% 
  mutate(
    outcome_type = "lr",
    adjusted = "Adjusted",
    sample = "Full",
  )

twomonth_main_lr_unadjusted_combined <- 
  tidy(twomonth_main_lr_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(twomonth_main_lr_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_lr_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_lr_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_lr_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(twomonth_main_lr_unadjusted[[6]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_unadjusted[[7]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_unadjusted[[8]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_unadjusted[[9]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_unadjusted[[10]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_unadjusted[[11]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_unadjusted[[12]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_unadjusted[[13]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_unadjusted[[14]], conf.int=T, conf.level = .995)[1,],
    tidy(twomonth_main_lr_unadjusted[[15]], conf.int=T, conf.level = .995)[1,]
  ) %>% 
  mutate(icd_rate = outcomes) %>% 
  left_join(xwalk %>% dplyr::select(icd_rate, outcome), by=c("icd_rate")) %>% 
  mutate(
    outcome_type = "lr",
    adjusted = "Unadjusted",
    sample = "Full",
  )

twomonth_main_lr_full_combined <- 
  twomonth_main_lr_adjusted_combined %>% 
  bind_rows(
    twomonth_main_lr_unadjusted_combined
  )

twomonth_lr_fig <- 
  ggplot(
    twomonth_main_lr_full_combined,
    aes(y=outcome, x=estimate, 
        xmin=conf.low, xmax=conf.high,
        shape=adjusted)) + 
  geom_pointrange(position=position_dodge(width=0.5)) +
  # scale_y_discrete(limits=rev(outcome_limits)) + 
  # ggtitle("Data aggregated to two month periods") + 
  scale_x_continuous(
    trans=scales::pseudo_log_trans(),
    breaks=c(-.025, -.01, -.005, 0,  .005, .01, .025),
    labels=scales::percent_format()
  ) + 
  geom_vline(xintercept=0) + 
  coord_cartesian(xlim=c(-.028, .015)) +
  theme_classic() + 
  ylab("") + 
  xlab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment") + 
  theme(
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

twomonth_lr_fig

twomonth_fig_combined <- 
  plot_grid(
    twomonth_lr_fig,
    twomonth_count_fig,
    nrow=2,
    rel_heights = c(1, 1.05)
  )


cowplot::ggsave2(
  "~/Desktop/Ecuador Electricity/Figures/twomonth_fig_combined_june_v2.pdf",
  plot = twomonth_fig_combined,
  dpi = 300,
  height = 300,
  width = 250,
  unit = "mm"
)




# Province-level regression -------

prov_combined_df <- 
  combined %>% 
  dplyr::group_by(province, month_year) %>% 
  dplyr::summarize(
    total = sum(total, na.rm=T),
    population_int = sum(population_int, na.rm=T),
    total_customers = sum(total_customers, na.rm=T),
    pec_customers_na = sum(pec_customers_na, na.rm=T),
    kwh_sub = sum(kwh_subsidized_pec_peccustomers_na, na.rm=T),
    bdh_int = weighted.mean(bdh_int, na.rm=T,wts=population_int),
    epobreza_int = weighted.mean(epobreza_int, na.rm=T,wts=population_int),
    correa_votes_percent_int = weighted.mean(correa_votes_percent_int, na.rm=T,wts=population_int),
    facilities_docnurse5_int_rate = weighted.mean(facilities_docnurse5_int_rate, na.rm=T,wts=population_int),
    doctors_nurses_int_rate = weighted.mean(doctors_nurses_int_rate, na.rm=T,wts=population_int),
    pm25 = weighted.mean(pm25, na.rm=T, wts=population_int)
  ) %>% 
  mutate(
    customers_percent_pec = pec_customers_na / total_customers,
    customers_percent_pec_1 = customers_percent_pec*100,
    kwh_sub_per = kwh_sub / pec_customers_na,
    total_rate = total / population_int * 100000
  )

prov_lr <- feols(
  log(total_rate) ~
    customers_percent_pec_1 |
    province +
    month_year,
  prov_combined_df,
  weights=prov_combined_df$population_int
)

prov_lr_adjusted <- feols(
  log(total_rate) ~
    customers_percent_pec_1 +
    bdh_int + 
    epobreza_int + 
    correa_votes_percent_int + 
    facilities_docnurse5_int_rate +
    doctors_nurses_int_rate +
    population_int + 
    pm25 |
    province +
    month_year,
  prov_combined_df,
  weights=prov_combined_df$population_int
)

prov_pois_adjusted <- fepois(
  total ~
    offset(log(population_int)) + 
    customers_percent_pec +
    bdh_int + 
    epobreza_int + 
    correa_votes_percent_int + 
    facilities_docnurse5_int_rate +
    doctors_nurses_int_rate +
    population_int + 
    pm25 |
    province +
    month_year,
  prov_combined_df,
  weights=prov_combined_df$population_int
)


prov_pois <- fepois(
  total ~
    offset(log(population_int)) + 
    customers_percent_pec_1 |
    province +
    month_year,
  prov_combined_df,
  weights=prov_combined_df$population_int
)

etable(prov_lr, prov_lr_adjusted, prov_pois, prov_pois_adjusted, tex=T)

provs <- combined %>% 
  distinct(province) %>% 
  filter(!is.na(province)) %>% 
  filter(province!="Orellana")

prov_df <- combined %>% 
  filter(province == provs$province[1])

prov_main_lr_adjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate),
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate),
      log(ab_rate),
      log(cancer_rate),
      log(f_rate),
      log(g_rate),
      log(circulatory_rate),
      log(k_rate),
      log(n_rate),
      log(o_rate),
      log(p_rate),
      log(st_rate)
    ) ~
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate +
      population_int + 
      pm25 |
      canton_id + 
      month_year,
    prov_df %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=prov_df$population_int
  )

prov_main_lr_adjusted_combined <- 
  tidy(prov_main_lr_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(prov_main_lr_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_adjusted[[15]], conf.int=T, conf.level = .95)[1,]
  )  %>% 
  mutate(province = provs$province[1]) %>% 
  mutate(icd_code = outcomes)

prov_main_lr_adjusted_combined_full <- prov_main_lr_adjusted_combined
  
prov_main_lr_unadjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate),
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate),
      log(ab_rate),
      log(cancer_rate),
      log(f_rate),
      log(g_rate),
      log(circulatory_rate),
      log(k_rate),
      log(n_rate),
      log(o_rate),
      log(p_rate),
      log(st_rate)
    ) ~
      customers_percent_pec_1 |
      canton_id + 
      month_year,
    prov_df %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=prov_df$population_int
  )

prov_main_lr_unadjusted_combined <- 
  tidy(prov_main_lr_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(prov_main_lr_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(prov_main_lr_unadjusted[[15]], conf.int=T, conf.level = .95)[1,]
  )  %>% 
  mutate(province = provs$province[1]) %>% 
  mutate(icd_code = outcomes)

prov_main_lr_unadjusted_combined_full <- prov_main_lr_unadjusted_combined

for (i in 2:23) {

  prov_df <- combined %>% 
    filter(province == provs$province[i])
  
  prov_main_lr_adjusted <- 
    feols(
      c(log(total_rate), 
        log(j_rate),
        log(copd_rate),
        log(flu_rate),
        log(asthma_rate),
        log(ab_rate),
        log(cancer_rate),
        log(f_rate),
        log(g_rate),
        log(circulatory_rate),
        log(k_rate),
        log(n_rate),
        log(o_rate),
        log(p_rate),
        log(st_rate)
      ) ~
        customers_percent_pec_1 +
        bdh_int + 
        epobreza_int + 
        correa_votes_percent_int + 
        facilities_docnurse5_int_rate + 
        doctors_nurses_int_rate + 
        population_int + 
        pm25 |
        canton_id + 
        month_year,
      prov_df %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=prov_df$population_int
    )
  
  prov_main_lr_adjusted_combined <- 
    tidy(prov_main_lr_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
    bind_rows(
      tidy(prov_main_lr_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[6]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[7]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[8]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[9]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[10]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[11]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[12]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[13]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[14]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_adjusted[[15]], conf.int=T, conf.level = .95)[1,]
    )  %>% 
    mutate(province = provs$province.x[i]) %>% 
    mutate(icd_code = outcomes)
  
  prov_main_lr_adjusted_combined_full <-
    prov_main_lr_adjusted_combined_full %>% 
    bind_rows(prov_main_lr_adjusted_combined) 
  
  prov_main_lr_unadjusted <- 
    feols(
      c(log(total_rate), 
        log(j_rate),
        log(copd_rate),
        log(flu_rate),
        log(asthma_rate),
        log(ab_rate),
        log(cancer_rate),
        log(f_rate),
        log(g_rate),
        log(circulatory_rate),
        log(k_rate),
        log(n_rate),
        log(o_rate),
        log(p_rate),
        log(st_rate)
      ) ~
        customers_percent_pec_1 |
        canton_id + 
        month_year,
      prov_df %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=prov_df$population_int
    )
  
  prov_main_lr_unadjusted_combined <- 
    tidy(prov_main_lr_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
    bind_rows(
      tidy(prov_main_lr_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[6]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[7]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[8]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[9]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[10]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[11]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[12]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[13]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[14]], conf.int=T, conf.level = .95)[1,],
      tidy(prov_main_lr_unadjusted[[15]], conf.int=T, conf.level = .95)[1,]
    )  %>% 
    mutate(province = provs$province[i]) %>% 
    mutate(icd_code = outcomes)
  
  prov_main_lr_unadjusted_combined_full <-
    prov_main_lr_unadjusted_combined_full %>% 
    bind_rows(prov_main_lr_unadjusted_combined)
  
  print(provs$province[i])
  
}

prov_main_df <- prov_main_lr_unadjusted_combined_full %>% 
  mutate(adjusted = "Unadjusted") %>% 
  bind_rows(prov_main_lr_adjusted_combined_full%>% 
              mutate(adjusted = "Adjusted") )



ggplot(prov_main_df %>% filter(adjusted=="Adjusted"), 
         # filter(province=="Galapagos"),
       aes(y=province, x=estimate, xmin=conf.low, 
           xmax=conf.high, group=adjusted, shape=adjusted)) + 
  geom_pointrange(position=position_dodge(width=0.5)) + 
  # scale_x_continous() +
  theme_classic() + 
  coord_cartesian(xlim=c(-0.1, 0.1)) + 
  facet_wrap(.~icd_code)


main_lr_unadjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate),
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate)
    ) ~
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined_nog %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_nog$population_int
  )


# Interact PEC kWh and enrollment -------

combined$pec_kwh_subsidized <- combined$kwh_subsidized_pec / combined$pec_customers_na
combined <- combined %>% 
  mutate(pec_kwh_subsidized = ifelse(is.na(pec_kwh_subsidized), 0, pec_kwh_subsidized))

combined_reg <- combined %>% 
  mutate(customers_percent_pec_1 = customers_percent_pec*100) %>% 
  mutate(log_total_rate = log(total_rate)) %>% 
  filter(!is.na(pec_kwh_subsidized)) %>% 
  filter(!is.na(customers_percent_pec_1)) %>% 
  filter(!is.na(log_total_rate) &
           !is.na(canton_id) &
           !is.na(month_year) &
           !is.na(population_int) &
           pec_kwh_subsidized!=Inf) %>% 
  mutate(pec_kwh_subsidized = ifelse(pec_kwh_subsidized>100, 100, pec_kwh_subsidized))

main_lr_interact <- 
  feols(
    log_total_rate ~
      customers_percent_pec_1 +
      pec_kwh_subsidized +
      customers_percent_pec_1*pec_kwh_subsidized +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate +
      population_int + 
      pm25 |
      canton_id + 
      month_year,
    combined_reg,
    weights=combined_reg$population_int
  )


main_lr_interact_fig <- 
  marginaleffects::plot_cme(
  main_lr_interact, 
  condition="pec_kwh_subsidized",
  effect="customers_percent_pec_1"
) +
  xlab("PEC-related kWh subsidized per customer") +
  ylab("Marignal effect of 1pp increase in PEC enrollment on hospitalization rate") + 
  scale_y_continuous(labels=scales::percent_format()) + 
  theme_classic() +
  coord_cartesian() +
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title =element_text(size = 6, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 6, color = "black", face="bold"),
    plot.subtitle = element_text(size = 6, color = "black"),
    axis.text.x = element_text(size = 6, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 


main_lr_interact_hist <- 
  ggplot(combined_reg %>% 
           filter(month_year>=ymd("2015-01-01")), 
         aes(x=pec_kwh_subsidized)) + 
  geom_histogram(bins=100) + 
  theme_classic() +
  coord_cartesian() +
  xlab("PEC-related kWh subsidized per customer") + ylab("canton-month observations") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title =element_text(size = 6, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 6, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

main_lr_interact_fig_combined <- 
  plot_grid(
  main_lr_interact_fig, 
  main_lr_interact_hist, 
  align="hv",
  nrow=2,
  rel_heights = c(1, 0.4)
)


# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/main_lr_interact_fig_combined_june_v2.pdf",
#   plot = main_lr_interact_fig_combined,
#   dpi = 300,
#   height = 150,
#   width = 150,
#   unit = "mm"
# )




# New idea on emblematic canton hosp rate trajectories -------

combined_2019_enroll <- 
  combined %>% 
  filter(month_year>=ymd("2019-06-01") & month_year<ymd("2020-03-01")) %>% 
  group_by(canton_id) %>% 
  summarize(customers_percent_pec=mean(customers_percent_pec),
            n=n()) %>% 
  mutate(customers_percent_pec_low = ifelse(customers_percent_pec<pec_15, 1, 0),
         customers_percent_pec_high = ifelse(customers_percent_pec>=pec_85, 1, 0))


pec_50 <- quantile(combined_2019_enroll$customers_percent_pec, na.rm=T, probs=.50)

pec_15 <- quantile(combined_2019_enroll$customers_percent_pec, na.rm=T, probs=.15)
pec_85 <- quantile(combined_2019_enroll$customers_percent_pec, na.rm=T, probs=.85)

pec_25 <- quantile(combined_2019_enroll$customers_percent_pec, na.rm=T, probs=.25)
pec_75 <- quantile(combined_2019_enroll$customers_percent_pec, na.rm=T, probs=.75)

combined_2019_enroll_low <- 
  combined_2019_enroll %>% 
  ungroup() %>% 
  filter(customers_percent_pec_low==1) %>% 
  dplyr::select(canton_id)

combined_low <- 
  combined %>% 
  ungroup() %>% 
  filter(canton_id %in% combined_2019_enroll_low$canton_id)

combined_2019_enroll_high <- 
  combined_2019_enroll %>% 
  ungroup() %>% 
  filter(customers_percent_pec_high==1) %>% 
  dplyr::select(canton_id)

combined_high <- 
  combined %>% 
  ungroup() %>% 
  filter(canton_id %in% combined_2019_enroll_high$canton_id)

combined_high_low_full <- 
  combined_high %>% 
  mutate(enroll_high_low = "High") %>% 
  bind_rows(combined_low %>% 
              mutate(enroll_high_low = "Low")) %>% 
  filter(month_year<ymd("2020-03-01")) %>% 
  mutate(kwh_subsidized_pec_customers = kwh_subsidized_pec / pec_customers_na)  %>% 
  mutate(enroll_high_low = ifelse(enroll_high_low=="High", 1, 0))

combined_high_low <- 
  combined_high %>% 
  mutate(enroll_high_low = "High") %>% 
  bind_rows(combined_low %>% 
              mutate(enroll_high_low = "Low")) %>% 
  filter(month_year<ymd("2020-03-01")) %>% 
  mutate(kwh_subsidized_pec_customers = kwh_subsidized_pec / pec_customers_na) %>% 
  group_by(month_year, enroll_high_low) %>% 
  summarize(
    pec_customers = sum(pec_customers_na, na.rm=T),
    total_customers = sum(total_customers, na.rm=T),
    
    customers_percent_pec_mean = mean(customers_percent_pec, wts=population_int, na.rm=T),
    customers_percent_pec_median = median(customers_percent_pec, na.rm=T),
    
    total_visits = sum(total, na.rm=T),
    total_rate_mean = mean(total_rate, na.rm=T),
    total_rate_median=median(total_rate, na.rm=T),
    j_rate_mean = mean(j_rate, na.rm=T),
    j_rate_median=median(j_rate, na.rm=T),
    copd_rate_mean = mean(copd_rate, na.rm=T),
    copd_rate_median=median(copd_rate, na.rm=T),
    
    kwh_sub_total = sum(kwh_subsidized_pec, na.rm=T),
    kwh_sub_mean = mean(kwh_subsidized_pec_customers, na.rm=T),
    kwh_sub_median = median(kwh_subsidized_pec_customers, na.rm=T),
    
    ingpc_mean = mean(ingpc_int, na.rm=T),
    ingpc_median = median(ingpc_int, na.rm=T),
    bdh_mean = mean(bdh_int, na.rm=T),
    bdh_median = median(bdh_int, na.rm=T),
    correa_mean = mean(correa_votes_percent_int, na.rm=T),
    correa_median = median(correa_votes_percent_int, na.rm=T),
    
    doctors_nurses_sum = sum(doctors_int+nurses_int, na.rm=T),
    # doctors_nurses_median = median(doctors_nurses_int_rate, na.rm=T),
    
    facilities_sum = sum(facilities_int, na.rm=T),
    facilities_median = median(facilities_int_rate, na.rm=T),
    
    population_total = sum(population_int, na.rm=T),
    population_mean = mean(population_int, na.rm=T),
    population_median = median(population_int, na.rm=T),
    
    pm25_mean = mean(pm25, na.rm=T)
  )

combined_high_low_enroll <-
  ggplot() +
  geom_line(data=combined_high_low, 
            aes(x=month_year, y=(pec_customers/total_customers), color=enroll_high_low), linewidth=1.03) + 
  # geom_smooth(data=combined_high_low %>% 
  #             filter(month_year>=ymd("2015-01-01")), 
  #           aes(x=month_year, y=(pec_customers/total_customers), color=enroll_high_low)) + 
  # geom_point(data=combined_high_low, 
  #           aes(x=month_year, y=(pec_customers/total_customers), color=enroll_high_low)) + 
  # geom_line(data=combined_high_low, 
  #           aes(x=month_year, y=customers_percent_pec_median, color=enroll_high_low), linetype="dotted") + 
  # geom_line(data=combined_high_low, 
  #           aes(x=month_year, y=customers_percent_pec_mean.wt, color=enroll_high_low), linetype="dotted") + 
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  scale_y_continuous(labels=scales::percent_format()) + 
  ggtitle("PEC enrollment") + 
  xlab("") + ylab("") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 


combined_high_low_kwh <-
  ggplot() +
  geom_smooth(data=combined_high_low %>% 
               filter(pec_customers>=1000), 
             aes(x=month_year, y=kwh_sub_total / pec_customers, color=enroll_high_low)) + 
  
  geom_point(data=combined_high_low %>% 
              filter(pec_customers>=1000), 
            aes(x=month_year, y=kwh_sub_total / pec_customers, color=enroll_high_low), size=0.4, alpha=0.5) +
  # geom_line(data=combined_high_low  %>% 
  #             filter(pec_customers>=1000), 
  #           aes(x=month_year, y=kwh_sub_median, color=enroll_high_low), linetype="dotted") + 
  scale_color_aaas() + 
  theme_classic() +
  # coord_cartesian(xlim=c(ymd("2012-01-01"), ymd("2021-01-01"))) +
  # scale_y_continuous(labels=scales::percent_format()) + 
  ggtitle("PEC kWh subsidized per customer") + 
  xlab("") + ylab("") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

pt_total_reg <- feols(total_rate ~ enroll_high_low*as.numeric(month_year) |  canton_id, combined_high_low_full %>% filter(month_year<ymd("2015-01-01")))

coef <- as.character(signif(tidy(pt_total_reg)[2,2], digits=2))
pvalue <- as.character(signif(tidy(pt_total_reg)[2,5], digits=2))

combined_high_low_total_rate <-
  ggplot() +
  geom_point(data=combined_high_low,
              aes(x=month_year, y=total_rate_mean, color=enroll_high_low), size=0.4, alpha=0.5) +
  geom_smooth(data=combined_high_low %>%
                filter(month_year<ymd("2015-01-01")),
            aes(x=month_year, y=total_rate_mean, color=enroll_high_low), method="lm") +

  geom_smooth(data=combined_high_low %>%
                filter(month_year>=ymd("2015-01-01") &
                         month_year<=ymd("2020-01-01")),
              aes(x=month_year, y=total_rate_mean, color=enroll_high_low), method="lm") +
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  ggtitle("All-cause hospitalization rate",
          subtitle=paste0("pre-PEC trend difference: beta=", coef, " visits per month, P=",pvalue)) + 
  xlab("") + ylab("visits per 100,000") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 



pt_j_reg <- feols(j_rate ~ enroll_high_low*as.numeric(month_year) |  canton_id, combined_high_low_full %>% filter(month_year<ymd("2015-01-01")))

coef <- as.character(signif(tidy(pt_j_reg)[2,2], digits=2))
pvalue <- as.character(signif(tidy(pt_j_reg)[2,5], digits=2))


combined_high_low_j_rate <-
  ggplot() +
  geom_point(data=combined_high_low,
             aes(x=month_year, y=j_rate_mean, color=enroll_high_low), size=0.4, alpha=0.5) +
  geom_smooth(data=combined_high_low %>%
                filter(month_year<ymd("2015-01-01")),
              aes(x=month_year, y=j_rate_mean, color=enroll_high_low), method="lm") +
  geom_smooth(data=combined_high_low %>%
                filter(month_year>=ymd("2015-01-01") &
                         month_year<ymd("2020-03-01")),
              aes(x=month_year, y=j_rate_mean, color=enroll_high_low), method="lm") +
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  # scale_y_continuous(labels=scales::percent_format()) + 
  ggtitle("Respiratory-related hospitalization rate",
          subtitle=paste0("pre-PEC trend difference: beta=", coef, " visits per month, P=",pvalue)) + 
  xlab("") + ylab("visits per 100,000") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

pt_copd_reg <- feols(copd_rate ~ enroll_high_low*as.numeric(month_year) |  canton_id, combined_high_low_full %>% filter(month_year<ymd("2015-01-01")))

coef <- as.character(signif(tidy(pt_copd_reg)[2,2], digits=2))
pvalue <- as.character(signif(tidy(pt_copd_reg)[2,5], digits=2))


combined_high_low_copd_rate <-
  ggplot() +
  geom_point(data=combined_high_low,
             aes(x=month_year, y=copd_rate_mean, color=enroll_high_low), size=0.4, alpha=0.5) +
  geom_smooth(data=combined_high_low %>%
                filter(month_year<ymd("2015-01-01")),
              aes(x=month_year, y=copd_rate_mean, color=enroll_high_low), method="lm") +
  geom_smooth(data=combined_high_low %>%
                filter(month_year>=ymd("2015-01-01") &
                         month_year<ymd("2020-03-01")),
              aes(x=month_year, y=copd_rate_mean, color=enroll_high_low), method="lm") +
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  # scale_y_continuous(labels=scales::percent_format()) + 
  ggtitle("COPD hospitalization rate",
          subtitle=paste0("pre-PEC trend difference: beta=", coef, " visits per month, P=",pvalue)) + 
  xlab("") + ylab("visits per 100,000") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

pt_ingpc_reg <- 
  feols(ingpc_int ~ enroll_high_low*as.numeric(month_year) |  canton_id, combined_high_low_full %>% filter(month_year<ymd("2015-01-01")))

coef <- as.character(signif(tidy(pt_ingpc_reg)[2,2], digits=2))
pvalue <- as.character(signif(tidy(pt_ingpc_reg)[2,5], digits=2))



combined_high_low_ingpc <-
  ggplot() +
  geom_point(data=combined_high_low,
             aes(x=month_year, y=ingpc_mean, color=enroll_high_low), size=0.4, alpha=0.5) +
  geom_smooth(data=combined_high_low %>%
                filter(month_year<ymd("2015-01-01")),
              aes(x=month_year, y=ingpc_mean, color=enroll_high_low), method="lm") +
  geom_smooth(data=combined_high_low %>%
                filter(month_year>=ymd("2015-01-01") &
                         month_year<ymd("2020-03-01")),
              aes(x=month_year, y=ingpc_mean, color=enroll_high_low), method="lm") +
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  # scale_y_continuous(labels=scales::percent_format()) + 
  ggtitle("Income per capita",
          subtitle=paste0("pre-PEC trend difference: beta=", coef, " USD per month, P=",pvalue)) + 
  xlab("") + ylab("USD") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

pt_bdh_reg <- 
  feols(bdh_int ~ enroll_high_low*as.numeric(month_year) | 
          canton_id, combined_high_low_full %>% 
          filter(month_year<ymd("2015-01-01")))

coef <- as.character(signif(tidy(pt_bdh_reg)[2,2], digits=2)*100)
pvalue <- as.character(signif(tidy(pt_bdh_reg)[2,5], digits=2))



combined_high_low_bdh <-
  ggplot() +
  geom_point(data=combined_high_low,
             aes(x=month_year, y=bdh_mean, color=enroll_high_low), size=0.4, alpha=0.5) +
  geom_smooth(data=combined_high_low %>%
                filter(month_year<ymd("2015-01-01")),
              aes(x=month_year, y=bdh_mean, color=enroll_high_low), method="lm") +
  geom_smooth(data=combined_high_low %>%
                filter(month_year>=ymd("2015-01-01") &
                         month_year<ymd("2020-03-01")),
              aes(x=month_year, y=bdh_mean, color=enroll_high_low), method="lm") +
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  # scale_y_continuous(labels=scales::percent_format()) + 
  ggtitle("Poverty alleviation program involvement",
          subtitle=paste0("pre-PEC trend difference: beta=", coef, "% per month, P=",pvalue)) + 
  xlab("") + ylab("share of adults") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 


pt_vote_reg <- 
  feols(correa_votes_percent_int ~ 
        enroll_high_low*as.numeric(month_year) |  
        canton_id, 
      combined_high_low_full %>% 
        ungroup() %>% 
        filter(month_year<ymd("2015-01-01")))

coef <- as.character(signif(tidy(pt_vote_reg)[2,2], digits=2)*100)
pvalue <- as.character(signif(tidy(pt_vote_reg)[2,5], digits=2))


combined_high_low_correa <-
  ggplot() +
  geom_point(data=combined_high_low,
             aes(x=month_year, y=correa_mean, color=enroll_high_low), size=0.4, alpha=0.5) +
  geom_smooth(data=combined_high_low %>%
                filter(month_year<ymd("2015-01-01")),
              aes(x=month_year, y=correa_mean, color=enroll_high_low), method="lm") +
  geom_smooth(data=combined_high_low %>%
                filter(month_year>=ymd("2015-01-01") &
                         month_year<ymd("2020-03-01")),
              aes(x=month_year, y=correa_mean, color=enroll_high_low), method="lm") +
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  scale_y_continuous(breaks=c(0.3, 0.4, 0.5, 0.6)) +
  ggtitle("Voting patterns",
          subtitle=paste0("pre-PEC trend difference: beta=", coef, "% per month, P=",pvalue)) + 
  xlab("") + ylab("fraction of votes") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

pt_doc_nurse_reg <- 
  feols(((doctors_int+nurses_int)/population_int) ~ 
        enroll_high_low*as.numeric(month_year) |  
        canton_id, 
      combined_high_low_full %>% 
        ungroup() %>% 
        filter(month_year<ymd("2015-01-01")))

coef <- as.character(signif(tidy(pt_doc_nurse_reg)[2,2], digits=2))
pvalue <- as.character(signif(tidy(pt_doc_nurse_reg)[2,5], digits=2))


combined_high_low_doctors_nurses <-
  ggplot() +
  geom_point(data=combined_high_low,
             aes(x=month_year, y=(doctors_nurses_sum / population_total), color=enroll_high_low), size=0.4, alpha=0.5) +
  geom_smooth(data=combined_high_low %>%
                filter(month_year<ymd("2015-01-01")),
              aes(x=month_year, y=(doctors_nurses_sum / population_total), color=enroll_high_low), method="lm") +
  geom_smooth(data=combined_high_low %>%
                filter(month_year>=ymd("2015-01-01") &
                         month_year<ymd("2020-03-01")),
              aes(x=month_year, y=(doctors_nurses_sum / population_total), color=enroll_high_low), method="lm") +
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  # scale_y_continuous(breaks=c(0.3, 0.4, 0.5, 0.6)) +
  ggtitle("Doctors and nurses per capita",
          subtitle=paste0("pre-PEC trend difference: beta=", coef, " professionals per month, P=",pvalue)) + 
  xlab("") + ylab("Doctors and nurses per capita") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 


pt_facilities_reg <- feols(((facilities_int)/population_int) ~ 
        enroll_high_low*as.numeric(month_year) |  
        canton_id, 
      combined_high_low_full %>% 
        ungroup() %>% 
        filter(month_year<ymd("2015-01-01")))

coef <- as.character(signif(tidy(pt_facilities_reg)[2,2], digits=2))
pvalue <- as.character(signif(tidy(pt_facilities_reg)[2,5], digits=2))

combined_high_low_facilities <-
  ggplot() +
  geom_point(data=combined_high_low,
             aes(x=month_year, y=(facilities_sum / population_total), color=enroll_high_low), size=0.4, alpha=0.5) +
  geom_smooth(data=combined_high_low %>%
                filter(month_year<ymd("2015-01-01")),
              aes(x=month_year, y=(facilities_sum / population_total), color=enroll_high_low), method="lm") +
  geom_smooth(data=combined_high_low %>%
                filter(month_year>=ymd("2015-01-01") &
                         month_year<ymd("2020-03-01")),
              aes(x=month_year, y=(facilities_sum / population_total), color=enroll_high_low), method="lm") +
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  # scale_y_continuous(labels=scales::percent_format()) +
  ggtitle("Health care facilities per capita",
          subtitle=paste0("pre-PEC trend difference: beta=", coef, " facilities per month, P=",pvalue)) + 
  xlab("") + ylab("facilities per capita") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

pt_pop_reg <-
  feols(population_int ~ 
        enroll_high_low*as.numeric(month_year) |  
        canton_id, 
      combined_high_low_full %>% 
        ungroup() %>% 
        filter(month_year<ymd("2015-01-01")))

coef <- as.character(signif(tidy(pt_pop_reg)[2,2], digits=2))
pvalue <- as.character(signif(tidy(pt_pop_reg)[2,5], digits=2))


combined_high_low_pop <-
  ggplot() +
  geom_point(data=combined_high_low,
             aes(x=month_year, y=population_total, color=enroll_high_low), size=0.4, alpha=0.5) +
  geom_smooth(data=combined_high_low %>%
                filter(month_year<ymd("2015-01-01")),
              aes(x=month_year, y=population_total, color=enroll_high_low), method="lm") +
  geom_smooth(data=combined_high_low %>%
                filter(month_year>=ymd("2015-01-01") &
                         month_year<ymd("2020-03-01")),
              aes(x=month_year, y=population_total, color=enroll_high_low), method="lm") +
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  ggtitle("Canton-level average population",
          subtitle=paste0("pre-PEC trend difference: beta=", coef, " people per month, P=",pvalue)) + 
  xlab("") + ylab("people") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 


pt_pm25_reg <-
  feols(pm25 ~ 
          enroll_high_low*as.numeric(month_year) |  
          canton_id, 
        combined_high_low_full %>% 
          ungroup() %>% 
          filter(month_year<ymd("2015-01-01")))

coef <- as.character(signif(tidy(pt_pm25_reg)[2,2], digits=2))
pvalue <- as.character(signif(tidy(pt_pm25_reg)[2,5], digits=2))


combined_high_low_pm25 <-
  ggplot() +
  geom_point(data=combined_high_low,
             aes(x=month_year, y=pm25_mean, color=enroll_high_low), size=0.4, alpha=0.5) +
  geom_smooth(data=combined_high_low %>%
                filter(month_year<ymd("2015-01-01")),
              aes(x=month_year, y=pm25_mean, color=enroll_high_low), method="lm") +
  geom_smooth(data=combined_high_low %>%
                filter(month_year>=ymd("2015-01-01") &
                         month_year<ymd("2020-03-01")),
              aes(x=month_year, y=pm25_mean, color=enroll_high_low), method="lm") +
  scale_color_aaas() + 
  theme_classic() +
  coord_cartesian() +
  ggtitle("Canton-level average PM2.5 concentrations",
          subtitle=paste0("pre-PEC trend difference: beta=", coef, " ug/m3, P=",pvalue)) + 
  xlab("") + ylab("ug/m3") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

combined_trends_fig <- 
  plot_grid(
    combined_high_low_enroll,
    combined_high_low_kwh,
    combined_high_low_total_rate,
    combined_high_low_j_rate,
    combined_high_low_copd_rate,
    nrow=5,
    labels = "AUTO",
    align="v"
  )


combined_trends_covs_fig <- 
  plot_grid(
    combined_high_low_bdh,
    combined_high_low_ingpc,
    combined_high_low_correa,
    combined_high_low_facilities,
    combined_high_low_doctors_nurses,
    combined_high_low_pop,
    combined_high_low_pm25,
    nrow=3,
    align="hv",
    labels = c("F", "G", "H", "I", "J", "K", "L")
  )

combined_trends_all <-
  plot_grid(
    combined_trends_fig,
    combined_trends_covs_fig,
    nrow=2
  )

cowplot::ggsave2(
  "~/Desktop/Ecuador Electricity/Figures/combined_trends_fig_june.pdf",
  plot = combined_trends_fig,
  dpi = 300,
  height = 250,
  width = 250,
  unit = "mm"
)

cowplot::ggsave2(
  "~/Desktop/Ecuador Electricity/Figures/combined_trends_all_june.pdf",
  plot = combined_trends_all,
  dpi = 300,
  height = 450,
  width = 300,
  unit = "mm"
)


# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/combined_trends_covs_fig_1585_june.pdf",
#   plot = combined_trends_covs_fig,
#   dpi = 300,
#   height = 150,
#   width = 250,
#   unit = "mm"
# )


# Province level regression -----

combined_prov <- 
  combined %>% 
  group_by(province, month_year) %>% 
  summarize(
    total = sum(total, na.rm=T),
    asthma = sum(asthma, na.rm=T),
    copd_icd10 = sum(copd_icd10, na.rm=T),
    flu_icd10 = sum(flu_icd10, na.rm=T),
    ab_icd10 = sum(ab_icd10, na.rm=T),
    cancer_icd10 = sum(cancer_icd10, na.rm=T),
    blood_icd10 = sum(blood_icd10, na.rm=T),
    e_icd10 = sum(e_icd10, na.rm=T),
    f_icd10 = sum(f_icd10, na.rm=T),
    g_icd10 = sum(g_icd10, na.rm=T),
    eye_icd10 = sum(eye_icd10, na.rm=T),
    ear_icd10 = sum(ear_icd10, na.rm=T),
    circulatory_icd10 = sum(circulatory_icd10, na.rm=T),
    j_icd10 = sum(j_icd10, na.rm=T),
    k_icd10 = sum(k_icd10, na.rm=T),
    l_icd10 = sum(l_icd10, na.rm=T),
    m_icd10 = sum(m_icd10, na.rm=T),
    n_icd10 = sum(n_icd10, na.rm=T),
    o_icd10 = sum(o_icd10, na.rm=T),
    p_icd10 = sum(p_icd10, na.rm=T),
    q_icd10 = sum(q_icd10, na.rm=T),
    r_icd10 = sum(r_icd10, na.rm=T),
    st_icd10 = sum(st_icd10, na.rm=T),
    
    customers_percent_pec = mean(customers_percent_pec, na.rm=T),
    population_int = sum(population_int, na.rm=T),
    
    bdh_int = mean(bdh_int, na.rm=T),
    epobreza_int  = mean(epobreza_int, na.rm=T),
    correa_votes_percent_int = mean(correa_votes_percent_int, na.rm=T),
    facilities_docnurse5_int_rate  = mean(facilities_docnurse5_int_rate, na.rm=T),
    doctors_nurses_int_rate = mean(doctors_nurses_int_rate, na.rm=T),
  ) %>% 
  mutate(
    total_rate = total / population_int * 100000,
    asthma_rate = asthma / population_int * 100000,
    copd_rate = copd_icd10 / population_int * 100000,
    flu_rate = flu_icd10 / population_int * 100000,
    ab_rate = ab_icd10 / population_int * 100000,
    cancer_rate = cancer_icd10 / population_int * 100000,
    blood_rate = blood_icd10 / population_int * 100000,
    e_rate = e_icd10 / population_int * 100000,
    f_rate = f_icd10 / population_int * 100000,
    g_rate = g_icd10 / population_int * 100000,
    eye_rate = eye_icd10 / population_int * 100000,
    ear_rate = ear_icd10 / population_int * 100000,
    circulatory_rate = circulatory_icd10 / population_int * 100000,
    j_rate = j_icd10 / population_int * 100000,
    k_rate = k_icd10 / population_int * 100000,
    l_rate = l_icd10 / population_int * 100000,
    m_rate = m_icd10 / population_int * 100000,
    n_rate = n_icd10 / population_int * 100000,
    o_rate = o_icd10 / population_int * 100000,
    p_rate = p_icd10 / population_int * 100000,
    q_rate = q_icd10 / population_int * 100000,
    r_rate = r_icd10 / population_int * 100000,
    st_rate = st_icd10 / population_int * 100000,
  )



prov_main_unadjusted <- 
  feols(
    log(total_rate)~
      customers_percent_pec_1 |
      province + 
      month_year,
    combined_prov %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_prov$population_int
  )


prov_main_adjusted <- 
  feols(
    log(total_rate)~
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate  +
      population_int|
      province + 
      month_year,
    combined_prov %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_prov$population_int
  )


prov_count_unadjusted <- 
  fepois(
   total ~
     offset(log(population_int)) + 
      customers_percent_pec_1 |
      province + 
      month_year,
    combined_prov %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_prov$population_int
  )


prov_count_adjusted <- 
  fepois(
    total ~
      offset(log(population_int)) + 
      customers_percent_pec_1+
      +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate  +
      population_int |
      province + 
      month_year,
    combined_prov %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_prov$population_int
  )



# Province-year regression ------


combined_1yr <- 
  combined %>% 
  mutate(month = month(month_year),
         year = year(month_year)) %>% 
  group_by(province, year) %>% 
  summarize(
    total = sum(total, na.rm=T),
    asthma = sum(asthma, na.rm=T),
    copd_icd10 = sum(copd_icd10, na.rm=T),
    flu_icd10 = sum(flu_icd10, na.rm=T),
    ab_icd10 = sum(ab_icd10, na.rm=T),
    cancer_icd10 = sum(cancer_icd10, na.rm=T),
    blood_icd10 = sum(blood_icd10, na.rm=T),
    e_icd10 = sum(e_icd10, na.rm=T),
    f_icd10 = sum(f_icd10, na.rm=T),
    g_icd10 = sum(g_icd10, na.rm=T),
    eye_icd10 = sum(eye_icd10, na.rm=T),
    ear_icd10 = sum(ear_icd10, na.rm=T),
    circulatory_icd10 = sum(circulatory_icd10, na.rm=T),
    j_icd10 = sum(j_icd10, na.rm=T),
    k_icd10 = sum(k_icd10, na.rm=T),
    l_icd10 = sum(l_icd10, na.rm=T),
    m_icd10 = sum(m_icd10, na.rm=T),
    n_icd10 = sum(n_icd10, na.rm=T),
    o_icd10 = sum(o_icd10, na.rm=T),
    p_icd10 = sum(p_icd10, na.rm=T),
    q_icd10 = sum(q_icd10, na.rm=T),
    r_icd10 = sum(r_icd10, na.rm=T),
    st_icd10 = sum(st_icd10, na.rm=T),
    
    customers_percent_pec = mean(customers_percent_pec, na.rm=T),
    population_int = sum(population_int, na.rm=T),
    
    bdh_int = mean(bdh_int, na.rm=T),
    epobreza_int  = mean(epobreza_int, na.rm=T),
    correa_votes_percent_int = mean(correa_votes_percent_int, na.rm=T),
    facilities_docnurse5_int_rate  = mean(facilities_docnurse5_int_rate, na.rm=T),
    doctors_nurses_int_rate = mean(doctors_nurses_int_rate, na.rm=T),
    pm25 = mean(pm25, na.rm=T)
  ) %>% 
  mutate(
    total_rate = total / population_int * 100000,
    asthma_rate = asthma / population_int * 100000,
    copd_rate = copd_icd10 / population_int * 100000,
    flu_rate = flu_icd10 / population_int * 100000,
    ab_rate = ab_icd10 / population_int * 100000,
    cancer_rate = cancer_icd10 / population_int * 100000,
    blood_rate = blood_icd10 / population_int * 100000,
    e_rate = e_icd10 / population_int * 100000,
    f_rate = f_icd10 / population_int * 100000,
    g_rate = g_icd10 / population_int * 100000,
    eye_rate = eye_icd10 / population_int * 100000,
    ear_rate = ear_icd10 / population_int * 100000,
    circulatory_rate = circulatory_icd10 / population_int * 100000,
    j_rate = j_icd10 / population_int * 100000,
    k_rate = k_icd10 / population_int * 100000,
    l_rate = l_icd10 / population_int * 100000,
    m_rate = m_icd10 / population_int * 100000,
    n_rate = n_icd10 / population_int * 100000,
    o_rate = o_icd10 / population_int * 100000,
    p_rate = p_icd10 / population_int * 100000,
    q_rate = q_icd10 / population_int * 100000,
    r_rate = r_icd10 / population_int * 100000,
    st_rate = st_icd10 / population_int * 100000,
  )

year_main_unadjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate), 
      log(copd_rate)
    ) ~
      customers_percent_pec_1|
      province + 
      year,
    combined_1yr %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_1yr$population_int
  )

year_main_adjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate), 
      log(copd_rate)
    ) ~
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate  +
      population_int + 
      pm25|
      province + 
      year,
    combined_1yr %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_1yr$population_int
  )

year_count_unadjusted <- 
  fepois(
    total ~
      offset(log(population_int)) + 
      customers_percent_pec_1|
      province + 
      year,
    combined_1yr %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_1yr$population_int
  )


year_count_adjusted <- 
  fepois(
   total ~
     offset(log(population_int)) + 
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate  +
      population_int + 
     pm25|
      province + 
      year,
    combined_1yr %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_1yr$population_int
  )

etable(prov_main_unadjusted, 
       prov_main_adjusted, 
       prov_count_unadjusted,
       prov_count_adjusted,
       year_main_unadjusted$`lhs: log(total_rate)`,
       year_main_adjusted$`lhs: log(total_rate)`,
       year_count_unadjusted,
       year_count_adjusted,
       tex=T)



# working on did w/ the definitions of high / low -----

study_month_date <-
  combined_high_low_full %>% 
  distinct(month_year) %>% 
  arrange(month_year) %>% 
  mutate(study_month = row_number())

study_month_enroll_5 <- 
  combined_high_low_full %>% 
  mutate(canton_id_did = as.numeric(as.factor(canton_id)),
         study_month_enroll_5 = 
           ifelse(enroll_high_low==1 & 
                    customers_percent_pec>0.05, 1, 0)) %>% 
  group_by(canton_id_did) %>% 
  filter(study_month_enroll_5==1) %>% 
  summarize(
    study_month_enroll_5 = min(month_year)
  ) %>% 
  rename(month_year = study_month_enroll_5) %>% 
  left_join(study_month_date) %>% 
  rename(study_month_enroll_5 = study_month)
  
# combined_high_low_full_1 %>% filter(month_year>=ymd("2015-01-01")) %>% dplyr::group_by(enroll_high_low) %>% dplyr::summarize(pec=mean(customers_percent_pec, na.rm=T))

combined_high_low_full_1 <- 
  combined_high_low_full %>% 
  mutate(canton_id_did = as.numeric(as.factor(canton_id))) %>% 
  mutate(study_month_enroll = 
           ifelse(enroll_high_low==1, 37, 0)) %>% 
  left_join(study_month_enroll_5 %>% 
              dplyr::select(canton_id_did, study_month_enroll_5), by=c("canton_id_did")) %>% 
  left_join(study_month_date) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(study_month_enroll_5 = ifelse(is.na(study_month_enroll_5), 0, study_month_enroll_5)) %>% 
  mutate(
    total_rate = as.numeric(total_rate),
    study_month = as.numeric(study_month),
    study_month_enroll = as.numeric(study_month_enroll)
  ) %>% 
  filter(
    !is.na(total_rate) &
      !is.na(study_month) &
      !is.na(canton_id_did) &
      !is.na(study_month_enroll)
  ) %>% 
  mutate(log_total_rate = log(total_rate)) %>% 
  mutate(mo = as.factor(lubridate::month(lubridate::ymd(month_year))))


# Main DiD figure with PEC enroll at 2015 ------
sunab_reg_adjusted <- 
  fixest::feols(log(total_rate) ~
                  sunab(study_month_enroll, study_month) +
                  bdh_int + 
                  epobreza_int + 
                  correa_votes_percent_int + 
                  facilities_docnurse5_int_rate + 
                  doctors_nurses_int_rate +
                  population_int +
                  pm25|
                  canton_id + 
                  study_month,
                combined_high_low_full_1,
                weights=combined_high_low_full_1$population_int)

sunab_reg_unadjusted <- 
  fixest::feols(log(total_rate) ~
                sunab(study_month_enroll, study_month)  |
                canton_id + 
                study_month,
                combined_high_low_full_1,
              weights=combined_high_low_full_1$population_int)

summary(sunab_reg_unadjusted, agg="ATT") # recovers the ATT estimator
# This new DiD design directly addresses the problem of negative weights. 
# The DiD returns the same estimate, with a cleaner counterfactual, and by 
# correcting the negative weights, gives us a lot of confidence.
# Confidence that the original approach is not biased by these issues.

summary(sunab_reg_adjusted, agg="ATT")

sunab_reg_df <- tidy(sunab_reg_unadjusted)
sunab_reg_df$month <- sapply(sunab_reg_df$term, function(x) strsplit(x, "::")[[1]][2])
sunab_reg_df$month <- as.numeric(sunab_reg_df$month)

sunab_reg_df_1 <- tidy(sunab_reg_adjusted)
sunab_reg_df_1$month <- sapply(sunab_reg_df_1$term, function(x) strsplit(x, "::")[[1]][2])
sunab_reg_df_1$month <- as.numeric(sunab_reg_df_1$month)

sunab_reg_df_comb <- 
  sunab_reg_df  %>% 
  mutate(adjusted = "Unadjusted") %>% 
  bind_rows(sunab_reg_df_1 %>% 
              mutate(adjusted = "Adjusted"))

hosp_did_high_low_fig <-
  ggplot() + 
  theme_classic() +
  geom_hline(yintercept=0) + 
  geom_line(data=sunab_reg_df_comb %>% 
              filter(month<0), 
            aes(x=month,y=estimate,
                linetype=adjusted), color="#3B4992FF") + 
  geom_ribbon(data=sunab_reg_df_comb %>% 
                filter(month<0), 
              aes(x=month,
                  ymin=estimate-1.96*std.error, 
                  ymax=estimate+1.96*std.error), 
              alpha=0.2, fill="#3B4992FF") +
  geom_line(data=sunab_reg_df_comb %>% 
              filter(month>=0), 
            aes(x=month,y=estimate, linetype=adjusted), color="#EE0000FF") + 
  geom_ribbon(data=sunab_reg_df_comb %>% 
                filter(month>=0), 
              aes(x=month,
                  ymin=estimate-1.96*std.error, 
                  ymax=estimate+1.96*std.error), 
              alpha=0.2, fill="#EE0000FF") +
  scale_y_continuous(labels=scales::percent_format(),
                     breaks=c(-.4, -.3, -.2, -.1, 0, .1, .2)) +
  coord_cartesian(ylim=c(-.35, .2),xlim=c(-36, 60)) +
  scale_x_continuous(breaks=c(-36, -24, -12, 0, 12, 24, 36, 48, 60)) + 
  ggtitle("Average effect by length of exposure",
          subtitle="high PEC enrollment cantons (>85th percentile) vs. low PEC enrollment cantons (<15th percentile)") +
  xlab("months relative to beginning of PEC\n(January 2015)") + ylab("change in hospitalization rate") + 
  theme(
    legend.title = element_blank(),
    legend.position="right",
    axis.title =element_text(size = 7, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 7, color = "black"),
    axis.text.y = element_text(size = 7, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 


sunab_reg_unadjusted_coef <- tidy(summary(sunab_reg_unadjusted, agg="ATT"),conf.int = T)
sunab_reg_adjusted_coef <- tidy(summary(sunab_reg_adjusted, agg="ATT"),conf.int = T)[1,]

sunab_reg_coefs <- 
  sunab_reg_unadjusted_coef %>% 
  mutate(adjusted="Unadjusted") %>% 
  bind_rows(
    sunab_reg_adjusted_coef %>% 
      mutate(adjusted="Adjusted")
  )



hosp_did_high_low_simple_fig <- 
  ggplot(
    sunab_reg_coefs, aes(x=adjusted, y=estimate, ymin=conf.low, ymax=conf.high, linetype=adjusted)
  ) + 
  geom_pointrange() + 
  theme_classic() +
  geom_hline(yintercept=0) + 
  annotate("text", x="Unadjusted", y=-.215, label="Unadjusted", size=2) + 
  annotate("text", x="Adjusted", y=-.212, label="Adjusted", size=2) + 
  scale_y_continuous(labels=scales::percent_format(),
                     breaks=c(-.4, -.3, -.2, -.1, 0, .1, .2)) +
  coord_cartesian(ylim=c(-.35, .2)) +
  ggtitle("Average effect") +
  xlab("") + ylab("") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title =element_text(size = 7, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

# Main DiD figure with time-varying PEC enroll ------
sunab_reg_adjusted5 <- 
  fixest::feols(log(total_rate) ~
                  sunab(study_month_enroll_5, study_month) +
                  bdh_int + 
                  epobreza_int + 
                  correa_votes_percent_int + 
                  facilities_docnurse5_int_rate + 
                  doctors_nurses_int_rate +
                  population_int +
                  pm25|
                  canton_id + 
                  study_month,
                combined_high_low_full_1,
                weights=combined_high_low_full_1$population_int)

sunab_reg_unadjusted5 <- 
  fixest::feols(log(total_rate) ~
                  sunab(study_month_enroll_5, study_month)  |
                  canton_id + 
                  study_month,
                combined_high_low_full_1,
                weights=combined_high_low_full_1$population_int)

summary(sunab_reg_unadjusted5, agg="ATT")
summary(sunab_reg_adjusted5, agg="ATT")

sunab_reg_df5 <- tidy(sunab_reg_unadjusted5)
sunab_reg_df5$month <- sapply(sunab_reg_df5$term, function(x) strsplit(x, "::")[[1]][2])
sunab_reg_df5$month <- as.numeric(sunab_reg_df5$month)

sunab_reg_df5_1 <- tidy(sunab_reg_adjusted5)
sunab_reg_df5_1$month <- sapply(sunab_reg_df5_1$term, function(x) strsplit(x, "::")[[1]][2])
sunab_reg_df5_1$month <- as.numeric(sunab_reg_df5_1$month)

sunab_reg_df5_1_comb <- 
  sunab_reg_df5  %>% 
  mutate(adjusted = "Unadjusted") %>% 
  bind_rows(sunab_reg_df5_1 %>% 
              mutate(adjusted = "Adjusted"))

hosp_did_5_high_low_fig <- 
  ggplot() + 
  theme_classic() +
  geom_hline(yintercept=0) + 
  geom_line(data=sunab_reg_df5_1_comb %>% 
              filter(month<0), 
            aes(x=month,y=estimate,
                linetype=adjusted), color="#3B4992FF") + 
  geom_ribbon(data=sunab_reg_df5_1_comb %>% 
                filter(month<0), 
              aes(x=month,
                  ymin=estimate-1.96*std.error, 
                  ymax=estimate+1.96*std.error), 
              alpha=0.2, fill="#3B4992FF") +
  geom_line(data=sunab_reg_df5_1_comb %>% 
              filter(month>=0), 
            aes(x=month,y=estimate, linetype=adjusted), color="#EE0000FF") + 
  geom_ribbon(data=sunab_reg_df5_1_comb %>% 
                filter(month>=0), 
              aes(x=month,
                  ymin=estimate-1.96*std.error, 
                  ymax=estimate+1.96*std.error), 
              alpha=0.2, fill="#EE0000FF") +
  scale_y_continuous(labels=scales::percent_format(),
                     breaks=c(-.4, -.3, -.2, -.1, 0, .1, .2)) +
  coord_cartesian(ylim=c(-.35, .2),xlim=c(-36, 60)) +
  scale_x_continuous(breaks=c(-36, -24, -12, 0, 12, 24, 36, 48, 60)) + 
  ggtitle("Average effect by length of exposure",
          subtitle="high PEC enrollment cantons (>85th percentile) vs. low PEC enrollment cantons (<15th percentile)") +
  xlab("months relative to canton reaching 5% enrollment") + ylab("change in hospitalization rate") + 
  theme(
    legend.title = element_blank(),
    legend.position="right",
    axis.title =element_text(size = 7, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 7, color = "black"),
    axis.text.y = element_text(size = 7, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

sunab_reg_unadjusted5_coef <- tidy(summary(sunab_reg_unadjusted5, agg="ATT"),conf.int = T)
sunab_reg_adjusted5_coef <- tidy(summary(sunab_reg_adjusted5, agg="ATT"),conf.int = T)[1,]

sunab_reg_5_coefs <- 
  sunab_reg_unadjusted5_coef %>% 
  mutate(adjusted="Unadjusted") %>% 
  bind_rows(
    sunab_reg_adjusted5_coef %>% 
      mutate(adjusted="Adjusted")
  )

hosp_did_5_high_low_simple_fig <- 
  ggplot(
    sunab_reg_5_coefs, aes(x=adjusted, y=estimate, ymin=conf.low, ymax=conf.high, linetype=adjusted)
  ) + 
  geom_pointrange() + 
  theme_classic() +
  geom_hline(yintercept=0) + 
  annotate("text", x="Unadjusted", y=-.215, label="Unadjusted", size=2) + 
  annotate("text", x="Adjusted", y=-.212, label="Adjusted", size=2) + 
  scale_y_continuous(labels=scales::percent_format(),
                     breaks=c(-.4, -.3, -.2, -.1, 0, .1, .2)) +
  coord_cartesian(ylim=c(-.35, .2)) +
  ggtitle("Average effect") +
  xlab("") + ylab("") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title =element_text(size = 7, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

hosp_did_high_low_fig_combined <- 
  plot_grid(
    hosp_did_high_low_fig,
    hosp_did_high_low_simple_fig,
    align="hv",
    rel_widths=c(1, 0.25),
    nrow=1
  )

hosp_did_5_high_low_fig_combined <- 
  plot_grid(
    hosp_did_5_high_low_fig,
    hosp_did_5_high_low_simple_fig,
    align="hv",
    rel_widths=c(1, 0.25),
    nrow=1
  )


hosp_did_high_low_fig_combined_comb <- 
  plot_grid(
    NULL,
    hosp_did_high_low_fig_combined,
    NULL, 
    hosp_did_5_high_low_fig_combined,
    align="hv",
    labels=c("Treatment starts January 2015", "", "Treatment starts when canton reaches 5% enrollment", ""),
    label_size=12,
    nrow=4,
    rel_heights = c(0.1, 1, 0.2, 1),
    label_x=c(-.1, 0, -.22, 0),
    label_y=c(1, 1, 0.5, 0)
  )

# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/hosp_did_high_low_fig_combined_june.pdf",
#   plot = hosp_did_high_low_fig_combined_comb,
#   dpi = 300,
#   height = 200,
#   width = 200,
#   unit = "mm"
# )

# Other did ------

tictoc::tic()
out1 <- att_gt(
  yname="log_total_rate",
  tname="study_month",
  idname="canton_id_did",
  gname="study_month_enroll",
  bstrap = T,
  allow_unbalanced_panel = F,
  xformla=~1,
  panel = T,
  control_group="notyettreated",
  data=combined_high_low_full_1,
  weightsname = "population_int",
  clustervars = "canton_id_did"
)
tictoc::toc()
out1

out1

did::aggte(out1, type = "simple", na.rm=T)


cond_pretest <- 
  did::conditional_did_pretest(
  yname="log_total_rate",
  tname="study_month",
  idname="canton_id_did",
  gname="study_month_enroll",
  bstrap = T,
  allow_unbalanced_panel = F,
  xformla=~bdh_int + 
    epobreza_int + 
    correa_votes_percent_int + 
    facilities_docnurse5_int_rate + 
    doctors_nurses_int_rate +
    pm25,
  panel = T,
  control_group="notyettreated",
  data=combined_high_low_full_1,
  weightsname = "population_int",
  clustervars = "canton_id_did"
)

cond_pretest
summary(cond_pretest)





tictoc::tic()
out2 <- att_gt(
  yname="log_total_rate",
  tname="study_month",
  idname="canton_id_did",
  gname="study_month_enroll",
  bstrap = T,
  allow_unbalanced_panel = F,
  xformla=~bdh_int + 
    epobreza_int + 
    correa_votes_percent_int + 
    facilities_docnurse5_int_rate + 
    doctors_nurses_int_rate,
  panel = T,
  # control_group="nevertreated",
  data=combined_high_low_full_1,
  # control_group="notyettreated",
  weightsname = "population_int",
  clustervars = "canton_id_did"
)
tictoc::toc()

summary(out2)


simple2 <- did::aggte(out2, type = "simple", na.rm=T)
simple2
# object.size(simple)


tictoc::tic()
agg.te.dynamic2 <- did::aggte(out2, type = "dynamic", na.rm=T)
tictoc::toc()


agg.te.dynamic.df <- 
  bind_cols(agg.te.dynamic$egt, agg.te.dynamic$att.egt, agg.te.dynamic$se.egt)
colnames(agg.te.dynamic.df) <- c("month", "att", "se")
agg.te.dynamic.df <- agg.te.dynamic.df %>% 
  mutate(low = att - (1.96*se),
         high = att + (1.96*se))

agg.te.dynamic2.df <- 
  bind_cols(agg.te.dynamic2$egt, agg.te.dynamic2$att.egt, agg.te.dynamic2$se.egt)
colnames(agg.te.dynamic2.df) <- c("month", "att", "se")
agg.te.dynamic2.df <- agg.te.dynamic2.df %>% 
  mutate(low = att - (1.96*se),
         high = att + (1.96*se))

agg.te.dynamic.df_comb <- 
  agg.te.dynamic.df %>% 
  mutate(model = "Fixed timing") %>% 
  bind_rows(agg.te.dynamic2.df %>% 
              mutate(model="Heterogeneous timing"))


hosp_did_high_low_fig <- 
  ggplot() + 
  theme_classic() +
  geom_hline(yintercept=0) + 
  geom_line(data=agg.te.dynamic.df %>% 
              filter(month<0), 
            aes(x=month,y=att), color="#3B4992FF") + 
  geom_ribbon(data=agg.te.dynamic.df %>% 
                filter(month<0), 
              aes(x=month,ymin=low, ymax=high), alpha=0.2, fill="#3B4992FF") +
  geom_line(data=agg.te.dynamic.df %>% 
              filter(month>=0), 
            aes(x=month,y=att), color="#EE0000FF") + 
  geom_ribbon(data=agg.te.dynamic.df %>% 
                filter(month>=0), 
              aes(x=month,ymin=low, ymax=high), alpha=0.2, fill="#EE0000FF") +
  scale_y_continuous(labels=scales::percent_format(),
                     breaks=c(-.4, -.3, -.2, -.1, 0, .1, .2)) +
  coord_cartesian(ylim=c(-.35, .2), xlim=c(-36, 48)) +
  scale_x_continuous(breaks=c(-36, -24, -12, 0, 12, 24, 36, 48, 60)) + 
  ggtitle("Average effect by length of exposure",
          subtitle="high PEC enrollment cantons (>85th percentile) vs. low PEC enrollment cantons (<15th percentile)") +
  xlab("months relative to beginning of PEC\n(January 2015)") + ylab("change in hospitalization rate") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title =element_text(size = 7, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 7, color = "black"),
    axis.text.y = element_text(size = 7, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 








# Population weighted, unweighted, full and post-pec only -------

# unadjusted all ------
# Main, unadjusted, weighted -----



for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  
  main_lr_unadjusted <- 
    feols(
      log(total_rate) ~
        customers_percent_pec_1  |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  main_lr_unadjusted_combined <- 
    tidy(main_lr_unadjusted, conf.int=T, conf.level = .95) %>% 
    mutate(model = as.character(i))
  
  main_lr_unadjusted_combined_full_popwt <-
    main_lr_unadjusted_combined_full_popwt %>% 
    bind_rows(main_lr_unadjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# Main, unadjusted, unweighted -----
for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  
  main_lr_unadjusted_unwt <- 
    feols(
      log(total_rate) ~
        customers_percent_pec_1  |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100)
    )
  
  main_lr_unadjusted_unwt <- 
    tidy(main_lr_unadjusted_unwt, conf.int=T, conf.level = .95) %>% 
    mutate(model = as.character(i))
  
  main_lr_unadjusted_combined_full_unwt <- 
    main_lr_unadjusted_combined_full_unwt %>% 
    bind_rows(main_lr_unadjusted_unwt)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# postpec, unadjusted, weighted -----
for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  subdata <- 
    subdata %>% 
    filter(month_year>=ymd("2015-01-01"))
  
  postpec_lr_unadjusted <- 
    feols(
      log(total_rate) ~
        customers_percent_pec_1  |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  postpec_lr_unadjusted_combined <- 
    tidy(postpec_lr_unadjusted, conf.int=T, conf.level = .95) %>% 
    mutate(model = as.character(i))
  
  postpec_lr_unadjusted_combined_full_popwt <-
    postpec_lr_unadjusted_combined_full_popwt %>% 
    bind_rows(postpec_lr_unadjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# postpec, unadjusted, unweighted -----
for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  subdata <- 
    subdata %>% 
    filter(month_year>=ymd("2015-01-01"))
  
  postpec_lr_unadjusted_unwt <- 
    feols(
      log(total_rate) ~
        customers_percent_pec_1  |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100)
    )
  
  postpec_lr_unadjusted_unwt <- 
    tidy(postpec_lr_unadjusted_unwt, conf.int=T, conf.level = .95) %>% 
    mutate(model = as.character(i))
  
  postpec_lr_unadjusted_combined_full_unwt <- 
    postpec_lr_unadjusted_combined_full_unwt %>% 
    bind_rows(postpec_lr_unadjusted_unwt)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# adjusted all ------

# Main, adjusted, weighted -----
main_lr_adjusted <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int +
      pm25 |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_lr_adjusted_combined <- 
  tidy(main_lr_adjusted, conf.int=T, conf.level = .95) %>% 
  mutate(model = "full")

main_lr_adjusted_combined_full_popwt <- main_lr_adjusted_combined

for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  
  main_lr_adjusted <- 
    feols(
      log(total_rate) ~
        customers_percent_pec_1  + 
        bdh_int + 
        epobreza_int + 
        correa_votes_percent_int + 
        facilities_docnurse5_int_rate + 
        doctors_nurses_int_rate + 
        population_int +
        pm25 |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  main_lr_adjusted_combined <- 
    tidy(main_lr_adjusted, conf.int=T, conf.level = .95) %>% 
    mutate(model = as.character(i))
  
  main_lr_adjusted_combined_full_popwt <-
    main_lr_adjusted_combined_full_popwt %>% 
    bind_rows(main_lr_adjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# Main, adjusted, unweighted -----

main_lr_adjusted_unwt <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int +
      pm25 |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100)
  )

main_lr_adjusted_unwt <- 
  tidy(main_lr_adjusted_unwt, conf.int=T, conf.level = .95) %>% 
  mutate(model = "full")

main_lr_adjusted_combined_full_unwt <- main_lr_adjusted_unwt

for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  
  main_lr_adjusted_unwt <- 
    feols(
      log(total_rate) ~
        customers_percent_pec_1  + 
        bdh_int + 
        epobreza_int + 
        correa_votes_percent_int + 
        facilities_docnurse5_int_rate + 
        doctors_nurses_int_rate + 
        population_int+
        pm25   |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100)
    )
  
  main_lr_adjusted_unwt <- 
    tidy(main_lr_adjusted_unwt, conf.int=T, conf.level = .95) %>% 
    mutate(model = as.character(i))
  
  main_lr_adjusted_combined_full_unwt <- 
    main_lr_adjusted_combined_full_unwt %>% 
    bind_rows(main_lr_adjusted_unwt)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# postpec, adjusted, weighted -----

combined_postpec <- 
  combined %>% 
  filter(month_year>=ymd("2015-01-01"))

postpec_lr_adjusted <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int +
      pm25 |
      canton_id + 
      month_year,
    combined_postpec  %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined_postpec$population_int
  )

postpec_lr_adjusted <- 
  tidy(postpec_lr_adjusted, conf.int=T, conf.level = .95) %>% 
  mutate(model = "full")

postpec_lr_adjusted_combined_full_popwt <- postpec_lr_adjusted

for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  subdata <- 
    subdata %>% 
    filter(month_year>=ymd("2015-01-01"))
  
  postpec_lr_adjusted <- 
    feols(
      log(total_rate) ~
        customers_percent_pec_1  + 
        bdh_int + 
        epobreza_int + 
        correa_votes_percent_int + 
        facilities_docnurse5_int_rate + 
        doctors_nurses_int_rate + 
        population_int +
        pm25  |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  postpec_lr_adjusted_combined <- 
    tidy(postpec_lr_adjusted, conf.int=T, conf.level = .95) %>% 
    mutate(model = as.character(i))
  
  postpec_lr_adjusted_combined_full_popwt <-
    postpec_lr_adjusted_combined_full_popwt %>% 
    bind_rows(postpec_lr_adjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# postpec, adjusted, unweighted -----
postpec_lr_adjusted_unwt <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int +
      pm25 |
      canton_id + 
      month_year,
    combined_postpec  %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100)
  )

postpec_lr_adjusted_unwt <- 
  tidy(postpec_lr_adjusted_unwt, conf.int=T, conf.level = .95) %>% 
  mutate(model = "full")

postpec_lr_adjusted_combined_full_unwt <- postpec_lr_adjusted_unwt


for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
  subdata <- inner_join(combined, samp, by="xunit")
  subdata <- 
    subdata %>% 
    filter(month_year>=ymd("2015-01-01"))
  
  postpec_lr_adjusted_unwt <- 
    feols(
      log(total_rate) ~
        customers_percent_pec_1  + 
        bdh_int + 
        epobreza_int + 
        correa_votes_percent_int + 
        facilities_docnurse5_int_rate + 
        doctors_nurses_int_rate + 
        population_int +
        pm25  |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100)
    )
  
  postpec_lr_adjusted_unwt <- 
    tidy(postpec_lr_adjusted_unwt, conf.int=T, conf.level = .95) %>% 
    mutate(model = as.character(i))
  
  postpec_lr_adjusted_combined_full_unwt <- 
    postpec_lr_adjusted_combined_full_unwt %>% 
    bind_rows(postpec_lr_adjusted_unwt)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}


# Combine all main, postpec, weighted, unweighted, adjsuted unadjusted for fig -------

combined_full_wt_unwt <- 
  main_lr_adjusted_combined_full_popwt %>% 
  mutate(
    adjusted = "Adjusted",
    weights = "Population-weighted"
  ) %>% 
  bind_rows(
    main_lr_adjusted_combined_full_unwt %>% 
      mutate(
        adjusted = "Adjusted",
        weights = "Unweighted"
      ) 
  ) %>% 
  bind_rows(
    main_lr_unadjusted_combined_full_popwt %>% 
      mutate(
        adjusted = "Unadjusted",
        weights = "Population-weighted"
      ) 
  ) %>% 
  bind_rows(
    main_lr_unadjusted_combined_full_unwt %>% 
      mutate(
        adjusted = "Unadjusted",
        weights = "Unweighted"
      ) 
  )%>% 
  filter(term=="customers_percent_pec_1")


combined_postpec_wt_unwt <- 
  postpec_lr_adjusted_combined_full_popwt %>% 
  mutate(
    adjusted = "Adjusted",
    weights = "Population-weighted"
  ) %>% 
  bind_rows(
    postpec_lr_adjusted_combined_full_unwt %>% 
      mutate(
        adjusted = "Adjusted",
        weights = "Unweighted"
      ) 
  ) %>% 
  bind_rows(
    postpec_lr_unadjusted_combined_full_popwt %>% 
      mutate(
        adjusted = "Unadjusted",
        weights = "Population-weighted"
      ) 
  ) %>% 
  bind_rows(
    postpec_lr_unadjusted_combined_full_unwt %>% 
      mutate(
        adjusted = "Unadjusted",
        weights = "Unweighted"
      ) 
  ) %>% 
  filter(term=="customers_percent_pec_1")

combined_full_wt_unwt_fig <- 
  ggplot(combined_full_wt_unwt,
       aes(x=estimate, color=weights, fill=weights, group=weights)) + 
  geom_density(alpha=0.5) + 
  scale_fill_aaas() + 
  scale_color_aaas() +
  scale_x_continuous(
    breaks=c(-.02, -.015, -.01, -.005, 0,  .005, .01, .025),
    labels=scales::percent_format()
  ) + 
  coord_cartesian(xlim=c(-0.02, 0.002)) + 
  theme_classic() + 
  ylab("# of estimates") + 
  xlab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) +
  facet_grid(adjusted~.)

combined_postpec_wt_unwt_fig <-
  ggplot(combined_postpec_wt_unwt,
       aes(x=estimate, color=weights, fill=weights, group=weights)) + 
  geom_density(alpha=0.5) + 
  scale_fill_aaas() + 
  scale_color_aaas() +
  scale_x_continuous(
    breaks=c(-.02, -.015, -.01, -.005, 0,  .005, .01, .025),
    labels=scales::percent_format()
  ) + 
  coord_cartesian(xlim=c(-0.02, 0.002)) + 
  theme_classic() + 
  ylab("# of estimates") + 
  xlab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment") + 
  theme(
    legend.title = element_blank(),
    legend.position="bottom",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) +
  facet_grid(adjusted~.)
  
combined_full_wt_unwt_summary <- 
  combined_postpec_wt_unwt %>% 
  mutate(study_period = "Post PEC") %>% 
  bind_rows(
    combined_full_wt_unwt %>% 
      mutate(study_period  = "Full")
  ) %>% 
  group_by(
    study_period, adjusted, weights
  ) %>% 
  summarize(
    median = median(estimate, na.rm=T)*100,
    q025 = quantile(estimate, probs=0.025, na.rm=T)*100,
    q975 = quantile(estimate, probs=0.975, na.rm=T)*100,
  ) %>% 
  mutate(
    median_95 = paste0(
      round(median, digits=2), "% (",
      round(q025, digits=2),"%, ",
      round(q975, digits=2), "%)"
    )
  )

combined_full_wt_unwt_fig_1 <- 
  plot_grid(
    NULL, 
    combined_full_wt_unwt_fig,
    NULL, 
    combined_postpec_wt_unwt_fig,
    align="hv",
    nrow=4,
    rel_heights = c(0.1, 1, 0.1, 1),
    labels=c("Full study period (1/2012 - 3/2020)",
             "",
             "Postpec study period (1/2015 - 3/2020)",
             "")
  )
   
# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/combined_full_wt_unwt_fig_1_june.pdf",
#   plot = combined_full_wt_unwt_fig_1,
#   dpi = 300,
#   height = 175,
#   width = 250,
#   unit = "mm"
# )


# Alternative controls and alternative models -------

combined <- 
  combined %>% 
  mutate(stratum = paste0(canton_id, ", ", month(month_year))) %>% 
  mutate(customers_percent_pec_1 = customers_percent_pec*100)

# Main log rate with alternative time controls -----

reg_lr_unadjusted1 <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_lr_adjusted1  <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_lr_unadjusted2  <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_lr_adjusted2 <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int + 
      pm25 |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_lr_unadjusted3 <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1 +
      ns(month_year,df=9) |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_lr_adjusted3  <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int   +
      pm25 +
      ns(month_year,df=9) |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_lr_unadjusted4 <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1 +
      ns(month_year,df=27) |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_lr_adjusted4  <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int   +
      pm25 + 
      ns(month_year,df=27) |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

combined <- combined %>% 
  mutate(customers_percent_pec_1 = customers_percent_pec*100)

reg_lr_adjusted5 = gnm::gnm(
  log(total_rate) ~
    customers_percent_pec_1 +
    bdh_int + 
    epobreza_int + 
    correa_votes_percent_int + 
    facilities_docnurse5_int_rate + 
    doctors_nurses_int_rate + 
    population_int + 
    pm25,
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int)

vcov <- cluster.vcov(reg_lr_adjusted5, combined$stratum)
coeftest(m1, vcov_firm)

reg_lr_adjusted6 = gnm::gnm(
  log(total_rate) ~
    customers_percent_pec_1 +
    bdh_int + 
    epobreza_int + 
    correa_votes_percent_int + 
    facilities_docnurse5_int_rate + 
    doctors_nurses_int_rate + 
    population_int + 
    pm25 + 
    ns(month_year, df=9),
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int)

reg_lr_adjusted7 = gnm::gnm(
  log(total_rate) ~
    customers_percent_pec_1 +
    bdh_int + 
    epobreza_int + 
    correa_votes_percent_int + 
    facilities_docnurse5_int_rate + 
    doctors_nurses_int_rate + 
    population_int + 
    pm25 +
    ns(month_year, df=27),
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int)


reg_lr_unadjusted5 = gnm::gnm(
  log(total_rate) ~
    customers_percent_pec_1,
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int)

reg_lr_unadjusted6 = gnm::gnm(
  log(total_rate) ~
    customers_percent_pec_1 +
    ns(month_year, df=9),
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int)

reg_lr_unadjusted7 = gnm::gnm(
  log(total_rate) ~
    customers_percent_pec_1 +
    ns(month_year, df=27),
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int)


# Total counts with alternative time controls -----

reg_count_unadjusted1 <- 
  fepois(
    total ~ offset(log(population_int)) +
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_count_adjusted1  <- 
  fepois(
    total ~ offset(log(population_int)) +
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int  + pm25|
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_count_unadjusted2  <- 
  fepois(
    total ~ offset(log(population_int)) +
      customers_percent_pec_1  |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_count_adjusted2 <- 
  fepois(
    total ~ offset(log(population_int)) +
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int  + pm25|
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_count_unadjusted3 <- 
  fepois(
    total ~ offset(log(population_int)) +
      customers_percent_pec_1 +
      ns(month_year,df=9) |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_count_adjusted3  <- 
  fepois(
    total ~ offset(log(population_int)) +
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int   +
      pm25 +
      ns(month_year,df=9) |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_count_unadjusted4 <- 
  fepois(
    total ~ offset(log(population_int)) +
      customers_percent_pec_1 +
      ns(month_year,df=27) |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_count_adjusted4  <- 
  fepois(
    total ~ offset(log(population_int)) +
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int   +
      pm25 + 
      ns(month_year,df=27) |
      canton_id + 
      month(month_year) + 
      year(month_year),
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_count_adjusted5 = gnm::gnm(
  total ~ offset(log(population_int)) +
    customers_percent_pec_1+
    bdh_int + 
    epobreza_int + 
    correa_votes_percent_int + 
    facilities_docnurse5_int_rate + 
    doctors_nurses_int_rate + 
    population_int + 
    pm25,
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int,
  family=poisson)

reg_count_adjusted6 = gnm::gnm(
  total ~ offset(log(population_int)) +
    customers_percent_pec_1 +
    bdh_int + 
    epobreza_int + 
    correa_votes_percent_int + 
    facilities_docnurse5_int_rate + 
    doctors_nurses_int_rate + 
    population_int + 
    pm25 +
    ns(month_year, df=9),
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int,
  family=poisson)

reg_count_adjusted7 = gnm::gnm(
  total ~ offset(log(population_int)) +
    customers_percent_pec_1 +
    bdh_int + 
    epobreza_int + 
    correa_votes_percent_int + 
    facilities_docnurse5_int_rate + 
    doctors_nurses_int_rate + 
    population_int + 
    pm25 + 
    ns(month_year, df=27),
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int,
  family=poisson)


reg_count_unadjusted5 = gnm::gnm(
  total ~ offset(log(population_int)) +
    customers_percent_pec_1,
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int,
  family=poisson)

reg_count_unadjusted6 = gnm::gnm(
  total ~ offset(log(population_int)) +
    customers_percent_pec_1 +
    ns(month_year, df=9),
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int,
  family=poisson)

reg_count_unadjusted7 = 
  gnm::gnm(
  total ~ offset(log(population_int)) +
    customers_percent_pec_1 +
    ns(month_year, df=27),
  data=combined,
  eliminate=factor(stratum),
  weights=combined$population_int,
  family=poisson)


# combining data from alternative approaches and time controls ------

alt_approach_hosp_lr <-
  tidy(reg_lr_adjusted1, conf.int=T)[1,] %>% 
  mutate(model = 1) %>% 
  bind_rows(
    tidy(reg_lr_adjusted2, conf.int=T)[1,]%>% 
      mutate(model = 2),
    tidy(reg_lr_adjusted3, conf.int=T)[1,]%>% 
      mutate(model = 3),
    tidy(reg_lr_adjusted4, conf.int=T)[1,]%>% 
      mutate(model = 4),
    tidy(reg_lr_adjusted5,conf.int = T)[1,] %>% 
      mutate(model = 5),
    tidy(reg_lr_adjusted6,conf.int = T)[1,] %>% 
      mutate(model = 6),
    tidy(reg_lr_adjusted7,conf.int = T)[1,] %>% 
      mutate(model = 7)
  ) %>% 
  mutate(adjusted = "Adjusted") %>% 
  bind_rows(
    bind_rows(
      tidy(reg_lr_unadjusted1,conf.int = T) %>% 
      mutate(model = 1),
      tidy(reg_lr_unadjusted2,conf.int = T)%>% 
          mutate(model = 2),
      tidy(reg_lr_unadjusted3,conf.int = T) %>% 
          mutate(model = 3),
      tidy(reg_lr_unadjusted4,conf.int = T) %>% 
          mutate(model = 4),
        tidy(reg_lr_unadjusted5, conf.int=T) %>% 
          mutate(model = 5),
      tidy(reg_lr_unadjusted6, conf.int=T) %>% 
          mutate(model = 6),
      tidy(reg_lr_unadjusted7, conf.int=T) %>% 
          mutate(model = 7)
      ) %>% 
      mutate(adjusted = "Unadjusted")
  )


alt_approach_hosp_count <-
  tidy(reg_count_adjusted1, conf.int=T)[1,] %>% 
  mutate(model = 1) %>% 
  bind_rows(
    tidy(reg_count_adjusted2, conf.int=T)[1,]%>% 
      mutate(model = 2),
    tidy(reg_count_adjusted3, conf.int=T)[1,]%>% 
      mutate(model = 3),
    tidy(reg_count_adjusted4, conf.int=T)[1,]%>% 
      mutate(model = 4),
    tidy(reg_count_adjusted5)[1,] %>% 
      mutate(model = 5),
    tidy(reg_count_adjusted6)[1,] %>% 
      mutate(model = 6),
    tidy(reg_count_adjusted7)[1,] %>% 
      mutate(model = 7)
  ) %>% 
  mutate(adjusted = "Adjusted") %>% 
  bind_rows(
    bind_rows(
      tidy(reg_count_unadjusted1,conf.int = T) %>% 
        mutate(model = 1),
      tidy(reg_count_unadjusted2,conf.int = T)%>% 
        mutate(model = 2),
      tidy(reg_count_unadjusted3,conf.int = T) %>% 
        mutate(model = 3),
      tidy(reg_count_unadjusted4,conf.int = T) %>% 
        mutate(model = 4),
      tidy(reg_count_unadjusted5, conf.int=T) %>% 
        mutate(model = 5),
      tidy(reg_count_unadjusted6, conf.int=T) %>% 
        mutate(model = 6),
      tidy(reg_count_unadjusted7, conf.int=T) %>% 
        mutate(model = 7)
    ) %>% 
      mutate(adjusted = "Unadjusted")
  )  %>% 
  filter(term=="customers_percent_pec_1")

alt_approach_hosp_lr_fig <-
  ggplot(alt_approach_hosp_lr %>% 
         filter(term=="customers_percent_pec_1"),
       aes(y=estimate, 
           ymin=estimate - 1.96*std.error,
           ymax=estimate + 1.96*std.error,
           x=model,
           group=adjusted, 
           shape=adjusted,
           linetype=adjusted)) + 
  geom_pointrange(position=position_dodge(width=0.3))  +
  geom_hline(yintercept=0) +
  scale_x_continuous(breaks=c(seq(1, 7))) + 
  scale_y_continuous(labels=scales::percent_format()) +
  theme_classic() + 
  ylab("") + 
  ylab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment\n(per 100,000 population)") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 




alt_approach_hosp_count_fig <-
  ggplot(alt_approach_hosp_count %>% 
         filter(term=="customers_percent_pec_1"),
       aes(y=exp(estimate), 
           ymin=exp(estimate - 1.96*std.error),
           ymax=exp(estimate + 1.96*std.error),
           x=model,
           group=adjusted, 
           shape=adjusted,
           linetype=adjusted)) + 
  scale_x_continuous(breaks=c(seq(1, 7))) + 
  geom_pointrange(position=position_dodge(width=0.3)) +
  geom_hline(yintercept=1) +
  theme_classic() + 
  xlab("") + 
  ylab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment\n(per 100,000 population)") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

alt_approaches_fig <- 
  plot_grid(
  alt_approach_hosp_lr_fig,
  alt_approach_hosp_count_fig,
  align = "hv",
  nrow=1,
  labels=c("log(hospitalization rate)", "count of total hospitalizations"),
  label_size=10
)


# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/alt_approaches_fig_june.pdf",
#   plot = alt_approaches_fig,
#   dpi = 300,
#   height = 150,
#   width = 300,
#   unit = "mm"
# )



# Table comparing raw vs. adjusted analyses --------


reg_lr_unadjusted1 <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_lr_adjusted1  <- 
  feols(
    log(total_rate) ~
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int +
      pm25 |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )


reg_count_unadjusted1 <- 
  fepois(
    total ~ offset(log(population_int)) +
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

reg_count_adjusted1  <- 
  fepois(
    total ~ offset(log(population_int)) +
      customers_percent_pec_1  + 
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int + 
      pm25 |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

sunab_reg_adjusted <- 
  fixest::feols(log(total_rate) ~
                  sunab(study_month_enroll, study_month) +
                  bdh_int + 
                  epobreza_int + 
                  correa_votes_percent_int + 
                  facilities_docnurse5_int_rate + 
                  doctors_nurses_int_rate +
                  population_int + pm25|
                  canton_id + 
                  study_month,
                combined_high_low_full_1,
                weights=combined_high_low_full_1$population_int)

sunab_reg_unadjusted <- 
  fixest::feols(log(total_rate) ~
                  sunab(study_month_enroll, study_month)  |
                  canton_id + 
                  study_month,
                combined_high_low_full_1,
                weights=combined_high_low_full_1$population_int)

summary(sunab_reg_unadjusted, agg="ATT") 
summary(sunab_reg_adjusted, agg="ATT") 

etable(reg_lr_unadjusted1, reg_lr_adjusted1,
       reg_count_unadjusted1, reg_count_adjusted1,
       summary(sunab_reg_unadjusted, agg="ATT") , summary(sunab_reg_adjusted, agg="ATT"),
       summary(sunab_reg_unadjusted5, agg="ATT") , summary(sunab_reg_adjusted5, agg="ATT"),
       tex=T)



# Block bootstrap ------


combined_canton_province <- combined %>% 
  dplyr::select(canton_id, province, province.x, province.y) %>% 
  distinct() %>% 
  mutate(province_1 = ifelse(is.na(province.x), province.y, province.x)) %>% 
  mutate(province_2 = ifelse(is.na(province_1), province, province_1)) %>% 
  mutate(province_3 = ifelse(is.na(province_2), str_extract(canton_id, "^[^,]+"), province_2)) %>% 
  dplyr::select(-province, -province.x, -province.y, -province_1, -province_2) %>% 
  dplyr::rename(province = province_3) %>% 
  distinct(canton_id, province)

combined <- combined %>% 
  dplyr::select(-province, -province.x, -province.y) %>% 
  left_join(combined_canton_province) %>% 
  mutate(province_yr = paste0(province, "_", yr))

combined$xunit = combined$province_yr
cc <- unique(combined$xunit)

# combined$xunit = combined$canton_id
# cc <- unique(combined$xunit)

main_lr_unadjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate),
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate),
      log(ab_rate),
      log(cancer_rate),
      log(f_rate),
      log(g_rate),
      log(circulatory_rate),
      log(k_rate),
      log(n_rate),
      log(o_rate),
      log(p_rate),
      log(st_rate)
    ) ~
      customers_percent_pec_1  |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_lr_unadjusted_combined <- 
  tidy(main_lr_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_lr_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_unadjusted[[15]], conf.int=T, conf.level = .95)[1,]
  ) 

main_lr_unadjusted_combined_full <- main_lr_unadjusted_combined %>% mutate(model="Full")

set.seed(8)

for (i in 1:1000) {
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T)) 
  subdata <- inner_join(combined, samp, by="xunit")
  
  main_lr_unadjusted <- 
    feols(
      c(log(total_rate), 
        log(j_rate),
        log(copd_rate),
        log(flu_rate),
        log(asthma_rate),
        log(ab_rate),
        log(cancer_rate),
        log(f_rate),
        log(g_rate),
        log(circulatory_rate),
        log(k_rate),
        log(n_rate),
        log(o_rate),
        log(p_rate),
        log(st_rate)
      ) ~
        customers_percent_pec_1  |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  main_lr_unadjusted_combined <- 
    tidy(main_lr_unadjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
    bind_rows(
      tidy(main_lr_unadjusted[[2]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[3]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[4]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[5]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[6]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[7]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[8]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[9]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[10]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[11]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[12]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[13]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[14]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_unadjusted[[15]], conf.int=T, conf.level = .95)[1,]
    )  %>% 
    mutate(model = as.character(i))
  
  main_lr_unadjusted_combined_full <-
    main_lr_unadjusted_combined_full %>% 
    bind_rows(main_lr_unadjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}


# Full, main, adjusted --------

main_lr_adjusted <- 
  feols(
    c(log(total_rate), 
      log(j_rate),
      log(copd_rate),
      log(flu_rate),
      log(asthma_rate),
      log(ab_rate),
      log(cancer_rate),
      log(f_rate),
      log(g_rate),
      log(circulatory_rate),
      log(k_rate),
      log(n_rate),
      log(o_rate),
      log(p_rate),
      log(st_rate)
    ) ~
      customers_percent_pec_1 +
      bdh_int + 
      epobreza_int + 
      correa_votes_percent_int + 
      facilities_docnurse5_int_rate + 
      doctors_nurses_int_rate + 
      population_int +
      pm25 |
      canton_id + 
      month_year,
    combined %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec*100),
    weights=combined$population_int
  )

main_lr_adjusted_combined <- 
  tidy(main_lr_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
  bind_rows(
    tidy(main_lr_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[6]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[7]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[8]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[9]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[10]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[11]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[12]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[13]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[14]], conf.int=T, conf.level = .95)[1,],
    tidy(main_lr_adjusted[[15]], conf.int=T, conf.level = .95)[1,]
  )  

main_lr_adjusted_combined_full <-main_lr_adjusted_combined %>% mutate(model="Full")


for (i in 1:1000) {
  
  samp <- data.frame(xunit=sample(cc,length(cc),replace=T))
  
  subdata <- inner_join(combined, samp, by="xunit")
  
  main_lr_adjusted <- 
    feols(
      c(log(total_rate), 
        log(j_rate),
        log(copd_rate),
        log(flu_rate),
        log(asthma_rate),
        log(ab_rate),
        log(cancer_rate),
        log(f_rate),
        log(g_rate),
        log(circulatory_rate),
        log(k_rate),
        log(n_rate),
        log(o_rate),
        log(p_rate),
        log(st_rate)
      ) ~
        customers_percent_pec_1 +
        bdh_int + 
        epobreza_int + 
        correa_votes_percent_int + 
        facilities_docnurse5_int_rate + 
        doctors_nurses_int_rate + 
        population_int +
        pm25 |
        canton_id + 
        month_year,
      subdata %>% 
        mutate(customers_percent_pec_1 = customers_percent_pec*100),
      weights=subdata$population_int
    )
  
  main_lr_adjusted_combined <- 
    tidy(main_lr_adjusted[[1]], conf.int=T, conf.level = .95)[1,] %>% 
    bind_rows(
      tidy(main_lr_adjusted[[2]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[3]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[4]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[5]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[6]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[7]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[8]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[9]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[10]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[11]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[12]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[13]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[14]], conf.int=T, conf.level = .95)[1,],
      tidy(main_lr_adjusted[[15]], conf.int=T, conf.level = .95)[1,]
    )  %>% 
    mutate(model = as.character(i))
  
  main_lr_adjusted_combined_full <-
    main_lr_adjusted_combined_full %>% 
    bind_rows(main_lr_adjusted_combined)
  
  if (round(i,-1)==i) {print(i)}  #print every 10
}

# Combining bootstrapped estimates into figures -------

outcomes <- c("total_rate", 
              "j_rate",
              "copd_rate",
              "flu_rate",
              "asthma_rate",
              "ab_rate",
              "cancer_rate",
              "f_rate",
              "g_rate",
              "circulatory_rate",
              "k_rate",
              "n_rate",
              "o_rate",
              "p_rate",
              "st_rate"
)

xwalk[15,1] <- "j_rate"

main_lr_combined_full <- 
  main_lr_unadjusted_combined_full  %>% 
  mutate(adjusted = "Unadjusted")%>% 
  mutate(icd_rate = rep(outcomes,1001)) %>% 
  bind_rows(main_lr_adjusted_combined_full %>% 
              mutate(adjusted = "Adjusted")%>% 
              mutate(icd_rate = rep(outcomes,1001))) %>% 
  left_join(xwalk) %>% 
  mutate(name = plyr::mapvalues(
    outcome,
    from=c("Total hospitalizations",
           "Respiratory system",
           "Influenza and pneumonia",
           "COPD",
           "Asthma"),
    to=c("All hospitalizations",
         "Respiratory",
         "Influenza and pneumonia",
         "COPD",
         "Asthma")
  ))


main_lr_medians <- 
  main_lr_combined_full %>% 
  filter(!is.na(outcome)) %>% 
  filter(model!="Full") %>% 
  group_by(outcome,adjusted) %>% 
  summarize(median = median(estimate, na.rm=T))

coef_fig <- 
  ggplot(main_lr_combined_full %>% 
           filter(!is.na(outcome)) %>% 
           filter(model!="Full"), 
         aes(y=name, x=estimate, fill=adjusted)) + 
  ggdist::stat_slab(
    position = position_dodge(width=.8),
    alpha=0.7
  ) +
  geom_point(
    data=main_lr_combined_full %>% 
      filter(!is.na(outcome)) %>% 
      filter(model=="Full"),
    aes(y=name, x=estimate, group=adjusted),
    position=position_dodge(width=0.8)
  ) +
  scale_fill_manual(values=c(met.brewer("Cassatt1")[2], met.brewer("Cassatt1")[6])) + 
  
  annotate("text", y= 5.3, x=-0.025, label="Unadjusted", size=3, color=met.brewer("Cassatt1")[6]) +
  annotate("text", y= 4.9, x=-0.025, label="Adjusted", size=3, color=met.brewer("Cassatt1")[2]) +
  
  scale_x_continuous(labels=scales::percent_format(),
                     breaks=c(-0.03,-.025,-0.02, -0.015, -0.01, -0.005, 0, 0.005))  + 
  scale_y_discrete(limits=
                     rev(c("All hospitalizations",
                           "Respiratory", 
                           "Influenza and pneumonia",
                           "COPD",
                           "Asthma"))) +
  coord_cartesian(xlim=c(-.031, 0.006)) + 
  geom_vline(xintercept=0) + 
  theme_classic() + 
  ylab("") + 
  xlab("Change in hospitalization rate per 1 p.p. increase in PEC enrollment") + 
  theme(
    legend.title = element_blank(),
    legend.position="none",
    axis.title.y =element_text(size = 8, color = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 9, color = "black", face="bold"),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.3, color = "grey75", linetype="dotted"),
    axis.line.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  ) 

coef_fig

main_lr_combined_full  %>% 
  filter(!is.na(outcome)) %>% 
  group_by(outcome, adjusted) %>% 
  summarize(p5 = mean(p.value<.05))

pvalue_df <- main_lr_combined_full %>% 
  filter(!is.na(outcome)) %>% 
  mutate(p_value_cat = ifelse(adjusted=="Adjusted" & p.value<=.05, "1",
                              ifelse(adjusted=="Adjusted" & p.value>.05, "2",
                                     ifelse(adjusted=="Unadjusted" & p.value<=.05, "3",
                                            ifelse(adjusted=="Unadjusted" & p.value>.05, "4", NA))))) %>% 
  ungroup()

pvalue_summary_df <- main_lr_combined_full %>% 
  group_by(icd_code, adjusted) %>% 
  summarize(
    p05 = mean(p.value<.05, na.rm=T)
  )

main_lr_combined_full_pvalues <-
  main_lr_combined_full %>% 
  group_by(outcome, adjusted) %>% 
  summarise(
    p05 = mean(p.value<0.05, na.rm=T),
    p01 = mean(p.value<0.01, na.rm=T),
    p005 = mean(p.value<0.005, na.rm=T),
  )


total_events_rate_df <- combined %>% 
  filter(month_year>=ymd("2012-01-01") &
           month_year<=ymd("2020-02-01")) %>% 
  filter(!is.na(customers_percent_pec) &
           !is.na(canton_id)) %>% 
  dplyr::select(total, j_icd10, asthma, copd_icd10, flu_icd10, population_int) %>% 
  summarize(
    total = sum(total, na.rm=T),
    j_icd10 = sum(j_icd10, na.rm=T),
    asthma = sum(asthma, na.rm=T),
    copd_icd10 = sum(copd_icd10, na.rm=T),
    flu_icd10 = sum(flu_icd10, na.rm=T),
    population = sum(population_int, na.rm=T)
  ) %>% 
  pivot_longer(-population) %>% 
  mutate(
    rate = value / population * 100000,
    events = value
  ) %>% 
  mutate(
    rate = round(rate, digits=2)
  ) %>% 
  mutate(
    name = plyr::mapvalues(
      name,
      from=c("total", "j_icd10", "asthma", "copd_icd10", "flu_icd10"),
      to=c("All hospitalizations","Respiratory", "Asthma", "COPD","Influenza and pneumonia")
    )
  )

hosp_regression_events_fig <- 
  ggplot(
    total_events_rate_df, 
    aes(y=name, x="Total events", label=value)
  ) + 
  geom_text(size=3) +
  scale_y_discrete(limits=
                     rev(c("All hospitalizations",
                           "Respiratory", 
                           "Influenza and pneumonia",
                           "COPD",
                           "Asthma"))) +
  xlab("Total events") + 
  theme_void() +
  theme(
    axis.text.y = element_text(size=8, color="black",hjust =1),
    axis.title.x.top = element_text(size=7, color="black", face="bold")
  )


hosp_regression_rate_fig <- 
  ggplot(
    total_events_rate_df, 
    aes(y=name, x="Rate per 100k population", label=rate)
  ) + 
  geom_text(size=3) +
  scale_y_discrete(limits=
                     rev(c("All hospitalizations",
                           "Respiratory", 
                           "Influenza and pneumonia",
                           "COPD",
                           "Asthma"))) +
  theme_void() +
  xlab("Rate per 100k population") + 
  theme(axis.title.x.top = element_text(size=7, color="black", face="bold"))


main_lr_combined_full_estimates <-
  main_lr_combined_full %>% 
  dplyr::group_by(outcome, adjusted) %>% 
  dplyr::summarise(
    q25 = quantile(estimate, probs=.025, na.rm=T)*100,
    q50 = quantile(estimate, probs=.50, na.rm=T)*100,
    q75 = quantile(estimate, probs=.975, na.rm=T)*100,
  ) %>% 
  mutate(
    effect_size = paste0(round(q50, digits=2), "% (", 
                         round(q25, digits=2),"%, ", 
                         round(q75, digits=2),"%)")
  ) %>% 
  filter(outcome=="Total hospitalizations" |
           outcome=="Respiratory system" | 
           outcome=="Influenza and pneumonia" |
           outcome=="COPD" |
           outcome=="Asthma") %>% 
  mutate(name = plyr::mapvalues(
    outcome,
    from=c("Total hospitalizations",
           "Respiratory system",
           "Influenza and pneumonia",
           "COPD",
           "Asthma"),
    to=c("All hospitalizations",
         "Respiratory",
         "Influenza and pneumonia",
         "COPD",
         "Asthma")
  ))


hosp_regression_effectsize_fig <- 
  ggplot(
    main_lr_combined_full_estimates %>% 
      ungroup() %>% 
      dplyr::mutate(label_group = paste0(name, adjusted),
                    number = row_number()), 
    aes(y=number, x="Effect size", label=effect_size)
  ) + 
  geom_text(size=3,nudge_y = .2) +
  theme_void() +
  xlab("Effect size") + 
  theme(axis.title.x.top = element_text(size=7, color="black", face="bold"))


coef_fig_comb <- plot_grid(
  hosp_regression_events_fig,
  hosp_regression_rate_fig,
  coef_fig,
  hosp_regression_effectsize_fig,
  align="h",
  nrow=1,
  rel_widths=c(0.4, 0.2, 1, 0.5)
)


coef_fig_comb_lab <- plot_grid(
  NULL,
  coef_fig_comb,
  nrow=2,
  rel_heights=c(0.05, 1)
) +
  annotate("text", x=0.08, y=0.95, label="Outcome", size=3, fontface="bold") +
  annotate("text", x=0.155, y=0.95, label="Total events", size=3, fontface="bold") +
  annotate("text", x=0.25, y=0.95, label="Rate per 100k", size=3, fontface="bold") +
  annotate("text", x=0.875, y=0.95, label="Median effect size (2.5%tile, 97.5%tile)", size=3, fontface="bold") 

# cowplot::ggsave2(
#   "~/Desktop/Ecuador Electricity/Figures/coef_fig_comb_block_prov_v_June7_2.pdf",
#   plot = coef_fig_comb_lab,
#   dpi = 300,
#   height = 120,
#   width = 300,
#   unit = "mm"
# )

