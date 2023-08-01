

# 0. Libraries ----

library(tidyverse)
library(readxl)
library(lubridate)
library(fixest)
library(broom)
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
library(arsenal)
library(fixest)
library(geomtextpath)


# 1. Data -------

toplot_electricity_pec_excess_kwh <- read_rds("~/Desktop/pec_kwh_coefs.rds")
kwh_co2e_summary <- read_rds("~/Desktop/kwh_co2e_summary.rds")
lpg_electricity_ctrfact_1 <- read_rds("~/Desktop/lpg_electricity_ctrfact_1_June.rds")
toplot_pec_gas <- read_rds("~/Desktop/toplot_pec_gas_coefs_June.rds")


# full arconel electricity data  ------

electricity_joined <-
  read_rds(
    "~/Desktop/Ecuador Electricity/Data/ARCONEL/Processed/arconel_electricity_joined.rds"
  )


# LPG data --------

lpg <-
  read_xlsx("~/Documents/ecuador_lpg_consumption.xlsx", sheet = 3)

# Shape file -----

ecu_shp <-
  sf::read_sf("~/Downloads/ecu_adm_inec_20190724_shp/ecu_admbnda_adm3_inec_20190724.shp")

ecu_shp_df_names <- read_xlsx("~/Documents/ecu_shp_df_names.xlsx")


# 1. Clean data and process data -------

# Model-based changes attributable to increased PEC enrollment ---------

# Linear regressions: kwh per customer ~ pec -------


reg0_lm <-
  fixest::feols(
    kwh_per_total_customers ~
      customers_percent_pec_1 |
      province_canton_parish +
      month_year_f +
      canton_month_fe,
    data = electricity_joined_1 %>%
      ungroup() %>% 
      mutate(customers_percent_pec_1 = customers_percent_pec * 100)
  )

tidy(reg0_lm, conf.int = T)

reg1_lm <-
  fixest::feols(
    kwh_per_total_customers ~
      customers_percent_pec_10 |
      province_canton_parish +
      month_year_f +
      canton_month_fe,
    data = electricity_joined_1 %>%
      ungroup() %>%
      filter(
        month_year < ymd("2021-07-01") &
          month_year != ymd("2020-03-01") &
          month_year != ymd("2020-04-01") &
          month_year != ymd("2020-05-01") &
          month_year != ymd("2020-06-01")
      ) %>%
      filter(month_year != ymd("2019-05-01")) %>%
      filter(kwh_billed > 0 & outlier == 0)
  )

reg1_lm

reg1b_lm <-
  fixest::feols(
    kwh_per_total_customers ~
      customers_percent_pec_10 |
      province_canton_parish +
      month_year_f +
      canton_month_fe,
    data = electricity_joined_1 %>%
      filter(month_year < ymd("2021-07-01")) %>%
      filter(kwh_billed > 0 & outlier == 0)
  )

reg1b_lm

reg2_lm <-
  fixest::feols(
    kwh_per_total_customers ~
      customers_percent_pec_10 |
      province_canton_parish +
      month_year_f,
    data = electricity_joined_1 %>%
      filter(kwh_billed > 0 & outlier == 0)
  )

reg3_lm <-
  fixest::feols(
    kwh_per_total_customers ~
      customers_percent_pec_10 |
      province_canton_parish +
      mo +
      yr +
      canton_month_fe,
    data = electricity_joined_1 %>%
      filter(kwh_billed > 0 & outlier == 0)
  )

reg4_lm <-
  fixest::feols(
    kwh_per_total_customers ~
      customers_percent_pec_10 |
      province_canton_parish +
      mo +
      yr,
    data = electricity_joined_1 %>%
      filter(kwh_billed > 0 & outlier == 0)
  )

# fixest::esttex(reg0_lm, reg1b_lm, reg1_lm, reg2_lm, reg3_lm, reg4_lm)


# Bootstrap main regression 1000 times -----

toplot <- data.frame(matrix(NA, 1, 2))
colnames(toplot) <- c("i", "cf")


xvar = "customers_percent_pec"
yvar = "kwh_per_total_customers"
fe = "parish_fe + month_year_f + canton_month_fe"
xunit = "province_canton_parish"
cc <- unique(electricity_joined_1[[xunit]])
set.seed(22)
out <- c()
electricity_joined_1$xunit <- electricity_joined_1[[xunit]]
fmla <- as.formula(paste(
  yvar,
  "~",
  xvar,
  " |",
  fe
))

electricity_joined_1$xunit = electricity_joined_1$province_canton_parish
cc <- unique(electricity_joined_1$xunit)

for (i in 1:1000) {
  samp <- data.frame(xunit = sample(cc, length(cc), replace = T)) %>% 
    mutate(parish_fe = factor(row_number()))
  
  subdata <-
    inner_join(electricity_joined_1, samp, by = "xunit")
  
  cf <- coef(feols(fmla, data = subdata))
  
  toplot <- toplot %>% 
    bind_rows(data.frame(i, cf))
  
  print(i)
  
}  

toplot_electricity_pec_excess_kwh <- 
  toplot %>% filter(!is.na(i)) %>% as_tibble()

quantile(toplot_electricity_pec_excess_kwh$cf, probs=c(0.025, .5, .975))/100

# write_rds(toplot_electricity_pec_excess_kwh, "~/Desktop/pec_kwh_coefs.rds")

# reg1_lm

# Block bootstrapping -------

toplot <- data.frame(matrix(NA, 1, 2))
colnames(toplot) <- c("i", "cf")

           
xvar = "customers_percent_pec_10"
yvar = "kwh_per_total_customers"
fe = "parish_fe + month_year_f + canton_month_fe"
xunit = "province_canton_parish"
cc <- unique(electricity_joined_1[[xunit]])
set.seed(22)
out <- c()
electricity_joined_1$xunit <- electricity_joined_1[[xunit]]
fmla <- as.formula(paste(
    yvar,
    "~",
    xvar,
    " |",
    fe
  ))

# block bootstrapping 
electricity_parish_canton <- electricity_joined_1 %>% 
  dplyr::select(parish_corrected1, canton_corrected) %>% 
  dplyr::distinct()

electricity_joined_1$xunit = electricity_joined_1$canton_corrected
cc <- unique(electricity_joined_1$xunit)

for (i in 1:1000) {
    samp <- data.frame(xunit = sample(cc, length(cc), replace = T)) %>% 
      mutate(parish_fe = factor(row_number()))
    
    subdata <-
      inner_join(electricity_joined_1, samp, by = "xunit")
    
      cf <- coef(feols(fmla, data = subdata))
      toplot <- toplot %>% 
        bind_rows(data.frame(i, cf))
      print(i)
}  

toplot_electricity_pec_excess_kwh <- 
  toplot %>% filter(!is.na(i)) %>% as_tibble()

# Prediction and estimating total kwh due to pec --------

# overall difference across all bootstrapped specifications --------


electricity_joined_1_ctrfact_month1 <- 
  electricity_joined_1 %>% 
  ungroup() %>% 
  filter(kwh_billed>0 & outlier==0) %>% 
  mutate(
    kwh_billed_ctrfact = 
      kwh_per_total_customers*total_customers - 
      (customers_percent_pec*6.42*total_customers)
  )  %>% 
  dplyr::group_by(month_year) %>% 
  dplyr::summarize(
    kwh_billed = sum(kwh_billed, na.rm=T),
    kwh_billed_ctrfact = sum(kwh_billed_ctrfact, na.rm=T)
  ) %>% 
  mutate(model="original")

kwh_co2e <- 
  electricity_joined_1_ctrfact_month1 %>%
  filter(month_year<=ymd("2021-09-01")) %>% 
  mutate(
    kwh_billed_difference = (kwh_billed - kwh_billed_ctrfact) * 10
  ) %>% 
  mutate(
    kwh_billed_difference_co2e = 
      ifelse(year(month_year)==2015, kwh_billed_difference*0.676,
             ifelse(year(month_year)==2016, kwh_billed_difference*0.6431,
                    ifelse(year(month_year)==2017, kwh_billed_difference*0.4867,
                           ifelse(year(month_year)==2018, kwh_billed_difference*0.5319,
                                  ifelse(year(month_year)==2019, kwh_billed_difference*0.4509,
                                         ifelse(year(month_year)==2020, kwh_billed_difference*0.3834,
                                                ifelse(year(month_year)==2021, kwh_billed_difference*0.3834, NA))))))),
    
    kwh_billed_co2e = 
      ifelse(year(month_year)==2015, kwh_billed*0.676,
             ifelse(year(month_year)==2016, kwh_billed*0.6431,
                    ifelse(year(month_year)==2017, kwh_billed*0.4867,
                           ifelse(year(month_year)==2018, kwh_billed*0.5319,
                                  ifelse(year(month_year)==2019, kwh_billed*0.4509,
                                         ifelse(year(month_year)==2020, kwh_billed*0.3834,
                                                ifelse(year(month_year)==2021, kwh_billed*0.3834, NA)))))))
  )

kwh_co2e_summary <-
  kwh_co2e %>% 
  summarize(
    kwh_billed_co2e = sum(kwh_billed_co2e, na.rm=T),
    kwh_billed = sum(kwh_billed, na.rm=T),
    kwh_billed_difference_co2e = sum(kwh_billed_difference_co2e, na.rm=T),
    kwh_billed_difference = sum(kwh_billed_difference, na.rm=T)) %>% 
  mutate(model = "Main")


electricity_joined_1_ctrfact1 <- 
  data.frame(matrix(NA, 1, 3))

colnames(electricity_joined_1_ctrfact1) <- c("n", "difference", "percent_difference")

for (n in 1:1000) {
  
  cf = toplot_electricity_pec_excess_kwh %>% filter(i==n) %>% dplyr::select(cf) %>% unlist()
  
  electricity_joined_1_tmp <-
    electricity_joined_1 %>%
    ungroup() %>% 
    filter(kwh_billed>0 & outlier==0) %>% 
    mutate(
      kwh_billed_ctrfact = 
        kwh_per_total_customers*total_customers - 
        (customers_percent_pec*(cf/100)*total_customers)
    ) %>% 
    dplyr::select(
      month_year, province, canton_corrected, parish_corrected1, 
      total_customers, customers_percent_pec,
      kwh_per_total_customers,
      kwh_billed, kwh_billed_ctrfact) %>% 
    mutate(
      model = n
      ) 
  
  difference <- 
    sum(electricity_joined_1_tmp$kwh_billed, na.rm=T) -
    sum(electricity_joined_1_tmp$kwh_billed_ctrfact, na.rm=T)
  
  percent_difference <- 
    difference / sum(electricity_joined_1_tmp$kwh_billed, na.rm=T)
  
  
  electricity_joined_1_ctrfact1 <- 
    electricity_joined_1_ctrfact1 %>% 
    bind_rows(data.frame(n, difference, percent_difference))
  
  
  electricity_joined_1_tmp <-
    electricity_joined_1 %>%
    ungroup() %>% 
    # filter(kwh_billed>0 & outlier==0) %>% 
    mutate(
      kwh_billed_ctrfact = 
        kwh_per_total_customers*total_customers - 
        (customers_percent_pec*cf*total_customers)
    ) %>% 
    dplyr::group_by(month_year) %>% 
    dplyr::summarize(
      kwh_billed = sum(kwh_billed, na.rm=T),
      kwh_billed_ctrfact = sum( kwh_billed_ctrfact, na.rm=T)
    ) %>% 
    mutate(
      model = as.character(n)
    ) 
  
  kwh_co2e <- 
    electricity_joined_1_tmp %>%
    filter(month_year<=ymd("2021-09-01")) %>% 
    mutate(
      kwh_billed_difference = (kwh_billed - kwh_billed_ctrfact) * 10
    ) %>% 
    mutate(
      kwh_billed_difference_co2e = 
        ifelse(year(month_year)==2015, kwh_billed_difference*0.676,
               ifelse(year(month_year)==2016, kwh_billed_difference*0.6431,
                      ifelse(year(month_year)==2017, kwh_billed_difference*0.4867,
                             ifelse(year(month_year)==2018, kwh_billed_difference*0.5319,
                                    ifelse(year(month_year)==2019, kwh_billed_difference*0.4509,
                                           ifelse(year(month_year)==2020, kwh_billed_difference*0.3834,
                                                  ifelse(year(month_year)==2021, kwh_billed_difference*0.3834, NA))))))),
      
      kwh_billed_co2e = 
        ifelse(year(month_year)==2015, kwh_billed*0.676,
               ifelse(year(month_year)==2016, kwh_billed*0.6431,
                      ifelse(year(month_year)==2017, kwh_billed*0.4867,
                             ifelse(year(month_year)==2018, kwh_billed*0.5319,
                                    ifelse(year(month_year)==2019, kwh_billed*0.4509,
                                           ifelse(year(month_year)==2020, kwh_billed*0.3834,
                                                  ifelse(year(month_year)==2021, kwh_billed*0.3834, NA)))))))
    )
  
  kwh_co2e1 <-
    kwh_co2e %>% 
    dplyr::summarize(
      kwh_billed_co2e = sum(kwh_billed_co2e, na.rm=T),
      kwh_billed = sum(kwh_billed, na.rm=T),
      kwh_billed_difference_co2e = sum(kwh_billed_difference_co2e, na.rm=T),
      kwh_billed_difference = sum(kwh_billed_difference, na.rm=T)) %>% 
    mutate(model = as.character(i))
  
  
  kwh_co2e_summary <- 
    kwh_co2e_summary %>% 
    bind_rows(kwh_co2e1)
  
  
  print(n)
  
}

summary(kwh_co2e_summary)

# write_rds(kwh_co2e_summary, "~/Desktop/kwh_co2e_summary1.rds")

# kwh_co2e_summary <- read_rds("~/Desktop/kwh_co2e_summary.rds")

# Excess electricity line plot ------

electricity_joined_1_ctrfact_example <-
  electricity_joined_1 %>%
  mutate(
    kwh_billed_ctrfact = 
      kwh_per_total_customers*total_customers -
      (customers_percent_pec*(64.2)*total_customers)
  ) 

excess_kwh_pec_line_plot <- 
  ggplot(electricity_joined_1_ctrfact_example %>% 
           filter(month_year<=ymd("2021-09-01")) %>% 
         dplyr::group_by(month_year) %>% 
           dplyr::summarize(
           kwh_billed = sum(kwh_billed, na.rm=T),
           kwh_billed_ctrfact = sum(kwh_billed_ctrfact, na.rm=T),
         ) %>% 
           filter(!is.na(kwh_billed_ctrfact)) %>%
         pivot_longer(-month_year) %>% 
         mutate(name = ifelse(name=="kwh_billed", "with PEC", "without PEC")), 
       aes(x=month_year, y=value, color=name)) + 
  geom_line(size=1.02) + 
  annotate("text", x=ymd("2021-08-01"), y=5.6e8, label="without PEC", color=met.brewer("Tiepolo")[1], size=2.75) + 
  annotate("text", x=ymd("2021-09-01"), y=6.1e8, label="with PEC",color=met.brewer("Tiepolo")[8], size=2.75) + 
  scale_color_manual(values=c(met.brewer("Tiepolo")[8], met.brewer("Tiepolo")[1])) + 
  scale_y_continuous(
    breaks = c(5e8,5.5e8, 6e8, 6.5e8, 7e8),
    limits = c(5e8, 7.3e8),
    labels = scales::unit_format(
      scale = .000001,
      suffix = " million kWh",
      accuracy = 1
    )) +

  scale_x_date(
    breaks = c(
      ymd("2015-01-01"),
      ymd("2016-01-01"),
      ymd("2017-01-01"),
      ymd("2018-01-01"),
      ymd("2019-01-01"),
      ymd("2020-01-01"),
      ymd("2021-01-01"),
      ymd("2022-01-01")
    ),
    limits = c(ymd("2015-01-01"), ymd("2021-09-01")),
    labels = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
  ) +
  ggtitle("Excess household electricity consumption from PEC") +
  theme_classic() +
  coord_cartesian(clip = "on") +
  theme(
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      size = 9,
      color = "black",
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 9,
      color = "black"
    ),
    legend.position="none",
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75"),
    axis.line.x.bottom = element_line(size = 0.5, color = "black"),
    axis.line.y.left = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  )

summary(electricity_joined_1_ctrfact1$difference*100)
summary(electricity_joined_1_ctrfact1$percent_difference*100)


total_diff_boxplot <-
  ggplot() + 
  geom_boxplot(data=electricity_joined_1_ctrfact1,
               aes(x=0, y=difference*100), outlier.shape = NA, width=.25) +
  geom_jitter(data=electricity_joined_1_ctrfact1, 
             aes(x=.25, y=difference*100), alpha=0.75, shape=95, width=.05) + 
  annotate("text", x=0, y=3.327e+09-0.15e9, label="6.5% increase", size=2) + 
  annotate("text", x=0, y=2.660e+09+0.2e9, label="5.2% increase", size=2) + 
  annotate("text", x=0, y=1.790e+09+0.2e9, label="3.5% increase", size=2) + 
  theme_classic() +
  ggtitle("Total excess kWh") + 
  scale_x_continuous(breaks=c(.25/2), labels=c("2015-2021")) + 
  scale_y_continuous(
    breaks=c(-2e9, 0, 2e9, 4e9, 6e9, 8e9),
    position = "right",
    labels = scales::unit_format(
      scale = .000000001,
      big.mark = ",",
      suffix = " billion kWh",
      accuracy = 1
    )) + 
  coord_cartesian(
    # ylim=c(0, 5e9),
    clip = "on") +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      size = 9,
      color = "black",
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 9,
      color = "black"
    ),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75"),
    axis.line.x.bottom = element_line(size = 0.5, color = "black"),
    axis.line.y.left = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  )

total_diff_boxplot_annotate <-
  ggdraw(total_diff_boxplot) + 
  annotate("text", x=.8, y=.88, label="1,000 draws shown", size=1.5) + 
  geom_curve(
    aes(x = .75, y = .86, xend = .65, yend = .76),
    curvature = 0.13,
    arrow = arrow(length = unit(0.03, "npc")),
    size=.4
  ) 

excess_kwh_pec_combined <- 
  plot_grid(
  excess_kwh_pec_line_plot,
  NULL,
  total_diff_boxplot,
  align="h",
  nrow=1,
  rel_widths = c(1, .15, 0.25), 
  labels=c("A", "", "B")
)

excess_kwh_pec_combined

# ggsave2(
#   "~/Desktop/excess_kwh_pec_combined.png",
#   excess_kwh_pec_combined,
#   height = 100,
#   width = 260,
#   units = "mm",
#   dpi = 600
# )

# GAS AND ELECTRICITY TRADE OFF ----------

# National level data since 2015 or earlier ------

lpg_electricity <- 
  lpg %>% 
  mutate(lpg_kg = lpg_internal_consumption_millionsbarrels*101.6064*1000) %>% 
  left_join(
    electricity_joined_1 %>% 
      dplyr::group_by(month_year) %>% 
      dplyr::summarize(
        kwh_billed = sum(kwh_billed, na.rm=T),
        kwh_subsidized = sum(kwh_subsidized_pec, na.rm=T),
        pec = sum(pec_customers_na, na.rm=T) / sum(total_customers, na.rm=T)
      )
  ) %>%
  left_join(
      electricity_joined_1 %>%
      ungroup() %>% 
      filter(kwh_billed>0 & outlier==0) %>% 
      mutate(
        kwh_billed_ctrfact = 
          kwh_per_total_customers*total_customers - 
          (customers_percent_pec*64.2*total_customers)
      )  %>% 
      mutate(kwh_billed_difference = kwh_billed - kwh_billed_ctrfact) %>% 
      dplyr::group_by(month_year) %>%
      dplyr::summarize(
        kwh_billed_difference = -sum(kwh_billed_difference, na.rm = T),
        total_customers = sum(total_customers, na.rm=T)
      ) 
  ) %>% 
  mutate(
    month = factor(month(month_year)),
    year = factor(year(month_year))
  )


lpg_elec_reg1 <- 
  fixest::feols(
    lpg_kg ~
      pec | 
      month + 
      year,
    lpg_electricity
  )

lpg_elec_reg1

cf = tidy(lpg_elec_reg1)[1,2] %>% unlist()

lpg_electricity %>%
  ungroup() %>% 
  mutate(
    lpg_kg_ctrfact = 
      lpg_kg +
      (cf*pec)
  ) %>% 
  mutate(
    lpg_kg_difference = lpg_kg - lpg_kg_ctrfact
  ) %>% 
  summarize(
    lpg_kg_difference =  sum(lpg_kg_difference, na.rm=T)
  )
# 113 million


lpg_elec_reg1b <- 
  fixest::feols(
    lpg_kg ~
      pec +
      total_customers | 
      month + 
      year,
    lpg_electricity
  )

lpg_elec_reg1b

lpg_elec_reg2 <- 
  fixest::feols(
    lpg_kg ~
      kwh_billed_difference | 
      month + 
      year,
    lpg_electricity
  )

lpg_elec_reg2
tidy(lpg_elec_reg2,conf.int = T)

cf = tidy(lpg_elec_reg2)[1,2] %>% unlist()

lpg_electricity_reg <- 
  lpg_electricity %>% 
  filter(month_year>=ymd("2015-01-01"))



lpg_electricity_reg %>%
  ungroup() %>% 
  mutate(
    lpg_kg_ctrfact = 
      lpg_kg +
      (cf*kwh_billed_difference)
  ) %>% 
  mutate(
    lpg_kg_difference = lpg_kg - lpg_kg_ctrfact
  ) %>% 
  summarize(
    lpg_kg_difference =  sum(lpg_kg_difference, na.rm=T)
  )




tidy(lpg_elec_reg2,conf.int = T)


lpg_electricity_difference_co2e <- data.frame(matrix(NA, 1, 7))
colnames(lpg_electricity_difference_co2e) <- c("k", "lpg_difference", "lpg_percent_difference", "lpg_difference_co2e", "kwh_difference", "kwh_percent_difference", "kwh_billed_difference_co2e")
toplot <- data.frame(matrix(NA, 1, 2))
colnames(toplot) <- c("i", "cf")

xvar = "kwh_billed_difference"
yvar = "lpg_kg"
fe = "month + year"
xunit = "month_year"
cc <- unique(lpg_electricity_reg[[xunit]])
set.seed(24)
out <- c()
lpg_electricity_reg$xunit <-
      lpg_electricity_reg[[xunit]]

fmla <- as.formula(paste(
      yvar,
      "~",
      xvar,
      " |",
      fe
    ))

tictoc::tic()
# This full bootstrapping can take a really long time
# One can break it up by doing 1:200, and then 200:400, etc.
for (i in 1:1000) {
  
  tictoc::tic()
  lpg_electricity <- 
    lpg %>% 
    mutate(lpg_kg = lpg_internal_consumption_millionsbarrels*101.6064*1000) %>% 
    left_join(
      electricity_joined_1 %>% 
        group_by(month_year) %>% 
        summarize(
          kwh_billed = sum(kwh_billed, na.rm=T),
          kwh_subsidized = sum(kwh_subsidized_pec, na.rm=T),
          pec = sum(pec_customers_na, na.rm=T) / sum(total_customers, na.rm=T)
        )
    ) %>%
    left_join(
      electricity_joined_1 %>%
        ungroup() %>% 
        filter(kwh_billed>0 & outlier==0) %>% 
        mutate(
          kwh_billed_ctrfact = 
            kwh_per_total_customers*total_customers - 
            (customers_percent_pec*toplot_electricity_pec_excess_kwh$cf[i]*total_customers)
        )  %>% 
        mutate(kwh_billed_difference = kwh_billed - kwh_billed_ctrfact) %>% 
        dplyr::group_by(month_year) %>%
        dplyr::summarize(
          kwh_billed_difference = -sum(kwh_billed_difference, na.rm = T)
        ) 
    ) %>% 
    mutate(
      month = factor(month(month_year)),
      year = factor(year(month_year))
    ) %>% 
    mutate(
      kwh_billed_difference_co2e = 
        ifelse(year(month_year)==2015, kwh_billed_difference*0.676,
               ifelse(year(month_year)==2016, kwh_billed_difference*0.6431,
                      ifelse(year(month_year)==2017, kwh_billed_difference*0.4867,
                             ifelse(year(month_year)==2018, kwh_billed_difference*0.5319,
                                    ifelse(year(month_year)==2019, kwh_billed_difference*0.4509,
                                           ifelse(year(month_year)==2020, kwh_billed_difference*0.3834,
                                                  ifelse(year(month_year)==2021, kwh_billed_difference*0.2953, NA)))))))
    )
  
  for (k in 1:1000) {
    samp <- data.frame(xunit = sample(cc, length(cc), replace = T))
    subdata <-
      inner_join(lpg_electricity_reg, samp, by = "xunit")
    
    cf <- coef(feols(fmla, data = subdata))
    
    lpg_electricity_ctrfact_1_tmp <-
      lpg_electricity_reg %>%
      ungroup() %>% 
      # filter(kwh_billed>0 & outlier==0) %>% 
      mutate(
        lpg_kg_ctrfact = 
          lpg_kg +
          (cf*kwh_billed_difference)
      )  %>% 
      mutate(
        model = k
      )
    
    lpg_difference <- 
      sum(lpg_electricity_ctrfact_1_tmp$lpg_kg_ctrfact, na.rm=T) - 
      sum(lpg_electricity_ctrfact_1_tmp$lpg_kg, na.rm=T)
    
    lpg_percent_difference <- 
      lpg_difference / sum(lpg_electricity_ctrfact_1_tmp$lpg_kg, na.rm=T)

    lpg_difference_co2e <- 
      lpg_difference*2.992
    
    kwh_difference <- 
      sum(lpg_electricity$kwh_billed_difference, na.rm=T) 
    
    kwh_billed_difference_co2e <- 
      sum(lpg_electricity$kwh_billed_difference_co2e, na.rm=T)
    
    kwh_percent_difference <- 
      kwh_difference / sum(lpg_electricity$kwh_billed, na.rm=T)
    

    lpg_electricity_difference_co2e <- 
      lpg_electricity_difference_co2e %>% 
      bind_rows(data.frame(i, k, 
                           lpg_difference, lpg_percent_difference, lpg_difference_co2e, 
                           kwh_difference, kwh_percent_difference, kwh_billed_difference_co2e)) 
    
    
  }  

  tictoc::toc()  
  print(i)
}
tictoc::toc()  

# write_rds(lpg_electricity_difference_co2e, "~/Desktop/lpg_electricity_difference_co2e_5.rds")

# this would be the result of 1:200, 200:400 etc. 
# lpg_electricity_difference_co2e_1 <- read_rds("~/Desktop/lpg_electricity_difference_co2e_1.rds")
# lpg_electricity_difference_co2e_2 <- read_rds("~/Desktop/lpg_electricity_difference_co2e_2.rds")
# lpg_electricity_difference_co2e_3 <- read_rds("~/Desktop/lpg_electricity_difference_co2e_3.rds")
# lpg_electricity_difference_co2e_4 <- read_rds("~/Desktop/lpg_electricity_difference_co2e_4.rds")
# lpg_electricity_difference_co2e_5 <- read_rds("~/Desktop/lpg_electricity_difference_co2e_5.rds")
# 
# lpg_electricity_difference_co2e <- 
#   lpg_electricity_difference_co2e_1 %>% 
#   bind_rows(lpg_electricity_difference_co2e_2,
#             lpg_electricity_difference_co2e_3,
#             lpg_electricity_difference_co2e_4,
#             lpg_electricity_difference_co2e_5)
# 
# hist(lpg_electricity_difference_co2e$lpg_difference_co2e + lpg_electricity_difference_co2e$kwh_billed_difference_co2e)
# lpg_electricity_difference_co2e$net_co2e <- lpg_electricity_difference_co2e$lpg_difference_co2e + lpg_electricity_difference_co2e$kwh_billed_difference_co2e
# summary(lpg_electricity_difference_co2e$net_co2e)
# table(lpg_electricity_difference_co2e$net_co2e>0)

# # write_rds(toplot_pec_gas, "~/Desktop/toplot_pec_gas_coefs_June.rds")
#  
# # toplot_pec_gas <- read_rds("~/Desktop/toplot_pec_gas_coefs.rds")
# 
# lpg_electricity_ctrfact_1 <- 
#   data.frame(matrix(NA, 1, 3))
# 
# colnames(lpg_electricity_ctrfact_1) <- c("n", "difference", "percent_difference")
# 
# for (n in 1:nrow(toplot_pec_gas)) {
#   
#   cf = toplot_pec_gas[n, "cf"] %>%  unlist()
#   
#   lpg_electricity_ctrfact_1_tmp <-
#     lpg_electricity_reg %>%
#     ungroup() %>% 
#     # filter(kwh_billed>0 & outlier==0) %>% 
#     mutate(
#       lpg_kg_ctrfact = 
#         lpg_kg +
#         (cf*kwh_billed_difference)
#     )  %>% 
#     mutate(
#       model = n
#     )
#   
#   difference <- 
#     sum(lpg_electricity_ctrfact_1_tmp$lpg_kg_ctrfact, na.rm=T) - 
#     sum(lpg_electricity_ctrfact_1_tmp$lpg_kg, na.rm=T)
#     
#   
#   percent_difference <- 
#     difference / sum(lpg_electricity_ctrfact_1_tmp$lpg_kg, na.rm=T)
#   
#   
#   lpg_electricity_ctrfact_1 <- 
#     lpg_electricity_ctrfact_1 %>% 
#     bind_rows(data.frame(n, difference, percent_difference))
#   
#   print(n)
#   
# }

# write_rds(lpg_electricity_ctrfact_1, "~/Desktop/lpg_electricity_ctrfact_1_June.rds")
# lpg_electricity_ctrfact_1 <- read_rds("~/Desktop/lpg_electricity_ctrfact_1_June.rds")

# differences ------

cf = tidy(lpg_elec_reg2)[1,2] %>% unlist()

lpg_electricity_ctrfact_long <- 
  lpg_electricity_reg %>% 
  ungroup() %>% 
  # filter(kwh_billed>0 & outlier==0) %>% 
  mutate(
    lpg_kg_ctrfact = 
      lpg_kg + 
      (cf*kwh_billed_difference)
  )  %>% 
  dplyr::select(month_year, lpg_kg, lpg_kg_ctrfact) %>% 
  pivot_longer(-month_year)


# lpg kg reductions fig --------
lpg_electricity_ctrfact_long

lpg_elec_line_plot <- 
  ggplot(lpg_electricity_ctrfact_long %>% 
               mutate(name = ifelse(name=="lpg_kg", "Observed", "without PEC")) %>% 
               mutate(month_year=ymd(month_year)) %>% 
               mutate(data = "Total"), 
         aes(x=month_year, y=value, color=name)) + 
  geom_line(size=1.005) + 
  annotate("text", x=ymd("2021-08-01"), y=132000000, label="without PEC", size=3, color=met.brewer("Tiepolo")[1]) + 
  annotate("text", x=ymd("2021-08-01"), y=122000000, label="with PEC", size=3, color=met.brewer("Tiepolo")[8]) + 
  scale_color_manual(values=c(met.brewer("Tiepolo")[1], met.brewer("Tiepolo")[8])) + 
  scale_y_continuous(
    breaks=c(70000000, 80000000, 90000000, 100000000, 110000000, 120000000, 130000000),
    labels = scales::unit_format(
      suffix = " million kg LPG",
      scale = 1/1000000,
      accuracy = 1,
      big.mark = ","
    )) +
  scale_x_date(
    breaks = c(
      ymd("2015-01-01"),
      ymd("2016-01-01"),
      ymd("2017-01-01"),
      ymd("2018-01-01"),
      ymd("2019-01-01"),
      ymd("2020-01-01"),
      ymd("2021-01-01"),
      ymd("2022-01-01")
    ),
    limits = c(ymd("2015-01-01"), ymd("2021-09-01")),
    labels = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
  ) +
  ggtitle("Reduced LPG sales from PEC") +
  theme_classic() +
  coord_cartesian(clip = "on") +
  theme(
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      size = 9,
      color = "black",
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 9
    ),
    legend.position="none",
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75"),
    axis.line.x.bottom = element_line(size = 0.5, color = "black"),
    axis.line.y.left = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  )


summary(lpg_electricity_ctrfact_1$percent_difference, na.rm=T)
summary(lpg_electricity_ctrfact_1$difference, na.rm=T)


lpg_kg_elec_boxplots <- 
  ggplot() + 
  geom_boxplot(
    data=lpg_electricity_ctrfact_1, 
    aes(x=0, y=difference), width=0.25, 
    outlier.shape = NA
  ) +
  geom_jitter(data=lpg_electricity_ctrfact_1 %>% 
                slice_sample(n=10000), 
              aes(x=.25, y=difference), alpha=0.75, shape=95, width=.05) + 
  annotate("text", x=0, y=-6.320e+08+.35e8, label="6.7% decrease", size=2) + 
  annotate("text", x=0, y=-4.817e+08+.35e8, label="5.1% decrease", size=2) + 
  annotate("text", x=0, y=-3.563e+08-.35e8, label="3.8% decrease", size=2) + 
  theme_classic() +
  ggtitle("Total averted LPG") +
  scale_x_continuous(breaks=c(.125), labels=c("2015-2021")) + 
  scale_y_continuous(
    position="right",
    breaks=c(-3e9, -2e9, -1e9, 0, 0.5e9),
    labels = scales::unit_format(  
      suffix = " million kg LPG",
      scale = 1/1000000,
      accuracy = 1,
      big.mark = ",",
    )) +
  theme_classic() +
  coord_cartesian(
    ylim=c(-3e9, 0.7e9),
    clip = "off"
  ) +
  theme(
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      size = 9,
      color = "black",
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 9,
      color = "black"
    ),
    legend.position="right",
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75"),
    axis.line.x.bottom = element_line(size = 0.5, color = "black"),
    axis.line.y.left = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  )


lpg_kg_elec_boxplots_annotate <-
  ggdraw(lpg_kg_elec_boxplots) + 
  annotate("text", x=.8, y=.88, label="10,000 of\175,000 draws shown", size=1.5) + 
  geom_curve(
    aes(x = .75, y = .86, xend = .65, yend = .76),
    curvature = 0.13,
    arrow = arrow(length = unit(0.03, "npc")),
    size=.4
  ) 


lpg_kg_elec_difference_combined <- 
  plot_grid(
    lpg_elec_line_plot,
    NULL,
    lpg_kg_elec_boxplots,
    align="h",
    nrow=1,
    rel_widths=c(1, 0.15, 0.3),
    labels=c("C", "", "D")
  )

lpg_kg_elec_difference_combined

# 4.2 Greenhouse gas emissions -------

# bootstrapped co2e from excess kwh from all runs ------

reg0_lm

electricity_joined_1_ctrfact_month <-
  electricity_joined_1 %>%
  ungroup() %>%
  filter(kwh_billed>0 & outlier==0) %>%
  mutate(
    kwh_billed_ctrfact =
      kwh_per_total_customers*total_customers -
      (customers_percent_pec*0.641967*total_customers)
  )  %>%
  group_by(month_year) %>%
  summarize(
    kwh_billed = sum(kwh_billed, na.rm=T),
    kwh_billed_ctrfact = sum(kwh_billed_ctrfact, na.rm=T)
  ) %>%
  mutate(model="original")

# for (n in 1:1000) {
#   
#   cf = toplot_electricity_pec_excess_kwh %>% filter(i==n) %>% dplyr::select(cf) %>% unlist()
#   
#   electricity_joined_1_tmp <-
#     electricity_joined_1 %>%
#     ungroup() %>% 
#     # filter(kwh_billed>0 & outlier==0) %>% 
#     mutate(
#       kwh_billed_ctrfact = 
#         kwh_per_total_customers*total_customers - 
#         (customers_percent_pec*cf*total_customers)
#     ) %>% 
#     group_by(month_year) %>% 
#     summarize(
#       kwh_billed = sum(kwh_billed, na.rm=T),
#       kwh_billed_ctrfact = sum( kwh_billed_ctrfact, na.rm=T)
#     ) %>% 
#     mutate(
#       model = as.character(n)
#     ) 
# 
#   electricity_joined_1_ctrfact_month <- 
#     electricity_joined_1_ctrfact_month %>% 
#     bind_rows(electricity_joined_1_tmp)
#   
#   print(n)
#   
# }

# bootstrapped co2e from change to lpg from all runs ------

lpg_electricity_ctrfact_1 <- 
  lpg_electricity_reg %>% 
  dplyr::group_by(month_year) %>% 
  dplyr::summarize(
    lpg_kg = sum(lpg_kg, na.rm=T)
  ) %>% 
  mutate(model="original")

for (n in 1:nrow(toplot_pec_gas)) {

  cf = toplot_pec_gas[n,"cf"] %>% unlist()

  lpg_electricity_ctrfact_1_tmp <-
    lpg_electricity_reg %>%
    ungroup() %>%
    mutate(
      lpg_kg_ctrfact =
        lpg_kg +
        (cf*kwh_billed_difference)
    )  %>%
    dplyr::select(month_year, lpg_kg, lpg_kg_ctrfact) %>%
    mutate(
      model = as.character(n)
    )

  lpg_electricity_ctrfact_1 <-
    lpg_electricity_ctrfact_1 %>%
    bind_rows(lpg_electricity_ctrfact_1_tmp)

  print(n)

}

# combining bootstrapped estimates ------

kwh_co2e_ctrfact <-
  electricity_joined_1_ctrfact_month %>%
  filter(month_year<=ymd("2021-09-01")) %>%
  mutate(
    kwh_billed_difference = (kwh_billed - kwh_billed_ctrfact)
  ) %>%
  mutate(
    kwh_billed_difference_co2e =
      ifelse(year(month_year)==2015, kwh_billed_difference*0.676,
         ifelse(year(month_year)==2016, kwh_billed_difference*0.6431,
                ifelse(year(month_year)==2017, kwh_billed_difference*0.4867,
                       ifelse(year(month_year)==2018, kwh_billed_difference*0.5319,
                              ifelse(year(month_year)==2019, kwh_billed_difference*0.4509,
                                     ifelse(year(month_year)==2020, kwh_billed_difference*0.3834,
                                            ifelse(year(month_year)==2021, kwh_billed_difference*0.3834, NA))))))),

    kwh_billed_co2e =
      ifelse(year(month_year)==2015, kwh_billed*0.676,
             ifelse(year(month_year)==2016, kwh_billed*0.6431,
                    ifelse(year(month_year)==2017, kwh_billed*0.4867,
                           ifelse(year(month_year)==2018, kwh_billed*0.5319,
                                  ifelse(year(month_year)==2019, kwh_billed*0.4509,
                                         ifelse(year(month_year)==2020, kwh_billed*0.3834,
                                                ifelse(year(month_year)==2021, kwh_billed*0.3834, NA)))))))
  ) %>% 
  left_join(lpg_electricity_reg %>%
              ungroup() %>%
              mutate(
                lpg_kg_ctrfact =
                  lpg_kg +
                  (0.27*kwh_billed_difference)
              ) %>%
              dplyr::select(month_year, lpg_kg, lpg_kg_ctrfact) %>% 
              mutate(lpg_difference = lpg_kg - lpg_kg_ctrfact)
            )

# kwh_co2e_summary <-
#   kwh_co2e %>% 
#   group_by(model) %>% 
#   summarize(
#     kwh_billed_co2e = sum(kwh_billed_co2e, na.rm=T),
#     kwh_billed = sum(kwh_billed, na.rm=T),
#     kwh_billed_difference_co2e = sum(kwh_billed_difference_co2e, na.rm=T),
#     kwh_billed_difference = sum(kwh_billed_difference, na.rm=T))
# 
# # summary(kwh_co2e_summary)
# 
# lpg_co2e_summary <- 
#   lpg_electricity_ctrfact_1 %>% 
#   filter(month_year<=ymd("2021-09-01")) %>% 
#   mutate(
#     lpg_kg_difference = lpg_kg - lpg_kg_ctrfact,
#   ) %>% 
#   mutate(
#     lpg_kg_ctrfact_co2e = -lpg_kg_difference*2.992,
#     lpg_kg_co2e = lpg_kg*2.992
#     ) %>% 
#   group_by(model) %>% 
#   summarize(
#     lpg_kg_ctrfact_co2e = sum(lpg_kg_ctrfact_co2e, na.rm=T),
#     lpg_kg_co2e = sum(lpg_kg_co2e, na.rm=T)
#     )


# lpg_electricity_ctrfact_1 <- read_rds("~/Desktop/lpg_electricity_ctrfact_1.rds")

combined_co2e_df <-
  dplyr::full_join(kwh_co2e_summary %>% filter(model!="Main") %>% dplyr::select(kwh_billed_difference_co2e), 
                   lpg_electricity_ctrfact_1 %>% 
                     filter(!is.na(n)) %>% 
                     mutate(lpg_kg_ctrfact_co2e = -difference*2.992) %>% 
                     dplyr::select(lpg_kg_ctrfact_co2e), by = character()) 

combined_co2e_df <- 
  combined_co2e_df %>% 
  mutate(net_co2e = kwh_billed_difference_co2e - lpg_kg_ctrfact_co2e)

# write_rds(combined_co2e_df, "~/Desktop/combined_co2e_df_june.rds")
summary(combined_co2e_df$net_co2e)
table(combined_co2e_df$net_co2e>0)
summary(combined_co2e_df$kwh_billed_difference_co2e)
summary(combined_co2e_df$lpg_kg_ctrfact_co2e)

# summary(combined_co2e_df$kwh_billed_difference_co2e)
# summary(combined_co2e_df$lpg_kg_ctrfact_co2e)

# main co2 fig ------

lpg_electricity_ctrfact_co2e <- 
  lpg_electricity_ctrfact_long %>% 
  pivot_wider() %>% 
  filter(month_year<=ymd("2021-09-01")) %>% 
  mutate(
    lpg_kg_difference = lpg_kg - lpg_kg_ctrfact
  ) %>% 
  left_join(
      electricity_joined_1 %>%
        mutate(
          kwh_billed_ctrfact = 
            kwh_per_total_customers*total_customers - 
            (customers_percent_pec*0.659*total_customers*kwh_per_total_customers)
          ) %>% 
        mutate(
          kwh_billed_difference = kwh_billed - kwh_billed_ctrfact
        ) %>% 
        dplyr::select(month_year, kwh_billed, kwh_billed_ctrfact, kwh_billed_difference) %>% 
      dplyr::group_by(month_year) %>% 
        dplyr::summarize(
          kwh_billed_difference = sum(kwh_billed_difference, na.rm=T),
          kwh_billed = sum(kwh_billed, na.rm=T))
  ) %>% 
  mutate(
    lpg_kg_ctrfact_co2e = -lpg_kg_difference*2.992,
    lpg_kg_co2e = lpg_kg*2.992,
    kwh_billed_difference_co2e = 
      ifelse(year(month_year)==2015, kwh_billed_difference*0.676,
             ifelse(year(month_year)==2016, kwh_billed_difference*0.6431,
                    ifelse(year(month_year)==2017, kwh_billed_difference*0.4867,
                           ifelse(year(month_year)==2018, kwh_billed_difference*0.5319,
                                  ifelse(year(month_year)==2019, kwh_billed_difference*0.4509,
                                         ifelse(year(month_year)==2020, kwh_billed_difference*0.3834,
                                                ifelse(year(month_year)==2021, kwh_billed_difference*0.3834, NA))))))),
    kwh_billed_co2e = 
      ifelse(year(month_year)==2015, kwh_billed*0.676,
             ifelse(year(month_year)==2016, kwh_billed*0.6431,
                    ifelse(year(month_year)==2017, kwh_billed*0.4867,
                           ifelse(year(month_year)==2018, kwh_billed*0.5319,
                                  ifelse(year(month_year)==2019, kwh_billed*0.4509,
                                         ifelse(year(month_year)==2020, kwh_billed*0.3834,
                                                ifelse(year(month_year)==2021, kwh_billed*0.3834, NA)))))))
  ) %>% 
  mutate(
    net_co2e = lpg_kg_ctrfact_co2e + kwh_billed_difference_co2e,
    total_co2e = lpg_kg_co2e + kwh_billed_co2e
  )

sum(lpg_electricity_ctrfact_co2e$net_co2e)
sum(lpg_electricity_ctrfact_co2e$total_co2e)
sum(lpg_electricity_ctrfact_co2e$lpg_kg_ctrfact_co2e)
sum(lpg_electricity_ctrfact_co2e$lpg_kg_co2e)
sum(lpg_electricity_ctrfact_co2e$lpg_kg_difference)
sum(lpg_electricity_ctrfact_co2e$kwh_billed_difference_co2e)
sum(lpg_electricity_ctrfact_co2e$kwh_billed_co2e)
sum(lpg_electricity_ctrfact_co2e$kwh_billed_difference)

lpg_electricity_co2e_lineplot <- 
  ggplot() + 
  geom_line(data=lpg_electricity_ctrfact_co2e %>% 
              dplyr::select(month_year, lpg_kg_ctrfact_co2e, kwh_billed_difference_co2e) %>% 
              pivot_longer(-month_year), 
            aes(x=ymd(month_year), y=value, color=name, fill=name)) +
  geom_line(data=lpg_electricity_ctrfact_co2e, 
            aes(x=ymd(month_year), y=net_co2e)) + 
  geom_area(data=lpg_electricity_ctrfact_co2e %>% 
              dplyr::select(month_year, lpg_kg_ctrfact_co2e, kwh_billed_difference_co2e) %>% 
              pivot_longer(-month_year), 
            aes(x=ymd(month_year), y=value, color=name, fill=name), alpha=0.2) +
  geom_area(data=lpg_electricity_ctrfact_co2e, 
            aes(x=ymd(month_year), y=net_co2e), fill="black", alpha=0.5) + 
  scale_color_manual(values=c(met.brewer("Tiepolo")[4], met.brewer("Tiepolo")[6])) + 
  scale_fill_manual(values=c(met.brewer("Tiepolo")[4], met.brewer("Tiepolo")[6])) +
  scale_y_continuous(
    breaks=c(-30e6, -20e6,  -10e6,  0e6,10e6,  20e6, 30e6),
    labels = scales::unit_format(
      suffix = " ktCO2e",
      scale = 1/1000000,
      accuracy = 1,
      big.mark = ","
    )) +
  scale_x_date(
    breaks = c(
      ymd("2015-01-01"),
      ymd("2016-01-01"),
      ymd("2017-01-01"),
      ymd("2018-01-01"),
      ymd("2019-01-01"),
      ymd("2020-01-01"),
      ymd("2021-01-01"),
      ymd("2022-01-01")
    ),
    limits = c(ymd("2015-01-01"), ymd("2021-09-01")),
    labels = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
  ) +
  ylab("") + 
  ggtitle("Monthly changes to greenhouse gas emissions from PEC") +
  annotate("text", x=ymd("2021-06-01"), y=18.25*1000000, label="Electricity", 
           color=met.brewer("Tiepolo")[4], size=3) + 
  annotate("text", x=ymd("2021-06-01"), y=-13.5*1000000, label="Net",
           color="black", 
           size=3) + 
  annotate("text", x=ymd("2021-06-01"), y=-28*1000000, label="LPG", 
           color=met.brewer("Tiepolo")[6], 
           size=3) + 
  theme_classic() +
  coord_cartesian(clip = "on") + 
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      size = 9,
      color = "black",
      face = "bold",
      hjust = 0.05
    ),
    plot.subtitle = element_text(
      size = 9,
      color = "black",
      hjust = 0.035
    ),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75"),
    axis.line.x.bottom = element_line(size = 0.5, color = "black"),
    axis.line.y.left = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  )


combined_co2e_df <- 
  combined_co2e_df %>% 
  ungroup() %>% 
  collapse::fmutate(percent_diff = (net_co2e) / 53099708553)

summary(combined_co2e_df$net_co2e)
summary(combined_co2e_df$percent_diff)

total_co2_diff_boxplot <-
  ggplot() + 
  geom_boxplot(data=lpg_electricity_difference_co2e, 
               aes(x=0, y=net_co2e),
                 outlier.shape=NA, width=.25) +
  geom_jitter(data=lpg_electricity_difference_co2e %>% 
                slice_sample(n=10000),
              aes(x=.25, y=net_co2e), alpha=0.75, shape=95, width=.05) +
  
  annotate("text", x=0, y=-1.715e+09+1.2e8, label="3.2% decrease", size=2) + 
  annotate("text", x=0, y=-9.28e+08-1.2e8, label="1.7% decrease", size=2) + 
  annotate("text", x=0, y=-2.535e+08-1.2e8, label="0.5% decrease", size=2) + 
  theme_classic() +
  ggtitle("Total CO2e averted") + 
  # coord_cartesian(ylim=c(-1.75e9, .3e9)) +
  scale_x_continuous(breaks=c(0.125), labels=c("2015-2021")) + 
  scale_y_continuous(
    # breaks=c(-1.75e9, -1.5e9, -1.25e9, -1e9,-.75e9, -.5e9, -.25e9, 0, .25e9),
    position = "right",
    labels = scales::unit_format(
      suffix = " ktCO2e",
      scale = 1/1000000,
      accuracy = 1,
      big.mark = ","
    )) + 
  coord_cartesian(clip="on", ylim=c(-7545593369, 2512050040)) + 
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      size = 9,
      color = "black",
      face = "bold",
      hjust = 0.05
    ),
    plot.subtitle = element_text(
      size = 9,
      color = "black",
      hjust = 0.035
    ),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_line(size = 0.3, color = "grey75"),
    axis.line.x.bottom = element_line(size = 0.5, color = "black"),
    axis.line.y.left = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.background = element_blank()
  )

total_co2_diff_boxplot


# total_co2_diff_boxplot_annotate <-
#   ggdraw(total_co2_diff_boxplot) + 
#   annotate("text", x=.8, y=.88, label="10,000 of 175m\ndraws shown", size=1.5) + 
#   geom_curve(
#     aes(x = .72, y = .88, xend = .6, yend = .75),
#     curvature = 0.13,
#     arrow = arrow(length = unit(0.03, "npc")),
#     size=.4
#   ) 
# 
# combined_co2e_fig <- 
#   plot_grid(
#     lpg_electricity_co2e_lineplot,
#     NULL,
#     total_co2_diff_boxplot_annotate,
#     nrow=1,
#     rel_widths=c(1, .15, 0.3), 
#     labels=c("E", "", "F")
# )

# combined_co2e_fig


# 4.4 Combining main figures ---------


combined_ts_fig <- 
  cowplot::plot_grid(
    excess_kwh_pec_line_plot,
    NULL, 
    lpg_elec_line_plot,
    NULL, 
    lpg_electricity_co2e_lineplot,
    align="hv",
    nrow=5,
    labels=c("A", "", "C", "","E"),
    rel_heights = c(1, 0.05, 1, .05, 1)
  )

combined_boxes_fig <- 
  cowplot::plot_grid(
    total_diff_boxplot,
    NULL, 
    lpg_kg_elec_boxplots,
    NULL, 
    total_co2_diff_boxplot,
    align="hv",
    nrow=5,
    labels=c("B", "", "D", "","F"),
    label_x = -.08,
    rel_heights = c(1, 0.05, 1, .05, 1)
  )

combined_boxes_ts_fig <- 
  cowplot::plot_grid(
    combined_ts_fig,
    NULL, 
    combined_boxes_fig,
    align="hv",
    nrow=1,
    rel_widths = c(1, 0.05, 1/3)
  )

# cowplot::ggsave2(
#   "~/Desktop/combined_boxes_ts_fig_v2_June.pdf",
#   plot = combined_boxes_ts_fig,
#   dpi = 600,
#   height = 250,
#   width = 275,
#   unit = "mm"
# )

