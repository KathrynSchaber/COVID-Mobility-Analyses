#####
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(RColorBrewer)
library(mgcv)
library(tidyr)
library(tidyverse)
library(rlang)
library(gtable)
library(grid)
library(gridExtra)
library(bbmle)
library(sjPlot)
library(rgdal)
library(usdata)
library(ggpubr)
library(maptools)
library(rgeos)
##
####### read in data #####
setwd("/home/verizon_sftp/jhu_analytics_phase_four/combined/feb_dec")
move_yes_zip_origin_no_age_rel3 <- read.csv("intracity_zip_from_no_outliers_no_age_feats.csv", sep=",", header=TRUE)
move_yes_zip_origin_no_age_rel3$date <- as.Date(move_yes_zip_origin_no_age_rel3$date)
move_yes_zip_origin_no_age_rel3$origin_zip <- as.character(move_yes_zip_origin_no_age_rel3$origin_zip)
move_yes_zip_origin_no_age_rel3$origin_zip[which(nchar(move_yes_zip_origin_no_age_rel3$origin_zip) == 4)] <- paste0("0",move_yes_zip_origin_no_age_rel3$origin_zip[which(nchar(move_yes_zip_origin_no_age_rel3$origin_zip) == 4)])
move_yes_zip_origin_no_age_rel3$origin_zip <- as.factor(move_yes_zip_origin_no_age_rel3$origin_zip)
move_yes_zip_origin_no_age_rel3$in_city_full <- as.factor(move_yes_zip_origin_no_age_rel3$in_city_full)
move_yes_zip_origin_no_age_rel3$in_city_part <- as.factor(move_yes_zip_origin_no_age_rel3$in_city_part)
move_yes_zip_origin_no_age_rel3$school_close_yn <- as.factor(move_yes_zip_origin_no_age_rel3$school_close_yn)
move_yes_zip_origin_no_age_rel3$business_close_yn <- as.factor(move_yes_zip_origin_no_age_rel3$business_close_yn)
move_yes_zip_origin_no_age_rel3$lifestyle_close_yn <- as.factor(move_yes_zip_origin_no_age_rel3$lifestyle_close_yn)
move_yes_zip_origin_no_age_rel3$stay_home_yn <- as.factor(move_yes_zip_origin_no_age_rel3$stay_home_yn)
move_yes_zip_origin_no_age_rel3$zip_dist_city_bord_cat_by_city <- factor(move_yes_zip_origin_no_age_rel3$zip_dist_city_bord_cat_by_city, 
                                                                         levels=c("In City Border", "Dist From City Q1", "Dist From City Q2", "Dist From City Q3", "Dist From City Q4"))
move_yes_zip_origin_no_age_rel3$zip_pop_dens_cat_by_city <- factor(move_yes_zip_origin_no_age_rel3$zip_pop_dens_cat_by_city)
move_yes_zip_origin_no_age_rel3$zip_land_area_cat_by_city <- factor(move_yes_zip_origin_no_age_rel3$zip_land_area_cat_by_city)
move_yes_zip_origin_no_age_rel3$zip_median_hh_income_cat_by_city <- factor(move_yes_zip_origin_no_age_rel3$zip_median_hh_income_cat_by_city)


#### add dist quartile overall variables #####
move_yes_zip_origin_no_age_rel3$zip_dist_city_bord_cat <- numeric(length=nrow(move_yes_zip_origin_no_age_rel3))
temp <- droplevels(move_yes_zip_origin_no_age_rel3[!duplicated(move_yes_zip_origin_no_age_rel3$origin_zip),])
breaks <- quantile(temp$dist_city_border_m[which(temp$in_city_part == 0)], c(0, 0.25, 0.5, 0.75))
move_yes_zip_origin_no_age_rel3$zip_dist_city_bord_cat <- findInterval(move_yes_zip_origin_no_age_rel3$dist_city_border_m, breaks)
move_yes_zip_origin_no_age_rel3$zip_dist_city_bord_cat <- factor(move_yes_zip_origin_no_age_rel3$zip_dist_city_bord_cat, levels=c(0,1,2,3,4), labels=c("In City Border", "Dist From City Q1", "Dist From City Q2", "Dist From City Q3", "Dist From City Q4"))

#### remove october #####
data2 <- droplevels(move_yes_zip_origin_no_age_rel3[which(move_yes_zip_origin_no_age_rel3$date < "2020-10-01"),])

#### add region and date groupings #####
data2$region <- ifelse(data2$city %in% c("Los_Angles_CA", "SanFrancisco_CA", "SanJose_CA", "SanDiego_CA"), "West Coast",
                       ifelse(data2$city %in% c("Austin_TX", "Dallas_TX", "ElPaso_TX", "Houston_TX", "Phoenix_AZ", "SanAntonio_TX"), "South",
                              ifelse(data2$city %in% c("Fargo_ND", "Lincoln_NE", "Omaha_NE", "SiouxFalls_SD"), "Midwest",
                                     ifelse(data2$city %in% c("Chicago_IL", "Columbus_OH", "Detroit_MI"), "Great Lakes",
                                            ifelse(data2$city %in% c("Baltimore_MD","NYC_NY", "Philadelphia_PA"), "Northeast", "Southeast")))))
data2$region <- as.factor(data2$region)

data2$date_period2 <- ifelse(data2$date < "2020-05-15", "mid March - mid May", "mid May - end Sept")
data2$date_period3 <- ifelse(data2$date < "2020-05-15", "mid March - mid May",
                             ifelse(data2$date >= "2020-08-01", "beg August - end Sept", "mid May - end July"))
data2$date_period2 <- factor(data2$date_period2, levels = c("mid March - mid May", "mid May - end Sept"))
data2$date_period3 <- factor(data2$date_period3, levels = c("mid March - mid May","mid May - end July","beg August - end Sept"))

#### create NPI count variable ####
temp <- data2[,c(57:60)]
temp <- temp %>% mutate_at(1:4, as.character)
temp <- temp %>% mutate_at(1:4, as.numeric)
temp$school_close_yn[which(is.na(temp$school_close_yn))] <- 0
temp$business_close_yn[which(is.na(temp$business_close_yn))] <- 0
temp$lifestyle_close_yn[which(is.na(temp$lifestyle_close_yn))] <- 0
temp$stay_home_yn[which(is.na(temp$stay_home_yn))] <- 0
temp$NPI_count <- rowSums(temp)
data2$NPI_count <- as.factor(as.character(temp$NPI_count))

temp$business_lifestyle_close_yn <- as.numeric(ifelse(temp$business_close_yn == 1, 1,
                                                      ifelse(temp$lifestyle_close_yn == 1, 1, 0)))
temp2 <- temp[,c(1,6,4)]
temp2$NPI_count_short <- rowSums(temp2)
data2$NPI_count_short <- as.factor(as.character(temp2$NPI_count_short))

data2$city[which(data2$city == "Los_Angles_CA")] <- "Los_Angeles_CA"
############## 
############## 
############## 
##### ##### ##### ##### TIME FX GAM ##### ##### ##### ##### ##### 
mod1 <- bam(rel_trips_from_oct2 ~  s(time_num) + s(weekday, bs="cc", k=7), 
            data=data2, method="REML",
            knots=list(weekday=c(1,7)))
(summary(mod1)$dev)*100 # % of deviance explained by time alone   24.92 (k default)
AIC(logLik(mod1)) # 8037851
#
data2$resid <- residuals(mod1)
##
##### plot #####
mod_pred <- with(data2,expand.grid(time_num=c(seq(from=min(time_num), to=max(time_num), by=1))))
mod_pred$weekday <- as.numeric(factor(weekdays(as.Date(mod_pred$time_num, origin="1970-01-01")), levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")))

mod_pred_overall <- cbind(mod_pred, predict(mod1, mod_pred, se.fit=TRUE, type="response"))
names(mod_pred_overall)[3] <- "fit_global"

mod_pred_overall1 <- mod_pred_overall[which(mod_pred_overall$weekday == 3),]
mod_pred_overall1$date <- as.Date(mod_pred_overall1$time_num,origin = "1970-01-01")

df2=data.frame(y=c(rep(3,3)),x=c(as.numeric(as.Date("2020-04-13")),
                                 as.numeric(as.Date("2020-06-22")),
                                 as.numeric(as.Date("2020-08-31"))),text=c("Initial Phase\n(March 14 - May 15)",
                                                                           "Rebound Phase\n(May 16 - July 31)",
                                                                           "Stable Phase\n(August 1 - September 30)"))

p <- ggplot() +
  geom_line(aes(y=fit_global, x=date),data=mod_pred_overall1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-05-16")), linetype = "dashed", color="gray25") +
  geom_vline(xintercept = as.numeric(as.Date("2020-08-01")), linetype = "dashed", color="gray25") +
  geom_rect(aes(xmin= as.Date("2020-03-14"),
                xmax =as.Date("2020-05-16"),
                ymin = -Inf,
                ymax = Inf), fill = "#F8766D", alpha = 0.25) +
  geom_rect(aes(xmin= as.Date("2020-05-16"),
                xmax = as.Date("2020-08-01"),
                ymin = -Inf,
                ymax = Inf), fill = "#00BA38", alpha = 0.25) +
  geom_rect(aes(xmin= as.Date("2020-08-01"),
                xmax =  as.Date("2020-09-30"),
                ymin = -Inf,
                ymax = Inf), fill = "#619CFF", alpha = 0.25) +
  scale_x_date("Date", breaks="2 weeks",date_labels = "%b %d") +
  scale_y_continuous("Predicted Relative Mobility", breaks=seq(from=-25,to=5,by=5)) +
  theme_classic() +
  coord_cartesian(ylim=c(-25.2,5.2), xlim=c(as.Date("2020-03-14"),as.Date("2020-09-30"))) +
  theme(axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 45, size=15, vjust=0.65),
        axis.text.y = element_text(size=16))

p2 <- p
for (i in 1:length(df2$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df2$text[i], gp = gpar(fontsize = 16)),
      xmin = df2$x[i],      # Vertical position of the textGrob
      xmax = df2$x[i],
      ymin = df2$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df2$y[i])
}

# Code to override clipping
gt_gen <- ggplot_gtable(ggplot_build(p2))
gt_gen$layout$clip[gt_gen$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_global_pred.pdf", height=8, width=13)
grid.draw(gt_gen)
dev.off() 
dev.off() 



############## full date range residual effects ##############
##### ##### location fx on residuals ##### ##### 
mod_region <- glm(resid ~ region, data=data2)
AIC(mod_region) # 8019048
mod_city <- glm(resid ~ city, data=data2)
AIC(mod_city) # 7978351
##### ##### zipcode level fx on residuals ##### ##### 
mod_popdens <- glm(resid ~ zip_pop_dens_cat, data=data2)
mod_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2)
mod_land <- glm(resid ~ zip_land_area_cat, data=data2)
mod_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2)
mod_income <- glm(resid ~ zip_median_hh_income_cat, data=data2)
mod_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2)
mod_incity <- glm(resid ~ in_city_part, data=data2)
mod_dist <- glm(resid ~ zip_dist_city_bord_cat, data=data2)
mod_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2)

AICctab(mod_popdens,mod_popdens_city,mod_land,mod_land_city,
        mod_incity,mod_dist,mod_dist_city,weights=TRUE)

# dAICc   df weight
# mod_popdens          0.0 5  1
# mod_popdens_city  9564.1 5  <0.001
# mod_land         14798.8 5  <0.001
# mod_dist         17259.2 6  <0.001
# mod_dist_city    17283.5 6  <0.001
# mod_land_city    21281.6 5  <0.001
# mod_incity       26402.1 3  <0.001



### compared to city/region fx ####
AICctab(mod_city,
        mod_region,
        mod_popdens_city,mod_land_city,
        mod_dist_city,weights=TRUE)

# dAICc   df weight
# mod_city             0.0 27 1
# mod_popdens_city 29577.1 5  <0.001
# mod_dist_city    37296.5 6  <0.001
# mod_region       40696.9 7  <0.001
# mod_land_city    41294.6 5  <0.001


#### comparing with income #####
data2bb <- data2[which(!(is.na(data2$zip_median_hh_income_cat_by_city))),]

mod0_region <- glm(resid ~ region, data=data2bb)
AIC(mod0_region) # 7893956

mod0_city <- glm(resid ~ 0 + city, data=data2bb)
AIC(mod0_city) # 7853351

mod0_popdens <- glm(resid ~ zip_pop_dens_cat, data=data2bb)
mod0_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2bb)
mod0_land <- glm(resid ~ zip_land_area_cat, data=data2bb)
mod0_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2bb)
mod0_income <- glm(resid ~ zip_median_hh_income_cat, data=data2bb)
mod0_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2bb)
mod0_incity <- glm(resid ~ in_city_part, data=data2bb)
mod0_dist <- glm(resid ~ zip_dist_city_bord_cat, data=data2bb)
mod0_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2bb)

mod0_npi <- glm(resid ~ NPI_count, data=data2bb)

AICctab(mod0_city,
        mod0_region,
        mod0_popdens_city,mod0_land_city,
         mod0_income_city,
        mod0_dist_city,mod0_npi,weights=TRUE)

# mod0_city             0.0 27 1
# mod0_popdens_city 30609.0 5  <0.001
# mod0_dist_city    38332.4 6  <0.001
# mod0_region       40605.5 7  <0.001
# mod0_land_city    43945.3 5  <0.001
# mod0_income_city  57277.4 5  <0.001


AICctab(mod0_city,
        mod0_region,
        mod0_popdens,mod0_land,
        mod0_income,
        mod0_dist,weights=TRUE)
# mod0_city        0.0 27 1
# mod0_popdens 17879.6 5  <0.001
# mod0_dist    35189.7 6  <0.001
# mod0_land    35537.3 5  <0.001
# mod0_region  40076.8 7  <0.001
# mod0_income  53990.9 5  <0.001

############
data2bb$region <- factor(data2bb$region, levels= levels(data2bb$region)[c(1:3,6,4,5)])
data2bb$city <- factor(data2bb$city, 
                           levels= c(  sort(unique(as.character(data2bb$city)[which(data2bb$region == levels(data2bb$region)[1])])),
                                       sort( unique(as.character(data2bb$city)[which(data2bb$region == levels(data2bb$region)[2])])),
                                       sort( unique(as.character(data2bb$city)[which(data2bb$region == levels(data2bb$region)[3])])),
                                       sort( unique(as.character(data2bb$city)[which(data2bb$region == levels(data2bb$region)[4])])),
                                       sort( unique(as.character(data2bb$city)[which(data2bb$region == levels(data2bb$region)[5])])),
                                       sort( unique(as.character(data2bb$city)[which(data2bb$region == levels(data2bb$region)[6])]))))




#mod0_all <- glm(resid ~ 0 + city + zip_pop_dens_cat_by_city + zip_dist_city_bord_cat_by_city + zip_land_area_cat_by_city + zip_median_hh_income_cat_by_city, data=data2bb)
## region excluded since moot with city

############## 
############## 
############## pandemic phase residual effects ##############
data2_pan <- droplevels(data2[which(data2$date_period3 == "mid March - mid May"),])
##### ##### location fx on residuals ##### ##### 
mod_pan_region <- glm(resid ~ region, data=data2_pan)
AIC(mod_pan_region) # 2475754
mod_pan_city <- glm(resid ~ city, data=data2_pan)
AIC(mod_pan_city) # 2462160
##### ##### zipcode level fx on residuals ##### ##### 
mod_pan_popdens <- glm(resid ~ zip_pop_dens_cat, data=data2_pan)
mod_pan_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2_pan)
mod_pan_land <- glm(resid ~ zip_land_area_cat, data=data2_pan)
mod_pan_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2_pan)
mod_pan_income <- glm(resid ~ zip_median_hh_income_cat, data=data2_pan)
mod_pan_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2_pan)
mod_pan_incity <- glm(resid ~ in_city_part, data=data2_pan)
mod_pan_dist <- glm(resid ~ zip_dist_city_bord_cat, data=data2_pan)
mod_pan_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2_pan)

AICctab(mod_pan_popdens,mod_pan_popdens_city,mod_pan_land,mod_pan_land_city,
        mod_pan_incity,mod_pan_dist,mod_pan_dist_city,weights=TRUE)

# dAICc   df weight
# mod_pan_popdens          0.0 5  1
# mod_pan_land          7231.4 5  <0.001
# mod_pan_popdens_city  7628.3 5  <0.001
# mod_pan_dist_city    13677.5 6  <0.001
# mod_pan_dist         15858.8 6  <0.001
# mod_pan_land_city    17254.7 5  <0.001
# mod_pan_incity       17577.5 3  <0.001

### compared to city/region fx ####
AICctab(mod_pan_city,
        mod_pan_region,
        mod_pan_popdens_city,mod_pan_land_city,
        mod_pan_dist_city,weights=TRUE)

# dAICc   df weight
# mod_pan_city             0.0 27 1
# mod_pan_region       13593.8 7  <0.001
# mod_pan_popdens_city 22103.9 5  <0.001
# mod_pan_dist_city    28153.0 6  <0.001
# mod_pan_land_city    31730.2 5  <0.001


#### comparing with income #####
data2_panbb <- data2_pan[which(!(is.na(data2_pan$zip_median_hh_income_cat_by_city))),]

mod_pan0_region <- glm(resid ~ region, data=data2_panbb)
mod_pan0_city <- glm(resid ~ city, data=data2_panbb)

mod_pan0_popdens <- glm(resid ~ zip_pop_dens_cat, data=data2_panbb)
mod_pan0_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2_panbb)
mod_pan0_land <- glm(resid ~ zip_land_area_cat, data=data2_panbb)
mod_pan0_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2_panbb)
mod_pan0_income <- glm(resid ~ zip_median_hh_income_cat, data=data2_panbb)
mod_pan0_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2_panbb)
mod_pan0_incity <- glm(resid ~ in_city_part, data=data2_panbb)
mod_pan0_dist <- glm(resid ~ zip_dist_city_bord_cat, data=data2_panbb)
mod_pan0_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2_panbb)

mod_pan0_npi <- glm(resid ~ NPI_count, data=data2_panbb)


AICctab(mod_pan0_city,
        mod_pan0_region,
        mod_pan0_popdens_city,mod_pan0_land_city,
        mod_pan0_income_city,
        mod_pan0_dist_city,mod_pan0_npi,weights=TRUE)

# mod_pan0_city             0.0 27 1
# mod_pan0_region       13207.8 7  <0.001
# mod_pan0_popdens_city 22318.9 5  <0.001
# mod_pan0_dist_city    28661.6 6  <0.001
# mod_pan0_land_city    32358.2 5  <0.001
# mod_pan0_income_city  34303.5 5  <0.001


AICctab(mod_pan0_city,
        mod_pan0_region,
        mod_pan0_popdens,mod_pan0_land,
        mod_pan0_income,
        mod_pan0_dist,mod_pan0_npi,weights=TRUE)
# dAICc   df weight
# mod_pan0_city        0.0 27 1
# mod_pan0_region  13058.0 7  <0.001
# mod_pan0_popdens 14302.9 5  <0.001
# mod_pan0_land    23076.9 5  <0.001
# mod_pan0_dist    31067.4 6  <0.001
# mod_pan0_income  31372.1 5  <0.001
# 



############
data2_panbb$region <- factor(data2_panbb$region, levels= levels(data2_panbb$region)[c(1:3,6,4,5)])
data2_panbb$city <- factor(data2_panbb$city, 
                       levels= c(  sort(unique(as.character(data2_panbb$city)[which(data2_panbb$region == levels(data2_panbb$region)[1])])),
                                   sort( unique(as.character(data2_panbb$city)[which(data2_panbb$region == levels(data2_panbb$region)[2])])),
                                   sort( unique(as.character(data2_panbb$city)[which(data2_panbb$region == levels(data2_panbb$region)[3])])),
                                   sort( unique(as.character(data2_panbb$city)[which(data2_panbb$region == levels(data2_panbb$region)[4])])),
                                   sort( unique(as.character(data2_panbb$city)[which(data2_panbb$region == levels(data2_panbb$region)[5])])),
                                   sort( unique(as.character(data2_panbb$city)[which(data2_panbb$region == levels(data2_panbb$region)[6])]))))




#mod0_all_pan <- glm(resid ~ 0 + city + zip_pop_dens_cat_by_city + zip_dist_city_bord_cat_by_city + zip_land_area_cat_by_city + zip_median_hh_income_cat_by_city, data=data2_panbb)
## region excluded since moot with city


############## 
############## 
############## rebound phase residual effects ##############
data2_reb <- droplevels(data2[which(data2$date_period3 == "mid May - end July"),])
##### ##### location fx on residuals ##### ##### 
mod_reb_region <- glm(resid ~ region, data=data2_reb)
AIC(mod_reb_region) # 3098287
mod_reb_city <- glm(resid ~ city, data=data2_reb)
AIC(mod_reb_city) # 3076442
##### ##### zipcode level fx on residuals ##### ##### 
mod_reb_popdens <- glm(resid ~ zip_pop_dens_cat, data=data2_reb)
mod_reb_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2_reb)
mod_reb_land <- glm(resid ~ zip_land_area_cat, data=data2_reb)
mod_reb_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2_reb)
mod_reb_income <- glm(resid ~ zip_median_hh_income_cat, data=data2_reb)
mod_reb_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2_reb)
mod_reb_incity <- glm(resid ~ in_city_part, data=data2_reb)
mod_reb_dist <- glm(resid ~ zip_dist_city_bord_cat, data=data2_reb)
mod_reb_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2_reb)

AICctab(mod_reb_popdens,mod_reb_popdens_city,mod_reb_land,mod_reb_land_city,
        mod_reb_incity,mod_reb_dist,mod_reb_dist_city,weights=TRUE)

# dAICc   df weight
# mod_reb_popdens          0.0 5  1
# mod_reb_popdens_city  4092.3 5  <0.001
# mod_reb_land_city     6325.3 5  <0.001
# mod_reb_land          6717.8 5  <0.001
# mod_reb_dist          6888.5 6  <0.001
# mod_reb_dist_city     7836.2 6  <0.001
# mod_reb_incity       11846.6 3  <0.001

### compared to city/region fx ####
AICctab(mod_reb_city,
        mod_reb_region,
        mod_reb_popdens_city,mod_reb_land_city,
        mod_reb_dist_city,weights=TRUE)

# dAICc   df weight
# mod_reb_city             0.0 27 1
# mod_reb_popdens_city 16083.0 5  <0.001
# mod_reb_land_city    18316.1 5  <0.001
# mod_reb_dist_city    19826.9 6  <0.001
# mod_reb_region       21844.7 7  <0.001


#### comparing with income #####
data2_rebbb <- data2_reb[which(!(is.na(data2_reb$zip_median_hh_income_cat_by_city))),]

mod_reb0_region <- glm(resid ~ region, data=data2_rebbb)
mod_reb0_city <- glm(resid ~ city, data=data2_rebbb)

mod_reb0_popdens <- glm(resid ~ zip_pop_dens_cat, data=data2_rebbb)
mod_reb0_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2_rebbb)
mod_reb0_land <- glm(resid ~ zip_land_area_cat, data=data2_rebbb)
mod_reb0_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2_rebbb)
mod_reb0_income <- glm(resid ~ zip_median_hh_income_cat, data=data2_rebbb)
mod_reb0_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2_rebbb)
mod_reb0_incity <- glm(resid ~ in_city_part, data=data2_rebbb)
mod_reb0_dist <- glm(resid ~ zip_dist_city_bord_cat, data=data2_rebbb)
mod_reb0_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2_rebbb)

mod_reb0_npi <- glm(resid ~ NPI_count, data=data2_rebbb)


AICctab(mod_reb0_city,
        mod_reb0_region,
        mod_reb0_popdens_city,mod_reb0_land_city,
        mod_reb0_income_city,
        mod_reb0_dist_city,mod_reb0_npi,weights=TRUE)

# dAICc   df weight
# mod_reb0_city             0.0 27 1
# mod_reb0_popdens_city 17523.2 5  <0.001
# mod_reb0_land_city    20806.9 5  <0.001
# mod_reb0_dist_city    20883.8 6  <0.001
# mod_reb0_region       22209.0 7  <0.001
# mod_reb0_income_city  31729.8 5  <0.001
# 


AICctab(mod_reb0_city,
        mod_reb0_region,
        mod_reb0_popdens,mod_reb0_land,
        mod_reb0_income,
        mod_reb0_dist,mod_reb0_npi,weights=TRUE)
# mod_reb0_city        0.0 27 1
# mod_reb0_popdens 11438.2 5  <0.001
# mod_reb0_dist    18057.6 6  <0.001
# mod_reb0_land    19847.7 5  <0.001
# mod_reb0_region  22055.1 7  <0.001
# mod_reb0_income  29594.6 5  <0.001


############
data2_rebbb$region <- factor(data2_rebbb$region, levels= levels(data2_rebbb$region)[c(1:3,6,4,5)])
data2_rebbb$city <- factor(data2_rebbb$city, 
                           levels= c(  sort(unique(as.character(data2_rebbb$city)[which(data2_rebbb$region == levels(data2_rebbb$region)[1])])),
                                       sort( unique(as.character(data2_rebbb$city)[which(data2_rebbb$region == levels(data2_rebbb$region)[2])])),
                                       sort( unique(as.character(data2_rebbb$city)[which(data2_rebbb$region == levels(data2_rebbb$region)[3])])),
                                       sort( unique(as.character(data2_rebbb$city)[which(data2_rebbb$region == levels(data2_rebbb$region)[4])])),
                                       sort( unique(as.character(data2_rebbb$city)[which(data2_rebbb$region == levels(data2_rebbb$region)[5])])),
                                       sort( unique(as.character(data2_rebbb$city)[which(data2_rebbb$region == levels(data2_rebbb$region)[6])]))))




# mod0_all_reb <- glm(resid ~ 0 + city + zip_pop_dens_cat_by_city + zip_dist_city_bord_cat_by_city + zip_land_area_cat_by_city + zip_median_hh_income_cat_by_city, data=data2_rebbb)
## region excluded since moot with city


############## 
############## 
############## stable phase residual effects ##############
data2_sta <- droplevels(data2[which(data2$date_period3 == "beg August - end Sept"),])
##### ##### location fx on residuals ##### ##### 
mod_sta_region <- glm(resid ~ region, data=data2_sta)
AIC(mod_sta_region) # 2399154
mod_sta_city <- glm(resid ~ city, data=data2_sta)
AIC(mod_sta_city) # 2383176
##### ##### zipcode level fx on residuals ##### ##### 
mod_sta_popdens <- glm(resid ~ zip_pop_dens_cat, data=data2_sta)
mod_sta_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2_sta)
mod_sta_land <- glm(resid ~ zip_land_area_cat, data=data2_sta)
mod_sta_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2_sta)
mod_sta_income <- glm(resid ~ zip_median_hh_income_cat, data=data2_sta)
mod_sta_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2_sta)
mod_sta_incity <- glm(resid ~ in_city_part, data=data2_sta)
mod_sta_dist <- glm(resid ~ zip_dist_city_bord_cat, data=data2_sta)
mod_sta_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2_sta)

AICctab(mod_sta_popdens,mod_sta_popdens_city,mod_sta_land,mod_sta_land_city,
        mod_sta_incity,mod_sta_dist,mod_sta_dist_city,weights=TRUE)

# dAICc  df weight
# mod_sta_dist            0.0 6  1
# mod_sta_dist_city    1674.6 6  <0.001
# mod_sta_popdens      2331.5 5  <0.001
# mod_sta_land_city    2369.7 5  <0.001
# mod_sta_popdens_city 2496.1 5  <0.001
# mod_sta_incity       3117.0 3  <0.001
# mod_sta_land         4175.6 5  <0.001


### compared to city/region fx ####
AICctab(mod_sta_city,
        mod_sta_region,
        mod_sta_popdens_city,mod_sta_land_city,
        mod_sta_dist_city,weights=TRUE)

# dAICc   df weight
# mod_sta_city             0.0 27 1
# mod_sta_region       15978.1 7  <0.001
# mod_sta_dist_city    24754.6 6  <0.001
# mod_sta_land_city    25449.7 5  <0.001
# mod_sta_popdens_city 25576.1 5  <0.001

#### comparing with income #####
data2_stabb <- data2_sta[which(!(is.na(data2_sta$zip_median_hh_income_cat_by_city))),]

mod_sta0_region <- glm(resid ~ region, data=data2_stabb)
mod_sta0_city <- glm(resid ~ city, data=data2_stabb)

mod_sta0_popdens <- glm(resid ~ zip_pop_dens_cat, data=data2_stabb)
mod_sta0_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2_stabb)
mod_sta0_land <- glm(resid ~ zip_land_area_cat, data=data2_stabb)
mod_sta0_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2_stabb)
mod_sta0_income <- glm(resid ~ zip_median_hh_income_cat, data=data2_stabb)
mod_sta0_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2_stabb)
mod_sta0_incity <- glm(resid ~ in_city_part, data=data2_stabb)
mod_sta0_dist <- glm(resid ~ zip_dist_city_bord_cat, data=data2_stabb)
mod_sta0_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2_stabb)

mod_sta0_npi <- glm(resid ~ NPI_count, data=data2_stabb)


AICctab(mod_sta0_city,
        mod_sta0_region,
        mod_sta0_popdens_city,mod_sta0_land_city,
        mod_sta0_income_city,
        mod_sta0_dist_city,mod_sta0_npi,weights=TRUE)

# dAICc   df weight
# mod_sta0_city             0.0 27 1
# mod_sta0_region       16064.5 7  <0.001
# mod_sta0_dist_city    25181.8 6  <0.001
# mod_sta0_popdens_city 26038.8 5  <0.001
# mod_sta0_land_city    26199.6 5  <0.001
# mod_sta0_income_city  28071.1 5  <0.001

AICctab(mod_sta0_city,
        mod_sta0_region,
        mod_sta0_popdens,mod_sta0_land,
        mod_sta0_income,
        mod_sta0_dist,mod_sta0_npi,weights=TRUE)
# dAICc   df weight
# mod_sta0_city        0.0 27 1
# mod_sta0_region  15909.8 7  <0.001
# mod_sta0_dist    21759.3 6  <0.001
# mod_sta0_popdens 24383.7 5  <0.001
# mod_sta0_income  25617.8 5  <0.001
# mod_sta0_land    26260.2 5  <0.001



############
data2_stabb$region <- factor(data2_stabb$region, levels= levels(data2_stabb$region)[c(1:3,6,4,5)])
data2_stabb$city <- factor(data2_stabb$city, 
                           levels= c(  sort(unique(as.character(data2_stabb$city)[which(data2_stabb$region == levels(data2_stabb$region)[1])])),
                                       sort( unique(as.character(data2_stabb$city)[which(data2_stabb$region == levels(data2_stabb$region)[2])])),
                                       sort( unique(as.character(data2_stabb$city)[which(data2_stabb$region == levels(data2_stabb$region)[3])])),
                                       sort( unique(as.character(data2_stabb$city)[which(data2_stabb$region == levels(data2_stabb$region)[4])])),
                                       sort( unique(as.character(data2_stabb$city)[which(data2_stabb$region == levels(data2_stabb$region)[5])])),
                                       sort( unique(as.character(data2_stabb$city)[which(data2_stabb$region == levels(data2_stabb$region)[6])]))))





# mod0_all_sta <- glm(resid ~ 0 + city + zip_pop_dens_cat_by_city + zip_dist_city_bord_cat_by_city + zip_land_area_cat_by_city + zip_median_hh_income_cat_by_city, data=data2_stabb)
## region excluded since moot with city


############## 
############## 
###### univariate plots ######
mod0_city <- glm(resid ~ 0 + city, data=data2bb)
mod_pan0_city <- glm(resid ~ 0 + city, data=data2_panbb)
mod_reb0_city <- glm(resid ~ 0 + city, data=data2_rebbb)
mod_sta0_city <- glm(resid ~ 0 + city, data=data2_stabb)

mod_pred <- with(data2bb,expand.grid(city=levels(city)))
mod_pred_overall <- cbind(mod_pred, predict(mod0_city, mod_pred, se.fit=TRUE, type="response"))
names(mod_pred_overall)[2] <- "fit_all"
mod_pred_overall2 <- cbind(mod_pred_overall, predict(mod_pan0_city, mod_pred, se.fit=TRUE, type="response"))
names(mod_pred_overall2)[5] <- "fit_pan"
mod_pred_overall3 <- cbind(mod_pred_overall2, predict(mod_reb0_city, mod_pred, se.fit=TRUE, type="response"))
names(mod_pred_overall3)[8] <- "fit_reb"
mod_pred_overall4 <- cbind(mod_pred_overall3, predict(mod_sta0_city, mod_pred, se.fit=TRUE, type="response"))
names(mod_pred_overall4)[11] <- "fit_sta"

mod_pred3 <- with(data2bb,expand.grid(city=levels(city), date_period=c(levels(date_period3))))

mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May")] <- mod_pred_overall4$fit_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July")] <- mod_pred_overall4$fit_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept")] <- mod_pred_overall4$fit_sta

mod_pred3$date_period <- factor(mod_pred3$date_period, levels=c("mid March - mid May","mid May - end July","beg August - end Sept"))
mod_pred3 <- mod_pred3[order(mod_pred3$city, mod_pred3$date_period),]


p <- ggplot() +
  geom_point(aes(y=fit, x=city, col=date_period,shape=date_period),data=mod_pred3, size=3) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 7.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 14.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 20.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  scale_x_discrete("") +
  scale_y_continuous("Coefficient Estimates") +
  scale_color_manual("Date Period", values=c("#F8766D", "#00BA38", "#619CFF")) +
  scale_shape_manual("Date Period", values=c(19,19,19)) +
  theme(plot.margin = unit(c(2,2,4,2), "lines"),
        axis.title=element_text(size=19),
        axis.text.x = element_text(angle = 90, size=15),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=17),
        legend.text = element_text(size=15))

df=data.frame(y=c(rep(-23,6)),x=c(1.7,5.6,9.0,12.5,17.5,23.5),text=c(as.character(levels(data2bb$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 +
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 18)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) +
    annotation_custom(
      grob = textGrob(label = "City by Region", gp = gpar(fontsize = 20)),
      xmin = 14,      # Vertical position of the textGrob
      xmax = 14,
      ymin = -26.5,         # Note: The grobs are positioned outside the plot area
      ymax = -26.5) +
    annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-24.5,ymax=-12.5) +
    annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=7.5,xmax=7.5, ymin=-24.5,ymax=-12.5) +
    annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=10.5,xmax=10.5, ymin=-24.5,ymax=-12.5) +
    annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=14.5,xmax=14.5, ymin=-24.5,ymax=-12.5) +
    annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=20.5,xmax=20.5, ymin=-24.5,ymax=-12.5)
}

# Code to override clipping
gt_city <- ggplot_gtable(ggplot_build(p2))
gt_city$layout$clip[gt_city$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/resids_city_new_time.pdf", height=8, width=13)
grid.draw(gt_city)
dev.off()
dev.off()
# #####
# 
# mod_pred4 <- with(data2bb,expand.grid(city=levels(city)))
# 
# mod_pred4$fit<- mod_pred_overall4$fit_all
# 
# p <- ggplot() +
#   geom_point(aes(y=fit, x=city), data=mod_pred4, size=3, shape=15) +
#   geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
#   geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
#   geom_vline(xintercept = 7.5, linetype = "dashed", color="gray25") +
#   geom_vline(xintercept = 10.5, linetype = "dashed", color="gray25") +
#   geom_vline(xintercept = 14.5, linetype = "dashed", color="gray25") +
#   geom_vline(xintercept = 20.5, linetype = "dashed", color="gray25") +
#   theme_classic() +
#   scale_x_discrete("") +
#   coord_cartesian(ylim=c(-12.5,15)) +
#   scale_y_continuous("Coefficient Estimates") +
#   theme(plot.margin = unit(c(2,2,4,2), "lines"),
#         axis.title=element_text(size=17),
#         axis.text.x = element_text(angle = 90, size=13),
#         axis.text.y = element_text(size=13),
#         legend.title = element_text(size=15),
#         legend.text = element_text(size=13))
# 
# df=data.frame(y=c(rep(-23,6)),x=c(1.7,5.6,9.0,12.5,17.5,23.5),text=c(as.character(levels(data2bb$region))))
# 
# p2 <- p
# for (i in 1:length(df$text))  {
#   p2 <- p2 +
#     annotation_custom(
#       grob = textGrob(label = df$text[i], gp = gpar(fontsize = 16)),
#       xmin = df$x[i],      # Vertical position of the textGrob
#       xmax = df$x[i],
#       ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
#       ymax = df$y[i]) +
#     annotation_custom(
#       grob = textGrob(label = "City by Region", gp = gpar(fontsize = 18)),
#       xmin = 14,      # Vertical position of the textGrob
#       xmax = 14,
#       ymin = -26.5,         # Note: The grobs are positioned outside the plot area
#       ymax = -26.5) +
#     annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-24.5,ymax=-12.5) +
#     annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=7.5,xmax=7.5, ymin=-24.5,ymax=-12.5) +
#     annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=10.5,xmax=10.5, ymin=-24.5,ymax=-12.5) +
#     annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=14.5,xmax=14.5, ymin=-24.5,ymax=-12.5) +
#     annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=20.5,xmax=20.5, ymin=-24.5,ymax=-12.5)
# }
# 
# # Code to override clipping
# gt <- ggplot_gtable(ggplot_build(p2))
# gt$layout$clip[gt$layout$name == "panel"] <- "off"
# 
# pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/resids_city_new_all.pdf", height=8, width=10)
# grid.draw(gt)
# dev.off()
# dev.off()
###### univariate plots ######

mod0_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2bb)
mod0_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2bb)
mod0_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2bb)
mod0_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2bb)
mod0_npi_city <- glm(resid ~ NPI_count, data=data2bb)

mod_pan0_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2_panbb)
mod_pan0_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2_panbb)
mod_pan0_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2_panbb)
mod_pan0_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2_panbb)
mod_pan0_npi_city <- glm(resid ~ NPI_count, data=data2_panbb)

mod_reb0_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2_rebbb)
mod_reb0_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2_rebbb)
mod_reb0_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2_rebbb)
mod_reb0_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2_rebbb)
mod_reb0_npi_city <- glm(resid ~ NPI_count, data=data2_rebbb)

mod_sta0_popdens_city <- glm(resid ~ zip_pop_dens_cat_by_city, data=data2_stabb)
mod_sta0_land_city <- glm(resid ~ zip_land_area_cat_by_city, data=data2_stabb)
mod_sta0_income_city <- glm(resid ~ zip_median_hh_income_cat_by_city, data=data2_stabb)
mod_sta0_dist_city <- glm(resid ~ zip_dist_city_bord_cat_by_city, data=data2_stabb)
mod_sta0_npi_city <- glm(resid ~ NPI_count, data=data2_stabb)


mod_pred_pd <- with(data2bb,expand.grid(zip_pop_dens_cat_by_city=levels(zip_pop_dens_cat_by_city)))
mod_pred_pd1 <- cbind(mod_pred_pd, predict(mod0_popdens_city, mod_pred_pd, se.fit=TRUE, type="response"))
mod_pred_pd2 <- cbind(mod_pred_pd1, predict(mod_pan0_popdens_city, mod_pred_pd, se.fit=TRUE, type="response"))
mod_pred_pd3 <- cbind(mod_pred_pd2, predict(mod_reb0_popdens_city, mod_pred_pd, se.fit=TRUE, type="response"))
mod_pred_pd4 <- cbind(mod_pred_pd3, predict(mod_sta0_popdens_city, mod_pred_pd, se.fit=TRUE, type="response"))
names(mod_pred_pd4)[2] <- "fit_pd_all"
names(mod_pred_pd4)[5] <- "fit_pd_pan"
names(mod_pred_pd4)[8] <- "fit_pd_reb"
names(mod_pred_pd4)[11] <- "fit_pd_sta"

mod_pred_l <- with(data2bb,expand.grid(zip_land_area_cat_by_city=levels(zip_land_area_cat_by_city)))
mod_pred_l1 <- cbind(mod_pred_l, predict(mod0_land_city, mod_pred_l, se.fit=TRUE, type="response"))
mod_pred_l2 <- cbind(mod_pred_l1, predict(mod_pan0_land_city, mod_pred_l, se.fit=TRUE, type="response"))
mod_pred_l3 <- cbind(mod_pred_l2, predict(mod_reb0_land_city, mod_pred_l, se.fit=TRUE, type="response"))
mod_pred_l4 <- cbind(mod_pred_l3, predict(mod_sta0_land_city, mod_pred_l, se.fit=TRUE, type="response"))
names(mod_pred_l4)[2] <- "fit_l_all"
names(mod_pred_l4)[5] <- "fit_l_pan"
names(mod_pred_l4)[8] <- "fit_l_reb"
names(mod_pred_l4)[11] <- "fit_l_sta"

mod_pred_d <- with(data2bb,expand.grid(zip_dist_city_bord_cat_by_city=levels(zip_dist_city_bord_cat_by_city)))
mod_pred_d1 <- cbind(mod_pred_d, predict(mod0_dist_city, mod_pred_d, se.fit=TRUE, type="response"))
mod_pred_d2 <- cbind(mod_pred_d1, predict(mod_pan0_dist_city, mod_pred_d, se.fit=TRUE, type="response"))
mod_pred_d3 <- cbind(mod_pred_d2, predict(mod_reb0_dist_city, mod_pred_d, se.fit=TRUE, type="response"))
mod_pred_d4 <- cbind(mod_pred_d3, predict(mod_sta0_dist_city, mod_pred_d, se.fit=TRUE, type="response"))
names(mod_pred_d4)[2] <- "fit_d_all"
names(mod_pred_d4)[5] <- "fit_d_pan"
names(mod_pred_d4)[8] <- "fit_d_reb"
names(mod_pred_d4)[11] <- "fit_d_sta"

mod_pred_i <- with(data2bb,expand.grid(zip_median_hh_income_cat_by_city=levels(zip_median_hh_income_cat_by_city)))
mod_pred_i1 <- cbind(mod_pred_i, predict(mod0_income_city, mod_pred_i, se.fit=TRUE, type="response"))
mod_pred_i2 <- cbind(mod_pred_i1, predict(mod_pan0_income_city, mod_pred_i, se.fit=TRUE, type="response"))
mod_pred_i3 <- cbind(mod_pred_i2, predict(mod_reb0_income_city, mod_pred_i, se.fit=TRUE, type="response"))
mod_pred_i4 <- cbind(mod_pred_i3, predict(mod_sta0_income_city, mod_pred_i, se.fit=TRUE, type="response"))
names(mod_pred_i4)[2] <- "fit_i_all"
names(mod_pred_i4)[5] <- "fit_i_pan"
names(mod_pred_i4)[8] <- "fit_i_reb"
names(mod_pred_i4)[11] <- "fit_i_sta"

mod_pred_n <- with(data2bb,expand.grid(NPI_count=levels(NPI_count)))
mod_pred_n1 <- cbind(mod_pred_n, predict(mod0_npi_city, mod_pred_n, se.fit=TRUE, type="response"))
mod_pred_n2 <- cbind(mod_pred_n1, predict(mod_pan0_npi_city, mod_pred_n, se.fit=TRUE, type="response"))
mod_pred_n3 <- cbind(mod_pred_n2, predict(mod_reb0_npi_city, mod_pred_n, se.fit=TRUE, type="response"))
mod_pred_n4a <- with(data2_stabb,expand.grid(NPI_count=levels(NPI_count)))
temp <- predict(mod_sta0_npi_city, mod_pred_n4a, se.fit=TRUE, type="response")
temp$fit[5] <- 0
temp$se.fit[5] <- 0
mod_pred_n4 <- cbind(mod_pred_n3, temp)
names(mod_pred_n4)[2] <- "fit_n_all"
names(mod_pred_n4)[5] <- "fit_n_pan"
names(mod_pred_n4)[8] <- "fit_n_reb"
names(mod_pred_n4)[11] <- "fit_n_sta"

mod_pred2 <- with(data2bb,expand.grid(date_period=c(levels(date_period3),"all"),level=c( levels(data2bb$zip_pop_dens_cat_by_city),levels(data2bb$zip_land_area_cat_by_city),
                                                                                         levels(data2bb$zip_dist_city_bord_cat_by_city),levels(data2bb$zip_median_hh_income_cat_by_city),
                                                                                         levels(data2bb$NPI_count))))
mod_pred2$type <- factor(ifelse(mod_pred2$level %in% as.character(levels(data2bb$zip_pop_dens_cat_by_city)), "popdens",
                                ifelse(mod_pred2$level %in% as.character(levels(data2bb$zip_land_area_cat_by_city)), "land",
                                       ifelse(mod_pred2$level %in% as.character(levels(data2bb$zip_dist_city_bord_cat_by_city)), "dist",
                                              ifelse(mod_pred2$level %in% as.character(levels(data2bb$zip_median_hh_income_cat_by_city)), "income", "npi")))))

mod_pred2$fit <- as.numeric(NA)
mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_all
mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_pan
mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_reb
mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_sta

mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_all
mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_pan
mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_reb
mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_sta

mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_all
mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_pan
mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_reb
mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_sta

mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_all
mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_pan
mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_reb
mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_sta

mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "npi")] <- mod_pred_n4$fit_n_all
mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "npi")] <- mod_pred_n4$fit_n_pan
mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "npi")] <- mod_pred_n4$fit_n_reb
mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "npi")] <- mod_pred_n4$fit_n_sta

mod_pred2$type <- factor(mod_pred2$type, levels=c("popdens","dist","land","income","npi"))
mod_pred2 <- mod_pred2[order(mod_pred2$type,mod_pred2$level,mod_pred2$date_period),]

levels <- c(as.character(levels(data2bb$zip_pop_dens_cat_by_city)), as.character(levels(data2bb$zip_dist_city_bord_cat_by_city)),
            as.character(levels(data2bb$zip_land_area_cat_by_city)), as.character(levels(data2bb$zip_median_hh_income_cat_by_city)),
            as.character(levels(data2bb$NPI_count)))
mod_pred2$level2 <- factor(ifelse(mod_pred2$level %in% levels[5], "In City",
                                  ifelse(mod_pred2$level %in% levels[c(1,6,10,14)], "Q1",
                                         ifelse(mod_pred2$level %in% levels[c(2,7,11,15)], "Q2",
                                                ifelse(mod_pred2$level %in% levels[c(3,8,12,16)], "Q3", 
                                                       ifelse(mod_pred2$level %in% levels[c(4,9,13,17)], "Q4",
                                                              ifelse(mod_pred2$level %in% levels[c(18)], "0 NPI",
                                                                     ifelse(mod_pred2$level %in% levels[c(19)], "1 NPI",
                                                                            ifelse(mod_pred2$level %in% levels[c(20)], "2 NPI",
                                                                                   ifelse(mod_pred2$level %in% levels[c(21)], "3 NPI","4 NPI"))))))))))



######
type.labs <- c("Population Density Quartile", "Dist. from City Limit Quartile",
               "Land Area Quartile", "Median Income Quartile", "NPIs in Place")
names(type.labs) <- c("popdens","dist","land","income","npi")

mod_pred2q <- droplevels(mod_pred2[which(mod_pred2$date_period != "all"),])

p <- ggplot() +
  geom_point(aes(y=fit, x=level2, col=date_period),data=mod_pred2q, size=3) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  facet_wrap(~type, scales="free_x", strip.position = "bottom",labeller = labeller(type = type.labs)) +
  theme_classic() +
  scale_x_discrete("") + 
  scale_y_continuous("Coefficient Estimates") +
  scale_color_manual("Date Period", values=c("#F8766D", "#00BA38", "#619CFF")) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        axis.title=element_text(size=17),
        axis.text.x = element_text(angle = 0, size=13),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(size=17))
pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/resids_zc_new_no_all.pdf", height=8, width=13)
p
dev.off() 

###########
names(mod_pred_pd4)[3] <- "fit.se_pd_all"
names(mod_pred_pd4)[6] <- "fit.se_pd_pan"
names(mod_pred_pd4)[9] <- "fit.se_pd_reb"
names(mod_pred_pd4)[12] <- "fit.se_pd_sta"
names(mod_pred_l4)[3] <- "fit.se_l_all"
names(mod_pred_l4)[6] <- "fit.se_l_pan"
names(mod_pred_l4)[9] <- "fit.se_l_reb"
names(mod_pred_l4)[12] <- "fit.se_l_sta"
names(mod_pred_d4)[3] <- "fit.se_d_all"
names(mod_pred_d4)[6] <- "fit.se_d_pan"
names(mod_pred_d4)[9] <- "fit.se_d_reb"
names(mod_pred_d4)[12] <- "fit.se_d_sta"
names(mod_pred_i4)[3] <- "fit.se_i_all"
names(mod_pred_i4)[6] <- "fit.se_i_pan"
names(mod_pred_i4)[9] <- "fit.se_i_reb"
names(mod_pred_i4)[12] <- "fit.se_i_sta"
names(mod_pred_n4)[3] <- "fit.se_n_all"
names(mod_pred_n4)[6] <- "fit.se_n_pan"
names(mod_pred_n4)[9] <- "fit.se_n_reb"
names(mod_pred_n4)[12] <- "fit.se_n_sta"


mod_pred3 <- with(data2bb,expand.grid(date_period=c(levels(date_period3),"all"),level=c( levels(data2bb$zip_pop_dens_cat_by_city),levels(data2bb$zip_land_area_cat_by_city),
                                                                                        levels(data2bb$zip_dist_city_bord_cat_by_city),levels(data2bb$zip_median_hh_income_cat_by_city),
                                                                                        levels(data2bb$NPI_count))))
mod_pred3$type <- factor(ifelse(mod_pred3$level %in% as.character(levels(data2bb$zip_pop_dens_cat_by_city)), "popdens",
                                ifelse(mod_pred3$level %in% as.character(levels(data2bb$zip_land_area_cat_by_city)), "land",
                                       ifelse(mod_pred3$level %in% as.character(levels(data2bb$zip_dist_city_bord_cat_by_city)), "dist",
                                              ifelse(mod_pred3$level %in% as.character(levels(data2bb$zip_median_hh_income_cat_by_city)), "income", "npi")))))

mod_pred3$fit <- as.numeric(NA)
mod_pred3$fit[which(mod_pred3$date_period == "all" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit_pd_all
mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit_pd_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit_pd_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit_pd_sta

mod_pred3$fit[which(mod_pred3$date_period == "all" & mod_pred3$type == "land")] <- mod_pred_l4$fit_l_all
mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "land")] <- mod_pred_l4$fit_l_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "land")] <- mod_pred_l4$fit_l_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "land")] <- mod_pred_l4$fit_l_sta

mod_pred3$fit[which(mod_pred3$date_period == "all" & mod_pred3$type == "dist")] <- mod_pred_d4$fit_d_all
mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "dist")] <- mod_pred_d4$fit_d_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "dist")] <- mod_pred_d4$fit_d_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "dist")] <- mod_pred_d4$fit_d_sta

mod_pred3$fit[which(mod_pred3$date_period == "all" & mod_pred3$type == "income")] <- mod_pred_i4$fit_i_all
mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "income")] <- mod_pred_i4$fit_i_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "income")] <- mod_pred_i4$fit_i_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "income")] <- mod_pred_i4$fit_i_sta

mod_pred3$fit[which(mod_pred3$date_period == "all" & mod_pred3$type == "npi")] <- mod_pred_n4$fit_n_all
mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "npi")] <- mod_pred_n4$fit_n_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "npi")] <- mod_pred_n4$fit_n_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "npi")] <- mod_pred_n4$fit_n_sta

mod_pred3$fit.se <- as.numeric(NA)
mod_pred3$fit.se[which(mod_pred3$date_period == "all" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit.se_pd_all
mod_pred3$fit.se[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit.se_pd_pan
mod_pred3$fit.se[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit.se_pd_reb
mod_pred3$fit.se[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit.se_pd_sta

mod_pred3$fit.se[which(mod_pred3$date_period == "all" & mod_pred3$type == "land")] <- mod_pred_l4$fit.se_l_all
mod_pred3$fit.se[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "land")] <- mod_pred_l4$fit.se_l_pan
mod_pred3$fit.se[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "land")] <- mod_pred_l4$fit.se_l_reb
mod_pred3$fit.se[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "land")] <- mod_pred_l4$fit.se_l_sta

mod_pred3$fit.se[which(mod_pred3$date_period == "all" & mod_pred3$type == "dist")] <- mod_pred_d4$fit.se_d_all
mod_pred3$fit.se[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "dist")] <- mod_pred_d4$fit.se_d_pan
mod_pred3$fit.se[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "dist")] <- mod_pred_d4$fit.se_d_reb
mod_pred3$fit.se[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "dist")] <- mod_pred_d4$fit.se_d_sta

mod_pred3$fit.se[which(mod_pred3$date_period == "all" & mod_pred3$type == "income")] <- mod_pred_i4$fit.se_i_all
mod_pred3$fit.se[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "income")] <- mod_pred_i4$fit.se_i_pan
mod_pred3$fit.se[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "income")] <- mod_pred_i4$fit.se_i_reb
mod_pred3$fit.se[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "income")] <- mod_pred_i4$fit.se_i_sta

mod_pred3$fit.se[which(mod_pred3$date_period == "all" & mod_pred3$type == "npi")] <- mod_pred_n4$fit.se_n_all
mod_pred3$fit.se[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "npi")] <- mod_pred_n4$fit.se_n_pan
mod_pred3$fit.se[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "npi")] <- mod_pred_n4$fit.se_n_reb
mod_pred3$fit.se[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "npi")] <- mod_pred_n4$fit.se_n_sta

mod_pred3$type <- factor(mod_pred3$type, levels=c("popdens","dist","land","income","npi"))
mod_pred3 <- mod_pred3[order(mod_pred3$type,mod_pred3$level,mod_pred3$date_period),]

levels <- c(as.character(levels(data2bb$zip_pop_dens_cat_by_city)), as.character(levels(data2bb$zip_dist_city_bord_cat_by_city)),
            as.character(levels(data2bb$zip_land_area_cat_by_city)), as.character(levels(data2bb$zip_median_hh_income_cat_by_city)),
            as.character(levels(data2bb$NPI_count)))
mod_pred3$level2 <- factor(ifelse(mod_pred3$level %in% levels[5], "In City",
                                  ifelse(mod_pred3$level %in% levels[c(1,6,10,14)], "Q1",
                                         ifelse(mod_pred3$level %in% levels[c(2,7,11,15)], "Q2",
                                                ifelse(mod_pred3$level %in% levels[c(3,8,12,16)], "Q3", 
                                                       ifelse(mod_pred3$level %in% levels[c(4,9,13,17)], "Q4",
                                                              ifelse(mod_pred3$level %in% levels[c(18)], "0 NPI",
                                                                     ifelse(mod_pred3$level %in% levels[c(19)], "1 NPI",
                                                                            ifelse(mod_pred3$level %in% levels[c(20)], "2 NPI",
                                                                                   ifelse(mod_pred3$level %in% levels[c(21)], "3 NPI","4 NPI"))))))))))


mod_pred3$fit.low <- mod_pred3$fit - mod_pred3$fit.se
mod_pred3$fit.high <- mod_pred3$fit + mod_pred3$fit.se



######
type.labs <- c("Population Density Quartile", "Dist. from City Limit Quartile",
               "Land Area Quartile", "Median Income Quartile","NPIs in Place")
names(type.labs) <- c("popdens","dist","land","income","npi")

mod_pred3q <- droplevels(mod_pred3[which(mod_pred3$date_period != "all"),])


p <- ggplot() +
  geom_point(aes(y=fit, x=level2, col=date_period),data=mod_pred3q, size=3) +
  geom_errorbar(aes(ymin=fit.low, ymax=fit.high, x=level2, col=date_period),data=mod_pred3q,width=0.35) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  facet_wrap(~type, scales="free_x", strip.position = "bottom",labeller = labeller(type = type.labs)) +
  theme_classic() +
  scale_x_discrete("") + 
  scale_y_continuous("Coefficient Estimates") +
  scale_color_manual("Date Period", values=c("#F8766D", "#00BA38", "#619CFF")) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        axis.title=element_text(size=17),
        axis.text.x = element_text(angle = 0, size=13),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(size=17))
pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/resids_zc_new_no_all_se.pdf", height=8, width=13)
p
dev.off() 

############## 
############## 
#####
setwd("/home/kschaber/verizon_graphs2/zip/daily/gam/temp")
bound <- readOGR("city_bound.shp", layer = "city_bound")
bound <- spTransform(bound, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"))

bound <- gSimplify(bound, tol = 0.00001)
bound <- gBuffer(bound, byid=TRUE, width=0)
sum(gIsValid(bound, byid=TRUE)==FALSE)

bound_df <- broom::tidy(bound, region = "NAME")
zip_data_coord <- read.csv("zip_data_coord_new.csv", sep=",", header=TRUE)
setwd("/home/kschaber/verizon_graphs2/zip/daily/gam/temp/tl_2020_us_zcta520")
zip_shp <- readOGR("tl_2020_us_zcta520.shp", layer = "tl_2020_us_zcta520")    
state <- map_data("state")
zip_data_coord$origin_zip <- as.character(zip_data_coord$origin_zip)
zip_data_coord$origin_zip[which(nchar(zip_data_coord$origin_zip) == 4)] <- paste0("0",zip_data_coord$origin_zip[which(nchar(zip_data_coord$origin_zip) == 4)])
zip_data_coord$state <- tolower(abbr2state(zip_data_coord$origin_state))

zip_shp_short <- zip_shp[which(zip_shp@data$ZCTA5CE20 %in% c(as.character(zip_data_coord$origin_zip))),]
zip_shp_short2 <- zip_shp_short
zip_shp_short2_df <- broom::tidy(zip_shp_short2, region = "ZCTA5CE20")
zip_shp_short2_df2 <- merge(zip_shp_short2_df, zip_data_coord,by.x="id", by.y="origin_zip", all.x=TRUE)
zip_shp_short2_df2$zip_pop_dens_cat_by_city <- as.factor(zip_shp_short2_df2$zip_pop_dens_cat_by_city)
zip_shp_short2_df2$zip_land_area_cat_by_city <- as.factor(zip_shp_short2_df2$zip_land_area_cat_by_city)
zip_shp_short2_df2$houses_per_sqmi_cat_by_city <- as.factor(zip_shp_short2_df2$houses_per_sqmi_cat_by_city)
zip_shp_short2_df2$zip_median_hh_income_cat_by_city <- as.factor(zip_shp_short2_df2$zip_median_hh_income_cat_by_city)


zip_data_coord$city2 <- as.character(zip_data_coord$city)
zip_data_coord$city2[which(zip_data_coord$city2 == "Los_Angles_CA")] <- "Los Angeles_CA"
zip_data_coord$city2[which(zip_data_coord$city2 == "ElPaso_TX")] <- "El Paso_TX"
zip_data_coord$city2[which(zip_data_coord$city2 == "SiouxFalls_SD")] <- "Sioux Falls_SD"
zip_data_coord$city2[which(zip_data_coord$city2 == "SanFrancisco_CA")] <- "San Francisco_CA"
zip_data_coord$city2[which(zip_data_coord$city2 == "SanDiego_CA")] <- "San Diego_CA"
zip_data_coord$city2[which(zip_data_coord$city2 == "SanAntonio_TX")] <- "San Antonio_TX"
zip_data_coord$city2[which(zip_data_coord$city2 == "SanJose_CA")] <- "San Jose_CA"
zip_data_coord$city2[which(zip_data_coord$city2 == "NYC_NY")] <- "New York_NY"
zip_data_coord$city2b <- unlist(strsplit(as.character(zip_data_coord$city2), "_"))[seq(from=1,to=2*nrow(zip_data_coord)-1, by=2)]
zip_data_coord <- zip_data_coord[order(zip_data_coord$city, zip_data_coord$origin_zip),]

#####
zip_shp_short2_df2 <- zip_shp_short2_df2[order(zip_shp_short2_df2$city, zip_shp_short2_df2$id),]
cities <- zip_shp_short2_df2[!duplicated(zip_shp_short2_df2$city),]
cities <- cities[which(!(is.na(cities$city))),c(2,3,8)]
cities <- cities[order(cities$long),]
cities$city
cities2 <- cities[c(1:8,10,12,13,9,11,14:25,26),]

cities2$long <- c(-122.1, -124.2, -117.6, -117.3,-112,
                  -106, -100, -99, -96.8, -96.4, 
                  -94.2, -96.5, -96.8, -93, -88.3,
                  -86.9, -84.5,-83.5, -83.2, -83.9, 
                  -80, -80.6,-77.8, -77, -79,
                  -72)
cities2$lat <- c(39, 36, 35.3, 32, 34.7,
                 34, 28, 31.4, 48, 44.45, 
                 42.45, 34, 39.5, 28.6, 40.5,
                 37.2, 32.25, 44.2, 38.9, 29.3, 
                 31.3, 36.6, 26.3, 38, 40.6, 
                 40.1)
cols1 <- c(rainbow(27)[c(1:3,5,6,10:16,9,17:19,7,20:24,8,25:27)])
cities2$city[3] <- "Los_Angeles_CA"
zip_shp_short2_df2$city[which(zip_shp_short2_df2$city == "Los_Angles_CA")] <- "Los_Angeles_CA"

map <- ggplot() + 
  geom_polygon(data = zip_shp_short2_df2, aes(x = long, y = lat, group = group, fill=city)) +
  geom_path(data=state, aes(x=long, y=lat, group=group), color="black", size=0.5) +
  geom_label(data=cities2, aes(label=city, x=long, y=lat, fill=city), size=7.5, color="black", alpha=0.6) +
  scale_fill_manual(values=cols1, na.value="white") +
  coord_map() +
  theme_classic() +
  guides(fill="none") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.line = element_blank()) 

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/map.pdf", width=22, height=12)
map
dev.off()
##
#### combined #####
#####
df2=data.frame(y=c(rep(3.5,3)),x=c(as.numeric(as.Date("2020-04-13")),
                                 as.numeric(as.Date("2020-06-22")),
                                 as.numeric(as.Date("2020-08-31"))),text=c("Initial Phase\n(March 14 - May 15)",
                                                                           "Rebound Phase\n(May 16 - July 31)",
                                                                           "Stable Phase\n(August 1 - September 30)"))

p <- ggplot() +
  geom_line(aes(y=fit_global, x=date),data=mod_pred_overall1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-05-16")), linetype = "dashed", color="gray25") +
  geom_vline(xintercept = as.numeric(as.Date("2020-08-01")), linetype = "dashed", color="gray25") +
  geom_rect(aes(xmin= as.Date("2020-03-14"),
                xmax =as.Date("2020-05-16"),
                ymin = -Inf,
                ymax = Inf), fill = "#F8766D", alpha = 0.25) +
  geom_rect(aes(xmin= as.Date("2020-05-16"),
                xmax = as.Date("2020-08-01"),
                ymin = -Inf,
                ymax = Inf), fill = "#00BA38", alpha = 0.25) +
  geom_rect(aes(xmin= as.Date("2020-08-01"),
                xmax =  as.Date("2020-09-30"),
                ymin = -Inf,
                ymax = Inf), fill = "#619CFF", alpha = 0.25) +
  scale_x_date("Date", breaks="2 weeks",date_labels = "%b %d") +
  scale_y_continuous("Predicted Relative Mobility", breaks=seq(from=-25,to=5,by=5)) +
  theme_classic() +
  coord_cartesian(ylim=c(-25.2,5.2), xlim=c(as.Date("2020-03-14"),as.Date("2020-09-30"))) +
  theme(axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 45, size=15, vjust=0.65),
        axis.text.y = element_text(size=16))

p2 <- p
for (i in 1:length(df2$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df2$text[i], gp = gpar(fontsize = 10)),
      xmin = df2$x[i],      # Vertical position of the textGrob
      xmax = df2$x[i],
      ymin = df2$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df2$y[i])
}

# Code to override clipping
gt_gen <- ggplot_gtable(ggplot_build(p2))
gt_gen$layout$clip[gt_gen$layout$name == "panel"] <- "off"


#####
p <- ggplot() +
  geom_point(aes(y=fit, x=city, col=date_period,shape=date_period),data=mod_pred3, size=2.5) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 7.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 14.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 20.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  scale_x_discrete("") +
  scale_y_continuous("Coefficient Estimates") +
  scale_color_manual("Date Period", values=c("#F8766D", "#00BA38", "#619CFF")) +
  scale_shape_manual("Date Period", values=c(19,19,19)) +
  theme(plot.margin = unit(c(0,0,1.5,0), "lines"),
        axis.title=element_text(size=13),
        axis.text.x = element_text(angle = 90, size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))

df=data.frame(y=c(rep(-29,6)),x=c(1.7,5.6,9.0,12.5,17.5,23.5),text=c(as.character(levels(data2bb$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 +
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 11)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) +
    annotation_custom(
      grob = textGrob(label = "City by Region", gp = gpar(fontsize = 14)),
      xmin = 14,      # Vertical position of the textGrob
      xmax = 14,
      ymin = -32,         # Note: The grobs are positioned outside the plot area
      ymax = -32) +
    annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-30,ymax=-11.5) +
    annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=7.5,xmax=7.5, ymin=-30,ymax=-11.5) +
    annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=10.5,xmax=10.5, ymin=-30,ymax=-11.5) +
    annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=14.5,xmax=14.5, ymin=-30,ymax=-11.5) +
    annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=20.5,xmax=20.5, ymin=-30,ymax=-11.5)
}

# Code to override clipping
gt_city <- ggplot_gtable(ggplot_build(p2))
gt_city$layout$clip[gt_city$layout$name == "panel"] <- "off"



#####
map <- ggplot() + 
  geom_polygon(data = zip_shp_short2_df2, aes(x = long, y = lat, group = group, fill=city)) +
  geom_path(data=state, aes(x=long, y=lat, group=group), color="black", size=0.45) +
  geom_label(data=cities2, aes(label=city, x=long, y=lat, fill=city), size=2.75, color="black", alpha=0.75) +
  scale_fill_manual(values=cols1, na.value="white") +
  coord_map() +
  theme_classic() +
  guides(fill="none") +
  theme(plot.margin = unit(c(0,0,0,0), "lines"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.line = element_blank()) 

# Code to override clipping
gt_map <- ggplot_gtable(ggplot_build(map))
gt_map$layout$clip[gt_map$layout$name == "panel"] <- "off"
gt_map$heights[7] <-  1.15*gt_map$heights[7]

#####
lay <- rbind(c(1,1,1,1,1,2,2,2,2),
             c(1,1,1,1,1,2,2,2,2),
             c(1,1,1,1,1,2,2,2,2),
             c(1,1,1,1,1,2,2,2,2),
             c(4,4,3,3,3,3,3,4,4),
             c(4,4,3,3,3,3,3,4,4),
             c(4,4,3,3,3,3,3,4,4))

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/fig1.pdf", height=10, width=16)
grid.arrange(arrangeGrob(gt_map,top=grid::textGrob("a", x = 0.05, hjust = 0,gp=gpar(fontsize=20))), 
             arrangeGrob(gt_gen,top=grid::textGrob("b", x = 0.05, hjust = 0,gp=gpar(fontsize=20))),
             arrangeGrob(gt_city,top=grid::textGrob("c", x = 0.05, hjust = 0.5,gp=gpar(fontsize=20))),
             layout_matrix = lay)
dev.off() 
############## 
############## 
############## 
############## 
############## 
##### ##### ##### ##### TIME + CITY FX GAM ##### ##### ##### ##### ##### 
data2$city <- as.factor(data2$city)
mod2 <- bam(rel_trips_from_oct2 ~  s(time_num, m=2) + s(time_num, city, bs="fs", m=2) + s(weekday, bs="cc", k=7), 
            data=data2, method="REML",
            knots=list(weekday=c(1,7)))
(summary(mod2)$dev)*100 # % of deviance explained by time and city  33.8 (k default)
AIC(logLik(mod2)) # 7917205
#
data2$resid_city <- residuals(mod2)

###
##### plot #####
mod_pred <- with(data2,expand.grid(time_num=c(seq(from=min(time_num), to=max(time_num), by=1)), city=levels(city)))
mod_pred$weekday <- as.numeric(factor(weekdays(as.Date(mod_pred$time_num, origin="1970-01-01")), levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")))
mod_pred$region <- ifelse(mod_pred$city %in% c("Los_Angeles_CA", "SanFrancisco_CA", "SanJose_CA", "SanDiego_CA"), "West Coast",
                          ifelse(mod_pred$city %in% c("Austin_TX", "Dallas_TX", "ElPaso_TX", "Houston_TX", "Phoenix_AZ", "SanAntonio_TX"), "South",
                                 ifelse(mod_pred$city %in% c("Fargo_ND", "Lincoln_NE", "Omaha_NE", "SiouxFalls_SD"), "Midwest",
                                        ifelse(mod_pred$city %in% c("Chicago_IL", "Columbus_OH", "Detroit_MI"), "Great Lakes",
                                               ifelse(mod_pred$city %in% c("Baltimore_MD","NYC_NY", "Philadelphia_PA"), "Northeast", "Southeast")))))
mod_pred$region <- as.factor(mod_pred$region)

mod_pred$region <- factor(mod_pred$region, levels= levels(mod_pred$region)[c(1:3,6,4,5)])

mod_pred$city <- factor(mod_pred$city, 
                        levels= c(  sort(unique(as.character(mod_pred$city)[which(mod_pred$region == levels(mod_pred$region)[1])])),
                                    sort( unique(as.character(mod_pred$city)[which(mod_pred$region == levels(mod_pred$region)[2])])),
                                    sort( unique(as.character(mod_pred$city)[which(mod_pred$region == levels(mod_pred$region)[3])])),
                                    sort( unique(as.character(mod_pred$city)[which(mod_pred$region == levels(mod_pred$region)[4])])),
                                    sort( unique(as.character(mod_pred$city)[which(mod_pred$region == levels(mod_pred$region)[5])])),
                                    sort( unique(as.character(mod_pred$city)[which(mod_pred$region == levels(mod_pred$region)[6])]))))

mod_pred$type <- as.character("city")

mod_pred1 <- mod_pred[!duplicated(mod_pred[,c(4,1)]),]
mod_pred1$type <- as.character("global")


mod_pred0 <- cbind(mod_pred, predict(mod2, mod_pred, se.fit=TRUE, type="response"))
names(mod_pred0)[6] <- "fit"
mod_pred1a <- cbind(mod_pred1, predict(mod1, mod_pred1, se.fit=TRUE, type="response"))
names(mod_pred1a)[6] <- "fit"
mod_pred1a$city <- as.factor(as.numeric(mod_pred1a$city))

mod_pred2 <- rbind(mod_pred0,mod_pred1a)
mod_pred2$type <- factor(mod_pred2$type,levels=c("global","city"))

mod_pred3 <- mod_pred2[which(mod_pred2$weekday == 3),]
mod_pred3$date <- as.Date(mod_pred3$time_num,origin = "1970-01-01")


df2=data.frame(y=c(rep(-33,3)),x=c((as.Date("2020-04-13")),
                                 (as.Date("2020-06-22")),
                                 (as.Date("2020-08-31"))),text=c("Pandemic\nPhase",
                                                                           "Rebound\nPhase",
                                                                           "Stable\nPhase"))


p <- ggplot() +
   geom_rect(aes(xmin= as.Date("2020-03-17"),
                xmax =as.Date("2020-05-16"),
                ymin = -Inf,
                ymax = Inf), fill = "#F8766D", alpha = 0.1,data=df2) +
  geom_rect(aes(xmin= as.Date("2020-05-16"),
                xmax = as.Date("2020-08-01"),
                ymin = -Inf,
                ymax = Inf), fill = "#00BA38", alpha = 0.1,data=df2) +
  geom_rect(aes(xmin= as.Date("2020-08-01"),
                xmax =  as.Date("2020-09-29"),
                ymin = -Inf,
                ymax = Inf), fill = "#619CFF", alpha = 0.1,data=df2) +
  geom_line(aes(y=fit, x=date, group=city, color=type),data=mod_pred3,lwd=0.85) +
  geom_vline(xintercept = (as.Date("2020-05-16")), linetype = "dashed", color="gray25") +
  geom_vline(xintercept = (as.Date("2020-08-01")), linetype = "dashed", color="gray25") +
  
  theme_classic() +
  scale_color_manual("Model", values=c("red","black"), labels=c("Global","City-specific")) +
  scale_x_date("Date", breaks="2 weeks",date_labels = "%b %d") +
  scale_y_continuous("Predicted Relative Mobility") +
  facet_wrap(region~.) +
  theme(axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 45, size=14, vjust=1,hjust=1),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13),
        strip.text.x = element_text(size=17)) 

p2 <- p
for (i in 1:length(df2$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df2$text[i], gp = gpar(fontsize = 16)),
      xmin = df2$x[i],      # Vertical position of the textGrob
      xmax = df2$x[i],
      ymin = df2$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df2$y[i])
}

# Code to override clipping
gt <- ggplot_gtable(ggplot_build(p2))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_pred_new.pdf", height=12, width=17)
grid.draw(gt)
dev.off() 
dev.off() 

##
##### ##### zipcode level fx on residuals ##### ##### 
mod_popdens <- glm(resid_city ~ zip_pop_dens_cat, data=data2)
mod_popdens_city <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2)
mod_land <- glm(resid_city ~ zip_land_area_cat, data=data2)
mod_land_city <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2)
mod_income <- glm(resid_city ~ zip_median_hh_income_cat, data=data2)
mod_income_city <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2)
mod_incity <- glm(resid_city ~ in_city_part, data=data2)
mod_dist <- glm(resid_city ~ zip_dist_city_bord_cat, data=data2)
mod_dist_city <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2)

AICctab(mod_popdens,mod_popdens_city,mod_land,mod_land_city,
        mod_incity,mod_dist,mod_dist_city,weights=TRUE)
# dAICc   df weight
# mod_popdens_city     0.0 5  1
# mod_popdens       3864.1 5  <0.001
# mod_dist_city     5338.3 6  <0.001
# mod_dist          6564.2 6  <0.001
# mod_land_city    13873.3 5  <0.001
# mod_incity       16181.6 3  <0.001
# mod_land         16960.3 5  <0.001


## to check income effect run dist with data where income is present only and compare ####
data2b <- droplevels(data2[which(!(is.na(data2$zip_median_hh_income_cat_by_city))),])
mod_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2b)
mod_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2b)
mod_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2b)
mod_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2b)

AICctab(mod_dist_city_short,mod_popdens_city_short,mod_land_city_short,mod_income_city_short,weights=TRUE)

# dAICc   df weight
# mod_popdens_city_short     0.0 5  1
# mod_dist_city_short     5564.1 6  <0.001
# mod_land_city_short    15641.1 5  <0.001
# mod_income_city_short  30260.7 5  <0.001

mod_dist_short <- glm(resid_city ~ zip_dist_city_bord_cat, data=data2b)
mod_popdens_short <- glm(resid_city ~ zip_pop_dens_cat, data=data2b)
mod_land_short <- glm(resid_city ~ zip_land_area_cat, data=data2b)
mod_income_short <- glm(resid_city ~ zip_median_hh_income_cat, data=data2b)
mod_npi <- glm(resid_city ~ NPI_count, data=data2b)

AICctab(mod_dist_short,mod_popdens_short,mod_land_short,mod_income_short,mod_npi,weights=TRUE)
# mod_popdens_short     0.0 5  1
# mod_dist_short     4339.4 6  <0.001
# mod_land_short    16070.0 5  <0.001
# mod_income_short  28033.8 5  <0.001
# mod_npi           29685.4 6  <0.001


#### city specfic fx for pop dens ######
mod_city_popdens_city <- glm(resid_city ~ zip_pop_dens_cat_by_city*city, data=data2)
mod_reg_popdens_city <- glm(resid_city ~ zip_pop_dens_cat_by_city*region, data=data2)

AICctab(mod_popdens_city,mod_city_popdens_city,mod_reg_popdens_city,weights=TRUE)
# mod_city_popdens_city     0.0 105 1
# mod_reg_popdens_city  16815.9 25  <0.001
# mod_popdens_city      30636.4 5   <0.001

#### city specfic fx of pop dens with income data only ####
mod_city_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city*city, data=data2b)
mod_time_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city*date_period3, data=data2b)
AICctab(mod_dist_city_short,mod_popdens_city_short,mod_land_city_short,mod_income_city_short,mod_time_popdens_city_short,mod_city_popdens_city_short,weights=TRUE)
# dAICc   df  weight
# mod_city_popdens_city_short     0.0 105 1
#mod_time_popdens_city_short 26846.3 13  <0.001
# mod_popdens_city_short      32345.6 5   <0.001
# mod_dist_city_short         37909.7 6   <0.001
# mod_land_city_short         47986.8 5   <0.001
# mod_income_city_short       62606.4 5   <0.001

############## 
############## 
############## pandemic phase residual effects ##############
data2_pan <- droplevels(data2[which(data2$date_period3 == "mid March - mid May"),])
##### ##### zipcode level fx on residuals ##### ##### 
mod_pan_popdens <- glm(resid_city ~ zip_pop_dens_cat, data=data2_pan)
mod_pan_popdens_city <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_pan)
mod_pan_land <- glm(resid_city ~ zip_land_area_cat, data=data2_pan)
mod_pan_land_city <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_pan)
mod_pan_income <- glm(resid_city ~ zip_median_hh_income_cat, data=data2_pan)
mod_pan_income_city <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_pan)
mod_pan_incity <- glm(resid_city ~ in_city_part, data=data2_pan)
mod_pan_dist <- glm(resid_city ~ zip_dist_city_bord_cat, data=data2_pan)
mod_pan_dist_city <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_pan)

AICctab(mod_pan_popdens,mod_pan_popdens_city,mod_pan_land,mod_pan_land_city,
        mod_pan_incity,mod_pan_dist,mod_pan_dist_city,weights=TRUE)

# dAICc   df weight
# mod_pan_popdens_city     0.0 5  1
# mod_pan_popdens       1295.6 5  <0.001
# mod_pan_dist_city     3021.9 6  <0.001
# mod_pan_dist          3641.8 6  <0.001
# mod_pan_incity        7234.6 3  <0.001
# mod_pan_land          9541.2 5  <0.001
# mod_pan_land_city    10637.2 5  <0.001


## to check income effect run dist with data where income is present only and compare ####
data2_panb <- droplevels(data2_pan[which(!(is.na(data2_pan$zip_median_hh_income_cat_by_city))),])
mod_pan_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_panb)
mod_pan_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_panb)
mod_pan_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_panb)

mod_pan_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_panb)

AICctab(mod_pan_dist_city_short,mod_pan_popdens_city_short,mod_pan_land_city_short,mod_pan_income_city_short,weights=TRUE)

# dAICc   df weight
# mod_pan_popdens_city_short     0.0 5  1
# mod_pan_dist_city_short     3466.9 6  <0.001
# mod_pan_land_city_short    11062.1 5  <0.001
# mod_pan_income_city_short  13011.3 5  <0.001

mod_pan_dist_short <- glm(resid_city ~ zip_dist_city_bord_cat, data=data2_panb)
mod_pan_popdens_short <- glm(resid_city ~ zip_pop_dens_cat, data=data2_panb)
mod_pan_land_short <- glm(resid_city ~ zip_land_area_cat, data=data2_panb)
mod_pan_income_short <- glm(resid_city ~ zip_median_hh_income_cat, data=data2_panb)
mod_pan_npi <- glm(resid_city ~ NPI_count, data=data2_panb)

AICctab(mod_pan_dist_short,mod_pan_popdens_short,mod_pan_land_short,mod_pan_income_short,mod_pan_npi,weights=TRUE)
# mod_pan_popdens_short     0.0 5  1
# mod_pan_dist_short     3578.1 6  <0.001
# mod_pan_land_short     9158.1 5  <0.001
# mod_pan_income_short  12798.7 5  <0.001
# mod_pan_npi           13090.5 6  <0.001

#### city specfic fx for pop dens ######
mod_pan_city_popdens_city <- glm(resid_city ~ zip_pop_dens_cat_by_city*city, data=data2_pan)
mod_pan_reg_popdens_city <- glm(resid_city ~ zip_pop_dens_cat_by_city*region, data=data2_pan)

AICctab(mod_pan_popdens_city,mod_pan_city_popdens_city,mod_pan_reg_popdens_city,weights=TRUE)
# mod_pan_city_popdens_city     0.0 105 1
# mod_pan_reg_popdens_city   8791.4 25  <0.001
# mod_pan_popdens_city      16133.9 5   <0.001


#### city specfic fx of pop dens with income data only ####
mod_pan_city_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city*city, data=data2_panb)
AICctab(mod_pan_dist_city_short,mod_pan_popdens_city_short,mod_pan_land_city_short,mod_pan_income_city_short,mod_pan_city_popdens_city_short,weights=TRUE)
# dAICc   df  weight
# mod_pan_city_popdens_city_short     0.0 105 1
# mod_pan_popdens_city_short      16330.6 5   <0.001
# mod_pan_dist_city_short         19797.5 6   <0.001
# mod_pan_land_city_short         27392.7 5   <0.001
# mod_pan_income_city_short       29342.0 5   <0.001
# 

############## 
############## 
############## rebound phase residual effects ##############
data2_reb <- droplevels(data2[which(data2$date_period3 == "mid May - end July"),])
##### ##### zipcode level fx on residuals ##### ##### 
mod_reb_popdens <- glm(resid_city ~ zip_pop_dens_cat, data=data2_reb)
mod_reb_popdens_city <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_reb)
mod_reb_land <- glm(resid_city ~ zip_land_area_cat, data=data2_reb)
mod_reb_land_city <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_reb)
mod_reb_income <- glm(resid_city ~ zip_median_hh_income_cat, data=data2_reb)
mod_reb_income_city <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_reb)
mod_reb_incity <- glm(resid_city ~ in_city_part, data=data2_reb)
mod_reb_dist <- glm(resid_city ~ zip_dist_city_bord_cat, data=data2_reb)
mod_reb_dist_city <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_reb)

AICctab(mod_reb_popdens,mod_reb_popdens_city,mod_reb_land,mod_reb_land_city,
        mod_reb_incity,mod_reb_dist,mod_reb_dist_city,weights=TRUE)

# dAICc  df weight
# mod_reb_popdens_city    0.0 5  1
# mod_reb_popdens      2274.4 5  <0.001
# mod_reb_land_city    3021.7 5  <0.001
# mod_reb_dist_city    3785.7 6  <0.001
# mod_reb_dist         4225.0 6  <0.001
# mod_reb_land         6532.0 5  <0.001
# mod_reb_incity       8773.3 3  <0.001


## to check income effect run dist with data where income is present only and compare ####
data2_rebb <- droplevels(data2_reb[which(!(is.na(data2_reb$zip_median_hh_income_cat_by_city))),])
mod_reb_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_rebb)
mod_reb_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_rebb)
mod_reb_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_rebb)

mod_reb_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_rebb)

AICctab(mod_reb_dist_city_short,mod_reb_popdens_city_short,mod_reb_land_city_short,mod_reb_income_city_short,weights=TRUE)
# 
# dAICc   df weight
# mod_reb_popdens_city_short     0.0 5  1
# mod_reb_dist_city_short     3484.5 6  <0.001
# mod_reb_land_city_short     4217.1 5  <0.001
# mod_reb_income_city_short  16383.2 5  <0.001

mod_reb_dist_short <- glm(resid_city ~ zip_dist_city_bord_cat, data=data2_rebb)
mod_reb_popdens_short <- glm(resid_city ~ zip_pop_dens_cat, data=data2_rebb)
mod_reb_land_short <- glm(resid_city ~ zip_land_area_cat, data=data2_rebb)
mod_reb_income_short <- glm(resid_city ~ zip_median_hh_income_cat, data=data2_rebb)
mod_reb_npi <- glm(resid_city ~ NPI_count, data=data2_rebb)

AICctab(mod_reb_dist_short,mod_reb_popdens_short,mod_reb_land_short,mod_reb_income_short,mod_reb_npi,weights=TRUE)
# mod_reb_popdens_short     0.0 5  1
# mod_reb_dist_short     2405.1 6  <0.001
# mod_reb_land_short     6274.9 5  <0.001
# mod_reb_income_short  14940.3 5  <0.001
# mod_reb_npi           17162.1 6  <0.001

#### city specfic fx for pop dens ######
mod_reb_city_popdens_city <- glm(resid_city ~ zip_pop_dens_cat_by_city*city, data=data2_reb)
mod_reb_reg_popdens_city <- glm(resid_city ~ zip_pop_dens_cat_by_city*region, data=data2_reb)

AICctab(mod_reb_popdens_city,mod_reb_city_popdens_city,mod_reb_reg_popdens_city,weights=TRUE)
# dAICc   df  weight
# mod_reb_city_popdens_city     0.0 105 1
# mod_reb_reg_popdens_city   9119.2 25  <0.001
# mod_reb_popdens_city      16330.5 5   <0.001

#### city specfic fx of pop dens with income data only ####
mod_reb_city_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city*city, data=data2_rebb)
AICctab(mod_reb_dist_city_short,mod_reb_popdens_city_short,mod_reb_land_city_short,mod_reb_income_city_short,mod_reb_city_popdens_city_short,weights=TRUE)
# dAICc   df  weight
# mod_reb_city_popdens_city_short     0.0 105 1
# mod_reb_popdens_city_short      17524.8 5   <0.001
# mod_reb_dist_city_short         21009.3 6   <0.001
# mod_reb_land_city_short         21741.8 5   <0.001
# mod_reb_income_city_short       33907.9 5   <0.001


############## 
############## 
############## stable phase residual effects ##############
data2_sta <- droplevels(data2[which(data2$date_period3 == "beg August - end Sept"),])
##### ##### zipcode level fx on residuals ##### ##### 
mod_sta_popdens <- glm(resid_city ~ zip_pop_dens_cat, data=data2_sta)
mod_sta_popdens_city <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_sta)
mod_sta_land <- glm(resid_city ~ zip_land_area_cat, data=data2_sta)
mod_sta_land_city <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_sta)
mod_sta_income <- glm(resid_city ~ zip_median_hh_income_cat, data=data2_sta)
mod_sta_income_city <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_sta)
mod_sta_incity <- glm(resid_city ~ in_city_part, data=data2_sta)
mod_sta_dist <- glm(resid_city ~ zip_dist_city_bord_cat, data=data2_sta)
mod_sta_dist_city <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_sta)

AICctab(mod_sta_popdens,mod_sta_popdens_city,mod_sta_land,mod_sta_land_city,
        mod_sta_incity,mod_sta_dist,mod_sta_dist_city,weights=TRUE)

# dAICc  df weight
# mod_sta_dist_city       0.0 6  1
# mod_sta_popdens       119.2 5  <0.001
# mod_sta_dist          208.5 6  <0.001
# mod_sta_popdens_city  275.6 5  <0.001
# mod_sta_land_city     330.5 5  <0.001
# mod_sta_land          609.2 5  <0.001
# mod_sta_incity       1872.1 3  <0.001


## to check income effect run dist with data where income is present only and compare ####
data2_stab <- droplevels(data2_sta[which(!(is.na(data2_sta$zip_median_hh_income_cat_by_city))),])
mod_sta_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_stab)
mod_sta_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_stab)
mod_sta_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_stab)

mod_sta_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_stab)

AICctab(mod_sta_dist_city_short,mod_sta_popdens_city_short,mod_sta_land_city_short,mod_sta_income_city_short,weights=TRUE)

# dAICc  df weight
# mod_sta_dist_city_short       0.0 6  1
# mod_sta_popdens_city_short  269.4 5  <0.001
# mod_sta_land_city_short     603.2 5  <0.001
# mod_sta_income_city_short  2722.9 5  <0.001


mod_sta_dist_short <- glm(resid_city ~ zip_dist_city_bord_cat, data=data2_stab)
mod_sta_popdens_short <- glm(resid_city ~ zip_pop_dens_cat, data=data2_stab)
mod_sta_land_short <- glm(resid_city ~ zip_land_area_cat, data=data2_stab)
mod_sta_income_short <- glm(resid_city ~ zip_median_hh_income_cat, data=data2_stab)
mod_sta_npi <- glm(resid_city ~ NPI_count, data=data2_stab)

AICctab(mod_sta_dist_short,mod_sta_popdens_short,mod_sta_land_short,mod_sta_income_short,mod_sta_npi,weights=TRUE)
# dAICc  df weight
# mod_sta_popdens_short    0.0 5  1
# mod_sta_dist_short     262.9 6  <0.001
# mod_sta_land_short    1035.7 5  <0.001
# mod_sta_npi           2364.0 5  <0.001
# mod_sta_income_short  2730.3 5  <0.001

#### city specfic fx for dist ######
mod_sta_city_dist_city <- glm(resid_city ~ zip_dist_city_bord_cat_by_city*city, data=data2_sta)
mod_sta_reg_dist_city <- glm(resid_city ~ zip_dist_city_bord_cat_by_city*region, data=data2_sta)

AICctab(mod_sta_city_dist_city,mod_sta_reg_dist_city,mod_sta_dist_city,weights=TRUE)
# mod_sta_city_dist_city    0.0 131 1
# mod_sta_reg_dist_city  4321.5 31  <0.001
# mod_sta_dist_city      8897.6 6   <0.001

#### city specfic fx of dist with income data only ####
mod_sta_city_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city*city, data=data2_stab)
AICctab(mod_sta_dist_city_short,mod_sta_popdens_city_short,mod_sta_land_city_short,mod_sta_income_city_short,mod_sta_city_dist_city_short,weights=TRUE)
# dAICc   df  weight
# mod_sta_city_dist_city_short     0.0 131 1
# mod_sta_dist_city_short       9196.7 6   <0.001
# mod_sta_popdens_city_short    9466.0 5   <0.001
# mod_sta_land_city_short       9799.9 5   <0.001
# mod_sta_income_city_short    11919.5 5   <0.001


############## 
############## 
############## 
# ###### univariate plots ######
# mod_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2b)
# mod_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2b)
# mod_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2b)
# mod_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2b)
# 
# mod_pan_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_panb)
# mod_pan_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_panb)
# mod_pan_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_panb)
# mod_pan_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_panb)
# 
# mod_reb_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_rebb)
# mod_reb_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_rebb)
# mod_reb_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_rebb)
# mod_reb_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_rebb)
# 
# mod_sta_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_stab)
# mod_sta_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_stab)
# mod_sta_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_stab)
# mod_sta_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_stab)
# 
# 
# mod_pred_pd <- with(data2b,expand.grid(zip_pop_dens_cat_by_city=levels(zip_pop_dens_cat_by_city)))
# mod_pred_pd1 <- cbind(mod_pred_pd, predict(mod_popdens_city_short, mod_pred_pd, se.fit=TRUE, type="response"))
# mod_pred_pd2 <- cbind(mod_pred_pd1, predict(mod_pan_popdens_city_short, mod_pred_pd, se.fit=TRUE, type="response"))
# mod_pred_pd3 <- cbind(mod_pred_pd2, predict(mod_reb_popdens_city_short, mod_pred_pd, se.fit=TRUE, type="response"))
# mod_pred_pd4 <- cbind(mod_pred_pd3, predict(mod_sta_popdens_city_short, mod_pred_pd, se.fit=TRUE, type="response"))
# names(mod_pred_pd4)[2] <- "fit_pd_all"
# names(mod_pred_pd4)[5] <- "fit_pd_pan"
# names(mod_pred_pd4)[8] <- "fit_pd_reb"
# names(mod_pred_pd4)[11] <- "fit_pd_sta"
# 
# mod_pred_l <- with(data2b,expand.grid(zip_land_area_cat_by_city=levels(zip_land_area_cat_by_city)))
# mod_pred_l1 <- cbind(mod_pred_l, predict(mod_land_city_short, mod_pred_l, se.fit=TRUE, type="response"))
# mod_pred_l2 <- cbind(mod_pred_l1, predict(mod_pan_land_city_short, mod_pred_l, se.fit=TRUE, type="response"))
# mod_pred_l3 <- cbind(mod_pred_l2, predict(mod_reb_land_city_short, mod_pred_l, se.fit=TRUE, type="response"))
# mod_pred_l4 <- cbind(mod_pred_l3, predict(mod_sta_land_city_short, mod_pred_l, se.fit=TRUE, type="response"))
# names(mod_pred_l4)[2] <- "fit_l_all"
# names(mod_pred_l4)[5] <- "fit_l_pan"
# names(mod_pred_l4)[8] <- "fit_l_reb"
# names(mod_pred_l4)[11] <- "fit_l_sta"
# 
# mod_pred_d <- with(data2b,expand.grid(zip_dist_city_bord_cat_by_city=levels(zip_dist_city_bord_cat_by_city)))
# mod_pred_d1 <- cbind(mod_pred_d, predict(mod_dist_city_short, mod_pred_d, se.fit=TRUE, type="response"))
# mod_pred_d2 <- cbind(mod_pred_d1, predict(mod_pan_dist_city_short, mod_pred_d, se.fit=TRUE, type="response"))
# mod_pred_d3 <- cbind(mod_pred_d2, predict(mod_reb_dist_city_short, mod_pred_d, se.fit=TRUE, type="response"))
# mod_pred_d4 <- cbind(mod_pred_d3, predict(mod_sta_dist_city_short, mod_pred_d, se.fit=TRUE, type="response"))
# names(mod_pred_d4)[2] <- "fit_d_all"
# names(mod_pred_d4)[5] <- "fit_d_pan"
# names(mod_pred_d4)[8] <- "fit_d_reb"
# names(mod_pred_d4)[11] <- "fit_d_sta"
# 
# mod_pred_i <- with(data2b,expand.grid(zip_median_hh_income_cat_by_city=levels(zip_median_hh_income_cat_by_city)))
# mod_pred_i1 <- cbind(mod_pred_i, predict(mod_income_city_short, mod_pred_i, se.fit=TRUE, type="response"))
# mod_pred_i2 <- cbind(mod_pred_i1, predict(mod_pan_income_city_short, mod_pred_i, se.fit=TRUE, type="response"))
# mod_pred_i3 <- cbind(mod_pred_i2, predict(mod_reb_income_city_short, mod_pred_i, se.fit=TRUE, type="response"))
# mod_pred_i4 <- cbind(mod_pred_i3, predict(mod_sta_income_city_short, mod_pred_i, se.fit=TRUE, type="response"))
# names(mod_pred_i4)[2] <- "fit_i_all"
# names(mod_pred_i4)[5] <- "fit_i_pan"
# names(mod_pred_i4)[8] <- "fit_i_reb"
# names(mod_pred_i4)[12] <- "fit_i_sta"
# 
# 
# mod_pred2 <- with(data2b,expand.grid(date_period=c(levels(date_period3),"all"),level=c( levels(data2b$zip_pop_dens_cat_by_city),levels(data2b$zip_land_area_cat_by_city),
#                                                                                          levels(data2b$zip_dist_city_bord_cat_by_city),levels(data2b$zip_median_hh_income_cat_by_city))))
# mod_pred2$type <- factor(ifelse(mod_pred2$level %in% as.character(levels(data2b$zip_pop_dens_cat_by_city)), "popdens",
#                                 ifelse(mod_pred2$level %in% as.character(levels(data2b$zip_land_area_cat_by_city)), "land",
#                                        ifelse(mod_pred2$level %in% as.character(levels(data2b$zip_dist_city_bord_cat_by_city)), "dist","income"))))
# 
# mod_pred2$fit <- as.numeric(NA)
# mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_all
# mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_pan
# mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_reb
# mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_sta
# 
# mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_all
# mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_pan
# mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_reb
# mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_sta
# 
# mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_all
# mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_pan
# mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_reb
# mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_sta
# 
# mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_all
# mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_pan
# mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_reb
# mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_sta
# 
# mod_pred2$type <- factor(mod_pred2$type, levels=c("popdens","dist","land","income"))
# mod_pred2 <- mod_pred2[order(mod_pred2$type,mod_pred2$level,mod_pred2$date_period),]
# 
# levels <- c(as.character(levels(data2b$zip_pop_dens_cat_by_city)), as.character(levels(data2b$zip_dist_city_bord_cat_by_city)),
#             as.character(levels(data2b$zip_land_area_cat_by_city)), as.character(levels(data2b$zip_median_hh_income_cat_by_city)))
# mod_pred2$level2 <- factor(ifelse(mod_pred2$level %in% levels[5], "In City",
#                                   ifelse(mod_pred2$level %in% levels[c(1,6,10,14)], "Q1",
#                                          ifelse(mod_pred2$level %in% levels[c(2,7,11,15)], "Q2",
#                                                 ifelse(mod_pred2$level %in% levels[c(3,8,12,16)], "Q3", "Q4")))))
# 
# 
# 
# ######
# type.labs <- c("Population Density Quartile", "Distance from City Limit Quartile",
#                "Land Area Quartile", "Median Income Quartile")
# names(type.labs) <- c("popdens","dist","land","income")
# 
# mod_pred2q <- droplevels(mod_pred2[which(mod_pred2$date_period != "all"),])
# 
# 
# p <- ggplot() +
#   geom_point(aes(y=fit, x=level2, col=date_period),data=mod_pred2q, size=3) +
#   geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
#   facet_wrap(~type, scales="free_x", strip.position = "bottom",labeller = labeller(type = type.labs)) +
#   theme_classic() +
#   scale_x_discrete("") + 
#   scale_y_continuous("Coefficient Estimates") +
#   scale_color_manual("Date Period", values=c("#F8766D", "#00BA38", "#619CFF")) +
#   theme(plot.margin = unit(c(1,1,1,1), "lines"),
#         axis.title=element_text(size=17),
#         axis.text.x = element_text(angle = 0, size=13),
#         axis.text.y = element_text(size=15),
#         legend.title = element_text(size=16),
#         legend.text = element_text(size=15),
#         strip.background = element_blank(),
#         strip.placement = "outside",
#         strip.text.x = element_text(size=17))
# pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/resids_city_zc_new_no_all.pdf", height=8, width=13)
p
dev.off() 

############## 
############## 
###### univariate plots newest ######
mod_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2b)
mod_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2b)
mod_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2b)
mod_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2b)
mod_npi_city_short <- glm(resid_city ~ NPI_count, data=data2b)

mod_pan_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_panb)
mod_pan_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_panb)
mod_pan_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_panb)
mod_pan_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_panb)
mod_pan_npi_city_short <- glm(resid_city ~ NPI_count, data=data2_panb)

mod_reb_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_rebb)
mod_reb_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_rebb)
mod_reb_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_rebb)
mod_reb_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_rebb)
mod_reb_npi_city_short <- glm(resid_city ~ NPI_count, data=data2_rebb)

mod_sta_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city, data=data2_stab)
mod_sta_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city, data=data2_stab)
mod_sta_land_city_short <- glm(resid_city ~ zip_land_area_cat_by_city, data=data2_stab)
mod_sta_income_city_short <- glm(resid_city ~ zip_median_hh_income_cat_by_city, data=data2_stab)
mod_sta_npi_city_short <- glm(resid_city ~ NPI_count, data=data2_stab)


mod_pred_pd <- with(data2b,expand.grid(zip_pop_dens_cat_by_city=levels(zip_pop_dens_cat_by_city)))
mod_pred_pd1 <- cbind(mod_pred_pd, predict(mod_popdens_city_short, mod_pred_pd, se.fit=TRUE, type="response"))
mod_pred_pd2 <- cbind(mod_pred_pd1, predict(mod_pan_popdens_city_short, mod_pred_pd, se.fit=TRUE, type="response"))
mod_pred_pd3 <- cbind(mod_pred_pd2, predict(mod_reb_popdens_city_short, mod_pred_pd, se.fit=TRUE, type="response"))
mod_pred_pd4 <- cbind(mod_pred_pd3, predict(mod_sta_popdens_city_short, mod_pred_pd, se.fit=TRUE, type="response"))
names(mod_pred_pd4)[2] <- "fit_pd_all"
names(mod_pred_pd4)[5] <- "fit_pd_pan"
names(mod_pred_pd4)[8] <- "fit_pd_reb"
names(mod_pred_pd4)[11] <- "fit_pd_sta"

mod_pred_l <- with(data2b,expand.grid(zip_land_area_cat_by_city=levels(zip_land_area_cat_by_city)))
mod_pred_l1 <- cbind(mod_pred_l, predict(mod_land_city_short, mod_pred_l, se.fit=TRUE, type="response"))
mod_pred_l2 <- cbind(mod_pred_l1, predict(mod_pan_land_city_short, mod_pred_l, se.fit=TRUE, type="response"))
mod_pred_l3 <- cbind(mod_pred_l2, predict(mod_reb_land_city_short, mod_pred_l, se.fit=TRUE, type="response"))
mod_pred_l4 <- cbind(mod_pred_l3, predict(mod_sta_land_city_short, mod_pred_l, se.fit=TRUE, type="response"))
names(mod_pred_l4)[2] <- "fit_l_all"
names(mod_pred_l4)[5] <- "fit_l_pan"
names(mod_pred_l4)[8] <- "fit_l_reb"
names(mod_pred_l4)[11] <- "fit_l_sta"

mod_pred_d <- with(data2b,expand.grid(zip_dist_city_bord_cat_by_city=levels(zip_dist_city_bord_cat_by_city)))
mod_pred_d1 <- cbind(mod_pred_d, predict(mod_dist_city_short, mod_pred_d, se.fit=TRUE, type="response"))
mod_pred_d2 <- cbind(mod_pred_d1, predict(mod_pan_dist_city_short, mod_pred_d, se.fit=TRUE, type="response"))
mod_pred_d3 <- cbind(mod_pred_d2, predict(mod_reb_dist_city_short, mod_pred_d, se.fit=TRUE, type="response"))
mod_pred_d4 <- cbind(mod_pred_d3, predict(mod_sta_dist_city_short, mod_pred_d, se.fit=TRUE, type="response"))
names(mod_pred_d4)[2] <- "fit_d_all"
names(mod_pred_d4)[5] <- "fit_d_pan"
names(mod_pred_d4)[8] <- "fit_d_reb"
names(mod_pred_d4)[11] <- "fit_d_sta"

mod_pred_i <- with(data2b,expand.grid(zip_median_hh_income_cat_by_city=levels(zip_median_hh_income_cat_by_city)))
mod_pred_i1 <- cbind(mod_pred_i, predict(mod_income_city_short, mod_pred_i, se.fit=TRUE, type="response"))
mod_pred_i2 <- cbind(mod_pred_i1, predict(mod_pan_income_city_short, mod_pred_i, se.fit=TRUE, type="response"))
mod_pred_i3 <- cbind(mod_pred_i2, predict(mod_reb_income_city_short, mod_pred_i, se.fit=TRUE, type="response"))
mod_pred_i4 <- cbind(mod_pred_i3, predict(mod_sta_income_city_short, mod_pred_i, se.fit=TRUE, type="response"))
names(mod_pred_i4)[2] <- "fit_i_all"
names(mod_pred_i4)[5] <- "fit_i_pan"
names(mod_pred_i4)[8] <- "fit_i_reb"
names(mod_pred_i4)[11] <- "fit_i_sta"

mod_pred_n <- with(data2b,expand.grid(NPI_count=levels(NPI_count)))
mod_pred_n1 <- cbind(mod_pred_n, predict(mod_npi_city_short, mod_pred_n, se.fit=TRUE, type="response"))
mod_pred_n2 <- cbind(mod_pred_n1, predict(mod_pan_npi_city_short, mod_pred_n, se.fit=TRUE, type="response"))
mod_pred_n3 <- cbind(mod_pred_n2, predict(mod_reb_npi_city_short, mod_pred_n, se.fit=TRUE, type="response"))
mod_pred_n4a <- with(data2_stab,expand.grid(NPI_count=levels(NPI_count)))
temp <- predict(mod_sta_npi_city_short, mod_pred_n4a, se.fit=TRUE, type="response")
temp$fit[5] <- 0
temp$se.fit[5] <- 0
mod_pred_n4 <- cbind(mod_pred_n3, temp)
names(mod_pred_n4)[2] <- "fit_n_all"
names(mod_pred_n4)[5] <- "fit_n_pan"
names(mod_pred_n4)[8] <- "fit_n_reb"
names(mod_pred_n4)[11] <- "fit_n_sta"


mod_pred2 <- with(data2b,expand.grid(date_period=c(levels(date_period3),"all"),level=c( levels(data2b$zip_pop_dens_cat_by_city),levels(data2b$zip_land_area_cat_by_city),
                                                                                        levels(data2b$zip_dist_city_bord_cat_by_city),levels(data2b$zip_median_hh_income_cat_by_city),
                                                                                        levels(data2b$NPI_count))))
mod_pred2$type <- factor(ifelse(mod_pred2$level %in% as.character(levels(data2b$zip_pop_dens_cat_by_city)), "popdens",
                                ifelse(mod_pred2$level %in% as.character(levels(data2b$zip_land_area_cat_by_city)), "land",
                                       ifelse(mod_pred2$level %in% as.character(levels(data2b$zip_dist_city_bord_cat_by_city)), "dist",
                                              ifelse(mod_pred2$level %in% as.character(levels(data2b$zip_median_hh_income_cat_by_city)), "income", "npi")))))

mod_pred2$fit <- as.numeric(NA)
mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_all
mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_pan
mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_reb
mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "popdens")] <- mod_pred_pd4$fit_pd_sta

mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_all
mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_pan
mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_reb
mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "land")] <- mod_pred_l4$fit_l_sta

mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_all
mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_pan
mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_reb
mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "dist")] <- mod_pred_d4$fit_d_sta

mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_all
mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_pan
mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_reb
mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "income")] <- mod_pred_i4$fit_i_sta

mod_pred2$fit[which(mod_pred2$date_period == "all" & mod_pred2$type == "npi")] <- mod_pred_n4$fit_n_all
mod_pred2$fit[which(mod_pred2$date_period == "mid March - mid May" & mod_pred2$type == "npi")] <- mod_pred_n4$fit_n_pan
mod_pred2$fit[which(mod_pred2$date_period == "mid May - end July" & mod_pred2$type == "npi")] <- mod_pred_n4$fit_n_reb
mod_pred2$fit[which(mod_pred2$date_period == "beg August - end Sept" & mod_pred2$type == "npi")] <- mod_pred_n4$fit_n_sta

mod_pred2$type <- factor(mod_pred2$type, levels=c("popdens","dist","land","income","npi"))
mod_pred2 <- mod_pred2[order(mod_pred2$type,mod_pred2$level,mod_pred2$date_period),]

levels <- c(as.character(levels(data2b$zip_pop_dens_cat_by_city)), as.character(levels(data2b$zip_dist_city_bord_cat_by_city)),
            as.character(levels(data2b$zip_land_area_cat_by_city)), as.character(levels(data2b$zip_median_hh_income_cat_by_city)),
            as.character(levels(data2b$NPI_count)))
mod_pred2$level2 <- factor(ifelse(mod_pred2$level %in% levels[5], "In City",
                                  ifelse(mod_pred2$level %in% levels[c(1,6,10,14)], "Q1",
                                         ifelse(mod_pred2$level %in% levels[c(2,7,11,15)], "Q2",
                                                ifelse(mod_pred2$level %in% levels[c(3,8,12,16)], "Q3", 
                                                       ifelse(mod_pred2$level %in% levels[c(4,9,13,17)], "Q4",
                                                              ifelse(mod_pred2$level %in% levels[c(18)], "0 NPI",
                                                                     ifelse(mod_pred2$level %in% levels[c(19)], "1 NPI",
                                                                            ifelse(mod_pred2$level %in% levels[c(20)], "2 NPI",
                                                                                   ifelse(mod_pred2$level %in% levels[c(21)], "3 NPI","4 NPI"))))))))))



######
type.labs <- c("Population Density Quartile", "Dist. from City Limit Quartile",
               "Land Area Quartile", "Median Income Quartile","NPIs in Place")
names(type.labs) <- c("popdens","dist","land","income","npi")

mod_pred2q <- droplevels(mod_pred2[which(mod_pred2$date_period != "all"),])


p <- ggplot() +
  geom_point(aes(y=fit, x=level2, col=date_period),data=mod_pred2q, size=3) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  facet_wrap(~type, scales="free_x", strip.position = "bottom",labeller = labeller(type = type.labs)) +
  theme_classic() +
  scale_x_discrete("") + 
  scale_y_continuous("Coefficient Estimates") +
  scale_color_manual("Date Period", values=c("#F8766D", "#00BA38", "#619CFF")) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        axis.title=element_text(size=17),
        axis.text.x = element_text(angle = 0, size=13),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(size=17))
pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/resids_city_zc_new_no_all.pdf", height=8, width=13)
p
dev.off() 

###########
names(mod_pred_pd4)[3] <- "fit.se_pd_all"
names(mod_pred_pd4)[6] <- "fit.se_pd_pan"
names(mod_pred_pd4)[9] <- "fit.se_pd_reb"
names(mod_pred_pd4)[12] <- "fit.se_pd_sta"
names(mod_pred_l4)[3] <- "fit.se_l_all"
names(mod_pred_l4)[6] <- "fit.se_l_pan"
names(mod_pred_l4)[9] <- "fit.se_l_reb"
names(mod_pred_l4)[12] <- "fit.se_l_sta"
names(mod_pred_d4)[3] <- "fit.se_d_all"
names(mod_pred_d4)[6] <- "fit.se_d_pan"
names(mod_pred_d4)[9] <- "fit.se_d_reb"
names(mod_pred_d4)[12] <- "fit.se_d_sta"
names(mod_pred_i4)[3] <- "fit.se_i_all"
names(mod_pred_i4)[6] <- "fit.se_i_pan"
names(mod_pred_i4)[9] <- "fit.se_i_reb"
names(mod_pred_i4)[12] <- "fit.se_i_sta"
names(mod_pred_n4)[3] <- "fit.se_n_all"
names(mod_pred_n4)[6] <- "fit.se_n_pan"
names(mod_pred_n4)[9] <- "fit.se_n_reb"
names(mod_pred_n4)[12] <- "fit.se_n_sta"


mod_pred3 <- with(data2b,expand.grid(date_period=c(levels(date_period3),"all"),level=c( levels(data2b$zip_pop_dens_cat_by_city),levels(data2b$zip_land_area_cat_by_city),
                                                                                        levels(data2b$zip_dist_city_bord_cat_by_city),levels(data2b$zip_median_hh_income_cat_by_city),
                                                                                        levels(data2b$NPI_count))))
mod_pred3$type <- factor(ifelse(mod_pred3$level %in% as.character(levels(data2b$zip_pop_dens_cat_by_city)), "popdens",
                                ifelse(mod_pred3$level %in% as.character(levels(data2b$zip_land_area_cat_by_city)), "land",
                                       ifelse(mod_pred3$level %in% as.character(levels(data2b$zip_dist_city_bord_cat_by_city)), "dist",
                                              ifelse(mod_pred3$level %in% as.character(levels(data2b$zip_median_hh_income_cat_by_city)), "income", "npi")))))

mod_pred3$fit <- as.numeric(NA)
mod_pred3$fit[which(mod_pred3$date_period == "all" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit_pd_all
mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit_pd_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit_pd_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit_pd_sta

mod_pred3$fit[which(mod_pred3$date_period == "all" & mod_pred3$type == "land")] <- mod_pred_l4$fit_l_all
mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "land")] <- mod_pred_l4$fit_l_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "land")] <- mod_pred_l4$fit_l_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "land")] <- mod_pred_l4$fit_l_sta

mod_pred3$fit[which(mod_pred3$date_period == "all" & mod_pred3$type == "dist")] <- mod_pred_d4$fit_d_all
mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "dist")] <- mod_pred_d4$fit_d_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "dist")] <- mod_pred_d4$fit_d_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "dist")] <- mod_pred_d4$fit_d_sta

mod_pred3$fit[which(mod_pred3$date_period == "all" & mod_pred3$type == "income")] <- mod_pred_i4$fit_i_all
mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "income")] <- mod_pred_i4$fit_i_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "income")] <- mod_pred_i4$fit_i_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "income")] <- mod_pred_i4$fit_i_sta

mod_pred3$fit[which(mod_pred3$date_period == "all" & mod_pred3$type == "npi")] <- mod_pred_n4$fit_n_all
mod_pred3$fit[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "npi")] <- mod_pred_n4$fit_n_pan
mod_pred3$fit[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "npi")] <- mod_pred_n4$fit_n_reb
mod_pred3$fit[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "npi")] <- mod_pred_n4$fit_n_sta

mod_pred3$fit.se <- as.numeric(NA)
mod_pred3$fit.se[which(mod_pred3$date_period == "all" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit.se_pd_all
mod_pred3$fit.se[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit.se_pd_pan
mod_pred3$fit.se[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit.se_pd_reb
mod_pred3$fit.se[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "popdens")] <- mod_pred_pd4$fit.se_pd_sta

mod_pred3$fit.se[which(mod_pred3$date_period == "all" & mod_pred3$type == "land")] <- mod_pred_l4$fit.se_l_all
mod_pred3$fit.se[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "land")] <- mod_pred_l4$fit.se_l_pan
mod_pred3$fit.se[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "land")] <- mod_pred_l4$fit.se_l_reb
mod_pred3$fit.se[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "land")] <- mod_pred_l4$fit.se_l_sta

mod_pred3$fit.se[which(mod_pred3$date_period == "all" & mod_pred3$type == "dist")] <- mod_pred_d4$fit.se_d_all
mod_pred3$fit.se[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "dist")] <- mod_pred_d4$fit.se_d_pan
mod_pred3$fit.se[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "dist")] <- mod_pred_d4$fit.se_d_reb
mod_pred3$fit.se[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "dist")] <- mod_pred_d4$fit.se_d_sta

mod_pred3$fit.se[which(mod_pred3$date_period == "all" & mod_pred3$type == "income")] <- mod_pred_i4$fit.se_i_all
mod_pred3$fit.se[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "income")] <- mod_pred_i4$fit.se_i_pan
mod_pred3$fit.se[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "income")] <- mod_pred_i4$fit.se_i_reb
mod_pred3$fit.se[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "income")] <- mod_pred_i4$fit.se_i_sta

mod_pred3$fit.se[which(mod_pred3$date_period == "all" & mod_pred3$type == "npi")] <- mod_pred_n4$fit.se_n_all
mod_pred3$fit.se[which(mod_pred3$date_period == "mid March - mid May" & mod_pred3$type == "npi")] <- mod_pred_n4$fit.se_n_pan
mod_pred3$fit.se[which(mod_pred3$date_period == "mid May - end July" & mod_pred3$type == "npi")] <- mod_pred_n4$fit.se_n_reb
mod_pred3$fit.se[which(mod_pred3$date_period == "beg August - end Sept" & mod_pred3$type == "npi")] <- mod_pred_n4$fit.se_n_sta

mod_pred3$type <- factor(mod_pred3$type, levels=c("popdens","dist","land","income","npi"))
mod_pred3 <- mod_pred3[order(mod_pred3$type,mod_pred3$level,mod_pred3$date_period),]

levels <- c(as.character(levels(data2b$zip_pop_dens_cat_by_city)), as.character(levels(data2b$zip_dist_city_bord_cat_by_city)),
            as.character(levels(data2b$zip_land_area_cat_by_city)), as.character(levels(data2b$zip_median_hh_income_cat_by_city)),
            as.character(levels(data2b$NPI_count)))
mod_pred3$level2 <- factor(ifelse(mod_pred3$level %in% levels[5], "In City",
                                  ifelse(mod_pred3$level %in% levels[c(1,6,10,14)], "Q1",
                                         ifelse(mod_pred3$level %in% levels[c(2,7,11,15)], "Q2",
                                                ifelse(mod_pred3$level %in% levels[c(3,8,12,16)], "Q3", 
                                                       ifelse(mod_pred3$level %in% levels[c(4,9,13,17)], "Q4",
                                                              ifelse(mod_pred3$level %in% levels[c(18)], "0 NPI",
                                                                     ifelse(mod_pred3$level %in% levels[c(19)], "1 NPI",
                                                                            ifelse(mod_pred3$level %in% levels[c(20)], "2 NPI",
                                                                                   ifelse(mod_pred3$level %in% levels[c(21)], "3 NPI","4 NPI"))))))))))


mod_pred3$fit.low <- mod_pred3$fit - mod_pred3$fit.se
mod_pred3$fit.high <- mod_pred3$fit + mod_pred3$fit.se



######
type.labs <- c("Population Density Quartile", "Dist. from City Limit Quartile",
               "Land Area Quartile", "Median Income Quartile","NPIs in Place")
names(type.labs) <- c("popdens","dist","land","income","npi")

mod_pred3q <- droplevels(mod_pred3[which(mod_pred3$date_period != "all"),])


p <- ggplot() +
  geom_point(aes(y=fit, x=level2, col=date_period),data=mod_pred3q, size=3) +
  geom_errorbar(aes(ymin=fit.low, ymax=fit.high, x=level2, col=date_period),data=mod_pred3q, width=0.35) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  facet_wrap(~type, scales="free_x", strip.position = "bottom",labeller = labeller(type = type.labs)) +
  theme_classic() +
  scale_x_discrete("") + 
  scale_y_continuous("Coefficient Estimates") +
  scale_color_manual("Date Period", values=c("#F8766D", "#00BA38", "#619CFF")) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        axis.title=element_text(size=17),
        axis.text.x = element_text(angle = 0, size=13),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(size=17))
pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/resids_city_zc_new_no_all_se.pdf", height=8, width=13)
p
dev.off() 

############## 
############## 
## city-specific pop dens effects pandemic ####
mod_pred <- with(data2_panb,expand.grid(time_num=c(seq(from=min(time_num), to=max(time_num), by=1)),
                                   city=levels(city),zip_pop_dens_cat_by_city=levels(zip_pop_dens_cat_by_city)))
mod_pred$weekday <- as.numeric(factor(weekdays(as.Date(mod_pred$time_num, origin="1970-01-01")), levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")))
mod_pred2 <- mod_pred[which(mod_pred$weekday == 3),]

mod_pan_city_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city*city, data=data2_panb)
mod_pred_pan0 <- cbind(mod_pred2, predict(mod_pan_city_popdens_city_short, mod_pred2, se.fit=TRUE, type="response"))
names(mod_pred_pan0)[5] <- "fit_city_popdens_city"
names(mod_pred_pan0)[6] <- "fit.se_city_popdens_city"
mod_pred_pan0$date <- as.Date(mod_pred_pan0$time_num,origin = "1970-01-01")

mod_pred_pan0$region <- ifelse(mod_pred_pan0$city %in% c("Los_Angeles_CA", "SanFrancisco_CA", "SanJose_CA", "SanDiego_CA"), "West Coast",
                                ifelse(mod_pred_pan0$city %in% c("Austin_TX", "Dallas_TX", "ElPaso_TX", "Houston_TX", "Phoenix_AZ", "SanAntonio_TX"), "South",
                                       ifelse(mod_pred_pan0$city %in% c("Fargo_ND", "Lincoln_NE", "Omaha_NE", "SiouxFalls_SD"), "Midwest",
                                              ifelse(mod_pred_pan0$city %in% c("Chicago_IL", "Columbus_OH", "Detroit_MI"), "Great Lakes",
                                                     ifelse(mod_pred_pan0$city %in% c("Baltimore_MD","NYC_NY", "Philadelphia_PA"), "Northeast", "Southeast")))))
mod_pred_pan0$region <- as.factor(mod_pred_pan0$region)
mod_pred_pan0$region <- factor(mod_pred_pan0$region, levels= levels(mod_pred_pan0$region)[c(1:3,6,4,5)])
mod_pred_pan0$city <- factor(mod_pred_pan0$city, 
                              levels= c(  sort(unique(as.character(mod_pred_pan0$city)[which(mod_pred_pan0$region == levels(mod_pred_pan0$region)[1])])),
                                          sort( unique(as.character(mod_pred_pan0$city)[which(mod_pred_pan0$region == levels(mod_pred_pan0$region)[2])])),
                                          sort( unique(as.character(mod_pred_pan0$city)[which(mod_pred_pan0$region == levels(mod_pred_pan0$region)[3])])),
                                          sort( unique(as.character(mod_pred_pan0$city)[which(mod_pred_pan0$region == levels(mod_pred_pan0$region)[4])])),
                                          sort( unique(as.character(mod_pred_pan0$city)[which(mod_pred_pan0$region == levels(mod_pred_pan0$region)[5])])),
                                          sort( unique(as.character(mod_pred_pan0$city)[which(mod_pred_pan0$region == levels(mod_pred_pan0$region)[6])]))))

mod_pred_pan0$fit_low <- mod_pred_pan0$fit_city_popdens_city - mod_pred_pan0$fit.se_city_popdens_city
mod_pred_pan0$fit_high <- mod_pred_pan0$fit_city_popdens_city + mod_pred_pan0$fit.se_city_popdens_city
#####


p <- ggplot() +
  geom_point(aes(y=fit_city_popdens_city, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_pan0, size=3) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 7.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 14.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 20.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility") +
  scale_colour_manual("Population Density\nQuartile", values=c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)])) +
  theme(plot.margin = unit(c(2,2,4,2), "lines"),
        axis.title=element_text(size=17),
        axis.text.x = element_text(angle = 90, size=13),
        axis.text.y = element_text(size=13),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13))

df=data.frame(y=c(rep(-22,6)),x=c(2,5.6,9,12.5,17.5,23.5),text=c(as.character(levels(mod_pred_pan0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 14)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 17)),
    xmin = 14,      # Vertical position of the textGrob
    xmax = 14,
    ymin = -24,         # Note: The grobs are positioned outside the plot area
    ymax = -24) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-22.5,ymax=-12) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=7.5,xmax=7.5, ymin=-22.5,ymax=-12) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=10.5,xmax=10.5, ymin=-22.5,ymax=-12) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=14.5,xmax=14.5, ymin=-22.5,ymax=-12) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=20.5,xmax=20.5, ymin=-22.5,ymax=-12)


# Code to override clipping
gt1 <- ggplot_gtable(ggplot_build(p2))
gt1$layout$clip[gt1$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_popdens_city_new_pan.pdf", height=8, width=13)
grid.draw(gt1)
dev.off() 
dev.off() 


##
#### se ####
p <- ggplot() +
  geom_point(aes(y=fit_city_popdens_city, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_pan0, size=3) +
  geom_errorbar(aes(ymin=fit_low, ymax=fit_high, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_pan0, width=0.4) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 7.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 14.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 20.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility") +
  scale_colour_manual("Population Density\nQuartile", values=c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)])) +
  theme(plot.margin = unit(c(2,2,4,2), "lines"),
        axis.title=element_text(size=17),
        axis.text.x = element_text(angle = 90, size=13),
        axis.text.y = element_text(size=13),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13))

df=data.frame(y=c(rep(-22,6)),x=c(2,5.6,9,12.5,17.5,23.5),text=c(as.character(levels(mod_pred_pan0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 14)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 17)),
    xmin = 14,      # Vertical position of the textGrob
    xmax = 14,
    ymin = -24,         # Note: The grobs are positioned outside the plot area
    ymax = -24) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-22.5,ymax=-12) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=7.5,xmax=7.5, ymin=-22.5,ymax=-12) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=10.5,xmax=10.5, ymin=-22.5,ymax=-12) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=14.5,xmax=14.5, ymin=-22.5,ymax=-12) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=20.5,xmax=20.5, ymin=-22.5,ymax=-12)


# Code to override clipping
gt1se <- ggplot_gtable(ggplot_build(p2))
gt1se$layout$clip[gt1se$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_popdens_city_new_se_pan.pdf", height=8, width=13)
grid.draw(gt1se)
dev.off() 
dev.off() 

##### short plot #####
mod_pred_pan0_short <- droplevels(mod_pred_pan0[which(mod_pred_pan0$city %in% c("Detroit_MI", "SiouxFalls_SD", "Baltimore_MD", "Los_Angeles_CA", "SanAntonio_TX", "Charlotte_NC")),])

p <- ggplot() +
  geom_point(aes(y=fit_city_popdens_city, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_pan0_short, size=3) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 2.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 5.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  coord_cartesian(ylim=c(-12.2,11.7)) +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility", breaks=c(-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5)) +
  scale_colour_manual("Population Density\nQuartile", values=c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)])) +
  theme(plot.margin = unit(c(2,2,5,2), "lines"),
        axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 90, size=14),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15))

df=data.frame(y=c(rep(-22.3,6)),x=c(0.9,2,3,4,5,6),text=c(as.character(levels(mod_pred_pan0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 16)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 18)),
    xmin = 3,      # Vertical position of the textGrob
    xmax = 3,
    ymin = -24.8,         # Note: The grobs are positioned outside the plot area
    ymax = -24.8) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=1.5,xmax=1.5, ymin=-23.2,ymax=-12.8) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=2.5,xmax=2.5, ymin=-23.2,ymax=-12.8) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-23.2,ymax=-12.8) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=4.5,xmax=4.5, ymin=-23.2,ymax=-12.8) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=5.5,xmax=5.5, ymin=-23.2,ymax=-12.8)


# Code to override clipping
gt1short <- ggplot_gtable(ggplot_build(p2))
gt1short$layout$clip[gt1short$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_popdens_city_new_short_pan.pdf", height=8, width=11)
grid.draw(gt1short)
dev.off() 
dev.off() 

#### se ####
p <- ggplot() +
  geom_point(aes(y=fit_city_popdens_city, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_pan0_short, size=3) +
  geom_errorbar(aes(ymin=fit_low, ymax=fit_high, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_pan0_short, width=0.4) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 2.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 5.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  coord_cartesian(ylim=c(-12.2,11.7)) +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility", breaks=c(-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5)) +
  scale_colour_manual("Population Density\nQuartile", values=c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)])) +
  theme(plot.margin = unit(c(2,2,5,2), "lines"),
        axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 90, size=14),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15))

df=data.frame(y=c(rep(-22.3,6)),x=c(0.9,2,3,4,5,6),text=c(as.character(levels(mod_pred_pan0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 16)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 18)),
    xmin = 3,      # Vertical position of the textGrob
    xmax = 3,
    ymin = -24.8,         # Note: The grobs are positioned outside the plot area
    ymax = -24.8) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=1.5,xmax=1.5, ymin=-23.2,ymax=-12.8) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=2.5,xmax=2.5, ymin=-23.2,ymax=-12.8) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-23.2,ymax=-12.8) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=4.5,xmax=4.5, ymin=-23.2,ymax=-12.8) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=5.5,xmax=5.5, ymin=-23.2,ymax=-12.8)


# Code to override clipping
gt1shortse <- ggplot_gtable(ggplot_build(p2))
gt1shortse$layout$clip[gt1shortse$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_popdens_city_new_se_short_pan.pdf", height=8, width=11)
grid.draw(gt1shortse)
dev.off() 
dev.off() 

##
############## 
## city-specific pop dens effects rebound ####
mod_pred <- with(data2_rebb,expand.grid(time_num=c(seq(from=min(time_num), to=max(time_num), by=1)),
                                        city=levels(city),zip_pop_dens_cat_by_city=levels(zip_pop_dens_cat_by_city)))
mod_pred$weekday <- as.numeric(factor(weekdays(as.Date(mod_pred$time_num, origin="1970-01-01")), levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")))
mod_pred2 <- mod_pred[which(mod_pred$weekday == 3),]

mod_reb_city_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city*city, data=data2_rebb)
mod_pred_reb0 <- cbind(mod_pred2, predict(mod_reb_city_popdens_city_short, mod_pred2, se.fit=TRUE, type="response"))
names(mod_pred_reb0)[5] <- "fit_city_popdens_city"
names(mod_pred_reb0)[6] <- "fit.se_city_popdens_city"
mod_pred_reb0$date <- as.Date(mod_pred_reb0$time_num,origin = "1970-01-01")

mod_pred_reb0$region <- ifelse(mod_pred_reb0$city %in% c("Los_Angeles_CA", "SanFrancisco_CA", "SanJose_CA", "SanDiego_CA"), "West Coast",
                               ifelse(mod_pred_reb0$city %in% c("Austin_TX", "Dallas_TX", "ElPaso_TX", "Houston_TX", "Phoenix_AZ", "SanAntonio_TX"), "South",
                                      ifelse(mod_pred_reb0$city %in% c("Fargo_ND", "Lincoln_NE", "Omaha_NE", "SiouxFalls_SD"), "Midwest",
                                             ifelse(mod_pred_reb0$city %in% c("Chicago_IL", "Columbus_OH", "Detroit_MI"), "Great Lakes",
                                                    ifelse(mod_pred_reb0$city %in% c("Baltimore_MD","NYC_NY", "Philadelphia_PA"), "Northeast", "Southeast")))))
mod_pred_reb0$region <- as.factor(mod_pred_reb0$region)
mod_pred_reb0$region <- factor(mod_pred_reb0$region, levels= levels(mod_pred_reb0$region)[c(1:3,6,4,5)])
mod_pred_reb0$city <- factor(mod_pred_reb0$city, 
                             levels= c(  sort(unique(as.character(mod_pred_reb0$city)[which(mod_pred_reb0$region == levels(mod_pred_reb0$region)[1])])),
                                         sort( unique(as.character(mod_pred_reb0$city)[which(mod_pred_reb0$region == levels(mod_pred_reb0$region)[2])])),
                                         sort( unique(as.character(mod_pred_reb0$city)[which(mod_pred_reb0$region == levels(mod_pred_reb0$region)[3])])),
                                         sort( unique(as.character(mod_pred_reb0$city)[which(mod_pred_reb0$region == levels(mod_pred_reb0$region)[4])])),
                                         sort( unique(as.character(mod_pred_reb0$city)[which(mod_pred_reb0$region == levels(mod_pred_reb0$region)[5])])),
                                         sort( unique(as.character(mod_pred_reb0$city)[which(mod_pred_reb0$region == levels(mod_pred_reb0$region)[6])]))))

mod_pred_reb0$fit_low <- mod_pred_reb0$fit_city_popdens_city - mod_pred_reb0$fit.se_city_popdens_city
mod_pred_reb0$fit_high <- mod_pred_reb0$fit_city_popdens_city + mod_pred_reb0$fit.se_city_popdens_city

#####
p <- ggplot() +
  geom_point(aes(y=fit_city_popdens_city, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_reb0, size=3) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 7.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 14.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 20.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility") +
  scale_colour_manual("Population Density\nQuartile", values=c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)])) +
  theme(plot.margin = unit(c(2,2,4,2), "lines"),
        axis.title=element_text(size=17),
        axis.text.x = element_text(angle = 90, size=13),
        axis.text.y = element_text(size=13),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13))

df=data.frame(y=c(rep(-19.8,6)),x=c(2,5.6,9,12.5,17.5,23.5),text=c(as.character(levels(mod_pred_reb0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 14)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 17)),
    xmin = 14,      # Vertical position of the textGrob
    xmax = 14,
    ymin = -21.5,         # Note: The grobs are positioned outside the plot area
    ymax = -21.5) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-20.3,ymax=-10.5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=7.5,xmax=7.5, ymin=-20.3,ymax=-10.5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=10.5,xmax=10.5, ymin=-20.3,ymax=-10.5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=14.5,xmax=14.5, ymin=-20.3,ymax=-10.5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=20.5,xmax=20.5, ymin=-20.3,ymax=-10.5)


# Code to override clipping
gt2 <- ggplot_gtable(ggplot_build(p2))
gt2$layout$clip[gt2$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_popdens_city_new_reb.pdf", height=8, width=13)
grid.draw(gt2)
dev.off() 
dev.off() 


##
### se #####
p <- ggplot() +
  geom_point(aes(y=fit_city_popdens_city, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_reb0, size=3) +
  geom_errorbar(aes(ymin=fit_low, ymax=fit_high, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_reb0, width=0.4) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 7.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 14.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 20.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility") +
  scale_colour_manual("Population Density\nQuartile", values=c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)])) +
  theme(plot.margin = unit(c(2,2,4,2), "lines"),
        axis.title=element_text(size=17),
        axis.text.x = element_text(angle = 90, size=13),
        axis.text.y = element_text(size=13),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13))

df=data.frame(y=c(rep(-19.8,6)),x=c(2,5.6,9,12.5,17.5,23.5),text=c(as.character(levels(mod_pred_reb0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 14)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 17)),
    xmin = 14,      # Vertical position of the textGrob
    xmax = 14,
    ymin = -21.5,         # Note: The grobs are positioned outside the plot area
    ymax = -21.5) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-20.3,ymax=-10.5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=7.5,xmax=7.5, ymin=-20.3,ymax=-10.5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=10.5,xmax=10.5, ymin=-20.3,ymax=-10.5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=14.5,xmax=14.5, ymin=-20.3,ymax=-10.5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=20.5,xmax=20.5, ymin=-20.3,ymax=-10.5)


# Code to override clipping
gt2se <- ggplot_gtable(ggplot_build(p2))
gt2se$layout$clip[gt2se$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_popdens_city_new_se_reb.pdf", height=8, width=13)
grid.draw(gt2se)
dev.off() 
dev.off() 


##### short plot #####
mod_pred_reb0_short <- droplevels(mod_pred_reb0[which(mod_pred_reb0$city %in% c("Detroit_MI", "SiouxFalls_SD", "Baltimore_MD", "Los_Angeles_CA", "SanAntonio_TX", "Charlotte_NC")),])

p <- ggplot() +
  geom_point(aes(y=fit_city_popdens_city, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_reb0_short, size=3) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 2.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 5.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  coord_cartesian(ylim=c(-8.5,9)) +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility", breaks=c(-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5)) +
  scale_colour_manual("Population Density\nQuartile", values=c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)])) +
  theme(plot.margin = unit(c(2,2,5,2), "lines"),
        axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 90, size=14),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15))

df=data.frame(y=c(rep(-15.8,6)),x=c(0.9,2,3,4,5,6),text=c(as.character(levels(mod_pred_reb0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 16)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 18)),
    xmin = 3,      # Vertical position of the textGrob
    xmax = 3,
    ymin = -17.3,         # Note: The grobs are positioned outside the plot area
    ymax = -17.3) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=1.5,xmax=1.5, ymin=-16.2,ymax=-9.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=2.5,xmax=2.5, ymin=-16.2,ymax=-9.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-16.2,ymax=-9.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=4.5,xmax=4.5, ymin=-16.2,ymax=-9.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=5.5,xmax=5.5, ymin=-16.2,ymax=-9.2)


# Code to override clipping
gt2short <- ggplot_gtable(ggplot_build(p2))
gt2short$layout$clip[gt2short$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_popdens_city_new_short_reb.pdf", height=8, width=11)
grid.draw(gt2short)
dev.off() 
dev.off() 


##
## se ####

p <- ggplot() +
  geom_point(aes(y=fit_city_popdens_city, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_reb0_short, size=3) +
  geom_errorbar(aes(ymin=fit_low, ymax=fit_high, x=city, col=zip_pop_dens_cat_by_city),data=mod_pred_reb0_short, width=0.4) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 2.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 5.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  coord_cartesian(ylim=c(-8.5,9)) +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility", breaks=c(-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5)) +
  scale_colour_manual("Population Density\nQuartile", values=c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)])) +
  theme(plot.margin = unit(c(2,2,5,2), "lines"),
        axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 90, size=14),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15))

df=data.frame(y=c(rep(-15.8,6)),x=c(0.9,2,3,4,5,6),text=c(as.character(levels(mod_pred_reb0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 16)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 18)),
    xmin = 3,      # Vertical position of the textGrob
    xmax = 3,
    ymin = -17.3,         # Note: The grobs are positioned outside the plot area
    ymax = -17.3) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=1.5,xmax=1.5, ymin=-16.2,ymax=-9.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=2.5,xmax=2.5, ymin=-16.2,ymax=-9.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-16.2,ymax=-9.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=4.5,xmax=4.5, ymin=-16.2,ymax=-9.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=5.5,xmax=5.5, ymin=-16.2,ymax=-9.2)


# Code to override clipping
gt2shortse <- ggplot_gtable(ggplot_build(p2))
gt2shortse$layout$clip[gt2shortse$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_popdens_city_new_se_short_reb.pdf", height=8, width=11)
grid.draw(gt2shortse)
dev.off() 
dev.off() 


############## 
############## 
## city-specific dist effects stable ####
mod_pred <- with(data2_stab,expand.grid(time_num=c(seq(from=min(time_num), to=max(time_num), by=1)),
                                        city=levels(city),zip_pop_dens_cat_by_city=levels(zip_pop_dens_cat_by_city)))
mod_pred$weekday <- as.numeric(factor(weekdays(as.Date(mod_pred$time_num, origin="1970-01-01")), levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")))
mod_pred2 <- mod_pred[which(mod_pred$weekday == 3),]

mod_sta_city_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city*city, data=data2_stab)
mod_pred_sta0 <- cbind(mod_pred2, predict(mod_sta_city_popdens_city_short, mod_pred2, se.fit=TRUE, type="response"))
names(mod_pred_sta0)[5] <- "fit_city_popdens_city"
names(mod_pred_sta0)[6] <- "fit.se_city_popdens_city"
mod_pred_sta0$date <- as.Date(mod_pred_sta0$time_num,origin = "1970-01-01")

mod_pred_sta0$region <- ifelse(mod_pred_sta0$city %in% c("Los_Angeles_CA", "SanFrancisco_CA", "SanJose_CA", "SanDiego_CA"), "West Coast",
                               ifelse(mod_pred_sta0$city %in% c("Austin_TX", "Dallas_TX", "ElPaso_TX", "Houston_TX", "Phoenix_AZ", "SanAntonio_TX"), "South",
                                      ifelse(mod_pred_sta0$city %in% c("Fargo_ND", "Lincoln_NE", "Omaha_NE", "SiouxFalls_SD"), "Midwest",
                                             ifelse(mod_pred_sta0$city %in% c("Chicago_IL", "Columbus_OH", "Detroit_MI"), "Great Lakes",
                                                    ifelse(mod_pred_sta0$city %in% c("Baltimore_MD","NYC_NY", "Philadelphia_PA"), "Northeast", "Southeast")))))
mod_pred_sta0$region <- as.factor(mod_pred_sta0$region)
mod_pred_sta0$region <- factor(mod_pred_sta0$region, levels= levels(mod_pred_sta0$region)[c(1:3,6,4,5)])
mod_pred_sta0$city <- factor(mod_pred_sta0$city, 
                             levels= c(  sort(unique(as.character(mod_pred_sta0$city)[which(mod_pred_sta0$region == levels(mod_pred_sta0$region)[1])])),
                                         sort( unique(as.character(mod_pred_sta0$city)[which(mod_pred_sta0$region == levels(mod_pred_sta0$region)[2])])),
                                         sort( unique(as.character(mod_pred_sta0$city)[which(mod_pred_sta0$region == levels(mod_pred_sta0$region)[3])])),
                                         sort( unique(as.character(mod_pred_sta0$city)[which(mod_pred_sta0$region == levels(mod_pred_sta0$region)[4])])),
                                         sort( unique(as.character(mod_pred_sta0$city)[which(mod_pred_sta0$region == levels(mod_pred_sta0$region)[5])])),
                                         sort( unique(as.character(mod_pred_sta0$city)[which(mod_pred_sta0$region == levels(mod_pred_sta0$region)[6])]))))

mod_pred_sta0$fit_low <- mod_pred_sta0$fit_city_popdens_city - mod_pred_sta0$fit.se_city_popdens_city
mod_pred_sta0$fit_high <- mod_pred_sta0$fit_city_popdens_city + mod_pred_sta0$fit.se_city_popdens_city

# mod_sta_city_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city*city, data=data2_stab)

# mod_city_popdens_city_short <- glm(resid_city ~ zip_pop_dens_cat_by_city*city, data=data2b)

mod_pred <- with(data2_stab,expand.grid(time_num=c(seq(from=min(time_num), to=max(time_num), by=1)),
                                    city=levels(city),zip_dist_city_bord_cat_by_city=levels(zip_dist_city_bord_cat_by_city)))
mod_pred$weekday <- as.numeric(factor(weekdays(as.Date(mod_pred$time_num, origin="1970-01-01")), levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")))
mod_pred2 <- mod_pred[which(mod_pred$weekday == 3),]

mod_sta_city_dist_city_short <- glm(resid_city ~ zip_dist_city_bord_cat_by_city*city, data=data2_stab)
mod_pred_all0 <- cbind(mod_pred2, predict(mod_sta_city_dist_city_short, mod_pred2, se.fit=TRUE, type="response"))
names(mod_pred_all0)[5] <- "fit_city_dist_city"
names(mod_pred_all0)[6] <- "fit.se_city_dist_city"

mod_pred_all0$date <- as.Date(mod_pred_all0$time_num,origin = "1970-01-01")

mod_pred_all0$region <- ifelse(mod_pred_all0$city %in% c("Los_Angeles_CA", "SanFrancisco_CA", "SanJose_CA", "SanDiego_CA"), "West Coast",
                               ifelse(mod_pred_all0$city %in% c("Austin_TX", "Dallas_TX", "ElPaso_TX", "Houston_TX", "Phoenix_AZ", "SanAntonio_TX"), "South",
                                      ifelse(mod_pred_all0$city %in% c("Fargo_ND", "Lincoln_NE", "Omaha_NE", "SiouxFalls_SD"), "Midwest",
                                             ifelse(mod_pred_all0$city %in% c("Chicago_IL", "Columbus_OH", "Detroit_MI"), "Great Lakes",
                                                    ifelse(mod_pred_all0$city %in% c("Baltimore_MD","NYC_NY", "Philadelphia_PA"), "Northeast", "Southeast")))))
mod_pred_all0$region <- as.factor(mod_pred_all0$region)
mod_pred_all0$region <- factor(mod_pred_all0$region, levels= levels(mod_pred_all0$region)[c(1:3,6,4,5)])
mod_pred_all0$city <- factor(mod_pred_all0$city, 
                             levels= c(  sort(unique(as.character(mod_pred_all0$city)[which(mod_pred_all0$region == levels(mod_pred_all0$region)[1])])),
                                         sort( unique(as.character(mod_pred_all0$city)[which(mod_pred_all0$region == levels(mod_pred_all0$region)[2])])),
                                         sort( unique(as.character(mod_pred_all0$city)[which(mod_pred_all0$region == levels(mod_pred_all0$region)[3])])),
                                         sort( unique(as.character(mod_pred_all0$city)[which(mod_pred_all0$region == levels(mod_pred_all0$region)[4])])),
                                         sort( unique(as.character(mod_pred_all0$city)[which(mod_pred_all0$region == levels(mod_pred_all0$region)[5])])),
                                         sort( unique(as.character(mod_pred_all0$city)[which(mod_pred_all0$region == levels(mod_pred_all0$region)[6])]))))

mod_pred_all0$fit_low <- mod_pred_all0$fit_city_dist_city - mod_pred_all0$fit.se_city_dist_city
mod_pred_all0$fit_high <- mod_pred_all0$fit_city_dist_city + mod_pred_all0$fit.se_city_dist_city

#####
p <- ggplot() +
  geom_point(aes(y=fit_city_dist_city, x=city, col=zip_dist_city_bord_cat_by_city),data=mod_pred_all0, size=3) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 7.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 14.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 20.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility", breaks=c(-5,-2.5,0,2.5,5,7.5,10)) +
  scale_colour_manual("Dist from City\nQuartile", values=c("grey",c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)]))) +
  theme(plot.margin = unit(c(2,2,4,2), "lines"),
        axis.title=element_text(size=17),
        axis.text.x = element_text(angle = 90, size=13),
        axis.text.y = element_text(size=13),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13))

df=data.frame(y=c(rep(-10.5,6)),x=c(2,5.6,9,12.5,17.5,23.5),text=c(as.character(levels(mod_pred_all0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 14)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 17)),
    xmin = 14,      # Vertical position of the textGrob
    xmax = 14,
    ymin = -12,         # Note: The grobs are positioned outside the plot area
    ymax = -12) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-11,ymax=-5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=7.5,xmax=7.5, ymin=-11,ymax=-5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=10.5,xmax=10.5, ymin=-11,ymax=-5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=14.5,xmax=14.5, ymin=-11,ymax=-5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=20.5,xmax=20.5, ymin=-11,ymax=-5)


# Code to override clipping
gt3 <- ggplot_gtable(ggplot_build(p2))
gt3$layout$clip[gt3$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_dist_city_new_sta.pdf", height=8, width=13)
grid.draw(gt3)
dev.off() 
dev.off() 


##
###
##### se #####
p <- ggplot() +
  geom_point(aes(y=fit_city_dist_city, x=city, col=zip_dist_city_bord_cat_by_city),data=mod_pred_all0, size=3) +
  geom_errorbar(aes(ymin=fit_low, ymax=fit_high, x=city, col=zip_dist_city_bord_cat_by_city),data=mod_pred_all0, width=0.4) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 7.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 14.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 20.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility", breaks=c(-5,-2.5,0,2.5,5,7.5,10)) +
  scale_colour_manual("Dist from City\nQuartile", values=c("grey",c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)]))) +
  theme(plot.margin = unit(c(2,2,4,2), "lines"),
        axis.title=element_text(size=17),
        axis.text.x = element_text(angle = 90, size=13),
        axis.text.y = element_text(size=13),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13))

df=data.frame(y=c(rep(-10.5,6)),x=c(2,5.6,9,12.5,17.5,23.5),text=c(as.character(levels(mod_pred_all0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 14)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 17)),
    xmin = 14,      # Vertical position of the textGrob
    xmax = 14,
    ymin = -12,         # Note: The grobs are positioned outside the plot area
    ymax = -12) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-11,ymax=-5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=7.5,xmax=7.5, ymin=-11,ymax=-5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=10.5,xmax=10.5, ymin=-11,ymax=-5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=14.5,xmax=14.5, ymin=-11,ymax=-5) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=20.5,xmax=20.5, ymin=-11,ymax=-5)


# Code to override clipping
gt3se <- ggplot_gtable(ggplot_build(p2))
gt3se$layout$clip[gt3se$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_dist_city_new_se_sta.pdf", height=8, width=13)
grid.draw(gt3se)
dev.off() 
dev.off() 

##### short plot #####
mod_pred_sta0_short <- droplevels(mod_pred_all0[which(mod_pred_all0$city %in% c("Detroit_MI", "SiouxFalls_SD", "Baltimore_MD", "Los_Angeles_CA", "SanAntonio_TX", "Charlotte_NC")),])

p <- ggplot() +
  geom_point(aes(y=fit_city_dist_city, x=city, col=zip_dist_city_bord_cat_by_city),data=mod_pred_sta0_short, size=3) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 2.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 5.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  coord_cartesian(ylim=c(-5,5)) +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility", breaks=c(-5,-2.5,0,2.5,5)) +
  scale_colour_manual("Dist from City\nQuartile", values=c("grey",c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)]))) +
  theme(plot.margin = unit(c(2,2,5,2), "lines"),
        axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 90, size=14),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15))

df=data.frame(y=c(rep(-9,6)),x=c(0.9,2,3,4,5,6),text=c(as.character(levels(mod_pred_all0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 16)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 18)),
    xmin = 3,      # Vertical position of the textGrob
    xmax = 3,
    ymin = -10.3,         # Note: The grobs are positioned outside the plot area
    ymax = -10.3) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=1.5,xmax=1.5, ymin=-9.3,ymax=-5.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=2.5,xmax=2.5, ymin=-9.3,ymax=-5.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-9.3,ymax=-5.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=4.5,xmax=4.5, ymin=-9.3,ymax=-5.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=5.5,xmax=5.5, ymin=-9.3,ymax=-5.2)


# Code to override clipping
gt3short <- ggplot_gtable(ggplot_build(p2))
gt3short$layout$clip[gt3short$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_dist_city_new_short_sta.pdf", height=8, width=11)
grid.draw(gt3short)
dev.off() 
dev.off() 


##### se  #####
mod_pred_sta0_short <- droplevels(mod_pred_all0[which(mod_pred_all0$city %in% c("Detroit_MI", "SiouxFalls_SD", "Baltimore_MD", "Los_Angeles_CA", "SanAntonio_TX", "Charlotte_NC")),])

p <- ggplot() +
  geom_point(aes(y=fit_city_dist_city, x=city, col=zip_dist_city_bord_cat_by_city),data=mod_pred_sta0_short, size=3) +
  geom_errorbar(aes(ymin=fit_low, ymax=fit_high, x=city, col=zip_dist_city_bord_cat_by_city),data=mod_pred_sta0_short, width=0.4) +
  geom_hline(yintercept = 0, lwd=0.25, linetype=2) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 2.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 3.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color="gray25") +
  geom_vline(xintercept = 5.5, linetype = "dashed", color="gray25") +
  theme_classic() +
  coord_cartesian(ylim=c(-5,5)) +
  scale_x_discrete("") + 
  scale_y_continuous("Predicted Residuals from City-Level Mobility", breaks=c(-5,-2.5,0,2.5,5)) +
  scale_colour_manual("Dist from City\nQuartile", values=c("grey",c(brewer.pal(name="Reds", n=9)[c(2,4,6,8)]))) +
  theme(plot.margin = unit(c(2,2,5,2), "lines"),
        axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 90, size=14),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15))

df=data.frame(y=c(rep(-9,6)),x=c(0.9,2,3,4,5,6),text=c(as.character(levels(mod_pred_all0$region))))

p2 <- p
for (i in 1:length(df$text))  {
  p2 <- p2 + 
    annotation_custom(
      grob = textGrob(label = df$text[i], gp = gpar(fontsize = 16)),
      xmin = df$x[i],      # Vertical position of the textGrob
      xmax = df$x[i],
      ymin = df$y[i],         # Note: The grobs are positioned outside the plot area
      ymax = df$y[i]) 
}
p2 <- p2 + 
  annotation_custom(
    grob = textGrob(label = "City by Region", gp = gpar(fontsize = 18)),
    xmin = 3,      # Vertical position of the textGrob
    xmax = 3,
    ymin = -10.3,         # Note: The grobs are positioned outside the plot area
    ymax = -10.3) + 
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=1.5,xmax=1.5, ymin=-9.3,ymax=-5.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=2.5,xmax=2.5, ymin=-9.3,ymax=-5.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=3.5,xmax=3.5, ymin=-9.3,ymax=-5.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=4.5,xmax=4.5, ymin=-9.3,ymax=-5.2) +
  annotation_custom(grob = linesGrob(gp=gpar(lty=2, lwd=1.5)),xmin=5.5,xmax=5.5, ymin=-9.3,ymax=-5.2)


# Code to override clipping
gt3shortse <- ggplot_gtable(ggplot_build(p2))
gt3shortse$layout$clip[gt3shortse$layout$name == "panel"] <- "off"

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/gam_city_dist_city_new_se_short_sta.pdf", height=8, width=11)
grid.draw(gt3shortse)
dev.off() 
dev.off() 

############## 
############## 
#### plot together ####
# gt1, gt2, gt3 -> all, no se
# gt1se, gt2se, gt3se -> all, se
# gt1short, gt2short, gt3short -> short, no se
# gt1shortse, gt2shortse, gt3shortse -> short, se

lay <- rbind(c(1,1,2,2),
             c(4,3,3,4))

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/game_times_combined.pdf", height=20, width=28)
grid.arrange(arrangeGrob(gt1,top=grid::textGrob("a", x = 0.05, hjust = 0,gp=gpar(fontsize=42))), 
             arrangeGrob(gt2,top=grid::textGrob("b", x = 0.05, hjust = 0,gp=gpar(fontsize=42))),
             arrangeGrob(gt3,top=grid::textGrob("c", x = 0.05, hjust = 0.5,gp=gpar(fontsize=42))),
             layout_matrix = lay)
dev.off() 

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/game_times_combined_se.pdf", height=20, width=28)
grid.arrange(arrangeGrob(gt1se,top=grid::textGrob("a", x = 0.05, hjust = 0,gp=gpar(fontsize=42))), 
             arrangeGrob(gt2se,top=grid::textGrob("b", x = 0.05, hjust = 0,gp=gpar(fontsize=42))),
             arrangeGrob(gt3se,top=grid::textGrob("c", x = 0.05, hjust = 0.5,gp=gpar(fontsize=42))),
             layout_matrix = lay)
dev.off() 

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/game_times_combined_short.pdf", height=18, width=26)
grid.arrange(arrangeGrob(gt1short,top=grid::textGrob("a", x = 0.05, hjust = 0,gp=gpar(fontsize=42))), 
             arrangeGrob(gt2short,top=grid::textGrob("b", x = 0.05, hjust = 0,gp=gpar(fontsize=42))),
             arrangeGrob(gt3short,top=grid::textGrob("c", x = 0.05, hjust = 0.5,gp=gpar(fontsize=42))),
             layout_matrix = lay)
dev.off() 

pdf("/home/kschaber/verizon_graphs2/zip/daily/gam/1.17.23/game_times_combined_short_se.pdf", height=18, width=26)
grid.arrange(arrangeGrob(gt1shortse,top=grid::textGrob("a", x = 0.05, hjust = 0,gp=gpar(fontsize=42))), 
             arrangeGrob(gt2shortse,top=grid::textGrob("b", x = 0.05, hjust = 0,gp=gpar(fontsize=42))),
             arrangeGrob(gt3shortse,top=grid::textGrob("c", x = 0.05, hjust = 0.5,gp=gpar(fontsize=42))),
             layout_matrix = lay)
dev.off() 

############## 
############## 
############## 
############## 
############## 