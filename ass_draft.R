# ACTL4001 Assignment

data_dir <- "C:/Study/ACTL4001/Assignment/Data"

#### Setup ####
# Packages
packages <- c("openxlsx", "data.table", "ggplot2", "zoo", "corrplot",
              "gridExtra", "fitdistrplus")
install.packages(packages)

library("openxlsx")
library("data.table")
library("ggplot2")
library("zoo")
library("corrplot")
library("gridExtra")
library("fitdistrplus")
# Parameters defined below
long_term_inflation <- 0.020


#### Data read ####
hazard <- read.xlsx(file.path(data_dir, "2023-student-research-hazard-event-data.xlsx"),
                    sheet = "Hazard Data",
                    rows = 13:3379, cols = 2:9)

dem <- read.xlsx(file.path(data_dir, "2023-student-research-eco-dem-data.xlsx"),
                 sheet = "Demographic-Economic",
                 rows = 8:51, cols = 2:8)

eco <- read.xlsx(file.path(data_dir, "2023-student-research-eco-dem-data.xlsx"),
                 sheet = "Inflation-Interest",
                 rows = 8:68, cols = 2:6)

raf <- read.xlsx(file.path(data_dir, "2023-student-research-emissions.xlsx"),
                 sheet = "Model",
                 rows = 20:34, cols = 20:23)

hazard <- as.data.table(hazard)
dem <- as.data.table(dem)
eco <- as.data.table(eco)
raf <- as.data.table(raf)

#### Data clean ####
hazard_edit <- copy(hazard)
hazard_edit[,Region := as.factor(Region)]

table(hazard_edit$Hazard.Event)
hazard_edit[Hazard.Event == "Severe Storm/Thunder Storm - Wind", Hazard.Event := "Severe Storm/Thunder Storm/Wind"]


eco_edit <- copy(eco) #should use "copy" when duplicating/making a copy of data.tables
#Simple average interpolation for missing/erroneous rates *warning: ugly
eco_edit[Year == 2018,]$`1-yr.risk.free.rate` <- eco_edit[Year == 2018,]$Government.Overnight.Bank.Lending.Rate
eco_edit[Year == 2003,]$Inflation <- mean(c(eco_edit[Year == 2002,]$Inflation, eco_edit[Year == 2004,]$Inflation ))
eco_edit[Year == 1987,]$Government.Overnight.Bank.Lending.Rate <- eco_edit[Year == 2018,]$`1-yr.risk.free.rate`

#Extrapolate for 1960, 1961
eco_edit <- rbind(c("Year" = 1960, "Inflation" = long_term_inflation, eco_edit[Year == 1962][,3:5]),
                  c("Year" = 1961, "Inflation" = long_term_inflation, eco_edit[Year == 1962][,3:5]),
                  eco_edit)

#### Data prep ####
# Inflate payments
eco_edit <- eco_edit[order(Year, decreasing = TRUE),]
eco_edit[, inf_edit := Inflation + 1]
eco_edit[,cumulative_inf := cumprod(inf_edit)]

hazard_edit <- merge(hazard_edit, eco_edit[,.(Year, cumulative_inf)], by = "Year", all.x = TRUE)
hazard_edit[,prop_dam_inf := Property.Damage*cumulative_inf]

# Year and quarter
hazard_edit[, yq := as.yearqtr(paste0(Year,"-",Quarter))]

# Region as factor
hazard_edit[, Region := as.factor(Region)]

# Hazard flags
event_names <- unique(trimws(unlist(strsplit(unique(hazard_edit$Hazard.Event), "/"))))
for (i in 1:length(event_names)) {
  hazard_edit[, paste0("event_",sub(" ", "_",event_names[i])) := grepl(event_names[i], Hazard.Event, ignore.case = TRUE)]
}

#### Basic summaries ####
# Prop_dam_inf by region, by yq
ggplot(hazard_edit, aes(x = yq, y = prop_dam_inf, col = Region)) +
  geom_line() +
  ylim(0,1000000)

ggplot(hazard_edit, aes(x = Year, y = prop_dam_inf, col = Region)) +
  geom_line() +
  ylim(0,1000000)

ggplot(hazard_edit, aes(x = Year, col = Region)) +
  geom_line(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

hazard_edit[,.(avg_dam = mean(prop_dam_inf)), by = "Region"]
for (reg in c("1","2","3","4","5","6")) {
  dam_by_region_plot <- ggplot(hazard_edit[Region == reg,], aes(x = yq, y = prop_dam_inf)) +
    geom_line() +
    ylim(0,1000000)
  print(dam_by_region_plot)
}
for (reg in c("1","2","3","4","5","6")) {
  dam_by_region_plot <- ggplot(hazard_edit[Region == reg,], aes(x = yq, y = avg_dam)) +
    geom_line() +
    ylim(0,1000000)
  print(dam_by_region_plot)
}

# Hazard by region, by yq
ggplot(hazard_edit, aes(x = Hazard.Event, col = Region)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

for (reg in c("1","2","3","4","5","6")) {
  hazard_by_region_plot <- ggplot(hazard_edit[Region == reg,], aes(x = Hazard.Event)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(hazard_by_region_plot)
}
# Region 3 has the most flooding, severe storm/thunderstorm/wind
# Region 2 has the most hail
# Region 1 has the most winter weather

# Correlation
correlation <- cor(hazard_edit[,.(Duration, Fatalities, Injuries, prop_dam_inf)])
corrplot(correlation)

event_index <- grepl("event", colnames(hazard_edit))
corr_hazards <- cor(hazard_edit[,..event_index])
corrplot(corr_hazards)

# Superimposed inflation (demand push/supply pull)
acs_by_freq <- hazard_edit[,.(.N, mean_acs = mean(prop_dam_inf)),by = yq]
corr_acs_by_freq <- cor(acs_by_freq[,.(N, mean_acs)])
corr_acs_by_freq #only 0.024

acs_by_freq_simple <- acs_by_freq[, .(mean_acs_again = mean(mean_acs)), by = N]
acs_by_freq_simple[order(N),]
plot(acs_by_freq_simple) #no clear relationship

# most frequent events by year for each region
# histogram for each event for each region
# quantile analysis

# Prop_dam_inf by region, by hazard
plot_list <- list()
for (haz in event_names) {
  for (ind in c("1","2","3","4","5","6")) {
    reg <- ind
    plot_list[[ind]] <- ggplot(hazard_edit[Region == reg & get(paste0("event_", haz)) == TRUE ,], aes(x = prop_dam_inf)) +
      geom_histogram() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle(paste0("Region ",reg," - ", haz))

  }
  grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], plot_list[[6]], ncol = 3)
}

# Export for excel analysis
write.xlsx(hazard_edit[,.(Region, Hazard.Event, Quarter, Year, Duration, Fatalities, Injuries, prop_dam_inf)],
           file.path(data_dir, "inf_hazard_event_data.xlsx"),
           sheetName = "inf_data")

#### Minor/medium/major or small/medium/large split ####
haz_mod_data <- copy(hazard_edit)
#** Replace with Farah/Waddah definitions

# Placeholder* parameters defined below
# cost_fat <- 500000
# cost_inj <- 50000

# Combined cost (sum_hazard)
# haz_mod_data[, sum_hazard := Fatalities * cost_fat / cumulative_inf + Injuries * cost_inj / cumulative_inf + prop_dam_inf]
haz_mod_data[, sum_hazard := prop_dam_inf]

# Parameters defined below
splits <- c("small", "medium", "large")
small_max <- quantile(haz_mod_data$sum_hazard, 0.8)
med_max <- quantile(haz_mod_data$sum_hazard, 0.9)

# Label data
event_group_fun<- function(x){
  if (x <small_max) {
    return('small')
  } else if (x > small_max  &x <= med_max) {
    return('medium')
  } else {
    return('large')
  }
}

haz_mod_data$event_group <- sapply(haz_mod_data$sum_hazard,event_group_fun)

haz_mod_data$post2005 <- 0
haz_mod_data[Year >= 2005,post2005 := 1]

#### Diagnostic graphs ####
# Graphs of sum_hazard by split, by region
plot_list <- list()
for (reg in c("1","2","3","4","5","6")) {
  plot_list[[1]] <- ggplot(haz_mod_data[Region == reg,], aes(x = sum_hazard)) +
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg))

  for (ind in c(1,2,3)) {
    split <- splits[ind]
    plot_list[[ind + 1]] <- ggplot(haz_mod_data[Region == reg & event_size == split,], aes(x = sum_hazard)) +
      geom_histogram() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle(paste0("Region ",reg," - ", split))
  }

  grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 4)
}

# Graphs of sum_hazard by split, by region, by year
plot_list <- list()
for (reg in c("1","2","3","4","5","6")) {
  plot_list[[1]] <- ggplot(haz_mod_data[Region == reg,], aes(x = Year, y = sum_hazard)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg))

  for (ind in c(1,2,3)) {
    split <- splits[ind]
    plot_list[[ind + 1]] <- ggplot(haz_mod_data[Region == reg & event_size == split,], aes(x = Year, y = sum_hazard)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle(paste0("Region ",reg," - ", split))
  }

  grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 4)
}

# Average sum_hazard by yq
haz_mod_data[, avg_sum_hazard_q := mean(sum_hazard), by = c("yq", "Region")]
haz_mod_data[, avg_sum_hazard_y := mean(sum_hazard), by = c("Year", "Region")]

haz_mod_data[, avg_sum_hazard_qg := mean(sum_hazard), by = c("yq", "Region", "event_size")]
haz_mod_data[, avg_sum_hazard_yg := mean(sum_hazard), by = c("Year", "Region", "event_size")]

# Column graph of avg_sum_hazard by year, by region
plot_list <- list()
for (reg in c("1","2","3","4","5","6")) {
  plot_list[[1]] <- ggplot(haz_mod_data[Region == reg,], aes(x = Year, y = avg_sum_hazard_q)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg))

  for (ind in c(1,2,3)) {
    split <- splits[ind]
    plot_list[[ind + 1]] <- ggplot(haz_mod_data[Region == reg & event_size == split,], aes(x = Year, y = avg_sum_hazard_qg)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle(paste0("Region ",reg," - ", split))
  }

  grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 4)
}

# Below 2 sets of graphs evaluate suitability of gamma/uniform distribution
# Histogram of avg_sum_hazard by year, by region
plot_list <- list()
for (reg in c("1","2","3","4","5","6")) {
  plot_list[[1]] <- ggplot(haz_mod_data[Region == reg,], aes(x = avg_sum_hazard_y)) +
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg))

  for (ind in c(1,2,3)) {
    split <- splits[ind]
    plot_list[[ind + 1]] <- ggplot(haz_mod_data[Region == reg & event_size == split,], aes(x = avg_sum_hazard_yg)) +
      geom_histogram() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle(paste0("Region ",reg," - ", split))
  }

  grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 4)
}

# Histogram of avg_sum_hazard by quarter, by region
plot_list <- list()
for (reg in c("1","2","3","4","5","6")) {
  plot_list[[1]] <- ggplot(haz_mod_data[Region == reg,], aes(x = avg_sum_hazard_q)) +
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg))

  for (ind in c(1,2,3)) {
    split <- splits[ind]
    plot_list[[ind + 1]] <- ggplot(haz_mod_data[Region == reg & event_size == split,], aes(x = avg_sum_hazard_qg)) +
      geom_histogram() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle(paste0("Region ",reg," - ", split))
  }

  grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 4)
}

# Graphs of sum_hazard by split, by region
plot_list <- list()
for (reg in c("1","2","3","4","5","6")) {
  plot_list[[1]] <- ggplot(haz_mod_data[Region == reg,], aes(x = sum_hazard)) +
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg))

  for (ind in c(1,2,3)) {
    split <- splits[ind]
    plot_list[[ind + 1]] <- ggplot(haz_mod_data[Region == reg & event_size == split,], aes(x = sum_hazard)) +
      geom_histogram() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle(paste0("Region ",reg," - ", split))
  }

  grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 4)
}

# Graphs of sum_hazard by split, by region, by year
plot_list <- list()
for (reg in c("1","2","3","4","5","6")) {
  plot_list[[1]] <- ggplot(haz_mod_data[Region == reg,], aes(x = Year, y = sum_hazard)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg))

  for (ind in c(1,2,3)) {
    split <- splits[ind]
    plot_list[[ind + 1]] <- ggplot(haz_mod_data[Region == reg & event_size == split,], aes(x = Year, y = sum_hazard)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle(paste0("Region ",reg," - ", split))
  }

  grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 4)
}


#### Check existing trends in notifications by split ####
# why the ~ 0 claims ~ Year 2000?
# why the jump in claims in the recent 20 years?
## edit to definition of regions implied by reduction in region 1 notifications and sudden hike in other region notifications
plot_list <- list()
for (reg in c("1","2","3","4","5","6")) {
  plot_list[[1]] <- ggplot(haz_mod_data[Region == reg,], aes(x = Year)) +
    geom_bar(stat = "count") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg), " count")

  for (ind in c(1,2,3)) {
    split <- splits[ind]
    plot_list[[ind + 1]] <- ggplot(haz_mod_data[Region == reg & event_size == split,], aes(x = Year)) +
      geom_bar(stat = "count") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle(paste0("Region ",reg," - ", split, " count"))
  }

  grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 4)
}

# take 2020 notifications as is or take average of past ~20 years of notifications as 2020 notifications for SSP model?

#### Predict number of events for each split, by region ####
sd(haz_mod_data[post2005 == FALSE, .N, yq]$N)
# [1] 10.39263
sd(haz_mod_data[post2005 == TRUE, .N, yq]$N) #lots more variation here
# [1] 24.60607
sd(haz_mod_data[, .N, yq]$N)
#[1] 15.89482

# Method 1 - Take average of last 10 years
freq_list <- list()
for (reg in c("1","2","3","4","5","6")) {
  reg_freq_list <- c()
  for (group in splits) {
    split_freq <- mean(haz_mod_data[Region == reg & Year >= 2010 & event_group == group, .N, Year]$N)
    reg_freq_list <- c(reg_freq_list, ifelse(is.na(split_freq),0, split_freq))
  }
  print(reg_freq_list)
  freq_list[[reg]] <- reg_freq_list
}
write.xlsx(reg_freq_list,
           file.path(data_dir, "events_group_freq.xlsx"),
           sheetName = "freq")

# Method 2 - fit distribution
freq.fit.pois <- fitdist(haz_mod_data[, .N, yq]$N, "pois" , method = "mme")
plot(freq.fit.pois) #overdispersed

freq.fit.nb <- fitdist(haz_mod_data[, .N, Year]$N, "nbinom", method = "mme") #'arg' should be one of “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
plot(freq.fit.nb) #better, and mme better than mse

# nb
freq.fit.nb <- fitdist(haz_mod_data[, .N, yq]$N, "nbinom", method = "mme") #'arg' should be one of “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
plot(freq.fit.nb)
freq.fit.nb$aic
freq.fit.nb$bic
prob.0 <- freq.fit.nb$estimate[1]/(freq.fit.nb$estimate[1] + freq.fit.nb$estimate[2])

freq.fit.nb.1 <- fitdist(haz_mod_data[post2005 == TRUE, .N, yq]$N, "nbinom", method = "mme") #'arg' should be one of “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
plot(freq.fit.nb.1)
freq.fit.nb.1$aic
freq.fit.nb.1$bic
prob.1 <- freq.fit.nb.1$estimate[1]/(freq.fit.nb.1$estimate[1] + freq.fit.nb.1$estimate[2])

#exp better for overall and post-2005 experience
freq.fit.exp <- fitdist(haz_mod_data[, .N, yq]$N, "exp", method = "mme") #'arg' should be one of “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
plot(freq.fit.exp)
freq.fit.exp$aic
freq.fit.exp$bic

freq.fit.exp.1 <- fitdist(haz_mod_data[post2005 == TRUE, .N, yq]$N, "exp", method = "mme") #'arg' should be one of “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
plot(freq.fit.exp.1)
freq.fit.exp.1$aic
freq.fit.exp.1$bic



#### Predict acs of events for each split, by region ####
# Not offset by populations of regions bc insufficient information provided
# We still assume a relationship between region and acs in projections.

# count distribution for nil claims
haz_mod_data[sum_hazard == 0, .N, Year] #assume follow a normal distribution
nil.fit.norm <- fitdist(haz_mod_data[sum_hazard == 0, .N, Year]$N, "norm" , method = "mse")
plot(nil.fit.norm)
nil.fit.exp <- fitdist(haz_mod_data[sum_hazard == 0, .N, Year]$N, "exp", method = "mse" )
plot(nil.fit.exp)
nil.fit.poi <- fitdist(haz_mod_data[sum_hazard == 0, .N, Year]$N, "pois", method = "mse" )
plot(nil.fit.poi)


event_names2 <- paste0("event_", sub(" ", "_", event_names))
colSums(haz_mod_data[,..event_names2])
# variables <- c("Duration", "Fatalities", "Injuries", event_names2)
# form <- paste(variables, collapse = '+')

# should we not insure for fogs, landslides? Only 1 event
# Severe storms and thunder storms exactly correlated. Thunder storms removed
# Hurricanes and tropical storms exactly correlated. Tropical storms removed
# Only hurricanes and tropical storms have significantly non-0 correlation with sum_hazard
event_names2_sh <- c(event_names2, "sum_hazard")
correlation <- cor(haz_mod_data[,..event_names2_sh])
corrplot(correlation)


fit.weibull <- fitdist(haz_mod_data[sum_hazard > 0]$sum_hazard, "weibull")


glm_test1 <- glm(formula = sum_hazard ~ Injuries + event_Hurricane,
                 family = Gamma,
                 data = haz_mod_data[sum_hazard > 0,],
                 offset = NULL,
                 control = list(), model = TRUE, method = "glm.fit"
)
summary(glm_test1)

# remove 1989
haz_mod_data_simple <- haz_mod_data[!(Year == 1989 & Hazard.Event == "Hurricane/Tropical Storm"),]

# backwards/forwards step

glm_test2 <- glm(formula = sum_hazard ~ Injuries + event_Hurricane + event_Severe_Storm + event_Flooding + event_Drought,
                 family = Gamma,
                 data = haz_mod_data_simple[sum_hazard > 0,],
                 offset = NULL,
                 control = list(), model = TRUE, method = "glm.fit"
)
summary(glm_test2)

glm_test3 <- glm(formula = sum_hazard ~ Region + Injuries + event_Hurricane + event_Severe_Storm + event_Flooding + event_Drought,
                 family = Gamma,
                 data = haz_mod_data_simple[sum_hazard > 0,],
                 offset = NULL,
                 control = list(), model = TRUE, method = "glm.fit"
)
summary(glm_test3)

glm_test4 <- glm(formula = sum_hazard ~ Injuries + event_Hurricane + event_Severe_Storm + event_Flooding + event_Drought,
                 family = Gamma,
                 data = haz_mod_data_simple[Region == 2 & sum_hazard > 0,],
                 offset = NULL,
                 control = list(), model = TRUE, method = "glm.fit"
)
summary(glm_test4)



