# ACTL4001 Assignment

data_dir <- "C:/Study/ACTL4001/Assignment/Data"

#### Setup ####
packages <- c("openxlsx", "data.table", "ggplot2", "zoo", "corrplot")
install.packages(packages)

library("openxlsx")
library("data.table")
library("ggplot2")
library("zoo")
library("corrplot")
library("gridExtra")

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

hazard <- as.data.table(hazard)
dem <- as.data.table(dem)
eco <- as.data.table(eco)

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

hazard_edit <- merge(hazard, eco_edit[,.(Year, cumulative_inf)], by = "Year", all.x = TRUE)
hazard_edit[,prop_dam_inf := Property.Damage*cumulative_inf]

# Year and quarter
hazard_edit[, yq := as.yearqtr(paste0(Year,"-",Quarter))]

# Hazard flags
event_names <- unique(trimws(unlist(strsplit(unique(hazard_edit$Hazard.Event), "/"))))
for (i in 1:length(event_names)) {
  hazard_edit[, paste0("event_",event_names[i]) := grepl(event_names[i], Hazard.Event, ignore.case = TRUE)]
}


#### Basic summaries ####
ggplot(hazard_edit, aes(x = yq, y = prop_dam_inf, col = Region)) +
  geom_line() +
  ylim(0,1000000)

hazard_edit[,.(avg_dam = mean(prop_dam_inf)), by = "Region"]

for (reg in c("1","2","3","4","5","6")) {
  dam_by_region_plot <- ggplot(hazard_edit[Region == reg,], aes(x = yq, y = prop_dam_inf)) +
    geom_line() +
    ylim(0,1000000)
  print(dam_by_region_plot)
}

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

for (haz in event_names) {
  reg <- 1
  p1 <- single_hazard_by_region_plot <- ggplot(hazard_edit[Region == reg & get(paste0("event_", haz)) == TRUE ,], aes(x = prop_dam_inf)) +
      geom_histogram() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle(paste0("Region ",reg," - ", haz))

  reg <- 2
  p2 <- single_hazard_by_region_plot <- ggplot(hazard_edit[Region == reg & get(paste0("event_", haz)) == TRUE ,], aes(x = prop_dam_inf)) +
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg," - ", haz))

  reg <- 3
  p3 <- single_hazard_by_region_plot <- ggplot(hazard_edit[Region == reg & get(paste0("event_", haz)) == TRUE ,], aes(x = prop_dam_inf)) +
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg," - ", haz))

  reg <- 4
  p4 <- single_hazard_by_region_plot <- ggplot(hazard_edit[Region == reg & get(paste0("event_", haz)) == TRUE ,], aes(x = prop_dam_inf)) +
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg," - ", haz))

  reg <- 5
  p5 <- single_hazard_by_region_plot <- ggplot(hazard_edit[Region == reg & get(paste0("event_", haz)) == TRUE ,], aes(x = prop_dam_inf)) +
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg," - ", haz))

  reg <- 6
  p6 <- single_hazard_by_region_plot <- ggplot(hazard_edit[Region == reg & get(paste0("event_", haz)) == TRUE ,], aes(x = prop_dam_inf)) +
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0("Region ",reg," - ", haz))

  grid.arrange(p1,p2,p3,p4,p5,p6, ncol = 3)
}


check <- "Wind"
hazard_edit[Region == 1 & get(paste0("event_", check)) == TRUE ,]
hazard_edit[Region == 1 & event_Wind == TRUE ,]
