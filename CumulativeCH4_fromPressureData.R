
# ======== calculating cumulative CH4 from pressure data  ==========# 

# read in data and setwd

setwd("~/Google Drive/Data & reports/R")
library(dplyr)
library(ggplot2)

data = read.table('EXP18-2_pressure.txt', header=T, sep='\t')
head(data)
  id vial_ID       tmt mins timePoint pressure
1  1      A1    N2:CO2    0        T0     24.3
2  2      A2    N2:CO2    0        T0     25.0
3  3      A3    N2:CO2    0        T0     28.7
4  4      B1 H2:CO2-Am    0        T0     51.6
5  5      B2 H2:CO2-Am    0        T0     50.3
6  6      B3 H2:CO2-Am    0        T0     55.7

sapply(data, class) # check what class your variables are

# use dplyr to arrange data by vial ID then work out change in pressure for each sample (vial ID)
# between each time point. 

pdif = data %>%
  group_by(vial_ID) %>%
  mutate(volChange = lag(pressure, default = pressure[1]) - pressure)

# if you didnt want to force first data point to zero (default = pressure[1]) then you'd do this:
#pdif = data %>%
#group_by(vial_ID) %>%
#mutate(volChange = lag(pressure, default = 0) - pressure)

# now work out cumulative change in mV eg cumulative change in sample A1 between each time point
# new df will be called cumMV
cumMV = pdif2 %>% 
  group_by(vial_ID) %>% 
  mutate(cumulativeMV=cumsum(volChange))

# now read in table of mvml data for each sample (aka vial).

DmvMl = read.table('exp18-02_mvmlData', header=T, sep='\t')

# add new column with mvml * 4 (because of mol of H2 to make CH4)
DmvMl$mvml4 <- with(DmvMl, mvml *4)

## Merge the two matrices where they will be matched by vial_ID so must be same in each
dat <- merge(cumMV, DmvMl, by = "vial_ID")

## Divide for your result which will be put in col called cumCH4
dat$cumCH4 =  with(dat, cumulativeMV / mvml4)

# ================ now work out rate for exponential phase =============== #


# TO DO 


# ================ now to plotting cumulative CH4 ============ #

Avs <- dat %>% 
  group_by(timePoint, tmt) %>% # in the order you want them grouped
  summarise(pr_m = mean(cumCH4), 
            pr_sd = sd(cumCH4),
            minit = first(mins))

# run setFactorOrder.R
#Avs[["tmt"]] <- setFactorOrder(Avs[["tmt"]], c("N2:CO2", "H2:CO2-Am", "H2:CO2+carb","H2:CO2+KI","H2:CO2+Am"))
Avs[["tmt"]] <- setFactorOrder(Avs[["tmt"]], c("N2:CO2", "H2:CO2-Am", "H2:CO2+low_Am","H2:CO2+med_Am","H2:CO2+high_Am"))

# plotting
library("wesanderson")
names(wes_palettes)


plot <- ggplot(Avs, aes(x=minit, y=pr_m, colour=tmt)) +
  geom_line(size=1) +
  geom_point(size=4, shape=21) +
  ## add error bars
  geom_errorbar(aes(ymin=pr_m-pr_sd, ymax=pr_m+pr_sd), width=.5) +
  #scale_colour_manual(values = wes_palette("Moonrise3")) +
  theme_bw() + labs(y=(expression(paste('Cumulative ', CH[4],' (ml)'))), colour="   Treatment\n", x = "Time (minutes)") +
  theme(text = element_text(size=20, color = "black"))

plot

#if you dont use wesanderson palette, then run ggthemr script to choose your own colours

# Batch1_20Dec17_pressureData.tiff

# ============ old version - just plotting unadjusted pressure data  ============ #

setwd("~/Google Drive/Data & reports/R")
library(dplyr)
library(ggplot2)


# read in data

data = read.table('EXP18-2_pressure.txt', header=T, sep='\t')
sapply(data, class) # see what class your variables are

# group by timepoint and treatment then get average and variance
# of these bio reps to do line plot

# average technical replicates by grouping by ID
Avs <- data %>% 
  group_by(timePoint, tmt) %>% # in the order you want them grouped
  summarise(pr_m = mean(pressure), 
            pr_sd = sd(pressure),
            minit = first(mins))

# run setFactorOrder.R
#Avs[["tmt"]] <- setFactorOrder(Avs[["tmt"]], c("N2:CO2", "H2:CO2-Am", "H2:CO2+carb","H2:CO2+KI","H2:CO2+Am"))
Avs[["tmt"]] <- setFactorOrder(Avs[["tmt"]], c("N2:CO2", "H2:CO2-Am", "H2:CO2+low_Am","H2:CO2+med_Am","H2:CO2+high_Am"))

# plotting
library("wesanderson")
names(wes_palettes)


plot <- ggplot(Avs, aes(x=minit, y=pr_m, colour=tmt)) +
  geom_line(size=1) +
  geom_point(size=4, shape=21) +
  ## add error bars
  geom_errorbar(aes(ymin=pr_m-pr_sd, ymax=pr_m+pr_sd), width=.5) +
  #scale_colour_manual(values = wes_palette("Moonrise3")) +
  theme_bw() + labs(y="Pressure", colour="   Treatment\n", x = "Time (minutes)") +
  theme(text = element_text(size=20, color = "black"))

plot

#if you dont use wesanderson palette, then run ggthemr script to choose your own colours

# Batch1_20Dec17_pressureData.tiff


# resources used:

# https://stackoverflow.com/questions/27275363/r-cumsum-per-group-in-dplyr


