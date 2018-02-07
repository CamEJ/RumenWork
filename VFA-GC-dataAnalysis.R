

# analysing GC vfa data

setwd("~/Google Drive/Data & reports/R")
library(dplyr)
library(ggplot2)

# read in data
# include na.strings argument so spaces are filled with NAs
data = read.table('EXP2018_003_VFA_all.txt', header=T, sep='\t',na.strings=c("","NA"))

# first check only one column contains NAs (column called result)
colnames(data)[colSums(is.na(data)) > 0]

# identify which rows contain NA
row.has.na <- apply(data, 1, function(x){any(is.na(x))})
# see how many there are. 
sum(row.has.na)
# now cut them out
final.filtered <- data[!row.has.na,]

# now check there are no NAs left
colnames(final.filtered)[colSums(is.na(final.filtered)) > 0]

head(final.filtered)

# got no use for internal standards so remove those too:

keepties = final.filtered[-grep("IS", final.filtered$Result), ]

what = read.table('treatmentDeets.txt', header=T, sep='\t')
mydata <- merge(keepties, what, by = "Sample_ID")
# check counts hasnt been converted to a factor

sapply(mydata, class)
# if it has, convert back to number. 
mydata$Counts = as.numeric(as.character(mydata$Counts))

# write this out for easy future use. 
write.csv(mydata, 'EXP2018_002_VFAdataTrimmed.csv')

data = read.table('EXP2018_002_VFAdataTrimmed.csv', header=T, sep=',',na.strings=c("","NA"))


# average bio replicates by grouping by ID
Avs <- mydata %>% 
  group_by(Tmt, Result) %>% # in the order you want them grouped
  summarise(pk_m = mean(Counts), 
            pk_sd = sd(Counts),
            tmt= first(Tmt)) 
# run setFactorOrder.R

#Avs[["Tmt"]] <- setFactorOrder(Avs[["Tmt"]], c("N2:CO2", "H2:CO2", "H2:CO2-Am"))
Avs[["tmt"]] <- setFactorOrder(Avs[["tmt"]], c("N2:CO2", "H2:CO2-Am", "H2:CO2+low_Am","H2:CO2+med_Am","H2:CO2+high_Am"))


# plotting

# create plot
plot <- ggplot(Avs, aes(x=tmt, y=pk_m, fill=Result), show_guide=FALSE) + # define variables to use in plot
  geom_bar(position="dodge",stat="identity") + # to create grouped bars
  # add error bars (using sdev calculated above)
  geom_errorbar(aes(ymin=pk_m-pk_sd, ymax=pk_m+pk_sd), width=.1, position=position_dodge(.9), colour="black") + 
  theme_bw() + labs(y="VFA concentration (mg/L)", fill="Treatment") + # change theme and add labels to Y and legend
  theme(axis.title.x = element_blank(), # remove x axis title
        text = element_text(size=20, color = "black"), # increase text size
        axis.text = element_text(color="black"), # change axis labels from default grey to black
        legend.text = element_text(size=19) # increase size of legend text
  )
# call plot
plot

# make plot more pretty:

# define colours for bars (4 VFAs = four colours)
dark_cols <- c('magenta4', 'gold2', 'olivedrab', 'cyan4')
# define names for re-labelling x axis bar groups (ie better names for each tmt)
myLabs = c("Negative\n control", "Positive\n control", 
           "Low LARS", "Medium LARS", 
           "High LARS")

# add new label names and bar colours to plot 
plot + scale_fill_manual(values = dark_cols) + scale_x_discrete(labels= myLabs)
                 

## ==================== statistical testing ====================== ##

data = read.table('EXP2018_002_VFAdataTrimmed.csv', header=T, sep=',',na.strings=c("","NA"))


# make df's per VFA

library(stringr)

acetate <- subset(data, Result=="Acetate")
acTrim <- acetate[- grep("N2:CO2", acetate$Tmt),]

butyrate <- subset(data, Result=="Butyrate")
propionate <- subset(data, Result=="Propionate")
valerate <- subset(data, Result=="Valerate")

library("ggpubr")
acBox = ggboxplot(acetate, x = "Tmt", y = "Counts", 
          color = "Tmt", palette = c('black', 'olivedrab', 'gold2', 'darkorange2', 'darkred'),
          order = c("N2:CO2", "H2:CO2-Am", "H2:CO2+low_Am","H2:CO2+med_Am","H2:CO2+high_Am"),
          ylab = "mg", xlab = "")

resAc.aov <- aov(Counts ~ Tmt, data = acetate)
# Summary of the analysis
summary(resAc.aov)
TukeyHSD(resAc.aov)


butBox = ggboxplot(butyrate, x = "Tmt", y = "Counts", 
                  color = "Tmt", palette = c('black', 'olivedrab', 'gold2', 'darkorange2', 'darkred'),
                  order = c("N2:CO2", "H2:CO2-Am", "H2:CO2+low_Am","H2:CO2+med_Am","H2:CO2+high_Am"),
                  ylab = "mg", xlab = "")

resBu.aov <- aov(Counts ~ Tmt, data = butyrate)
# Summary of the analysis
summary(resBu.aov)
TukeyHSD(resBu.aov)

propBox = ggboxplot(propionate, x = "Tmt", y = "Counts", 
                  color = "Tmt", palette = c('black', 'olivedrab', 'gold2', 'darkorange2', 'darkred'),
                  order = c("N2:CO2", "H2:CO2-Am", "H2:CO2+low_Am","H2:CO2+med_Am","H2:CO2+high_Am"),
                  ylab = "mg", xlab = "")

resPr.aov <- aov(Counts ~ Tmt, data = propionate)
# Summary of the analysis
summary(resPr.aov)
TukeyHSD(resPr.aov)



valBox = ggboxplot(valerate, x = "Tmt", y = "Counts", 
                  color = "Tmt", palette = c('black', 'olivedrab', 'gold2', 'darkorange2', 'darkred'),
                  order = c("N2:CO2", "H2:CO2-Am", "H2:CO2+low_Am","H2:CO2+med_Am","H2:CO2+high_Am"),
                  ylab = "mg", xlab = "")


resVa.aov <- aov(Counts ~ Tmt, data = valerate)
# Summary of the analysis
summary(resVa.aov)
TukeyHSD(resVa.aov)


