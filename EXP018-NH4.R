
# EXP18-018 NH4 data

library(plater)

setwd("~/Google Drive/Data & reports/R/EXP18-018/NH4/")

DataPlate = read_plate("EXP18-018_BsCs.csv", well_ids_column = "Wells")
TmtPlate = read_plate("EXP18-018_BsCs_key.csv", well_ids_column = "Wells")

data = merge(DataPlate, TmtPlate, by = "Wells")
colnames(data)[2] <- "OD"
colnames(data)[3] <- "ID"


blank = data[grep("blank", data$ID), ]
# av blank = 0.07

# new data set w/o blanks
keepties = data[-grep("blank", data$ID), ]

# subract blank from all and put data in new column
keepties$corrOD = with(keepties, OD - 0.07)

# read in treatment details. 
Tmts = read.table('EXP18-018_treatments.txt', header=T, sep='\t') # read in data. 
# add to plate data via merge
fulldata = merge(keepties, Tmts, by = "ID")
# set factor order for plotting later
fulldata$Treatment <- factor(fulldata$Treatment, levels = unique(fulldata$Treatment)) # set order for plotting later

data = fulldata

# subset out standards 
stan = data[grep("Standard", data$Treatment),]

# remove these from dataset now
data = data[-grep("Standard", data$Treatment),]

#check cols are numeric
sapply(stan, class)

# convert column with um conc of NH4 into numeric. 
stan$ID = as.numeric(as.character(stan$ID))

## getting standard curve for OD to NH4

# comput simple linear models from standards
# "mg_nitrogen as modeled by area under curve"
lm.NH4 <- lm(ID ~ corrOD, data=stan)

# check std curve stats: (ie R2 val)
summary(lm.NH4)

# now use the predict function to apply this lm to the df which now
# has standards removed. Will make a new column in 'data' called um_Nh4 in which predicted
# nh4 will be put
data$uM_NH4 <- predict(lm.NH4, data)
write.csv(data, "NH4_To_As_Bs.csv")

# gonna rename data for ease when joining datasets

dataSet1 = data
rm(data)
dataSet1 = read.csv("NH4_To_Bs_Cs.csv")
### now repeat for hte other data sets

## ======= Ds Es Fs Gs ================== ##

DataPlate2 = read_plate("EXP18-018_DsEsFsGs.csv", well_ids_column = "Wells")
TmtPlate2 = read_plate("EXP18-018_DsEsFsGs_key.csv", well_ids_column = "Wells")

data2 = merge(DataPlate2, TmtPlate2, by = "Wells")
colnames(data2)[2] <- "OD"
colnames(data2)[3] <- "ID"


blank2 = data2[grep("blank", data2$ID), ]
# av blank = 0.059

# new data set w/o blanks
keepties = data2[-grep("blank", data2$ID), ]

# subract blank from all and put data in new column
keepties$corrOD = with(keepties, OD - 0.059)

# read in treatment details. 
Tmts = read.table('EXP18-018_treatments.txt', header=T, sep='\t') # read in data. 
# add to plate data via merge
fulldata2 = merge(keepties, Tmts, by = "ID")
# set factor order for plotting later
fulldata2$Treatment <- factor(fulldata2$Treatment, levels = unique(fulldata2$Treatment)) # set order for plotting later

data = fulldata2

# subset out standards 
stan = data[grep("Standard", data$Treatment),]

# remove these from dataset now
data = data[-grep("Standard", data$Treatment),]

#check cols are numeric
sapply(stan, class)

# convert column with um conc of NH4 into numeric. 
stan$ID = as.numeric(as.character(stan$ID))

## getting standard curve for OD to NH4

# comput simple linear models from standards
# "mg_nitrogen as modeled by area under curve"
lm.NH4 <- lm(ID ~ corrOD, data=stan)

# check std curve stats: (ie R2 val)
summary(lm.NH4)

# now use the predict function to apply this lm to the df which now
# has standards removed. Will make a new column in 'data' called um_Nh4 in which predicted
# nh4 will be put
data$uM_NH4 <- predict(lm.NH4, data)
# write out for future use in case
write.csv(data, "NH4_DsEsFsGs.csv")

# gonna rename data for ease when joining datasets

dataSet2 = data

# now i clean my global env and will read this back at end when i join the three

## ======= As Hs and Is ================== ##

DataPlate2 = read_plate("EXP18-018_AsHsIs.csv", well_ids_column = "Wells")
TmtPlate2 = read_plate("EXP18-018_AsHsIs_key.csv", well_ids_column = "Wells")

data2 = merge(DataPlate2, TmtPlate2, by = "Wells")
colnames(data2)[2] <- "OD"
colnames(data2)[3] <- "ID"

# find blanks
blank2 = data2[grep("blank", data2$ID), ]
# av blank = 0.057

# new data set w/o blanks
keepties = data2[-grep("blank", data2$ID), ]

# subract blank from all and put data in new column
keepties$corrOD = with(keepties, OD - 0.057)

# read in treatment details. 
Tmts = read.table('EXP18-018_treatments.txt', header=T, sep='\t') # read in data. 
# add to plate data via merge
fulldata2 = merge(keepties, Tmts, by = "ID")
# set factor order for plotting later
fulldata2$Treatment <- factor(fulldata2$Treatment, levels = unique(fulldata2$Treatment)) # set order for plotting later

data = fulldata2

# subset out standards 
stan = data[grep("Standard", data$Treatment),]

# remove these from dataset now
data = data[-grep("Standard", data$Treatment),]

#check cols are numeric
sapply(stan, class)

# convert column with um conc of NH4 into numeric. 
stan$ID = as.numeric(as.character(stan$ID))

## getting standard curve for OD to NH4

# comput simple linear models from standards
# "mg_nitrogen as modeled by area under curve"
lm.NH4 <- lm(ID ~ corrOD, data=stan)

# check std curve stats: (ie R2 val)
summary(lm.NH4)

# now use the predict function to apply this lm to the df which now
# has standards removed. Will make a new column in 'data' called um_Nh4 in which predicted
# nh4 will be put
data$uM_NH4 <- predict(lm.NH4, data)
# write out for future use in case
write.csv(data, "NH4_As_Hs_Is.csv")

# gonna rename data for ease when joining datasets

dataSet3 = data

### now read in other data sets so they can all be joined into one
# an extra column was added when i read back in those csv
# cant be bohtered figuring out why now, so will just cut then out. 

dataSet1 = read.csv("NH4_To_Bs_Cs.csv")

dataSet2 = read.csv("NH4_DsEsFsGs.csv")

# now use rbind to join all data together

dataSet1 = dataSet1[,2:7]
dataSet2 = dataSet2[,2:7]

allData = rbind(dataSet1, dataSet2, dataSet3)

# now it's all together, correct for dilution factor 
# which is 400
# at same time i will convert from um to mM

allData$mM_NH4_corr = with (allData, (uM_NH4 * 400)/1000)

# run setFactorOrder.R

levels(allData$Treatment)


allData[["Treatment"]] <- setFactorOrder(allData[["Treatment"]], c("TimeZero","NegativeControl", "Untreated_24hours", "Lars_24hours", 
                                                                   "Untreated_24hours_refed", "Lars_24hours_refed", 
                                                                   "Untreated_48hours", "Lars_48hours", 
                                               "Untreated_48hours_refed", "Lars_48hours_refed"))

# ========= to plotting ================

library(ggplot2)


## =============== Plotting as box plots instead ===========================

PrettyLittleBox = ggplot(allData, aes(Treatment, mM_NH4_corr, fill = factor(Treatment))) + # define variables
  geom_boxplot(size=1) + # change line width, 
  scale_fill_manual(name = "", values = c('peru','grey52', 'olivedrab1', 'coral', 'olivedrab2', 'coral3', 
                                          'springgreen3',  'orangered', 'forestgreen', 'darkorange2', 'olivedrab')) +
  theme_bw() +  # theme_bw() to make background white
  labs(x="", y="mM NH4 per vial") +
  theme(axis.title.x = element_blank(),
        text = element_text(size=19, color = "black"),
        axis.text = element_text(color="black"),
        legend.key.size=unit(1.5, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) 

PrettyLittleBox

myLabs = c("Time Zero", "Unfed", "24 hours\n No LARS", "24 hours\n Plus LARS",
           "24 hours refed\n No LARS", "24 hours refed\n Plus LARS",
           "48 hours\n No LARS", "48 hours\n Plus LARS",
           "48 hours refed\n No LARS", "48 hours refed\n Plus LARS")


PrettyLittleBox + scale_x_discrete(labels= myLabs) + guides(fill=FALSE) #+ # turn off legend and change labels on x axis

PrettyLittleBox  + guides(fill=FALSE) #+ # turn off legend and change labels on x axis


## =============== stats ===========================

res.aov <- aov(mM_NH4_corr ~ Treatment, data = allData)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)

# remove neg control & time zero

avTrim <- allData[- grep(paste("NegativeControl", "TimeZero", sep="|"), allData$Treatment),]

#do shapiro wilk to test for normality
tapply(avTrim$mM_NH4_corr, INDEX=avTrim$Treatment, FUN=shapiro.test)
# can do anova


res.aov <- aov(mM_NH4_corr ~ Treatment, data = avTrim)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)


## resources used:
# for preducting um NH4 based on OD:
# https://casoilresource.lawr.ucdavis.edu/software/r-advanced-statistical-package/using-lm-and-predict-apply-standard-curve-analytical-data/


