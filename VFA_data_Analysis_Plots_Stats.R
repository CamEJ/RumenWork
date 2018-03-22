

# analysing GC vfa data exp 007

## ================ setting up =========

# set working dir (can also do thru RStudio: Session > Set Working Directory > Choose directory)
setwd("~/Google Drive/Data & reports/R")
# load libraries needed
library(dplyr)
library(ggplot2)

# read in data (where dataset needs to be in wdir unless you give path in file name.)
# include na.strings argument so any spaces in df are filled with NAs
# need to define how columns are separated. ie sep = '\t' means tab delimited

data = read.table('EXP007-vfas2.txt', header=T, sep='\t',na.strings=c("","NA")) 
# where this is what data looks like:
head(data) # output:
# TIME  Width     Counts     Result Sample_ID
# 1 4.90 182382 880.940918    Acetate        E4
# 2 0.00    933          0       <NA>        E4
# 3 3.63  76100 220.367386 Propionate        E4
# 4 2.75  16540          0       <NA>        E4
# 5 2.45   2401          0       <NA>        E4
# 6 3.28  51579 123.227982   Butyrate        E4

# first check only which columns contains NAs (column called result)
colnames(data)[colSums(is.na(data)) > 0]

# identify which rows contain NA (we'll then use this to remove them)
row.has.na <- apply(data, 1, function(x){any(is.na(x))})
# see how many there are. 
sum(row.has.na)
# now cut them out
final.filtered <- data[!row.has.na,]

# now check there are no NAs left (should say character(0) if no more NAs)
colnames(final.filtered)[colSums(is.na(final.filtered)) > 0]

# have a look at first few lines to check that dataset is how it should be
head(final.filtered)

# actually got no use for internal standards so will remove those too using grep:
keepties = final.filtered[-grep("IS", final.filtered$Result), ]

## ============ addding a column of treatments =========

# read in a small table saying what treatment each vial is
# where column one must have same header as sample ID column in keepties data set
what = read.table('EXO07-TMTdeets.txt', header=T, sep='\t')

# where this df looks like this:
head(what)
# Sample_ID           Tmt
# 1        A1        N2:CO2
# 2        A2        N2:CO2
# 3        A3        N2:CO2
# 4        B1 H2:CO2_NoLARS
# 5        B2 H2:CO2_NoLARS
# 6        B3 H2:CO2_NoLARS

# now merge the two datasets, this will create a new dataset which has an extra
# column with treatment of each Sample_ID
mydata <- merge(keepties, what, by = "Sample_ID")

# check counts hasnt been converted to a factor
sapply(mydata, class)
# if it has, convert back to number. 
mydata$Counts = as.numeric(as.character(mydata$Counts))

# write this out for easy future use. 
write.csv(mydata, 'EXP2018_007_VFAdataTrimmed.csv')

# if returning to script here, read data back in
data = read.table('EXP2018_007_VFAdataTrimmed.csv', header=T, sep=',',na.strings=c("","NA"))

## ========= averages of biological replicates ==============

# use dplyr to average bio replicates by grouping by ID
Avs <- mydata %>% 
  group_by(Tmt, Result) %>% # in the order you want them grouped
  summarise(pk_m = mean(Counts), 
            pk_sd = sd(Counts),
            tmt= first(Tmt)) 

# run setFactorOrder.R function (you can find this function in shared R folder)
# then define the order you want your treatments to be displayed
# do this as ggplot default re-orders what it plots alphabetically/numberically

Avs[["tmt"]] <- setFactorOrder(Avs[["tmt"]], c("Inoculum", "N2:CO2", "H2:CO2_NoLARS", "H2:CO2_4mgLARS",
                                               "H2:CO2_5mgLARS", "H2:CO2_6mgLARS", "H2:CO2_8mgLARS"))

## ========== plotting using ggplot2 ==================

# Bar plot of all data

plot <- ggplot(Avs, aes(x=tmt, y=pk_m, fill=Result), show_guide=FALSE) + # define dataset to use & 
                                                                          # variables to use in plot
  geom_bar(position="dodge",stat="identity") + # argument for creating bars; grouped bars position=dodge
  # add error bars (using sdev calculated above)
  geom_errorbar(aes(ymin=pk_m-pk_sd, ymax=pk_m+pk_sd), width=.1, position=position_dodge(.9), colour="black") + 
  theme_bw() + labs(y="VFA concentration (mg/L)", fill="Treatment") + # change theme and add labels to Y and legend
  theme(axis.title.x = element_blank(), # remove x axis title by making blank
        text = element_text(size=20, color = "black"), # increase text size of axes tick marks and axes text
        axis.text = element_text(color="black"), # change axis labels from default grey to black
        legend.text = element_text(size=19) # increase size of legend text
  )
# call plot
plot

# make plot more pretty:

# define colours for bars (4 VFAs = four colours; but I'll put extra just got)
dark_cols <- c('brown', 'black', 'olivedrab', 'gold2', 'darkorange2', 'darkred')
# define names for re-labelling x axis bar groups (ie better names for each tmt)
myLabs = c("Inoculum", "-ve \ncontrol", "No \nLARS", 
           "70 mg/L \nLARS","80 mg/L \nLARS", 
           "100 mg/L \nLARS", 
           "130 mg/L \nLARS")

# add these to plot by
plot +  # re calling plot +
  scale_fill_manual(values = dark_cols) +  # define new colours for the fill
  scale_x_discrete(labels= myLabs) # assign new labels for x axis


## ==================== statistical testing ====================== ##

data = read.table('EXP2018_007_VFAdataTrimmed.csv', header=T, sep=',',na.strings=c("","NA"))


# make separate dataframes per VFA

acetate <- subset(data, Result=="Acetate") # ie use subset() to make a new dataset called 'acetate' filled 
                                            # with all the occurences of Acetate in column called 'Result' 
                                            # in the dataset called 'data' 
butyrate <- subset(data, Result=="Butyrate")
propionate <- subset(data, Result=="Propionate")
valerate <- subset(data, Result=="Valerate")

## ================= stats: ANOVAs =============

# run anova using aov() in format: aov(variable ~ treatment, data = NameOfDataSet)
# 1 acetate
resAc.aov <- aov(Counts ~ Tmt, data = acetate)
# call Summary of the analysis to extract ANOVA results
summary(resAc.aov)

# if p < 0.01 then run a Tukey post hoc to see where differences lie
TukeyHSD(resAc.aov)
# this will print out to console. can copy and paste into text/excel



# 2. butyrate

resBu.aov <- aov(Counts ~ Tmt, data = butyrate)
# Summary of the analysis
summary(resBu.aov)
TukeyHSD(resBu.aov)

# 3 propionate

resPr.aov <- aov(Counts ~ Tmt, data = propionate)
# Summary of the analysis
summary(resPr.aov)
TukeyHSD(resPr.aov)

# 4. valerate

resVa.aov <- aov(Counts ~ Tmt, data = valerate)
# Summary of the analysis
summary(resVa.aov)
TukeyHSD(resVa.aov)

## ======== pretty box plots ===================

# define the order for each treatment to be plotted within each dataset
# probably a more effcient way to do this but this works for me. 
acetate[["Tmt"]] <- setFactorOrder(acetate[["Tmt"]], c("Inoculum", "N2:CO2", "H2:CO2_NoLARS", "H2:CO2_4mgLARS",
                                               "H2:CO2_5mgLARS", "H2:CO2_6mgLARS", "H2:CO2_8mgLARS"))

butyrate[["Tmt"]] <- setFactorOrder(butyrate[["Tmt"]], c("Inoculum", "N2:CO2", "H2:CO2_NoLARS", "H2:CO2_4mgLARS",
                                                       "H2:CO2_5mgLARS", "H2:CO2_6mgLARS", "H2:CO2_8mgLARS"))

propionate[["Tmt"]] <- setFactorOrder(propionate[["Tmt"]], c("Inoculum", "N2:CO2", "H2:CO2_NoLARS", "H2:CO2_4mgLARS",
                                                       "H2:CO2_5mgLARS", "H2:CO2_6mgLARS", "H2:CO2_8mgLARS"))

valerate[["Tmt"]] <- setFactorOrder(valerate[["Tmt"]], c("Inoculum", "N2:CO2", "H2:CO2_NoLARS", "H2:CO2_4mgLARS",
                                                       "H2:CO2_5mgLARS", "H2:CO2_6mgLARS", "H2:CO2_8mgLARS"))

# define the labels you want for your plot. 
myLabs = c("Inoculum", "-ve \ncontrol", "No \nLARS", 
           "70 mg/l \nLARS","80 mg/l \nLARS", 
           "100 mg/l \nLARS", 
           "130 mg/l \nLARS")

# now to plotting:


AcetBox = ggplot(acetate, aes(Tmt, Counts, fill = factor(Tmt))) + # define variables
  geom_boxplot(size=1) + # define that you want box plot; size argument changes line width, 
  scale_fill_manual(name = "", 
                    values = c('chocolate4', 'black', 'olivedrab', 'gold2', 'darkorange2', 'red', 'darkred')) +
  theme_bw() +  # theme_bw() to make background white
  labs(x=" ", y="Acetate (mg/L)") + # label y axes
  theme(axis.title.x = element_blank(), 
        text = element_text(size=18, color = "black"),
        axis.text = element_text(color="black"), 
        legend.key.size=unit(1.5, "cm") # increase size of legend icons 
  ) 

a = AcetBox + scale_x_discrete(labels=myLabs) + guides(fill=FALSE) # change x axis labels and remove legend
# give this new plot the name 'a' as we'll use this to plot all 4 VFA plots together

ab = AcetBox + scale_x_discrete(labels=myLabs) + guides(fill=FALSE) +theme(axis.text.x = element_blank())
# make a second version w/o x axis labels (as wont need when plotted together)

# do exactly the same for the other VFAs
ButyBox = ggplot(butyrate, aes(Tmt, Counts, fill = factor(Tmt))) + # define variables
  geom_boxplot(size=1) + # change line width, 
  scale_fill_manual(name = "", 
                    values = c('chocolate4', 'black', 'olivedrab', 'gold2', 'darkorange2', 'red', 'darkred')) +
  theme_bw() +  # theme_bw() to make background white
  labs(x=" ", y="Butyrate (mg/L)") + # change y axis label
  theme(axis.title.x = element_blank(),
        text = element_text(size=18, color = "black"),
        axis.text = element_text(color="black"),
        legend.key.size=unit(1.5, "cm")
  ) 

b = ButyBox + scale_x_discrete(labels= myLabs) + guides(fill=FALSE) #
# give this new plot the name 'b' as we'll use this to plot all 4 VFA plots together

bb = ButyBox + scale_x_discrete(labels= myLabs) + guides(fill=FALSE) +theme(axis.text.x = element_blank())#
# make a second version w/o x axis labels (as wont need when plotted together)


PropBox = ggplot(propionate, aes(Tmt, Counts, fill = factor(Tmt))) + # define variables
  geom_boxplot(size=1) + # change line width, 
  scale_fill_manual(name = "", 
                    values = c('chocolate4', 'black', 'olivedrab', 'gold2', 'darkorange2', 'red', 'darkred')) +
  theme_bw() +  # theme_bw() to make background white
  labs(x="", y="Propionate (mg/L") +
  theme(axis.title.x = element_blank(),
        text = element_text(size=18, color = "black"),
        axis.text = element_text(color="black"),
        legend.key.size=unit(1.5, "cm")
  ) 

c = PropBox + scale_x_discrete(labels= myLabs) + guides(fill=FALSE) #

ValBox = ggplot(valerate, aes(Tmt, Counts, fill = factor(Tmt))) + # define variables
  geom_boxplot(size=1) + # change line width, 
  scale_fill_manual(name = "", 
                    values = c('chocolate4', 'black', 'olivedrab', 'gold2', 'darkorange2', 'red', 'darkred')) +
  theme_bw() +  # theme_bw() to make background white
  labs(x="", y="Valerate (mg/L)") +
  theme(axis.title.x = element_blank(),
        text = element_text(size=18, color = "black"),
        axis.text = element_text(color="black"),
        legend.key.size=unit(1.5, "cm")
  ) 

d = ValBox + scale_x_discrete(labels= myLabs) + guides(fill=FALSE) 


## combine into one plot using ggpubr

library(ggpubr)

ggarrange(ab, bb, c, d, # put name of plots, in the order you want them to appear
          common.legend = TRUE, # include this arg if they share the same legend
          align="hv") # align horizontally and vertically so the 4 plot line up nicely. 
# export 

