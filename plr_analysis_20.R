##---------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------##
##----------------Does Pupil Size Depend on Expected Brightness of the Visual Input?-----------------##
##---------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------##

cat(inverse("Navigating to the data folder...\n"))
setwd('C:/Users/Richard Naar/Documents/dok/vision/pupil/PAPER/Analysis/Pupil_R_data')

cat(inverse("Loding (and installing if nessesary) the packages used in the pipeline...\n"))
if (!require("pacman")) install.packages("pacman")
pacman::p_load("Rmisc", "ggplot2", "quickpsy", "ez", "plyr", "reshape", "ggplot2", "quickpsy", "ez", "nlme", "psycho", + 
               "sjPlot", "ggpubr", "readr", "crayon")

##--------------------------------------------RT ANALYSIS--------------------------------------------##

# import raw data
rd <- read.delim("pupil_rt.txt"); cat(inverse("Loading the data...\n"))
summary(rd)



# define two columns based on the cue
cat(inverse("Defining new variables: cond (w,b,rnd), cond2 (w,b,rndv,rndb), and order (trial progression)...\n"))
for (ij in 1:length(rd$subid)) {
  if (rd$WhichCue[ij] == 132 || rd$WhichCue[ij] == 231) {
    rd$condition[ij] <- 'anticipated'
    
    ifelse(rd$WhichStim[ij] == 1, rd$cond[ij] <- 'Anticipated white', rd$cond[ij] <- 'Anticipated black')
    ifelse(rd$WhichStim[ij] == 1, rd$cond2[ij] <- 'Anticipated white', rd$cond2[ij] <- 'Anticipated black')
    
  } else if (rd$WhichCue[ij] == 182 || rd$WhichCue[ij] == 281)  {
    rd$condition[ij] <- 'unexpected'
    
    ifelse(rd$WhichStim[ij] == 1, rd$cond[ij] <- 'Unexpected white', rd$cond[ij] <- 'Unexpected black')
    ifelse(rd$WhichStim[ij] == 1, rd$cond2[ij] <- 'Unexpected', rd$cond2[ij] <- 'Unexpected')
    
  }
}

# define new column 'order' based on the trial number
gg=0 # keeps track of all the iterations
for (ij in 1:length(unique(rd$subid))) {

  countw=0; countb=0; countrnd=0;
  
  data <- subset(rd, subid == unique(rd$subid)[ij])
  data <- data[order(data$trialNumber),]

  for (ii in 1:length(data$subid)) {
  gg = gg + 1
  
    if (data$cond[ii] == 'Anticipated white') {
      countw = countw + 1; rd$order[gg] = countw
    } else if (data$cond[ii] == 'Anticipated black') {
      countb = countb + 1; rd$order[gg] = countb
    } else {
      countrnd = countrnd + 1; rd$order[gg] = countrnd
    }
  
  }

}

cat(inverse("Removing too slow or too fast RTs and incorrect trials...\n"))
nTrials = nrow(rd)
rd <- subset(rd, ReactionTime > 100) # Mora-Cortes, Ridderinkhof, & Cohen (2018) - cut-off
rd <- subset(rd, correct == '1')

#rd <- subset(rd, ReactionTime < 1500)

# condition spesific upper cut-off (3.5 x median) # Mora-Cortes, Ridderinkhof, & Cohen (2018) - 2.5 SD cut-off
rd$cond <- as.factor(rd$cond); sds <- 0; rd2 <- data.frame()
for (ij in 1:length(unique(rd$cond))){
  
  data = subset( rd,  cond == levels(rd$cond)[ij] ); nrowsbefore <- nrow(data)
#  hist(data$ReactionTime,30)
  data = subset( data, ReactionTime <  median(data$ReactionTime)*3.5 )
#  print(nrowsbefore-nrow(data))
#  hist(data$ReactionTime,30)
  
  rd2 = rbind(rd2, data)
}

rd <- rd2; remove(rd2, data)

cat(inverse(paste('Proportion of trials left out:', round( 100-(nrow(rd)*100)/nTrials,2), "%\n" )))

rd$order <- as.numeric(rd$order)

cat(inverse("Taking condition averages...\n"))
rdm <- summarySE(rd, measurevar="ReactionTime", groupvars=c("cond2", "order")) # 

cat(inverse("... and plotting.\n"))

ggplot(rdm, aes(x=order, y=ReactionTime, group = cond2, color=cond2)) + #  , group=subid, color=subid
  geom_errorbar(aes(ymin=ReactionTime-ci, ymax=ReactionTime+ci), width=.3, size= 1, position=position_dodge(.5)) + 
  geom_line(size=1.3, position=position_dodge(.5)) +
  geom_point(size=3, position=position_dodge(.5)) +
  scale_colour_hue(l=30)  + 
  theme_bw()+
  theme(
    plot.background = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) +
  theme(axis.ticks = element_line(size = 2), 
        legend.text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(.85, .905)) +
  theme(axis.line.x=element_line(size = 1),
        axis.line.y=element_line(size = 1))+
  theme(text = element_text(size=12)) +
  ylab("Reaction Time") +
  xlab("Trials completed") +
  expand_limits(y=c(150, 600)) + 
  theme(axis.text.y = element_text(size="12", angle = 0, hjust = 0)) +
  theme(axis.text.x = element_text(size="12", angle = 0, hjust = 0))

# lmer
cat(inverse("Fitting Linerar Mixed-Effects Models\n"))

fit <- lmer(ReactionTime ~ WhichStim + condition + KeyOrder + EST + gender + subage + (1|subid), data=rd)

summary(fit)

library(sjPlot)

cat(inverse("Forest-plot of standardized beta values...\n"))
plot_model(fit, type = c("std"))


cat(inverse("Comparison between predicted compared to non-predicted...\n"))
pairwise.t.test(rd$ReactionTime, rd$condition, p.adjust.method="BH", paired=F)

cat(inverse("Comparison between men and women...\n"))
pairwise.t.test(rd$ReactionTime, rd$gender, p.adjust.method="BH", paired=F)


cat(inverse("Coputing averages over age and plotting...\n"))
agem <- summarySE(rd, measurevar="ReactionTime", groupvars=c("subage")) # 

cor.test(agem$subage, agem$ReactionTime)

ggscatter(agem, x = "subage", y = "ReactionTime", 
          add = "reg.line", conf.int = TRUE, size = 3,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Reaction Time",
          add.params = list(color = "red",
                            fill = "lightgray")) 

# feedback

fb <- summarySE(rd, measurevar="feedback", groupvars=c("condition")) # , "subid"
cat(inverse(paste("Note: participants got feedback almost exclucively in the 'predicted' condition: ", +
                    round(fb$feedback[1],2)*100, "% vs", round(fb$feedback[2],2)*100, "%\n" )))

##---------------------------------------------PUPIL ANALYSIS----------------------------------------##
cat(inverse("Loading the data...\n")); pupil_data <- read_csv("data.csv")

View(pupil_data)

cat(inverse("WARNING: May not work properly if the lines of code before the message 'Proportion of trials left out...' not run yet."))
for (ij in 1:length(rd$subid)) {
  rd$subid[ij] <- paste(c('PLR', rd$subid[ij]),collapse = "")
}

# coercing columns (subid, trialNumber) to factors
cols = c("subid","trialNumber"); inds = which(colnames(rd)%in%cols); rd[inds] <- lapply(rd[inds], factor)
inds = which(colnames(pupil_data)%in%cols); pupil_data[inds] <- lapply(pupil_data[inds], factor)

cat(inverse("Merging pupil data with RT data...\n"))
all_data <- transform( merge(pupil_data, rd, by=c('subid', 'trialNumber')) )
#df_merged <- merge(pupil_data,rd, by = c('subid', 'trialNumber'), all=FALSE) # by = c('subid', 'trialNumber')


inds <- grep("window", colnames(all_data) )
cnames <- colnames(all_data)[inds]
all_data <- reshape(all_data, varying=cnames, direction="long", idvar=c('subid', 'trialNumber'), timevar = "timeWindow", sep="") # 

pupind <- grep('window',names(all_data)); names(all_data)[pupind] <- "pupilMean";


all_data <- all_data


 for (ij in 1:length(all_data$subid)) {
  all_data$cond_pre_post[ij] <- paste(c(all_data$cond2[ij] ,all_data$pre_post[ij]), collapse = " ")
 }

all_data$predict <- all_data$cond2
all_data$predict <- revalue(all_data$predict, c("Anticipated black" = "Anticipated", "Anticipated white" = "Anticipated", "Unexpected" = "Unexpected"))

cat(inverse("Taking condition averages and plotting...\n"))

dfpm <- summarySE(all_data, measurevar="pupilMean", groupvars=c("timeWindow", "cond_pre_post")) # , "subid"


ggplot(dfpm, aes(x=timeWindow, y=pupilMean, group = cond_pre_post, color=cond_pre_post, shape=cond_pre_post)) + #  , group=subid, color=subid
  geom_errorbar(aes(ymin=pupilMean-ci, ymax=pupilMean+ci), width=.3, size= 1, position=position_dodge(.15)) + 
  geom_line(size=1.8, position=position_dodge(.15)) +
  geom_point(size=3.5, position=position_dodge(.15)) +
  scale_colour_hue(l=40)  + 
  theme_bw()+
  theme(
    plot.background = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) +
  theme(axis.ticks = element_line(size = 2), 
        legend.text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(.90, .20)) +
  theme(axis.line.x=element_line(size = 1),
        axis.line.y=element_line(size = 1))+
  # ggtitle("Predictor type and RT") +
  theme(text = element_text(size=12)) +
  ylab("Pupil Change") +
  xlab("Analysis Windows") +
  theme(axis.text.y = element_text(size="12", angle = 0, hjust = 0)) +
  theme(axis.text.x = element_text(size="12", angle = 0, hjust = 0))


cols = c("timeWindow", "pre_post", "WhichStim", "predict"); inds = which(colnames(all_data)%in%cols); all_data[inds] <- lapply(all_data[inds], factor)

# lmer

library(lmerTest)
fit <- lmer(pupilMean ~ WhichStim + pre_post + predict + timeWindow + (1|subid), data=all_data)

summary(fit)

#library(sjPlot)

cat(inverse("Forest-plot of standardized beta values...\n"))
plot_model(fit, type = c("std"))

# ezANOVA

ezANOVA(all_data, dv=pupilMean, wid=subid, within=.(timeWindow, predict, pre_post, WhichStim),  type=3, detailed = T) # , within_full = .(timeWindow, predict, pre_post, WhichStim)

pairwise.t.test(all_data$pupilMean, all_data$WhichStim, p.adjust.method="BH", paired=F)
pairwise.t.test(all_data$pupilMean, all_data$cond2, p.adjust.method="BH", paired=F)
# pairwise.t.test(all_data$pupilMean, all_data$cond_pre_post, p.adjust.method="BH", paired=F)

