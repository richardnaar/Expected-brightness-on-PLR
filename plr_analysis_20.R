##---------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------##
##----------------Does Pupil Size Depend on Expected Brightness of the Visual Input?-----------------##
##---------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------##

library(crayon)
cat(inverse("Navigating to the data folder...\n"))
setwd('C:/Users/Richard Naar/Documents/dok/vision/pupil/PAPER/Analysis/Pupil_R_data')

cat(inverse("Loding (and installing if nessesary) the packages used in the pipeline...\n"))
if (!require("pacman")) install.packages("pacman")
pacman::p_load("Rmisc", "ggplot2", "quickpsy", "ez", "plyr", "reshape", "ggplot2", "quickpsy", "ez", "nlme", "psycho", + 
               "sjPlot", "ggpubr", "readr", "crayon")

cat(bgGreen("---------------------------------RT ANALYSIS-----------------------------------------\n"))

# import raw data
rd <- read.delim("pupil_rt.txt"); cat(inverse("Loading the data...\n"))
#summary(rd)

# the meaning of the cue depended on KeyOrder
for (ij in 1:length(rd$subid)) {
  if (rd$KeyOrder[ij] ==  1) {#  
    
    if (rd$WhichCue[ij] == 132){
      rd$WhichCue[ij] = 231
    } else if (rd$WhichCue[ij] == 231){
      rd$WhichCue[ij] = 132
    } else if (rd$WhichCue[ij] == 182){
      rd$WhichCue[ij] = 281
    } else if (rd$WhichCue[ij] == 281){
      rd$WhichCue[ij] = 182
    }
    
  } 
}

cat(inverse("Loading the data...\n")); pupil_data <- read_csv("data_wins.csv") #data, data_wins, data_wins_no_baseline
pupil_data$feedback <- NULL # this already exists in rd 

#View(pupil_data)

# Similar naming conventions of both id variables
for (ij in 1:length(rd$subid)) {
  rd$subid[ij] <- paste(c('PLR', rd$subid[ij]),collapse = "")
}

# coercing columns (subid, trialNumber) to factors
cols = c("subid","trialNumber"); inds = which(colnames(rd)%in%cols); rd[inds] <- lapply(rd[inds], factor)
inds = which(colnames(pupil_data)%in%cols); pupil_data[inds] <- lapply(pupil_data[inds], factor)

cat(inverse("Merging pupil data with RT data...\n"))
all_data <- transform( merge(pupil_data, rd, by=c('subid', 'trialNumber')) )
#df_merged <- merge(pupil_data,rd, by = c('subid', 'trialNumber'), all=FALSE) # by = c('subid', 'trialNumber')


# define two columns based on the cue
cat(inverse("Defining new variables: cond (w,b,rnd), cond2 (w,b,rndv,rndb), and order (trial progression)...\n"))
for (ij in 1:length(all_data$subid)) {
  if (all_data$condition[ij] == 'anticipated_w' || all_data$condition[ij] == 'anticipated_b') {
    all_data$predict[ij] <- 'anticipated'
    
    ifelse(all_data$condition[ij] == 'anticipated_w', all_data$cond[ij] <- 'Anticipated white', all_data$cond[ij] <- 'Anticipated black')
    ifelse(all_data$condition[ij] == 'anticipated_w', all_data$cond2[ij] <- 'Anticipated white', all_data$cond2[ij] <- 'Anticipated black')
    
  } else if (all_data$condition[ij] == 'unexpected_w' || all_data$condition[ij] == 'unexpected_b')  {
    all_data$predict[ij] <- 'unexpected'
    
    ifelse(all_data$condition[ij] == 'unexpected_w', all_data$cond[ij] <- 'Unexpected white', all_data$cond[ij] <- 'Unexpected black')
    ifelse(all_data$condition[ij] == 'unexpected_w', all_data$cond2[ij] <- 'Unexpected', all_data$cond2[ij] <- 'Unexpected')
    
  }
}



cat(inverse("Removing too slow or too fast RTs and incorrect trials...\n"))
nTrials = nrow(all_data)
all_data <- subset(all_data, ReactionTime > 100) # Mora-Cortes, Ridderinkhof, & Cohen (2018) - cut-off
all_data <- subset(all_data, correct == '1')

#all_data <- subset(rd, ReactionTime < 1500)

# condition spesific upper cut-off (3.5 x median) # Mora-Cortes, Ridderinkhof, & Cohen (2018) - 2.5 SD cut-off
all_data$cond <- as.factor(all_data$cond); sds <- 0; rd2 <- data.frame()
for (ij in 1:length(unique(all_data$cond))){
  
  data = subset( all_data,  cond == levels(all_data$cond)[ij] ); nrowsbefore <- nrow(data)
#  hist(data$ReactionTime,30)
  data = subset( data, ReactionTime <  median(data$ReactionTime)*3.5 )
#  print(nrowsbefore-nrow(data))
#  hist(data$ReactionTime,30)
  
  rd2 = rbind(rd2, data)
}

all_data <- rd2; remove(rd2, data)

cat(inverse(paste('Proportion of trials left out:', round( 100-(nrow(all_data)*100)/nTrials,2), "%\n" )))

all_data$trialprogl <- as.numeric(all_data$trialprog)

cat(inverse("Taking condition averages...\n"))

rdm <- summarySE(all_data, measurevar="ReactionTime", groupvars=c("cond", "trialprog")) # 

cat(inverse("... and plotting.\n"))

ggplot(rdm, aes(x=trialprog, y=ReactionTime, group = cond, color=cond)) + #  , group=subid, color=subid
  geom_errorbar(aes(ymin=ReactionTime-ci, ymax=ReactionTime+ci), width=.3, size= 1, position=position_dodge(.5)) + 
  geom_line(size=1.3, position=position_dodge(.5)) +
  geom_point(size=3, position=position_dodge(.5)) +
  scale_colour_hue(l=30)  + 
#  facet_wrap(~ WhichStim) +
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
  ylim(c(0, 1000)) +
  #expand_limits(y=c(150, 600)) + 
  theme(axis.text.y = element_text(size="12", angle = 0, hjust = 0)) +
  theme(axis.text.x = element_text(size="12", angle = 0, hjust = 0))

# lmer
cat(inverse("Fitting Linerar Mixed-Effects Models\n"))

library(lmerTest)
#fit <- lmer(ReactionTime ~ WhichStim + condition + KeyOrder + EST + gender + subage + (1|subid), data=all_data)
fit <- lmer(ReactionTime ~ as.factor(WhichStim) + as.factor(WhichCue) + (1|subid), data=all_data)


summary(fit)

library(sjPlot)

cat(inverse("Forest-plot of standardized beta values...\n"))
plot_model(fit, type = c("eff"))


cat(inverse("Comparison between predicted compared to non-predicted...\n"))
pairwise.t.test(all_data$ReactionTime, all_data$condition, p.adjust.method="BH", paired=F)

cat(inverse("Comparison between men and women...\n"))
pairwise.t.test(all_data$ReactionTime, all_data$gender, p.adjust.method="BH", paired=F)


cat(inverse("Coputing averages over age and plotting...\n"))
agem <- summarySE(all_data, measurevar="ReactionTime", groupvars=c("subage")) # 

cor.test(agem$subage, agem$ReactionTime)

ggscatter(agem, x = "subage", y = "ReactionTime", 
          add = "reg.line", conf.int = TRUE, size = 3,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Reaction Time",
          add.params = list(color = "red",
                            fill = "lightgray")) 

# feedback

fb <- summarySE(all_data, measurevar="feedback", groupvars=c("predict")) # , "subid"
cat(inverse(paste("Note: participants got feedback almost exclucively in the 'predicted' condition: ", +
                    round(fb$feedback[1],2)*100, "% vs", round(fb$feedback[2],2)*100, "%\n" )))

cat(bgGreen("----------------------------------PUPIL ANALYSIS----------------------------------------\n"))


inds <- grep("window", colnames(all_data) )
cnames <- colnames(all_data)[inds]
all_data <- reshape(all_data, varying=cnames, direction="long", idvar=c('subid', 'trialNumber'), timevar = "timeWindow", sep="") # 

pupind <- grep('window',names(all_data)); names(all_data)[pupind] <- "pupilMean";

all_data$winCat <- NULL

for (ij in 1:nrow(all_data)) {
ifelse(all_data$timeWindow[ij] < 11, all_data$winCat[ij] <- 'before event', all_data$winCat[ij] <- 'after event')
  }

cat(inverse("Taking condition averages and plotting...\n"))

dfpm <- summarySE(all_data, measurevar="pupilMean", groupvars=c("timeWindow", "condition")) # , "subid"


ggplot(dfpm, aes(x=timeWindow, y=pupilMean, group = condition, color=condition, shape=condition)) + #  , group=subid, color=subid
  geom_errorbar(aes(ymin=pupilMean-ci, ymax=pupilMean+ci), width=.3, size= 1, position=position_dodge(.15)) + 
  geom_line(size=1.8, position=position_dodge(.15)) +
  geom_point(size=3.5, position=position_dodge(.15)) +
  scale_colour_hue(l=40)  + 
#  facet_wrap(~ subid) +
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


cols = c("timeWindow", "pre_post", "predict", "condition", "bgcol"); inds = which(colnames(all_data)%in%cols); all_data[inds] <- lapply(all_data[inds], factor)

# lmer

all_data <- subset(all_data, winCat == "before event")

#all_data$timeWindow <- as.numeric(all_data$timeWindow)
#all_data <- subset(all_data, timeWindow < 13)
#all_data$timeWindow <- as.factor(all_data$timeWindow)

library(lmerTest)
fit <- lmer(pupilMean ~ bgcol * predict * pre_post * timeWindow +  (1|subid) + (1|baseline), 
            data=all_data %>% mutate(pre_post = relevel(pre_post, "pre")))

#fit <- lmer(pupilMean ~  bgcol * predict * pre_post2 * timeWindow +  (1|subid) + (1|baseline), 
#            data=all_data )


summary(fit)

plot_model(fit, type = "eff", terms = c("timeWindow", "bgcol", "pre_post", "predict"))
#plot_model(fit, type = "eff", terms = c("timeWindow", "bgcol", "pre_post2", "predict"))



#library(sjPlot)

cat(inverse("Forest-plot of standardized beta values...\n"))
plot_model(fit, type = c("std"))

# ezANOVA

ezANOVA(all_data, dv=pupilMean, wid=subid, within=.(timeWindow, predict, pre_post, bgcol),  type=3, detailed = T) # , within_full = .(timeWindow, predict, pre_post, WhichStim)


pairwise.t.test(all_data$pupilMean, all_data$WhichStim, p.adjust.method="BH", paired=F)
pairwise.t.test(all_data$pupilMean, all_data$cond, p.adjust.method="BH", paired=F)

