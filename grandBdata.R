setwd('C:/Users/Richard Naar/Documents/dok/vision/pupil/Pupil_R_data')

# Paketid
require(ggplot2)
require(quickpsy)
require(ez)

if (!require("pacman")) install.packages("pacman")  # a package for loading and installing package with a single comman
pacman::p_load("Rmisc", "ggplot2", "quickpsy", "ez", "plyr", "reshape")

# import raw data
rd <- read.delim("pupil_rt.txt")


for (ij in 1:length(rd$subid)) {
  if (rd$WhichCue[ij] == 132 || rd$WhichCue[ij] == 231) {
    rd$condition[ij] <- 'anticipated'
    
    ifelse(rd$WhichStim[ij] == 1, rd$cond[ij] <- 'anticipated_1', rd$cond[ij] <- 'anticipated_0')
    ifelse(rd$WhichStim[ij] == 1, rd$cond2[ij] <- 'anticipated_1', rd$cond2[ij] <- 'anticipated_0')
    
  } else if (rd$WhichCue[ij] == 182 || rd$WhichCue[ij] == 281)  {
    rd$condition[ij] <- 'unexpected'
    
    ifelse(rd$WhichStim[ij] == 1, rd$cond[ij] <- 'unexpected_1', rd$cond[ij] <- 'unexpected_0')
    ifelse(rd$WhichStim[ij] == 1, rd$cond2[ij] <- 'unexpected', rd$cond2[ij] <- 'unexpected')
    
  }
}


gg=0
for (ij in 1:length(unique(rd$subid))) {

  count=0
  count0=0
  count1=0
  
  data <- subset(rd, subid == unique(rd$subid)[ij])

  for (ii in 1:length(data$subid)) {
  gg = gg + 1
    
    if (data$cond[ii] == 'anticipated_1') {
      count1 = count1 + 1
      rd$order[gg] = count1
    } else if (data$cond[ii] == 'anticipated_0') {
      count0 = count0 + 1
      rd$order[gg] = count0
    } else {
      count = count + 1
      rd$order[gg] = count
    }
  
  }

}

rd$cond2 <- revalue(rd$cond2, c("anticipated_1"="Anticipated white", "anticipated_0"="Anticipated black", "unexpected"="Unexpected"))


# feedback

fb <- summarySE(rd, measurevar="feedback", groupvars=c("WhichCue", "subid")) # , "subid"


rd <- subset(rd, ReactionTime < 1500)

rd <- subset(rd, correct == '1')


# anticipated_1 <- subset(rd, cond2 == 'anticipated_1')
# anticipated_0 <- subset(rd, cond2 == 'anticipated_0')
# unpredictable <- subset(rd, cond2 == 'unexpected')

# rd1 <- subset(anticipated_1, ReactionTime < 3*sd(anticipated_1$ReactionTime))
# rd2 <- subset(anticipated_0, ReactionTime < 3*sd(anticipated_0$ReactionTime))
# rd3 <- subset(unpredictable, ReactionTime < 3*sd(unpredictable$ReactionTime))

# rd <- rbind(rd1, rd2, rd3)

rd$order <- as.numeric(rd$order)
# Keskmistamine
rdm <- summarySE(rd, measurevar="ReactionTime", groupvars=c("cond2", "order")) # 

# rdm$subid <- as.factor(rdm$subid)



##PLOT katseisiku tulemused (hinnang, et etalon on suurem) sõltuvalt deviandi suurusest (-12, -20, -28 või 12, 20, 28).
## Kas KI-d tegid ülesannet kaasa/kas oli jõukohane

rdmPlot <- ggplot(rdm, aes(x=order, y=ReactionTime, group = cond2, color=cond2)) + #  , group=subid, color=subid
  geom_errorbar(aes(ymin=ReactionTime-ci, ymax=ReactionTime+ci), width=.3, size= 1, position=position_dodge(.5)) + 
  geom_line(size=1.3, position=position_dodge(.5)) +
  geom_point(size=3, position=position_dodge(.5)) +
  scale_colour_hue(l=30)  + 
#  facet_wrap(~ subid) +
  theme_bw()+
  theme(
    plot.background = element_blank()
    # ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) +
  theme(axis.ticks = element_line(size = 2), 
        legend.text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(.85, .905)) +
  theme(axis.line.x=element_line(size = 1),
        axis.line.y=element_line(size = 1))+
  # ggtitle("Predictor type and RT") +
  theme(text = element_text(size=12)) +
  ylab("Reaction Time") +
  xlab("Trials completed") +
  expand_limits(y=c(150, 600)) + 
  theme(axis.text.y = element_text(size="12", angle = 0, hjust = 0)) +
  theme(axis.text.x = element_text(size="12", angle = 0, hjust = 0))

# pole päris veendunud, mis selle eesmärk pidi olema

rd$WhichCue <- as.factor(rd$WhichCue)

rd$WhichCue <- revalue(rd$WhichCue, c("132"="Right", "231"="Left","182"="RND", "281"="RND"))

right <- subset(rd, WhichCue == "Right")
RND <- subset(rd, WhichCue == "RND")
left <- subset(rd, WhichCue == "Left")


#\\\\\\\\\\ BASIC PUPIL COMPARISON

library(readr)
pupil_data <- read_csv("data.csv")

View(pupil_data)


# subject loop
allDat <- data.frame()
for (subi in 1:length(unique(pupil_data$subid))){
  # reading data in
  subs <- unique(pupil_data$subid)
  subDat <- subset(pupil_data, subid == subs[subi])
  
#  subDat$window10 <- scale(subDat$window10 , center=TRUE, scale = TRUE)
   
  for ( wi in 1:(length(subDat[1,])-9) ){
    subDat[9+wi] <- scale(subDat[9+wi], center=TRUE, scale = TRUE) # participant level zscore 
  }
  
  allDat = rbind(allDat, subDat)
  
}

pupil_data <- allDat

pupil_data$condition2 <- revalue(pupil_data$condition, c("anticipated_w"="anticipated white", "anticipated_b"="anticipated black", "unexpected_w"="unexpected", "unexpected_b"="unexpected"))


basicMean <- summarySE(pupil_data, measurevar="window4", groupvars=c("condition2", "pre_post")) # , "pre_post", "subid"

names(basicMean)[5] <- 'y'

ggplot(basicMean, aes(x=condition2, y=y, group=pre_post, linetype = pre_post)) + # , color=pre_post, group=subid, color=subid
  geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=.3, size= 1, position=position_dodge(.15)) + 
  geom_line(size=1.8, position=position_dodge(.15)) +
  geom_point(size=3.5, position=position_dodge(.15)) +
  scale_colour_hue(l=40)  + 
#   facet_wrap(~ subid) +
  theme_bw()+
  theme(
    plot.background = element_blank()
    # ,panel.grid.major = element_blank()
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
  # expand_limits(y=c(150, 600)) + 
  theme(axis.text.y = element_text(size="12", angle = 0, hjust = 0)) +
  theme(axis.text.x = element_text(size="12", angle = 0, hjust = 0))


ezANOVA(pupil_data, dv=window10, wid=subid, within=.(condition), within_full = .(condition), type=3, detailed = T)
pairwise.t.test(pupil_data$window10, pupil_data$condition, p.adjust.method="BH", paired=F)

#/////////// END OF BASIC

library(readr)
pupil_data <- read_csv("data.csv")

View(pupil_data)

# names(pupil_data)[2]<-"subid"
# names(pupil_data)[1]<-"trialNumber"

# import raw data
rd <- read.delim("pupil_rt.txt")

# df_merged <- merge(pupil_data,rd, by = c('subid', 'trialNumber'), all=TRUE) # by = c('subid', 'trialNumber')

for (ij in 1:length(rd$subid)) {
  rd$subid[ij] <- paste(c('PLR', rd$subid[ij]),collapse = "")
}

rd$condition <- NULL

rd$subid <- as.factor(rd$subid)
rd$trialNumber <- as.factor(rd$trialNumber)
pupil_data$subid <- as.factor(pupil_data$subid)
pupil_data$trialNumber <- as.factor(pupil_data$trialNumber)

rd <- rd[order(rd$subid), ]
rd <- rd[order(rd$trialNumber), ]
pupil_data <- pupil_data[order(pupil_data$subid), ]
pupil_data <- pupil_data[order(pupil_data$trialNumber), ]


all_data <- cbind(pupil_data, rd)

# pupil_data1 <- as.data.frame(pupil_data[1:9])

#all_data$baseline <- NULL
#all_data$window0 <- NULL


# baseline <- as.data.frame(pupil_data$baseline)
# baseline$timeWindow <- 'baseline'
# names(baseline)[1]<-"pupilMean"
#window_0 <- as.data.frame(pupil_data$window0)
#window_0$timeWindow <- 'window_0'
#names(window_0)[1]<-"pupilMean"
window_1 <- as.data.frame(all_data$window1)
names(window_1)[1]<-"pupilMean"
window_1$timeWindow <- 'window_1'
window_2 <- as.data.frame(all_data$window2)
names(window_2)[1]<-"pupilMean"
window_2$timeWindow <- 'window_2'
window_3 <- as.data.frame(all_data$window3)
names(window_3)[1]<-"pupilMean"
window_3$timeWindow <- 'window_3'
window_4 <- as.data.frame(all_data$window4)
names(window_4)[1]<-"pupilMean"
window_4$timeWindow <- 'window_4'

df_pupil <- rbind(all_data, all_data, all_data, all_data)
pupil <- rbind(window_1, window_2, window_3, window_4)
df_all <- cbind(df_pupil, pupil)

# df_all$condition2 <- df_all$condition

# df_all$condition2 <- revalue(df_all$condition2, c("anticipated_w"="anticipated white", "anticipated_b"="anticipated black", "unexpected_w"="unexpected", "unexpected_b"="unexpected"))
# ei tea, kas siin oli seda mõeldud
df_all$cond2 <- df_all$condition
df_all$cond2 <- revalue(df_all$cond2, c("anticipated_w"="anticipated white", "anticipated_b"="anticipated black", "unexpected_w"="unexpected", "unexpected_b"="unexpected"))


 for (ij in 1:length(df_all$subid)) {
  df_all$condition3[ij] <- paste(c(df_all$cond2[ij] ,df_all$pre_post[ij]), collapse = " ")
 }

df_all$cond2 <- as.factor(df_all$cond2)

df_all$condition3 <- revalue(df_all$condition3, c("anticipated black post"="Anticipated black (post)", "anticipated black pre" ="Anticipated black (pre)", "anticipated white post"="Anticipated white (post)", "anticipated white pre" = "Anticipated white (pre)", "unexpected post"="Unexpected (post)", "unexpected pre"="Unexpected (pre)"))

df_all$predict <- df_all$cond2
  
df_all$predict <- revalue(df_all$predict, c("anticipated black" = "Anticipated", "anticipated white" = "Anticipated", "unexpected" = "Unexpected"))


df_all$timeWindow <- revalue(df_all$timeWindow, c("window_1" = "1st window", "window_2" = "2nd window", "window_3" = "3rd window", "window_4" = "4th window"))

# df_all <- subset(df_all, ReactionTime < 1500)

df_all1 <- subset(df_all, correct == '1')

df_all2 <- subset(df_all1, ReactionTime < 1500)

dfpm <- summarySE(df_all2, measurevar="pupilMean", groupvars=c("timeWindow", "condition3")) # , "subid"

# dfpm$condition <- as.factor(dfpm$condition)
# dfpm$condition3 <- as.factor(dfpm$condition3)
# dfpm$timeWindow <- as.factor(dfpm$timeWindow)
# dfpm$sub_id <- as.factor(dfpm$sub_id)



rdmPlot <- ggplot(dfpm, aes(x=timeWindow, y=pupilMean, group = condition3, color=condition3, shape=condition3)) + #  , group=subid, color=subid
  geom_errorbar(aes(ymin=pupilMean-ci, ymax=pupilMean+ci), width=.3, size= 1, position=position_dodge(.15)) + 
  geom_line(size=1.8, position=position_dodge(.15)) +
  geom_point(size=3.5, position=position_dodge(.15)) +
  scale_colour_hue(l=40)  + 
 # facet_wrap(~ sub_id)
  theme_bw()+
  theme(
    plot.background = element_blank()
    # ,panel.grid.major = element_blank()
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
  # expand_limits(y=c(150, 600)) + 
  theme(axis.text.y = element_text(size="12", angle = 0, hjust = 0)) +
  theme(axis.text.x = element_text(size="12", angle = 0, hjust = 0))

df_all2$condition3 <- as.factor(df_all2$condition3)
df_all2$timeWindow <- as.factor(df_all2$timeWindow)
df_all2$pre_post <- as.factor(df_all2$pre_post)
df_all2$WhichStim<- as.factor(df_all2$WhichStim)
df_all2$predict<- as.factor(df_all2$predict)

summary(aov(pupilMean ~ timeWindow * pre_post * predict * WhichStim + Error(subid), data=df_all2)) #

summary(aov(pupilMean ~ timeWindow * cond2 + Error(subid), data=df_all2)) #

mod <- aov(pupilMean ~ timeWindow * condition3, data=df_all2) # + Error(subid)

mod2 <- aov(pupilMean ~ timeWindow * cond2, data=df_all2) #

TukeyHSD(mod2, which = "cond2")


ezANOVA(df_all2, dv=pupilMean, wid=subid, within=.(timeWindow, predict, pre_post, WhichStim), within_full = .(timeWindow, predict, pre_post, WhichStim), type=3, detailed = T)
pairwise.t.test(df_all2$pupilMean, df_all2$cond2, p.adjust.method="BH", paired=F)


library(nlme)

lme_p = lme(pupilMean ~ condition3 + timeWindow, data=df_all, random = ~1|subid)

anova(lme_p)

install.packages('multcomp')

library(multcomp)

summary(glht(lme_p, linfct=mcp(condition3 = "Tukey")), test = adjusted(type = "bonferroni"))



# install.packages("multcomp")
# library("multcomp")
# ?glht()

right <- subset(rd, WhichCue == "Right")
RND <- subset(rd, WhichCue == "RND")
left <- subset(rd, WhichCue == "Left")


# names(data)[6] <- "baseline_1"
# names(data)[10] <- "window_0"
# names(data)[11] <- "window_1"
# names(data)[12] <- "window_2"
# names(data)[13] <- "window_3"


# df.long <- reshape(data, varying = c("baseline_1", "window_0", "window_1", "window_2", "window_3"), timevar = "trial_id", idvar = "sub_id", direction = "long", sep = "")




