
#Set wd



#Add information from psychophy

#load
fix = read.csv("FixationsV1.csv")
fix = fix[-c(194), ]

sac = read.csv("SaccadesV1.csv")

samp = read.csv("SamplesV1.csv")

#add the things for searchorder
fix$searchType[fix$SearchOrder == 1 & fix$Trial < 6] = "star"
fix$searchType[fix$SearchOrder == 1 & fix$Trial > 5] = "count"
fix$searchType[fix$SearchOrder == 2 & fix$Trial > 5] = "star"
fix$searchType[fix$SearchOrder == 2 & fix$Trial < 6] = "count"

sac$searchType[sac$SearchOrder == 1 & sac$Trial < 6] = "star"
sac$searchType[sac$SearchOrder == 1 & sac$Trial > 5] = "count"
sac$searchType[sac$SearchOrder == 2 & sac$Trial > 5] = "star"
sac$searchType[sac$SearchOrder == 2 & sac$Trial < 6] = "count"

samp$searchType[samp$SearchOrder == 1 & samp$Trial < 6] = "star"
samp$searchType[samp$SearchOrder == 1 & samp$Trial > 5] = "count"
samp$searchType[samp$SearchOrder == 2 & samp$Trial > 5] = "star"
samp$searchType[samp$SearchOrder == 2 & samp$Trial < 6] = "count"

#add the things for stimuli-thingy in social engagement
files = list.files(path = "C:/Users/hille/Desktop/Assignment-1-2018/PupilsLogs")

N = 1
for(file in files){
  file = paste("PupilsLogs/", file, sep  = "")
  file = read.csv(file)
  
  file$X = file$X+1
  
  file = plyr::rename(file, c("X" = "Trial", "subject" = "ParticipantID"))
  
  if(N == "1"){
    binded = file
    N = N+1
  }
  else{
    binded = rbind(binded, file)
  }
  
}

#merge with the three data files
fix = merge(fix, binded, by = c("Trial", "ParticipantID"), all = T)
sac = merge(sac, binded, by = c("Trial", "ParticipantID"), all = T)
samp = merge(samp, binded, by = c("Trial", "ParticipantID"), all = T)



fix$direction[grepl("dir", fix$video)] = "TowardsYou"
fix$direction[grepl("div", fix$video)] = "Towards3rd"
fix$Gender[grepl("f", fix$video)] = "female"
fix$Gender[grepl("m", fix$video)] = "male"
fix$Ostensiveness[grepl("+o", fix$video)] = "Ostensive"
fix$Ostensiveness[grepl("-o", fix$video)] = "nonOstensive"

sac$direction[grepl("dir", sac$video)] = "TowardsYou"
sac$direction[grepl("div", sac$video)] = "Towards3rd"
sac$Gender[grepl("f", sac$video)] = "female"
sac$Gender[grepl("m", sac$video)] = "male"
sac$Ostensiveness[grepl("+o", sac$video)] = "Ostensive"
sac$Ostensiveness[grepl("-o", sac$video)] = "nonOstensive"

samp$direction[grepl("dir", samp$video)] = "TowardsYou"
samp$direction[grepl("div", samp$video)] = "Towards3rd"
samp$Gender[grepl("f", samp$video)] = "female"
samp$Gender[grepl("m", samp$video)] = "male"
samp$Ostensiveness[grepl("+o", samp$video)] = "Ostensive"
samp$Ostensiveness[grepl("-o", samp$video)] = "nonOstensive"


#----------------------------------------------------------------------------------------------------------#

#Next step. #WE MADE IT!
#Making a sanity check of the data

#test which of the following gives the least error during cross-validation.
#3 folds
#F_duration ~ SearchType*Trial+(1+SearchType*Trial|ParticipantID)
#F_duration ~ SearchType+Trial+(1+SearchType+Trial|ParticipantID)
#F_duration ~ SearchType+(1+SearchType|ParticipantID)

#as the data is not normally distributed add: family = gaussian(link = log)

#Use Ludvigs function next
#

#function called fold from package cvms
#fold
#"cvms"

#In order for us to make our create folds work, we will make a new row called SUBJ where the ID is in numbers from 1-x
fixSubset = fix[fix$Task == "VisualSearch",]

fixSubset$SUBJ = as.numeric(factor(fixSubset$ParticipantID))

library(caret)
folds = createFolds(unique(fixSubset$SUBJ), k = 3, list = T, returnTrain = F)





library(Metrics)
library(MuMIn)
library(lme4)
library(Metrics)
library(modelr)
library(ModelMetrics)

#scale duration

train_RMSE = NULL
test_RMSE = NULL

n = 1
for (fold in folds){
  test = subset(fixSubset, SUBJ %in% fold)
  train = subset(fixSubset, !(SUBJ %in% fold))
  modelCV = glmer(Duration ~ searchType*Trial+(1+searchType*Trial|ParticipantID), family = gaussian(link = log), train)
  
  pred = predict(modelCV, newdata = test, allow.new.levels = T)
  train_RMSE[n] = Metrics::rmse(train$Duration, fitted(modelCV))
  test_RMSE[n] = Metrics::rmse(pred, test$Duration)
  #error of 300 ms more or less. fits well with the data
  
  n = n+1}


#just plus

train_RMSE1 = NULL
test_RMSE1 = NULL

n = 1
for (fold in folds){
  test = subset(fixSubset, SUBJ %in% fold)
  train = subset(fixSubset, !(SUBJ %in% fold))
  modelCV1 = glmer(Duration ~ searchType+Trial+(1+searchType+Trial|ParticipantID), family = gaussian(link = log), train)
  
  pred = predict(modelCV1, newdata = test, allow.new.levels = T)
  train_RMSE1[n] = Metrics::rmse(train$Duration, fitted(modelCV1))
  test_RMSE1[n] = Metrics::rmse(pred, test$Duration)#error of 300 ms more or less. fits well with the data
  
  n = n+1}


#Simple model

train_RMSE2 = NULL
test_RMSE2 = NULL

n = 1
for (fold in folds){
  test = subset(fixSubset, SUBJ %in% fold)
  train = subset(fixSubset, !(SUBJ %in% fold))
  modelCV2 = glmer(Duration ~ searchType+(1+searchType|ParticipantID), family = gaussian(link = log), train)
  
  pred = predict(modelCV2, newdata = test, allow.new.levels = T)
  train_RMSE2[n] = Metrics::rmse(train$Duration, fitted(modelCV2))
  test_RMSE2[n] = Metrics::rmse(pred, test$Duration)
  #error of 300 ms more or less. fits well with the data
  
  n = n+1}


#actually we get more or less the same error in all cases

#Run the model on all data
modelSearch = glmer(Duration ~ searchType+(1+searchType|ParticipantID), family = gaussian(link = log), fixSubset)


#-----------------------------------------------------------------------------------------------------#

#Using ggplot to plot stuff
library(ggplot2)
ggplot(data = fixSubset, aes(searchType, Duration, color = searchType))+
  geom_boxplot()+
  ggtitle("Fixation duration as a function of search type")



library(grid)
library(jpeg)

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

img <- readJPEG('eyetrackingscripts/Foraging/ng090ws.jpg')
g <- rasterGrob(img, interpolate=TRUE)

#density
ggplot(subset(fix, Task=='VisualSearch' & ParticipantID=='6_3_m2' & Trial==6), aes(x = PositionX, y = 1081-PositionY)) +
  xlim(0,1920) + #match with resolution of picture
  ylim(0, 1080) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-0, ymax=1080) +#specifies how big the picture should be
  #xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=1000) + 
  scale_alpha(range = c(0.1, 0.6)) + scale_fill_gradientn(colours = jet.colors(10), trans = "sqrt")

#Plot scanpath for same participant for search task
ScanPath = subset(fixSubset, Task == 'VisualSearch' & ParticipantID=='6_3_m2' & Trial==6)
ScanPath = ScanPath[order(ScanPath$Fixation),]
ggplot(ScanPath, aes(x=PositionX, y= 1081 - PositionY, label=Fixation)) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-0, ymax=1080)  + # adds the picture
  geom_point(size = sqrt(ScanPath$Duration/4), alpha = 0.5, color = "magenta") +
  geom_path(size = 1, alpha = 0.3, color = "yellow") +
  geom_text(aes(label = Fixation, size = 3)) 



#One picture from the count task
img2 <- readJPEG('eyetrackingscripts/Foraging/ng038ws.jpg')
g2 <- rasterGrob(img2, interpolate=TRUE)

#density
ggplot(subset(fix, Task=='VisualSearch' & ParticipantID=='2_2_f2' & Trial==2), aes(x = PositionX, y = 1081-PositionY)) +
  xlim(0,1920) + #match with resolution of picture
  ylim(0, 1080) +
  annotation_custom(g2, xmin=-Inf, xmax=Inf, ymin=-0, ymax=1080) +#specifies how big the picture should be
  #xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=1000) + 
  scale_alpha(range = c(0.1, 0.6)) + scale_fill_gradientn(colours = jet.colors(10), trans = "sqrt")

#Scanpath
ScanPath2 = subset(fixSubset, Task == 'VisualSearch' & ParticipantID=='2_2_f2' & Trial==2)
ScanPath2 = ScanPath2[order(ScanPath2$Fixation),]
ggplot(ScanPath2, aes(x=PositionX, y= 1081 - PositionY, label=Fixation)) +
  annotation_custom(g2, xmin=-Inf, xmax=Inf, ymin=-0, ymax=1080)  + # adds the picture
  geom_point(size = sqrt(ScanPath2$Duration/4), alpha = 0.5, color = "magenta") +
  geom_path(size = 1, alpha = 0.3, color = "yellow") +
  geom_text(aes(label = Fixation, size = 3)) 




#----------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------#

#Function to split groups: facet_grid
#(lots of different types of facet functions)

#OTHER F****** EXPERIMENT
#Graph based on ostensiveness and directedness
ggplot(subset(samp, Task=='SocialEngagement' & ParticipantID == "5_2_m"), aes(x = TrialTime, y = PupilSize, color = Ostensiveness))+
  geom_smooth()+
  facet_grid(.~direction)



#OKAY! MODELS! CV again!

#In order for us to make our create folds work, we will make a new row called SUBJ where the ID is in numbers from 1-x
sampSubset = samp[samp$Task == "SocialEngagement",]

#remove data from blinks
sampSubset = sampSubset[sampSubset$Blink == "0",]


#scale pupil size
sampSubset$PupilSize = scale(sampSubset$PupilSize)
#Now we need to average over each 100 miliseconds
n = 100
sampSubset = sampSubset[seq(1, nrow(sampSubset), 100), ]



sampSubset$SUBJ = as.numeric(factor(sampSubset$ParticipantID))
folds = createFolds(unique(sampSubset$SUBJ), k = 3, list = T, returnTrain = F)



#Try with fixation data instead
fixSub = fix[fix$Task == "SocialEngagement",]
fixSub$SUBJ = as.numeric(factor(fixSub$ParticipantID))

#Remove NAs
fixSub = fixSub[complete.cases(fixSub$Trial),]

folds = createFolds(unique(fixSub$SUBJ), k = 3, list = T, returnTrain = F)


train_RMSESE1 = NULL
test_RMSESE1 = NULL

n = 1
ptm <- proc.time()
for (fold in folds){
  test = subset(fixSub, SUBJ %in% fold)
  test=test[complete.cases(test[,c("PupilSize","direction","SUBJ")]),]
  
  train = subset(fixSub, !(SUBJ %in% fold))
  train=train[complete.cases(train[,c("PupilSize","direction","SUBJ")]),] #this is done so NAs are not removed
  
  modelCV1 = lmer(PupilSize ~ Ostensiveness + (1 + Ostensiveness|ParticipantID), train)
  
  pred1 = predict(modelCV1, newdata = test, allow.new.levels = T)
  train_RMSESE1[n] = Metrics::rmse(train$Duration, fitted(modelCV1))
  test_RMSESE1[n] = Metrics::rmse(pred1, test$Duration)
  
  n = n+1}
proc.time() - ptm


train_RMSESE2 = NULL
test_RMSESE2 = NULL

n = 1
ptm <- proc.time()
for (fold in folds){
  test = subset(fixSub, SUBJ %in% fold)
  train = subset(fixSub, !(SUBJ %in% fold))
  modelCV2 = lmer(PupilSize ~ direction + (1 + direction|ParticipantID), train)
  
  pred2 = predict(modelCV2, newdata = test, allow.new.levels = T)
  train_RMSESE2[n] = Metrics::rmse(train$Duration, fitted(modelCV2))
  test_RMSESE2[n] = Metrics::rmse(pred2, test$Duration)
  
  n = n+1}
proc.time() - ptm

train_RMSESE3 = NULL
test_RMSESE3 = NULL

n = 1
for (fold in folds){
  test = subset(fixSub, SUBJ %in% fold)
  train = subset(fixSub, !(SUBJ %in% fold))
  modelCV3 = lmer(PupilSize ~ Ostensiveness + direction + (1 + Ostensiveness + direction|ParticipantID), train)
  
  pred3 = predict(modelCV3, newdata = test, allow.new.levels = T)
  train_RMSESE3[n] = Metrics::rmse(train$Duration, fitted(modelCV3))
  test_RMSESE3[n] = Metrics::rmse(pred3, test$Duration)
  
  n = n+1}


train_RMSESE4 = NULL
test_RMSESE4 = NULL

n = 1
for (fold in folds){
  test4 = subset(fixSub, SUBJ %in% fold)
  train4 = subset(fixSub, !(SUBJ %in% fold))
  modelCV4 = lmer(PupilSize ~ Ostensiveness*direction + (1 + Ostensiveness*direction|ParticipantID), train4)
  
  pred4 = predict(modelCV4, newdata = test4, allow.new.levels = T)
  train_RMSESE4[n] = Metrics::rmse(train4$Duration, fitted(modelCV4))
  test_RMSESE4[n] = Metrics::rmse(pred4, test4$Duration)
  
  n = n+1}
#Latency due to loading libraries and videos into psychophy
#in this specific case the last part is correct! First few 100 miliseconds should be cut
#might be different in different scripts where latencies happens at different points



#Model all data
modelFULL = lmerTest::lmer(scale(PupilSize) ~ Ostensiveness + (1 + Ostensiveness|ParticipantID), fixSub)

modelFULLNEXT = lmerTest::lmer(PupilSize ~ Ostensiveness*direction + (1 + Ostensiveness*direction|ParticipantID), fixSub)










#TRY WITH LMER (and scaled data) BECAUSE THEY ARE VERY SIMILARY!
#Scale it
fixSub$PupilSize = scale(fixSub$PupilSize)

#THIS IS THE SAME; DOESN*T MAKE ANY SENSE


train_RMSESE1 = NULL
test_RMSESE1 = NULL

n = 1
for (fold in folds){
  test = subset(fixSub, SUBJ %in% fold)
  train = subset(fixSub, !(SUBJ %in% fold))
  modelCV = lmer(PupilSize ~ Ostensiveness + (1 + Ostensiveness|ParticipantID), train)
  
  pred = predict(modelCV, newdata = test, allow.new.levels = T)
  train_RMSESE1[n] = Metrics::rmse(train$Duration, fitted(modelCV))
  test_RMSESE1[n] = Metrics::rmse(pred, test$Duration)
  
  n = n+1}


train_RMSESE2 = NULL
test_RMSESE2 = NULL

n = 1
for (fold in folds){
  test = subset(fixSub, SUBJ %in% fold)
  train = subset(fixSub, !(SUBJ %in% fold))
  modelCV = lmer(PupilSize ~ direction + (1 + direction|ParticipantID), train)
  
  pred = predict(modelCV, newdata = test, allow.new.levels = T)
  train_RMSESE2[n] = Metrics::rmse(train$Duration, fitted(modelCV))
  test_RMSESE2[n] = Metrics::rmse(pred, test$Duration)
  
  n = n+1}

train_RMSESE3 = NULL
test_RMSESE3 = NULL

n = 1
for (fold in folds){
  test = subset(fixSub, SUBJ %in% fold)
  train = subset(fixSub, !(SUBJ %in% fold))
  modelCV = lmer(PupilSize ~ Ostensiveness + direction + (1 + Ostensiveness + direction|ParticipantID), train)
  
  pred = predict(modelCV, newdata = test, allow.new.levels = T)
  train_RMSESE3[n] = Metrics::rmse(train$Duration, fitted(modelCV))
  test_RMSESE3[n] = Metrics::rmse(pred, test$Duration)
  
  n = n+1}

train_RMSESE4 = NULL
test_RMSESE4 = NULL

n = 1
for (fold in folds){
  test = subset(fixSub, SUBJ %in% fold)
  train = subset(fixSub, !(SUBJ %in% fold))
  modelCV = lmer(PupilSize ~ Ostensiveness*direction + (1 + Ostensiveness*direction|ParticipantID), train)
  
  pred = predict(modelCV, newdata = test, allow.new.levels = T)
  train_RMSESE4[n] = Metrics::rmse(train$Duration, fitted(modelCV))
  test_RMSESE4[n] = Metrics::rmse(pred, test$Duration)
  
  n = n+1}











#Try with stealing code from someone else. Maybe I'm just stupido!
pO_rmse_test = NULL
pO_rmse_train = NULL

n = 1
for(i in folds){
  print(i)
  train = subset(fixSub, !(fixSub$SUBJ %in% i))
  train=train[complete.cases(train[,c("PupilSize","direction","SUBJ")]),] #this is done so NAs are not removed
  
  test = subset(fixSub, fixSub$SUBJ %in%i)
  test=test[complete.cases(test[,c("PupilSize","direction","SUBJ")]),]
  
  newmodel = glmer(PupilSize ~ 1 + Ostensiveness + (1 + Ostensiveness | SUBJ), family = gaussian(link = log), data = train)
  
  newpred = predict(newmodel, newdata = test, allow.new.levels = TRUE)
  pO_rmse_test[n] = Metrics::rmse(newpred, test$PupilSize )
  pO_rmse_train[n] = Metrics::rmse(train$PupilSize, fitted(newmodel) )
  
  n = n+1
}



pD_rmse_test = NULL
pD_rmse_train = NULL

n = 1
for(i in folds){
  print(i)
  train = subset(fixSub, !(fixSub$SUBJ %in% i))
  train=train[complete.cases(train[,c("PupilSize","direction","SUBJ")]),] #this is done so NAs are not removed
  
  test = subset(fixSub, fixSub$SUBJ %in%i)
  test=test[complete.cases(test[,c("PupilSize","direction","SUBJ")]),]
  
  newmodel = lmer(PupilSize ~ 1 + direction + (1 + direction | SUBJ), data = train)
  
  newpred = predict(newmodel, newdata = test, allow.new.levels = TRUE)
  pD_rmse_test[n] = Metrics::rmse(newpred, test$PupilSize )
  pD_rmse_train[n] = Metrics::rmse(train$PupilSize, fitted(newmodel) )
  
  n = n+1
}



pOD_rmse_test = NULL
pOD_rmse_train = NULL

n = 1
for(i in folds){
  print(i)
  train = subset(fixSub, !(fixSub$SUBJ %in% i))
  train=train[complete.cases(train[,c("PupilSize","direction","SUBJ")]),] #this is done so NAs are not removed
  
  test = subset(fixSub, fixSub$SUBJ %in%i)
  test=test[complete.cases(test[,c("PupilSize","direction","SUBJ")]),]
  
  newmodel = lmer(PupilSize ~ 1 + Ostensiveness + direction + (1 + Ostensiveness + direction | SUBJ), data = train)
  
  newpred = predict(newmodel, newdata = test, allow.new.levels = TRUE)
  pOD_rmse_test[n] = Metrics::rmse(newpred, test$PupilSize )
  pOD_rmse_train[n] = Metrics::rmse(train$PupilSize, fitted(newmodel) )
  
  n = n+1
}


pODint_rmse_test = NULL

pODint_rmse_train = NULL

n = 1
for(i in folds){
  print(i)
  train = subset(fixSub, !(fixSub$SUBJ %in% i))
  train=train[complete.cases(train[,c("PupilSize","direction","SUBJ")]),] #this is done so NAs are not removed
  
  test = subset(fixSub, fixSub$SUBJ %in%i)
  test=test[complete.cases(test[,c("PupilSize","direction","SUBJ")]),]
  
  newmodel = lmer(PupilSize ~ 1 + Ostensiveness*direction + (1 + Ostensiveness*direction | SUBJ), data = train)
  
  newpred = predict(newmodel, newdata = test, allow.new.levels = TRUE)
  pODint_rmse_test[n] = Metrics::rmse(newpred, test$PupilSize )
  pODint_rmse_train[n] = Metrics::rmse(train$PupilSize, fitted(newmodel) )
  
  n = n+1
}
