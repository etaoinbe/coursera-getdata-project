# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
# The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers 
# on a series of yes/no questions related to the project. You will be required to submit: 
#   1) a tidy data set as described below, 2) a link to a Github repository with your script for 
# performing the analysis, and 3) a code book that describes the variables, the data, and any transformation
# s or work that you performed to clean up the data called CodeBook.md. You should also include a README.md 
# in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  
# 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each
#variable for each activity and each subject.
# 
# Good luck!
# 6.  Please upload the tidy data set created in step 5 of the instructions. 
#Please upload your data set as a txt file created with write.table() using row.name=FALSE 
#(do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).
# 


# BI Link<code>MathPreviewEdit: Rich
# 
# Attach a file  (supports: txt, png, jpg, gif, pdf)
# Evaluation/feedback on the above work
# 
# Note: this section can only be filled out during the evaluation phase.
# Has the student submitted a tidy data set? Either a wide or a long form of the data is acceptable if it meets the tidy data principles of week 1 (Each variable you measure should be in one column, Each different observation of that variable should be in a different row).
# 
# 
# Please submit a link to a Github repo with the code for performing your analysis. The code should have a file run_analysis.R in the main directory that can be run as long as the Samsung data is in your working directory. The output should be the tidy data set you submitted for part 1. You should include a README.md in the repo describing how the script works and the code book describing the variables.
# BI Link<code>MathPreviewEdit: Rich
# 
# Evaluation/feedback on the above work
# 
# Note: this section can only be filled out during the evaluation phase.
# Did the student submit a Github repo with the required scripts?
# 
# Was code book submitted to GitHub that modifies and updates the codebooks available to you with the data to indicate all the variables and summaries you calculated, along with units, and any other relevant information?
# 
# 
# I was able to follow the README in the directory that explained what the analysis files did. 
# 
# Overall evaluation/feedback
# 
# Note: this section can only be filled out during the evaluation phase.
# As far as you can determine, does it appear that the work submitted for this project is the work of the student who submitted it? 
# 
# Please use the space below to provide constructive feedback to the student who submitted the work. Point out the submission's strengths as well as areas in need of improvement. You may also use this space to explain your grading decisions.


############################################################################
# https://class.coursera.org/getdata-010/forum/thread?thread_id=49
# colnames(df) <- make.names(chr.vector, unique=TRUE)
# they normalised the data into a -1 to 1 range (this is also why there are negative standard deviation values).
# As normalisation divides a unit by itself, that makes the results unitlless, so for this assignment it is
# fine to note that measurements are normalised and so unitlless.

library(reshape2)
library(dplyr)

setwd("C:\\data\\lectures\\getdata-004\\quiz\\UCI HAR Dataset")

u <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
fname="projectfiles.zip"
download.file(u,destfile=fname)

library(data.table)
dt <- fread(fname)
unzip(fname)

#

fname="test\\X_test.txt" 
dt1=read.csv(fname,sep="",header=FALSE)
fname="train\\X_train.txt" 
dt2=read.csv(fname,sep="",header=FALSE)
activity_labels=read.csv("activity_labels.txt",sep="",header=FALSE)
y1=read.csv('test/y_test.txt',header=FALSE)
y2=read.csv('train/y_train.txt',header=FALSE)

fname1="test\\subject_test.txt" 
subject1=read.csv(fname1,sep="",header=FALSE)
fname2="train\\subject_train.txt" 
subject2=read.csv(fname2,sep="",header=FALSE)

fname="features.txt" 
features=read.csv(fname,sep="",header=FALSE)
featnames=features$V2
#
keepnames= grep("(-mean\\(\\)|-std\\(\\))",featnames,ignore.case=TRUE,value=TRUE)
idxnames2keep= grep("(-mean\\(\\)|-std\\(\\))",featnames,ignore.case=TRUE)
prettykeepnames=as.vector(sapply(keepnames,  make.names))

#n2= sapply(featnames,  make.names)
#
# n2=gsub("(","_",names,fixed=TRUE)
# n2=gsub(")","_",n2,fixed=TRUE)
# n2=gsub("-","_",n2,fixed=TRUE)
# n2=gsub(",","_",n2,fixed=TRUE)
#colnames(dt1)=n2
#colnames(dt2)=n2

# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#idxnames2keep=grep("(mean|std)",n2,ignore.case=TRUE)
#names1= grep("(mean|std)",n2,ignore.case=TRUE,value=TRUE)
#idxnames2keep= grep("(mean|std)",n2,ignore.case=TRUE)

# My thoughts:  The requirement is this:  mean and standard deviation for each measurement. 
# (Italics mine.)  Each measurement has a set of computed quantities, including "mean" and "std"
# and "meanFreq" and a bunch more.  See features_info.txt.  (I had to read it a few times!)  Thus,
# if we are to take the mean and standard deviation (and not the mean frequency), then we need to 
# include "mean" and "std" but not "meanFreq".  In other words, if we include meanFreq, then we're
# including three items for each measurement, not two.

#idxnames2keep=grep("(meanFreq)", names1, ignore.case=TRUE,invert=TRUE)
#names2=n2[idxnames2keep]
dtnames=names(dt1) 
dt1k=dt1[ ,dtnames[idxnames2keep] ]
colnames(dt1k)=prettykeepnames
#edit(idxnames2keep)
dt2k=dt2[ ,dtnames[idxnames2keep] ] 
colnames(dt2k)=prettykeepnames

dt1k[,"subject"]=subject1
dt2k[,"subject"]=subject2
dt1k[,"activitynum"]=y1
dt2k[,"activitynum"]=y2

intersect(names(dt1k),names(dt2k))
dt12=merge(dt1k,dt2k,all=TRUE)
dt3=merge(dt12,activity_labels,by.x="activitynum",by.y="V1",all=TRUE)
dt3=rename(dt3,activity_name=V2)

dt3g=group_by(dt3,activity_name,subject)
dt4=summarise_each(dt3g,funs(mean))

write.table(dt4,"tidydata.txt", row.names=FALSE)

names(dt4)

