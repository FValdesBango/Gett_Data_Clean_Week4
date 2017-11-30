Raw data has been adquired by downloading it from the UCI website (the study of Human Activity Recognition Using Smartphones Data Set)
without having run any additional program on it.

For the processing of the data only one script has been applied, which has been submitted here as "Run_analysis.R"
The script requires the use of the package "dplyr" that it calls to install and load at the beginning.

This script obtains the data directly fro the .zip file and merges the data set from both test and train groups of study and renames 
its original values for the feature labelsgiven by the study researchers, that are easier to understand. Additionaly it changes activity 
associated Id numbers for its descriptive labels, again with the same aim of make more readable. After that a subset of the data has 
been perform to get only the mean and standard deviation values of the study features.

Finally a mean of the values of each one of this variablesi performed grouped by subject of study and kind of acitvity taking place.

The script writes these last values in a data frame who outputs as a .txt file.

==================================================================
Code itself
==================================================================
#Script for Getting and Cleaning Data Course Project Week4 from FVB.

#The idea is to get some clean and order data from a raw source.
#The script uses the "dplyr" package and so it installs it in case it's absent.
install.packages("dplyr")
library(dplyr)

#It's informed that the script should run in our directory after the raw data
#it has been downloaded in our working directory, we assume that its files 
#haven't been extracted yet, so we unzip them in that case.
#
if (!dir.exists("./UCI HAR Dataset")) {
        unzip(zipfile ="getdata%2Fprojectfiles%2FUCI HAR Dataset.zip")
}

#We read all the data files independently from their sources. 
#
#Both from the test group... 

subj.test<-read.table("UCI HAR Dataset/test/subject_test.txt")
x.test<-read.table("UCI HAR Dataset/test/x_test.txt")
y.test<-read.table("UCI HAR Dataset/test/y_test.txt")

#...and from the train group...

subj.train<-read.table("UCI HAR Dataset/train/subject_train.txt")
x.train<-read.table("UCI HAR Dataset/train/x_train.txt")
y.train<-read.table("UCI HAR Dataset/train/y_train.txt")

#...but also the labels for the activities and variable names

feat.label<-read.table("UCI HAR Dataset/features.txt")
act.label<-read.table("UCI HAR Dataset/activity_labels.txt")

#After we do that create a new "complete" tables for each one of the groups by
#merging the columns of the subjects ID numbers with the tables of the 
#associated activities and measurements, and we add a new column tu identify 
#their origin as a part of the test or train group (basically a character 
#variable saying with two values "test" or "train"). 
#
#In principle this is not requested by the task but it could have a potential 
#utility
#
#
dat.test  <- cbind(subj.test,group=rep("test",dim(subj.test)[1]),y.test,x.test)
dat.train <- cbind(subj.train,group=rep("train",dim(subj.train)[1]),y.train,x.train)

#Now we merge the two "complete" data sets from each group,combining the rows 
#of both tables, as the share the same variable columns (nº1 of request list)

dat.tot<- rbind(dat.test,dat.train)

#We now create a character vector, containing the more descriptive labels of the
#data set, adding names for the merged columns of subject ID, group belonging 
#and activity.
# 
dat.names<-c(c("subject.id","group","activity"),as.character(feat.label[,2]))

#We also perform some cleaning and ordering of the labels in order to have only
# "." as a separator marker in our variable names by substituting "-" or "()" 
# marks with "." and then erasing the double ".." resulting from this 
dat.names<-gsub("-|\\()",".",dat.names)
dat.names<-gsub("\\.\\.",".",dat.names)

#We set the appropiate labels to our variables in the previously created 
#merged data set  of each group (nº4 or the request list)
names(dat.tot)<-dat.names

#We now subset from the merged data only the variable columns whose values are
#related to the mean and the standar deviation, by looking for them in their
#variable labels (nº 2 in the request list)

dat.red<- dat.tot[,c(1:3,grep("mean|std",names(dat.tot)))]
dat.red<- dat.red[,-grep("meanFreq",names(dat.red))]

#We substitute the activities numbers ID in their variable column by their 
#complete label (nº 3 in the request list)

for(i in 1:6){dat.red$activity[which(dat.red$activity==i)]<-act.label$V2[i]}

#Finally from the reduced data set we create a second, independent tidy data set
# with the average of each variable for each activity and each subject (nº5 in
# the request list)

dat.red.by_subj_act <- group_by(dat.red,subject.id,activity)
dat.tidy.sum <-summarise_at(.tbl = dat.red.by_subj_act ,
                         .vars = names(dat.red.by_subj_act)[-1:-3],.funs = mean)
#Finally we output an .txt file with the summarised data set we've just created
write.table(dat.tidy.sum,file ="dat_tidy_sum.txt" ,row.names = FALSE)