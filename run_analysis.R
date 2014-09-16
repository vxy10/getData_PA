library(dplyr)
# Step 1: Read data sets
# reading test data
xtest = read.table('./UCI HAR Dataset/test/X_test.txt', 
                   header = FALSE,stringsAsFactors = FALSE)
ytest = read.table('./UCI HAR Dataset/test/Y_test.txt', 
                   header = FALSE,stringsAsFactors = FALSE)
subtest = read.table('./UCI HAR Dataset/test/subject_test.txt', 
                      header = FALSE,stringsAsFactors = FALSE)
# reading train data   
xtrain = read.table('./UCI HAR Dataset/train/X_train.txt', 
                   header = FALSE,stringsAsFactors = FALSE)
ytrain = read.table('./UCI HAR Dataset/train/Y_train.txt', 
                   header = FALSE,stringsAsFactors = FALSE)
subtrain = read.table('./UCI HAR Dataset/train/subject_train.txt', 
                      header = FALSE,stringsAsFactors = FALSE)
# reading features
features = read.table('./UCI HAR Dataset/features.txt', 
                    header = FALSE,stringsAsFactors = FALSE)
# 3. reading Activity labels
ActivityLabels = read.table('./UCI HAR Dataset/Activity_labels.txt', 
                      header = FALSE,stringsAsFactors = FALSE)
ActivityLabels_train = subtrain
for (i in ActivityLabels$V1) { ActivityLabels_train[i==ytrain] =  ActivityLabels$V2[i] }

ActivityLabels_test = subtest
for (i in ActivityLabels$V1) { ActivityLabels_test[i==ytest] =  ActivityLabels$V2[i] }


# 2: getting features with mean and std. Took mean and std before to store fewer numbers.
mean_ind<-grep("mean()",features$V2) # did not use fixed = TRUE to include mean of frequency data also.
std_ind<-grep("std()",features$V2)

# 1a: creating test data
## added 4 new columns to quantify subject, Activity, ActivityLabel, and dataType (test/train)
data_test<- data.frame(subtest,ytest,ActivityLabels_test,
                       rep("test", times = dim(ytest)[1]),xtest[,mean_ind],
                       xtest[,std_ind])
# 4. adding column names
colnames(data_test)<-c("subject","Activity","ActivityLabel"
                       ,"dataType",features$V2[mean_ind],
             features$V2[std_ind])

# 1b: creating train data
data_train<- data.frame(subtrain,ytrain,ActivityLabels_train,
                        rep("train", times = dim(ytrain)[1]),
                        xtrain[,mean_ind],
                       xtrain[,std_ind])
# 4. adding column names
colnames(data_train)<-c("subject","Activity","ActivityLabel"
                        ,"dataType",features$V2[mean_ind],
                       features$V2[std_ind])
# 1: Merging data sets data
data_all<-rbind(data_test,data_train) # dataset with test and train data combined.
#write.table(data_all,"TidyActivityData.txt",row.name=FALSE)
# taking aggregate
aggdata <-aggregate(data_all, by=list(subject = data_all$subject , Activity =  data_all$Activity,
                                      ActivityLabel =  data_all$ActivityLabel,
                                      dataType = data_all$dataType), 
                                      FUN=mean,rm.na = T)
# removing redundant columns
aggdata<-aggdata[,-c(5:8)]
write.table(aggdata,"TidyAveragedActivityData.txt",row.name=FALSE)

# read using 
# data_agg<- read.table('TidyAveragedActivityData.txt',header=TRUE)

