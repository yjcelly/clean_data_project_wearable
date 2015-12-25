setwd("F:\\coursera\\clean_data\\project\\clean_data_project_wearable")

if(!dir.exists("UCI HAR Dataset"))
{
	zipName <- "getdata_projectfiles_UCI HAR Dataset.zip"
	fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	download.file(fileUrl,destfile=zipName,method="curl")
	unzip(zipName)
}


activity_lab<- read.table("UCI HAR Dataset\\activity_labels.txt", header=FALSE)


subject_train <- read.table("UCI HAR Dataset\\train\\subject_train.txt", header=FALSE)
subject_test <- read.table("UCI HAR Dataset\\test\\subject_test.txt", header=FALSE)
names(subject_train)<-c("subject")
names(subject_test)<-c("subject")


test_x<-read.table("UCI HAR Dataset\\test\\X_test.txt")
test_y<-read.table("UCI HAR Dataset\\test\\y_test.txt")
train_x<-read.table("UCI HAR Dataset\\train\\X_train.txt")
train_y<-read.table("UCI HAR Dataset\\train\\y_train.txt")
feat_info<-read.table("UCI HAR Dataset\\features.txt")


test_y$V1=activity_lab[test_y$V1,2]
train_y$V1=activity_lab[train_y$V1,2]


names(train_x)<-feat_info[,2]
names(test_x)<-feat_info[,2]
names(train_y)<-c("activity_lab")
names(test_y)<-c("activity_lab")


data_train<-cbind(train_x,train_y,subject_train)
data_test<-cbind(test_x,test_y,subject_test)

data_set<-rbind(data_train,data_test)


copy_data_set <- data_set
activity_kind_tab <- table(copy_data_set[,562])
subject_king_tab <- table(copy_data_set[,563])
m <- matrix(nrow=dim(activity_kind_tab)*dim(subject_king_tab),ncol=563)
out_avg <- data.frame(m)

names(out_avg)[1] <- names(copy_data_set)[562]
names(out_avg)[2] <- names(copy_data_set)[563]
names(out_avg)[3:563] <- names(copy_data_set)[1:561]

for(i in 1:dim(activity_kind_tab))
{
	for(j in 1:dim(subject_king_tab))
	{
		index <- (i-1) * dim(subject_king_tab) + j
		datatmp <-	copy_data_set[(copy_data_set$activity_lab == names(activity_kind_tab)[i]) & (copy_data_set$subject == names(subject_king_tab)[j]),]

		if( (dim(datatmp)[1] >1 ) &&  (dim(datatmp)[2] >1 )  )
		{
			out_avg[index,3:563] <- format(colMeans(datatmp[,1:561]),scientific=TRUE,digit=6)
			out_avg[index,1] <- names(activity_kind_tab)[i]
			out_avg[index,2] <- names(subject_king_tab)[j]
		}
	}
}

write.table(out_avg,"data.txt",row.name=FALSE,quote = FALSE)
