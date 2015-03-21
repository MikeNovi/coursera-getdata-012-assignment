library(data.table)
library(dplyr)

# Loads the data of a set specified by the set variable
# and adds the feature name as header as the column names
loadSetWithLabels <- function(setName) {
  fn <- paste("data", "/", setName, "/", "X_", setName, ".txt", sep="");
  tmpFrame <- read.table(fn);
  dt <- data.table(tmpFrame);
  
  # Load the labels (clean out bad headers)
  labelMap <- read.table("data/features.txt");
  labelMap <- within(labelMap, V2 <- gsub("BodyBody", "Body", V2))
  setnames(dt, as.vector(labelMap[,1]), as.vector(labelMap[,2]));
  
  # Remove all columns that are not mean or standard devations
  removeColumns <- grep("(\\-mean\\(\\)|\\-std\\(\\))", labelMap$V2, invert=TRUE)
  dt <- dt[,(removeColumns):=NULL]
  
  dt;
}

# Adds the test subject numerical designation as a column to 
# the specified and provided data set
addSubjectColumn <- function(setName, dataSet) {
  fn <- paste("data", "/", setName, "/subject_", setName, ".txt", sep="");
  subjects <- read.table(fn);
  
  dataSet[,subject:=as.vector(subjects[,1])];
  dataSet;
}

# Adds the numerical identifier indicating activity as a column
# to the specified and provided data set
addClassColumn <- function(setName, dataSet) {
  fn <- paste("data", "/", setName, "/y_", setName, ".txt", sep="");
  activities <- read.table(fn);
  
  dataSet[,activity:=as.vector(activities[,1])];
  dataSet;
}

# Replaces the numerical identifier indicating activity in the
# data set with a human readable string
mapActivityNames <- function(dataSet) {
  map <- read.table("data/activity_labels.txt");
  dataSet[,activity:=as.character(map[activity,'V2'])]
  dataSet
}

# Loads the annotated data set. Adding headers, subjects, and add the activity names
loadAnnotatedDataSet <- function(setName) {
  ds <- loadSetWithLabels(setName);
  ds <- addSubjectColumn(setName, ds);
  ds <- addClassColumn(setName, ds);
  ds <- mapActivityNames(ds);
  ds
}

run_analysis <- function(resultFile) {
  testDT <- loadAnnotatedDataSet('test');
  trainDT <- loadAnnotatedDataSet('train');
  DT <- rbind(testDT, trainDT);
  DT;
  
  DTTidy <- DT %>% group_by(subject, activity) %>% summarise_each(funs(mean))
  write.table(DTTidy, resultFile, sep = ",", row.names = FALSE)
}
