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

# Translate provided headers into human readable headers
translateColumnLabels <- function(label) {
  label <- gsub("BodyBody", "Body", label);
  sLabel <- strsplit(label, "-");
  
  if (label == sLabel) {
    return(label);
  }
  
  component <- switch(sLabel[[1]][2],
    "std()"="STD_DEV",
    "mean()"="MEAN"
  );
  
  domain <- switch(substring(sLabel[[1]][1], 1, 1),
    t="TIME",
    f="FREQ"
  );

  measure = switch(substring(sLabel[[1]][1], 2),
    BodyAcc="BODY_LIN_ACC",
    GravityAcc="GRAVITY_LIN_ACC",
    BodyAccJerk="BODY_JERK_ACC",
    BodyGyro="BODY_ANG_VELOCITY",
    BodyGyroJerk="BODY_JERK_ANG_VELOCITY",
    BodyAccMag="BODY_LIN_ACC_MAGNITUDE",
    GravityAccMag="GRAVITY_ACC_MAGNITUDE",
    BodyAccJerkMag="BODY_JERK_ACC_MAGNITUDE",
    BodyGyroMag="BODY_ANG_VELOCITY_MAGNITUDE",
    BodyGyroJerkMag="BODY_JERK_ANG_VELOCITY_MAGNITUDE"
  );
  
  if (length(sLabel[[1]]) > 2) {
    lbl <- paste(component, measure, sLabel[[1]][3], domain, sep="_") 
  }
  else {
    lbl <- paste(component, measure, domain, sep="_") 
  }
  lbl
}

run_analysis <- function(resultFile) {
  testDT <- loadAnnotatedDataSet('test');
  trainDT <- loadAnnotatedDataSet('train');
  DT <- rbind(testDT, trainDT);
  DT;
  
  DTTidy <- DT %>% group_by(subject, activity) %>% summarise_each(funs(mean))
  c <- colnames(DTTidy)
  setnames(DTTidy, c, sapply(c, translateColumnLabels, simplify=array, USE.NAMES=FALSE))
  write.table(DTTidy, resultFile, sep = ",", row.names = FALSE)
}
