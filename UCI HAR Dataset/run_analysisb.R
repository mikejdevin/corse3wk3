
## find variable names and cols
featurelist <- read.table("features.txt")
v2 <- as.character(featurelist[,2])
wheremeans <- grep("mean",v2)
wherestds <- grep("std",v2)
thedroidswerelookingfor <- sort(append(wheremeans,wherestds))
varnames <- v2[thedroidswerelookingfor]

## combine test and train tables
xr <- read.table("train/X_train.txt")
yr <- read.table("train/y_train.txt")
xs <- read.table("test/X_test.txt")
xa <- rbind(xr,xs)
ys <- read.table("test/y_test.txt")
ya <- rbind(yr,ys)

## filter features for what we want
reducedtable <- subset(xa, subset = TRUE , select = thedroidswerelookingfor)
## name them
colnames(reducedtable) <- varnames

## get action labels
actylabs <- read.table("activity_labels.txt")

##swap in the labels
subbed <- actylabs[ya[,1]]
colnames(ya) <- "activity"

## add activities to data
datastep4 <- cbind(xa,subbed)

## subject id numbers
ids <- read.table("test/subject_test.txt")
idt <- read.table("train/subject_train.txt")
ida <- append(idt,ids)
colnames(ida) <- "subjecs_id"

## add subjects to data
datastep4 <- cbind(xa,ida)

## new data set is list of tables
numguys <- max(ida,na.rm=TRUE)
finaldata <- list(numguys)

for ( i in 1:numguys ) {
  minerownums <- ( ida = i )
  minerows <- reducedtable[minerownums,]
  d1 <- length(actylabs)
  d2 <- length(varnames)
  mineavg <- array(d1*d2, dim = c(d1,d2) )
  
  for ( j in  1:d1 ) {
        mineavg[j,] <- colMeans(minerows)
  }
  colnames(mineavg) <- varnames
  rownames(mineavg) <- actylabs
  finaldata[[i]] <- mineavg
}


