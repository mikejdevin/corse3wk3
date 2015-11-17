
## find variable names and cols
featurelist <- read.table("features.txt")
allvarnames <- as.character(featurelist[,2])
wheremeans <- grep("mean",v2)
wherestds <- grep("std",v2)
thedroidswerelookingfor <- sort(append(wheremeans,wherestds))
myvarnames <- allvarnames[thedroidswerelookingfor]

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
colnames(reducedtable) <- myvarnames

## get action labels
actylabs <- read.table("activity_labels.txt")

##swap in the labels

subbed <- actylabs[ya[,1],2]

## add activities to data
datastep4 <- cbind(reducedtable,subbed)
colnames(datastep4)[length(datastep4)] <- "activity"

## subject id numbers
ids <- read.table("test/subject_test.txt")
idt <- read.table("train/subject_train.txt")
ida <- rbind(idt,ids)
colnames(ida) <- "subjecs_id"

## add subjects to data
datastep4 <- cbind(xa,ida)

## step 5 n 2 tables, one for activities, one for subjects
d1 <- length(actylabs[,1])
d2 <- length(reducedtable[1,])
meansbyactivity <- data.frame(matrix(ncol = d2, nrow = d1))
for ( i in 1:d1) {
  activia <- ( ya == i )
  meansbyactivity[i,] <- colMeans(reducedtable[ activia ,],na.rm=TRUE )
}
d3 <-max(ida)
meansbyguys <- data.frame(matrix(ncol = d2, nrow = d3))
for ( m in 1:d3) {
  guy <- ( ida == m )
  meansbyguys[m,] <- colMeans( reducedtable[ guy, ],na.rm=TRUE)
  }

table5 <- rbind( meansbyactivity,meansbyguys )
colnames(table5) <- myvarnames

write.table(table5,"table5.txt",row.names = FALSE)