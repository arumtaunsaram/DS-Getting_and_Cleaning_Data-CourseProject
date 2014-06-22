run_analysis = function () {

	if(exists("records")) {
		rm("records")
	}

	# Loads column names
	variable_names <- read.table("UCI HAR Dataset/features.txt")
	
	# Loads data
	test <- read.table("UCI HAR Dataset/test/X_test.txt")
	train <- read.table("UCI HAR Dataset/train/X_train.txt")
	
	# Combines them
	consolidated <- rbind(test, train)
	# Names columns
	colnames(consolidated) <- variable_names[,2]
	
	# Drop unnecessary columns
	limited <- consolidated[,grepl("mean\\(\\)|std\\(\\)",names(consolidated))]
	
	# Loads and merge subject id for each rows
	test.subject <- read.table("UCI HAR Dataset/test/y_test.txt")
	train.subject <- read.table("UCI HAR Dataset/train/y_train.txt")
	subjectids <- rbind(test.subject, train.subject)
	
	# Appends the subject class IDs.
	limited.w.subjectid <- cbind(limited, subjectids)
	
	# Loads associations between subject ID and activity names.
	activities <- read.table("UCI HAR Dataset/activity_labels.txt")
	limited.w.activities <- merge(limited.w.subjectid, activities, by.x="V1", by.y="V1", all=TRUE)
	
	
	ordered <- limited.w.activities[order(limited.w.activities$V1),]
	tmp <- split(limited.w.activities, ordered$V2)
	#print(names(tmp))
	# Iterates each activity
	for (dname in names(tmp)) {
		#str(df)
		#print(class(df))
		df <- tmp[[dname]]
		# Iterates each activity data.
		for (cname in names(df)) {
			if (exists("columns") && cname == "V2") {
				#print("Handling V2, df$V2:")
				#print(df$V2[1])
				columns = cbind(columns, dname)
			} else if (exists("columns")) {
				columns = cbind(columns, mean(df[[cname]]))
			} else {
				columns = c(mean(df[[cname]]))
			}
		}
		#print(columns)
		#print("number of clumns")
		print(nrow(columns))
		if (exists("records")) {
			records <- rbind(records, columns)
		} else {
			records <- columns
		}
		rm("columns")
	}
	colnames(records) <- names(limited.w.activities)
	
	# Drops unnecessary columns.
	tidy <- records[, !(colnames(records) %in% c("V1"))]
	# Renames the column name, V2, into ActivityName
	colnames(tidy)[which(names(tidy) == "V2")] <- "ActivityName"
	print(tidy)
	write.csv(tidy, file="result.csv")
}
run_analysis()