
## Bits to Change ##

# Set working directory
setwd("/Users/danielpett/Documents/research/micropasts/analysis/eesAbydosTombs/") #MacOSX


projects <- c('abydosTombLMN','abydosTombA','abydosTombB','abydosTombC','abydosTombD','abydosTombE','abydosTombF',
              'abydosTombG','abydosTombK','abydosTombO','abydosTombP','abydosTombR','abydosTombT','abydosTombU',
              'abydosTombW','abydosTombX','abydosTombY','abydosTombZ'
)
print(length(projects))
for(a in projects) {
  # Set the project name
  project <- a
  # ID of good users to prioritse or bad ones to ignore
  superusers <- c(433,580,64,226,473,243) # rank order, best first
  ignoreusers <- c(652,677)
  # Create three working sub-directories if they do not exist
  if (!file.exists('csv')){ dir.create('csv') }
  if (!file.exists('csv/consolidated')){ dir.create('csv/consolidated') }
  if (!file.exists('csv/raw')){ dir.create('csv/raw') }
  if (!file.exists('archives')){ dir.create('archives') }
  if (!file.exists('json')){ dir.create('json') }
  
  ## End of Bits to Change ##
  
  # Load user data
  #http://crowdsourced.micropasts.org/admin/users/export?format=csv (when logged in as admin)
  users <- read.csv("csv/all_users.csv", header=TRUE)
  users <- users[,c("id","fullname","name")]
  
  # Set the base url of the application
  baseUrl <- 'http://crowdsourced.micropasts.org/project/'
  # Set the task runs api path
  tasks <- '/tasks/export?type=task&format=json'
  # Form the export url
  url <- paste(baseUrl,project, tasks, sep='')
  print(url)
  archives <- paste('archives/',project,'Tasks.zip', sep='')
  print(archives)
  # Import tasks from json, this method has changed due to coding changes by SciFabric to their code
  download.file(url,archives)
  unzip(archives)
  taskPath <- paste('json/', project, '.json', sep='')
  rename <- paste(project, '_task.json', sep='')
  file.rename(rename, taskPath)
  
  # Read json files
  which(lapply(readLines(taskPath), function(x) tryCatch({jsonlite::fromJSON(x); 1}, error=function(e) 0)) == 0)
  trT <- fromJSON(paste(readLines(taskPath), collapse=""))
  trT <- cbind(trT$id,trT$info)
  trTfull <- trT
  
  # extract just task id and image URL, title
  trT <- trT[,c(1,4,6)]
  names(trT) <- c("taskID","imageURL", "imageTitle")
  
  # Import task runs from json
  taskruns <- '/tasks/export?type=task_run&format=json'
  urlRuns <- paste(baseUrl,project, taskruns, sep='')
  print(urlRuns)
  archiveRuns <-paste('archives/', project, 'TasksRun.zip', sep='')
  download.file(urlRuns,archiveRuns)
  unzip(archiveRuns)
  taskruns <- paste('json/', project, '_task_run.json', sep='')
  renameRuns <-paste(project, '_task_run.json', sep='')   
  
  file.rename(renameRuns, taskruns)
  which(lapply(readLines(taskruns), function(x) tryCatch({jsonlite::fromJSON(x); 1}, error=function(e) 0)) == 0)
  trTr <- fromJSON(paste(readLines(taskruns), collapse=""))
  
  # Re-arrange slightly and drop some columns
  trTr <- cbind(trTr$info,trTr$user_id,trTr$task_id)
  names(trTr)[length(names(trTr))] <- "taskID"
  names(trTr)[length(names(trTr))-1] <- "userID"
  
  # Sort by user ID then by task ID
  trTr <- trTr[with(trTr, order(taskID, userID)), ]
  
  # Add user credit
  tsks <- unique(as.character(trTr$taskID))
  credits <- data.frame(taskID=character(length(tsks)),inputBy=character(length(tsks)), stringsAsFactors = FALSE) #blank df to fill
  
  for (a in 1:length(tsks)){
    atask <- trTr[trTr$taskID == tsks[a],]
    contribs <- sort(unique(as.numeric(as.character(atask$userID))))
    contribsNm <- users[users$id %in% contribs,]
    credits$taskID[a] <- tsks[a]
    credits$inputBy[a] <- paste(as.character(contribsNm$fullname), collapse="; ")
  }
  
  # Merge task summaries with image URL and user credit data.
  credurl <- merge(credits, trT, by="taskID")
  trTr <- merge(trTr,credurl, by="taskID")
  
  # Add two skipped lines between each unique index cards (i.e. between task sets).
  trTr1 <- trTr[which(is.na(trTr$taskID)), ] #blank df to fill
  newrow <- rep(NA,ncol(trTr))
  
  for (a in 1:length(tsks)){
    atask <- trTr[trTr$taskID == tsks[a],]
    trTr1 <- rbind(trTr1,atask,newrow,newrow)
  }
  
  # Annotations column not really needed, so dropped
  keeps <- c("taskID","userID","description", "reverseCard", "comments", "discoveryDate", 
             'cardNumber', 'excavatorName', 'inputBy', 'imageURL')
  trTr2 <- trTr1[,keeps,drop=FALSE]
  
  # Finally reorder the columns fo the data to something easier to refer to:
  preforder <- c(
    'taskID',"description", "reverseCard", "comments", "discoveryDate", 
    'cardNumber', 'excavatorName', 'inputBy','userID','imageURL'
  )  
  trTr2 <- trTr2[ ,preforder]
  
  # Export as raw csv file
  csvname <- paste0('csv/raw/', project, '_raw.csv')
  write.csv(trTr2, file=csvname, row.names=FALSE, na="")
  
  ## Consolidation steps ##
  trTrc <- as.data.frame(trTr,stringsAsFactors=FALSE)
  # Get rid of factors in facour of plain text
  i <- sapply(trTrc, is.factor)
  trTrc[i] <- lapply(trTrc[i], as.character)
  
  # We will loop through each task run in turn.
  # For most columns, we return the top-ranked superuser's contribution if it exists.
  # If a superuser does not exist, then we first exclude really bad users, then check for agreement between two or more other users 
  # for all columns of data. If agreement over a non-empty value exists, we take that value. Otherwise, we combine the contributions of the 
  # users and separate them by a "|". These will need a manual check.
  # We keep a note of which action was taken in MyNotes.
  # Coordinates are treated similarly but we either return the median value in the case of 3 or more task runs, the agreed value where there 
  # are only two task runs and both agree  or NA if only two task runs and they don't
  
  taskids <- sort(unique(trTrc$taskID))
  superusersdf <- data.frame(userID=superusers,Rank=1:length(superusers),stringsAsFactors=FALSE)
  trTrc$Processing <- NA
  # Loop
  for (a in 1:length(taskids)){
    cat(paste(a,"; ",sep=""))
    mydup <- trTrc[trTrc$taskID==taskids[a],]
    mydup <- mydup[!mydup$userID %in% ignoreusers,]
    if (nrow(mydup)==1){
      myrec <- mydup
      myrec$Processing <- "only good record"
    } else {
      sucheck <- superusersdf[superusersdf$userID %in% mydup$userID,]
      if (nrow(sucheck)>0){
        myrec <- mydup[mydup$userID==sucheck$userID[1] & !is.na(mydup$userID),]
        myrec$Processing <- "superuser"
      } else {
        myrec <- mydup[1,]
        myrec[] <- NA  #empty row
        myrec$taskID <- mydup$taskID[1] #identical
        myrec$imageURL <- mydup$imageURL[1] #identical
        mycols <- names(trTrc)
        
        myrec$Processing <- "merged"
      }
    }
    if (a==1){
      allrecs <- myrec
    } else {
      allrecs <- rbind(allrecs,myrec)
    }
  }
  
  allrecs$project <- project
  # Export consolidated data
  csvname <- paste0('csv/consolidated/', project, '_cons.csv')
  write.csv(allrecs, file=csvname,row.names=FALSE, na="")
}