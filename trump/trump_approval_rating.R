####################################################################
#                   Trump Approval Rating                          #  
#                     21st October 2017                            #  
#                         Jill Daly                                #
####################################################################

# Check for Packages that need to be installed & loaded
for (package in c("RCurl", "XML", "ggplot2")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


# Scrape table from site
trumpPollUrl <- getURL("https://www.realclearpolitics.com/epolls/other/president_trump_job_approval-6179.html")


# Strip out the mobile labels so that they don't get joined to the poll names
trumpPollUrl <- gsub("<a class=\"mobile.*?</a>", "", trumpPollUrl)


# Read into data frame, specifying the table number and data types, 
# as well as setting stringsAsFactors to flase
fullPollTable <- readHTMLTable(trumpPollUrl, which=4, stringsAsFactors = FALSE,
                    colClasses = c("character", "character", "character", 
                                   "numeric", "numeric", "character"))

# convert the 'Tie' values to 0 so that we can convert the spread col to numeric
fullPollTable$Spread <- as.numeric(gsub("Tie","0",fullPollTable$Spread))


# Split the date range to get a  start date vector and add this to our data frame
# note: this col is never used, but was asked for in the assignment desc.
startDate <- vapply(strsplit(as.character(fullPollTable$Date), " - "), `[`, 1, FUN.VALUE=character(1))
fullPollTable[["Start"]] <- as.Date(paste(startDate, "17", sep="/"), format="%m/%d/%y")


# Split the date range into end date Vector, and reverse the order for our date cleaning
endDate <- vapply(strsplit(as.character(fullPollTable$Date), " - "), `[`, 2, FUN.VALUE=character(1))
endDate <- rev(as.Date(paste(endDate, "17", sep="/"), format="%m/%d/%y"))


# Clean our new Date vector by calculate the earliest possible date that each report/poll could 
# have been actually have been recieved (so that the ordering in the table is meaningful)
accurateDate = sapply(seq_along(endDate), function(i) { 
  if (i > 1) {
    if (endDate[[i]] < endDate[[i-1]]) {
      replace(endDate, i, as.Date(endDate[[i-1]]))
      return(as.Date(endDate[[i-1]]))
    } 
  }
  return(as.Date(endDate[[i]]))
})


# Remove unused Columns :: 2 = Date (Range), 3 = Sample, 7 = Start
fullPollTable <- fullPollTable[c(-2,-3,-7)]


# Convert our clean list of End Dates back to a single vector of dates
fullPollTable[["Date"]] <- rev(as.Date(unlist(accurateDate),origin="1970-01-01"))


# Remove RCP row
fullPollTable <- fullPollTable[!(fullPollTable$Poll=="RCP Average"),]


# Function which takes a date our original polling dateframe, and returns a list
# of the Date, Approval, Disapproval, Spread values for the given date
pollDateRow <- function(pollDate, pollDF) {
  
  pollDateDF <- pollDF[pollDF$Date <= as.Date(pollDate),]
  
  pollMeans <- colMeans(Filter(is.numeric,
                    head(pollDateDF[!(
                      duplicated(pollDateDF$Poll, fromLast = FALSE)
                      ), ], 11)))
  
  return(list("Date"=pollDate, 
              "Approve"=pollMeans[[1]], 
              "Disapprove"=pollMeans[[2]], 
              "Spread"=pollMeans[[3]]))
}


# Create a Data Frame for the dates and statisitics that we're interested in
meanPollDF <- data.frame(stringsAsFactors=FALSE)
dates <- seq(as.Date("2017-01-27"), Sys.Date(), by=1)
for (i in seq_along(dates)){
  meanPollDF <- rbind(meanPollDF, pollDateRow(dates[[i]],fullPollTable))
}


# convert the numeric date vector back to a date vector
meanPollDF$Date <- as.Date(meanPollDF$Date,origin="1970-01-01")


# do a quick visual check to see if the chart matches the 
ggplot(data=meanPollDF)+
  geom_line(aes(x=Date,y=Approve),colour="black") +
  geom_line(aes(x=Date,y=Disapprove),colour="red")


# Clean up the env, remove the all onjects/data except for the resulting dataframe
rm(fullPollTable, accurateDate,dates, endDate, i, startDate, package, pollDateRow, trumpPollUrl)
