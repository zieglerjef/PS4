##########################
### Scraping Wikipedia ###
##########################

# load libraries
library(htmltab)

# load wikipedia page with election table
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

# by using htmltab, it removes the span from the table, which would require much more code to clean if I used rvest
electionResults <- htmltab(doc = wikiURL, which = "//th[text() = 'Year']/ancestor::table")

# rename columns of electionResults
colnames(electionResults) <- c("Number","Year","Winner","Winner Party",
                               "Winner Pop. vote (%)","Win Margin (%)","Winner Pop. vote (total)",
                               "Win Margin (total)","Runner-up","Runner-up Party",
                               "Turnout")

# remove % sign from entries
electionResults$"Winner Pop. vote (%)" <- as.numeric(gsub("%", "", as.character(electionResults$"Winner Pop. vote (%)")))
electionResults$"Win Margin (%)" <- gsub("%", "", as.character(electionResults$"Win Margin (%)"))
electionResults$Turnout <- as.numeric(gsub("%", "", as.character(electionResults$Turnout)))


##############
### Plots ####
##############

plotFunction <- function(destination=NULL, filename=NULL, dataSet, pdfHeight, pdfWidth){
### figure 1: Popular Vote (%) of Winning Presidential Candidates ###

# load grid package to arrange plots
library(gridExtra)
# set working directory
setwd(destination)
# begin pdf file
pdf(file=filename, h=pdfHeight, w=pdfWidth)

# generate plot
plot(as.numeric(dataSet$Year), dataSet$"Winner Pop. vote (%)",
     # axes limits, point type
     ylim=c(0, 100), xlim=c(1820, 2030), pch=19, axes=F,
     # generate labels
     xlab="Year", ylab="Popular vote share (%)", main="Fig 1: Popular Vote (%) of Winning Presidential Candidates",
     # fill point with color of winner party
     col=ifelse(dataSet$"Winner Party" == "D.-R.", "green",
               ifelse(dataSet$"Winner Party" == "Rep.", "red",
                      ifelse(dataSet$"Winner Party" == "Dem.", "blue", "gold"))))
# create axes and abline placed at 50%
axis(1, at=seq(1824, 2016, by=8)); axis(2)
abline(50, 0, lty=2)
# add labels to points
text(as.numeric(dataSet$Year), dataSet$"Winner Pop. vote (%)",
     dataSet$"Winner", cex=0.6, pos=4, col="black")

# create legend
legend("topright",
       legend=c("Democrat-Republican", "Democrats", "Republicans", "Whig"),
       pch=19, bty = "n",
       col=c("green", "blue", "red", "gold"), cex=0.8)

### figure 2: Voter turnout (%) by year and presidential winner party ###

# generate plot
plot(as.numeric(dataSet$Year), dataSet$Turnout,
     # axes limits, point type
     ylim=c(0, 100), xlim=c(1820, 2030), pch=19, axes=F,
     # generate labels
     xlab="Year", ylab="Popular vote share (%)", main="Fig 2: Voter turnout (%)",
     # fill point with color of winner party
     col=ifelse(dataSet$"Winner Party" == "D.-R.", "green",
                ifelse(dataSet$"Winner Party" == "Rep.", "red",
                       ifelse(dataSet$"Winner Party" == "Dem.", "blue", "gold"))))
# create axes and abline placed at 50%
axis(1, at=seq(1824, 2016, by=8)); axis(2)
abline(50, 0, lty=2)
# add labels to points
text(as.numeric(dataSet$Year), dataSet$Turnout,
     dataSet$"Winner", cex=0.6, pos=4, col="black")

# create legend
legend("topright",
       legend=c("Democrat-Republican", "Democrats", "Republicans", "Whig"),
       pch=19, bty = "n",
       col=c("green", "blue", "red", "gold"), cex=0.8)

# close pdf file
dev.off()
}

plotFunction(destination = "~/Google Drive/WashU/Spring2016/appliedStats",
             filename = "plotOutput.pdf", 
             dataSet = electionResults, pdfHeight = 9, pdfWidth = 15)

##########################
### GRAD STUDENTS ONLY ###
##########################

# load wikipedia page with election table
wikiURL2 <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

# by using htmltab, it removes the span from the table, which would require much more code to clean with rvest
electoralCollege <- htmltab(doc = wikiURL2, which = "//th[text() = 'Order']/ancestor::table")

# make Order variable numeric
electoralCollege$Order <- as.numeric(gsub("[[:alpha:]]*", "", electoralCollege$Order))

# rename variable to merge with electionResults dataframe
names(electoralCollege)[names(electoralCollege)=="Order"] <- "Number"

# create new variable with electoral votes for winner
electoralCollege$WinnerEC <- str_extract(electoralCollege$Winner, "[0-9]{1,3}")

# create new variable with electoral votes for all the other major candidates
electoralCollege$RunnerUpEC <- str_extract_all(electoralCollege$"Other major candidates",
                                                    "\\(?[0-9,.]+\\)?")

# create new variable with party of winner
electoralCollege$WinnerParty <- gsub("[\\(\\)]", "", regmatches(electoralCollege$Winner,
                                  gregexpr("\\(.*?\\)", electoralCollege$Winner)))

# create new variable with party of other major candidates
electoralCollege$RunnerParty <- regmatches(electoralCollege$"Other major candidates",
                                  gregexpr("(?<=\\().*?(?=\\))",
                                  electoralCollege$"Other major candidates", perl=T))

# join dataframes
combinedElections <- join(electionResults, electoralCollege, by="Number")

# Saving as .RData file
save("combinedElections", file = "problemSet4JZ.Rdata")