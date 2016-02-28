##########################
### Scraping Wikipedia ###
##########################

# load libraries
library(htmltab)

# load wikipedia page with election table
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

# by using htmltab, it removes the span from the table, which would require much more code to clean
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

setwd("~/Google Drive/WashU/Spring2016/appliedStats")
pdf(file="presidentialWinners.pdf", h=6, w=10)

# generate plot
plot(as.numeric(electionResults$Year), electionResults$"Winner Pop. vote (%)",
     ylim=c(20, 80), xlim=c(1820, 2030), pch=19, axes=F,
     # generate labels
     xlab="Year", ylab="Popular vote share (%)", main="Fig 1: Popular Vote (%) of Winning Presidential Candidates",
     # fill point with color of winner party
     col=ifelse(electionResults$"Winner Party" == "D.-R.", "green",
               ifelse(electionResults$"Winner Party" == "Rep.", "red",
                      ifelse(electionResults$"Winner Party" == "Dem.", "blue", "gold"))))
# create axes and abline placed at 50%
axis(1, at=seq(1824, 2016, by=8)); axis(2)
abline(50, 0, lty=2)
# add labels to points
text(as.numeric(electionResults$Year), electionResults$"Winner Pop. vote (%)",
     electionResults$"Winner", cex=0.6, pos=4, col="black")

# create legend
legend("topright",
       legend=c("Democrat-Republican", "Democrats", "Republicans", "Whig"),
       pch=19, bty = "n",
       col=c("green", "blue", "red", "gold"), cex=0.8)

dev.off()

##########################
### GRAD STUDENTS ONLY ###
##########################

# load wikipedia page with election table
wikiURL2 <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

# by using htmltab, it removes the span from the table, which would require much more code to clean
electoralCollege <- htmltab(doc = wikiURL2, which = "//th[text() = 'Order']/ancestor::table")
