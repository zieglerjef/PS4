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
