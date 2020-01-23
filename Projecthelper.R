##########################
##                      ##
##    HELPER FOR        ##
##    PROJECT WORK      ##
##                      ##
##########################


#getwd()
#settin working directory
setwd("/Users/Lahtinen/FoDS_SS2019/Project_work")

#reading the data set
#vote92 <- read.csv("vote92.csv", sep = ",", header = T)

getVote92 <- function(file = "/Users/Lahtinen/FoDS_SS2019/Project_work/vote92.csv"){
  vote92 <- read.csv("vote92.csv", sep = ",", header = T)


#IDENTIFYIING MISSING VALUES
apply(vote92, 2, function(g) sum(is.na(g)))
colSums(vote92 == "")

#no missing values

#CONVERTING VOTE TO BINARY
vote92$vote1 <- as.numeric(as.factor(vote92$vote))

# SKEWED DISTRIBUTIONS
# cube root transformation
vote92$cr.clintondis <- vote92$clintondis^(1/3)
vote92$cr.bushdis <- vote92$bushdis^(1/3)
vote92$cr.perotdis <- vote92$perotdis^(1/3)

#Creating a new variable voteclinton from vote 1
vote92$voteclinton <- vote92$vote1

vote92$voteclinton[vote92$voteclinton == 1] <- 0
vote92$voteclinton[vote92$voteclinton == 3] <- 0
vote92$voteclinton[vote92$voteclinton == 2] <- 1

#creating a new variable, other, when respondent is not republican or democratic

vote92$other <- vote92$dem + vote92$rep
vote92$other <- vote92$other - 1
vote92$other[vote92$other == -1] <- 1

return(vote92)
}
