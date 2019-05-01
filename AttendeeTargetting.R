AttendeeDetails <- read.csv("D:/AzureML/Datasets/AttendeeDetails.csv")
Attendeedf <- as.data.frame(AttendeeDetails)
str(Attendeedf)
attach(AttendeeDetails)
Attendeedf$EventStartDate <- as.Date(Attendeedf$EventStartDate, '%Y-%m-%d')
colnames(Attendeedf) <- c("AttendeeKey_SK","FirstName","EmailAddress","EventName","EventStartDate","Title")

View(Attendeedf)

# How many attendees on the whole 

aid <- Attendeedf[!duplicated(Attendeedf[,"AttendeeKey_SK"]),]
View(aid)
dim(aid)
# endDate <- Sys.Date()

# function for recency and frequency
getDataFrame <- function(Attendeedf,tIDColName="AttendeeKey_SK",tDateColName="EventStartDate"){
  #order the dataframe by date descendingly
  Attendeedf <- Attendeedf[order(Attendeedf[,tDateColName],decreasing = TRUE),]
  
#   #remove the record before the start data and after the end Date
#   Attendeedf <- Attendeedf[df[,tDateColName]>= startDate,]
#   Attendeedf <- Attendeedf[df[,tDateColName]<= endDate,]
  
  #remove the rows with the duplicated IDs, and assign the df to a new df.
  newdf <- Attendeedf[!duplicated(Attendeedf[,tIDColName]),]
  
  # caculate the Recency(days) to the endDate, the smaller days value means more recent
  Recency<-as.numeric(difftime(Sys.Date(),newdf[,tDateColName],units="days"))
  
  # add the Days column to the newdf data frame
  newdf <-cbind(newdf,Recency)
  
  #order the dataframe by ID to fit the return order of table() and tapply()
  newdf <- newdf[order(newdf[,tIDColName]),]
  
  # caculate the frequency
  fre <- as.data.frame(table(Attendeedf[,tIDColName]))
  Frequency <- fre[,2]
  newdf <- cbind(newdf,Frequency)
  
#   #caculate the Money per deal
#   m <- as.data.frame(tapply(df[,tAmountColName],df[,tIDColName],sum))
#   Monetary <- m[,1]/Frequency
#   newdf <- cbind(newdf,Monetary)
  
  return(newdf)
  
} # end of function getDataFrame

Attendeedf <- getDataFrame(Attendeedf)
View(Attendeedf)

#Scoring 

scoring <- function (df,column,r=5){
  
  #get the length of rows of df
  len <- dim(df)[1]
  
  score <- rep(0,times=len)
  
  # get the quantity of rows per 1/r e.g. 1/5
  nr <- round(len / r)
  if (nr > 0){
    
    # seperate the rows by r aliquots
    rStart <-0
    rEnd <- 0
    for (i in 1:r){
      
      #set the start row number and end row number
      rStart = rEnd+1
      
      #skip one "i" if the rStart is already in the i+1 or i+2 or ...scope.
      if (rStart> i*nr) next
      
      if (i == r){
        if(rStart<=len ) rEnd <- len else next
      }else{
        rEnd <- i*nr
      }
      
      # set the Recency score
      score[rStart:rEnd]<- r-i+1
      
      # make sure the customer who have the same recency have the same score
      s <- rEnd+1
      if(i<r & s <= len){
        for(u in s: len){
          if(df[rEnd,column]==df[u,column]){
            score[u]<- r-i+1
            rEnd <- u
          }else{
            break;
          }
        }
        
      }
      
    }
    
  }
  return(score)
  
} #end of function Scoring

#function to score recency and frequency

getIndependentScore <- function(Attendeedf,r=5,f=5) {
  
  if (r<=0 || f<=0) return
 
  #order and the score
  
  Attendeedf <- Attendeedf[order(-Attendeedf$Frequency,Attendeedf$Recency),]
  F_Score <- scoring(Attendeedf,"Frequency",f)
  Attendeedf <- cbind(Attendeedf, F_Score)
  
  Attendeedf <- Attendeedf[order(Attendeedf$Recency,-Attendeedf$Frequency),]
  R_Score <- scoring(Attendeedf,"Recency",r)
  Attendeedf <- cbind(Attendeedf, R_Score)
  
  
  #order the dataframe by R_Score, F_Score, and M_Score desc
  Attendeedf <- Attendeedf[order(-Attendeedf$F_Score,-Attendeedf$R_Score),]
  
  # caculate the total score
  Total_Score <- c(10*Attendeedf$F_Score + 1*Attendeedf$R_Score)
  
  Attendeedf <- cbind(Attendeedf,Total_Score)
  
  return (Attendeedf)
  
} # end of function getIndependentScore

Attendeedf1 <-getIndependentScore(Attendeedf)
View(Attendeedf1)

require(histogram)

# draw histogram
Attendeetop <- Attendeedf1[order(Attendeedf1[,"Total_Score"],decreasing = TRUE),]

View(Attendeetop)

FinalAttendee <- Attendeetop[1:1000,]
FinalAttendee$Recency<-NULL
FinalAttendee$Frequency <- NULL
FinalAttendee$R_Score <- NULL
FinalAttendee$F_Score<- NULL
FinalAttendee$Total_Score <- NULL
FinalAttendee$Title <- NULL
FinalAttendee$EventName<- NULL
FinalAttendee$EventStartDate<- NULL



View(FinalAttendee)





