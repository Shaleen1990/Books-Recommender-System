# Setting the working directory
setwd("C:\\Users\\manis\\Desktop\\BooksVilla\\V5")

#-------------------------------
# All required packages
#-------------------------------

# Shiny related packages
library(shiny)
require(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(ShinyRatingInput)

# Other required packages
library(tableHTML)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)

# Package for using sql related queries in R
require(sqldf)

# Packages for splitting an expression
library(splitstackshape)
library(rex)

# arules package For implementing Apriori algorithm
library(arules)
# package for implementing recommender systems
library(recommenderlab)


# Reading the data
books <- read.csv("C:\\Users\\manis\\Desktop\\Final_filtered_columns_v3.csv", nrows = 30000)

# Extracting the list of unique Genres, Authors, Titles from the data
unique_genres <- distinct(books, Genre)
unique_authors <- distinct(books, Author)
unique_books <- distinct(books, Title)


                            

#--------------------------------------------------------------------
# REUSABLE FUNCTION CODE - INDIVIDUAL BOOK RATING
#-------------------------------------------------------------------

# Calculating the average rating for a particular book in the batabase appearing multiple 
# no. of times in the database
Book_rating <- function(book_isbn)
{
  Ratings_filtered <- books %>% filter(books$ISBN == book_isbn & books$Rating != 0) %>% select(Rating)
  avg_rating <- (mean(Ratings_filtered$Rating) / 2)
  avg_rating_final <- round(avg_rating, digits = 1)
  return(avg_rating_final)
}



##############################################################
# MODELS
##############################################################

#--------------------------------------------------------------
# 1. ARM FUNCTION_CODE (ASSOCIATIVE RULES MINING)
#--------------------------------------------------------------

# For the ARM algorithm to process, any user should log in with his/hert user-id and 
# then search for any book from the pool of books available in the app.

arm4 <- function(id, isbn)
{ 
  try(
    {
      # Considering only the required columns(the ones required for creating a transaction matrix with userids as rows and ISBNs as columns and whether he bought that book or 
      # not with the rating value), eliminating the remaining columns from the data
      data <- books[, -c(4:11)]
      # Rrenaming the columns
      colnames(data) <- c("UserID","Region","ISBN","Rating")
      
      # Fetch the Region for a logged in user since we like to fetch region wise recommendations as the data is huge and sparse
      region <- fn$sqldf("select distinct(Region) from data where UserID = '$id'")
      region_name <- region[1,]
      
      # Filter region dataframe
      data_region <- data[data$Region == region_name, ]
      
      # Deleting the region column(since it's not required from now on for transaction matrix creation)
      data_region$Region <- NULL
      
      # Checking the unique #users and #books
      # Keeping R's computation power in check, we have taken the value(268435000) for 2GB ram capacity
      if(length(unique(data_region$`UserID`)) * length(unique(data_region$ISBN)) <= 268435000)
      {
        # Change the datatype of columns to factor
        data_region$`UserID` <- factor(data_region$`UserID`)
        data_region$ISBN <- factor(data_region$ISBN)
        
        # Create Transaction matrix
        trans_region <- as(split(data_region$ISBN, data_region$`UserID`), "transactions")
      }
      
      else
      {
        # Filtering data
        # (When the value- unique users*unique books exceeds the above mentioned value, then only we would like to filter the data)
        
        #---------------------------------------------------
        # Creating book Vs Ratings per user dataframe
        #---------------------------------------------------
        
        pivot <- aggregate(data_region$Rating ~ data_region$ISBN, data = data_region, FUN = length)
        # Setting the appropriate column names
        colnames(pivot) <- c("ISBN", "#Ratings")
        
        #--------------------------------------------------------------------
        # Deleting the books which are rated by less than 10 users from pivot 
        #--------------------------------------------------------------------
        
        data_region2 <- pivot[!pivot$`#Ratings` < 10,]
        
        #---------------------------------------------------------------
        # Deleting the books which has less than 10 ratings from dataframe
        #---------------------------------------------------------------
        
        join_string1 <- "select data_region.* from data_region inner join data_region2 
        on data_region.ISBN = data_region2.ISBN"
        
        data_region3 <- sqldf(join_string1,stringsAsFactors = FALSE)
        
        # Change the datatype of columns to factor
        data_region3$`UserID` <- factor(data_region3$`UserID`)
        data_region3$ISBN <- factor(data_region3$ISBN)
        
        # Removing the data_region to free up space
        rm(data_region)
        data_region <- data_region3
        
        
        # Create Transaction matrix
        trans_region <- as(split(data_region$ISBN, data_region$`UserID`), "transactions")
        
      }
      
      
      #Fitting Apriori algorithm 
      rules_region <- apriori(data = trans_region, parameter = list(supp = 0.0005, conf = 0.01, minlen=2 ,maxlen=5),
                              appearance = list(default = 'rhs', lhs = isbn), control = list(verbose = F))
      
      #Sorting the generated rules
      rules_region_sorted <- sort(rules_region, decreasing = T, by =c('lift','support'))
      
      #Selecting books from the RHS of the rules
      #Selecting only the top rule from the ARM results
      finalrules <- inspect(rules_region_sorted[0:1]@rhs)
      
      arm_results <- finalrules
      
      #Fetching individual ISBNs from the top rule
      matches <- re_matches(arm_results$items,
                            rex(
                              "{",
                              capture(name = "text", except_any_of("}")),
                              "}"),
                            global = TRUE)
      
      arm_recc_items <- c(isbn)
      
      for (i in 1:length(matches))
      {
        arm_recc_items <- c(arm_recc_items, matches[[i]]$text)
      }
      return(arm_recc_items)
      
    },silent = T)
  
}




#--------------------------------------------------------------
# GENERAL RECOMMENDATIONS FUNCTION_CODE
#--------------------------------------------------------------

# NOTE: We show General Recommendations for the user as soon as he logs in
# We take the top two genres which the user has read avidly and recommend him to buy top rated books from those genres

grm <- function(id)
{
  require(sqldf)
  
  data_grm <- books
  colnames(data_grm) <- c("UserID","Region","ISBN",
                          "Title","Author","Genre","Genre_original",
                          "Year","Publisher","URL","Image.URL.L","Rating")
  
  # Fetch the Region for a logged in user
  Region_grm <- fn$sqldf("select distinct(Region) from data_grm where UserID = '$id'")
  Regionname_grm <<- Region_grm[1,]
  
  # Filter the Region dataframe from the total database inorder to fetch region wise recommendations
  data_grm_Region <- data_grm[data_grm$Region == Regionname_grm, ]
  
  # Considering only the required columns from the region data
  details1 <- fn$sqldf("select ISBN,Genre,Rating
                       from data_grm_Region where UserID = '$id'")
  
  # Getting the number of books read by user in each Genre & sum of ratings:
  details2 <- sqldf("select Genre,sum(Rating) as rating, 
                    count(details1.ISBN) as books from details1 
                    where Genre != 'Unknown' group by Genre ")
  
  # Sorting the Number of books and taking the top two:
  details3 <- details2[order(-details2$books),][1:2,]
  
  # Creating a pivot table by taking avg ratings for books:
  pivot <- aggregate(data_grm$Rating ~ data_grm$ISBN  + data_grm$Genre, 
                     data = data_grm, FUN = mean)
  
  
  # Renaming the columns for pivot:
  colnames(pivot) <- c("ISBN","Genre","Avg_Ratings")
  
  #Extracting all the books for the Genre preferred by the user:
  final_list <- sqldf("select Genre, ISBN, Avg_Ratings from pivot where Genre in (
                      select distinct Genre from details3) order by Avg_Ratings desc")
  
  
  # Finding and creating the rank:
  a <- setDT(final_list)[,rank:= rank(final_list$Avg_Ratings), by = final_list$Genre]
  
  # Extracting the top 6 books for each Genre :
  recom <- final_list[,head(.SD,6), by = "Genre"]
  
  v <- as.list(recom$ISBN)
  grm_recc_items_list <- c()
  for(i in v)
  {
    i <- as.character(i)
    grm_recc_items_list <- append(grm_recc_items_list, i)
  }
  
  return(grm_recc_items_list)
  
}



#--------------------------------------------------------------
# 2. USER BASED COLLABORATIVE FILTERING_FUNCTION_CODE
#--------------------------------------------------------------

ubcf <- function(id)
{
  # Considering only the required columns
  data <- books[, -c(4:11)]
  colnames(data) <- c("UserID","Region","ISBN","Rating")
  
  # Fetch the Region for a logged in user
  region <- fn$sqldf("select distinct(Region) from data where UserID = '$id'")
  region_name <- region[1,]
  
  # Filter region dataframe
  data_region <- data[data$Region == region_name, ]
  
  # Deleting the region column
  data_region$Region <- NULL
  
  # Deleting the implicit rating rows(implicit ratings are the ones with '0' rating)
  data_region <- data_region[!(data_region$Rating == 0),]
  
  # Checking the unique #users and #books inorder to cope up with R's computation power
  # For smooth processing, we have considered 2GB limit(thus the value: 268435000)
  if(length(unique(data_region$`UserID`)) * length(unique(data_region$ISBN)) <= 268435000)
  {
    # Change the datatype of columns to factor
    data_region$`UserID` <- factor(data_region$`UserID`)
    data_region$ISBN <- factor(data_region$ISBN)
    
    # Create Ratings matrix
    # A matrix with userids as rows and ISBNs as columns and the rating as the values in matrix)
    rat_mat <- acast(data_region, `UserID` ~ ISBN, value.var = 'Rating', fun.aggregate = sum)
  }else
  {
    # Filtering data 
    # (When the value- unique users*unique books exceeds the above mentioned value, then only we would like to filter the data)
    
    ###############################
    # FILTER 1
    ###############################
    
    #---------------------------------------------------
    # Creating User Vs Ratings per user dataframe
    #---------------------------------------------------
    
    pivot_user <- aggregate(data_region$Rating ~ data_region$UserID, data = data_region, FUN = length)
    colnames(pivot_user) <- c("UserID", "#Ratings")
    
    #-----------------------------------------------------------------
    # Deleting the users who have rated less than 10 books from pivot 
    #-----------------------------------------------------------------
    
    pivot_user_filtered <- pivot_user[!pivot_user$`#Ratings` < 10,]
    
    #-----------------------------------------------------------------------
    # Deleting the users who have rated less than 10 books from dataframe
    #-----------------------------------------------------------------------
    
    join_string1 <- "select data_region.* from data_region inner join pivot_user_filtered 
    on data_region.UserID = pivot_user_filtered.UserID"
    
    data_region2 <- sqldf(join_string1,stringsAsFactors = FALSE)
    
    # Change the datatype of columns to factor
    data_region2$`UserID` <- factor(data_region2$`UserID`)
    data_region2$ISBN <- factor(data_region2$ISBN)
    
    # For freeing up the space
    rm(data_region)
    data_region <- data_region2
    
    
    ###############################
    # FILTER 2
    ###############################
    
    #---------------------------------------------------
    # Creating Book Vs Ratings per user dataframe
    #---------------------------------------------------
    
    pivot_book <- aggregate(data_region$Rating ~ data_region$ISBN, data = data_region, FUN = length)
    colnames(pivot_book) <- c("ISBN", "#Ratings")
    
    #----------------------------------------------------
    # Deleting the books which has less than 10 ratings
    #----------------------------------------------------
    
    pivot_book_filtered <- pivot_book[!pivot_book$`#Ratings` < 10,]
    
    #---------------------------------------------------------------------
    # Deleting the users who have rated less than 10 books from dataframe
    #---------------------------------------------------------------------
    
    join_string2 <- "select data_region.* from data_region inner join pivot_book_filtered 
    on data_region.ISBN = pivot_book_filtered.ISBN"
    
    data_region3 <- sqldf(join_string2,stringsAsFactors = FALSE)
    
    # Change the datatype of columns to factor
    data_region3$`UserID` <- factor(data_region3$`UserID`)
    data_region3$ISBN <- factor(data_region3$ISBN)
    
    # For freeing up the space
    rm(data_region)
    data_region <- data_region3
    
    # Create Ratings matrix
    # A matrix with userids as rows and ISBNs as columns and the rating as the values in matrix)
    rat_mat <- acast(data_region, `UserID` ~ ISBN, value.var = 'Rating', fun.aggregate = sum)
    
  }
  
  
  # Replacing 0's with NA's in matrix (Since we are using recommenderlab)
  rat_mat[rat_mat == 0] <- NA
  
  # Convert rat_mat into realRatingMatrix data structure
  # RealRatingMatrix is a recommenderlab sparse-matrix like data-structure
  real_rat_mat <- as(rat_mat, "realRatingMatrix")
  
  
  # Creating a recommender object (model)
  rec = Recommender(real_rat_mat[1:nrow(real_rat_mat)],
                    method = "UBCF",                    #User based collaborative filtering
                    param = list(normalize = "Z-score", method="Cosine", nn = 15)) #nn = 15 => how many similar users we want
  
  
  # 1. Rating Prediction
  pred_rating <- predict(rec, real_rat_mat[1:nrow(real_rat_mat)], type="ratings")
  
  
  # 2. Predicting Top-n item recommendations for a user
  recc_predicted <- predict(object = rec, newdata = real_rat_mat, n = 5, type="topNList")
  recc_items <- as(recc_predicted, "list")
  
  
  return(recc_items[id])
  
}



#--------------------------------------------------------------
# 3. ITEM BASED COLLABORATIVE FILTERING_FUNCTION_CODE
#--------------------------------------------------------------

ibcf <- function(id)
{
  # Considering only the required columns
  data <- books[, -c(4:11)]
  colnames(data) <- c("UserID","Region","ISBN","Rating")
  
  # Fetch the Region for a logged in user
  region <- fn$sqldf("select distinct(Region) from data where UserID = '$id'")
  region_name <- region[1,]
  
  # Filter region dataframe
  data_region <- data[data$Region == region_name, ]
  
  # Deleting the region column
  data_region$Region <- NULL
  
  # Deleting the implicit rating rows(implicit ratings are the ones with '0' rating)
  data_region <- data_region[!(data_region$Rating == 0),]
  
  # Checking the unique #users and #books inorder to cope up with R's computation power
  # For smooth processing, we have considered 2GB limit(thus the value: 268435000)
  if(length(unique(data_region$`UserID`)) * length(unique(data_region$ISBN)) <= 268435000)
  {
    # Change the datatype of columns to factor
    data_region$`UserID` <- factor(data_region$`UserID`)
    data_region$ISBN <- factor(data_region$ISBN)
    
    # Create Ratings matrix
    # A matrix with userids as rows and ISBNs as columns and the rating as the values in matrix)
    rat_mat <- acast(data_region, `UserID` ~ ISBN, value.var = 'Rating', fun.aggregate = sum)
  }else
  {
    # Filtering data 
    # (When the value- unique users*unique books exceeds the above mentioned value, then only we would like to filter the data)
    
    ###############################
    # FILTER 1
    ###############################
    
    #---------------------------------------------------
    # Creating User Vs Ratings per user dataframe
    #---------------------------------------------------
    
    pivot_user <- aggregate(data_region$Rating ~ data_region$UserID, data = data_region, FUN = length)
    colnames(pivot_user) <- c("UserID", "#Ratings")
    
    #-----------------------------------------------------------------
    # Deleting the users who have rated less than 10 books from pivot 
    #-----------------------------------------------------------------
    
    pivot_user_filtered <- pivot_user[!pivot_user$`#Ratings` < 10,]
    
    #-----------------------------------------------------------------------
    # Deleting the users who have rated less than 10 books from dataframe
    #-----------------------------------------------------------------------
    
    join_string1 <- "select data_region.* from data_region inner join pivot_user_filtered 
    on data_region.UserID = pivot_user_filtered.UserID"
    
    data_region2 <- sqldf(join_string1,stringsAsFactors = FALSE)
    
    # Change the datatype of columns to factor
    data_region2$`UserID` <- factor(data_region2$`UserID`)
    data_region2$ISBN <- factor(data_region2$ISBN)
    
    # For freeing up the space
    rm(data_region)
    data_region <- data_region2
    
    
    ###############################
    # FILTER 2
    ###############################
    
    #---------------------------------------------------
    # Creating Book Vs Ratings per user dataframe
    #---------------------------------------------------
    
    pivot_book <- aggregate(data_region$Rating ~ data_region$ISBN, data = data_region, FUN = length)
    colnames(pivot_book) <- c("ISBN", "#Ratings")
    
    #----------------------------------------------------
    # Deleting the books which has less than 10 ratings
    #----------------------------------------------------
    
    pivot_book_filtered <- pivot_book[!pivot_book$`#Ratings` < 10,]
    
    #---------------------------------------------------------------------
    # Deleting the users who have rated less than 10 books from dataframe
    #---------------------------------------------------------------------
    
    join_string2 <- "select data_region.* from data_region inner join pivot_book_filtered 
    on data_region.ISBN = pivot_book_filtered.ISBN"
    
    data_region3 <- sqldf(join_string2,stringsAsFactors = FALSE)
    
    # Change the datatype of columns to factor
    data_region3$`UserID` <- factor(data_region3$`UserID`)
    data_region3$ISBN <- factor(data_region3$ISBN)
    
    # For freeing up the space
    rm(data_region)
    data_region <- data_region3
    
    # Create Ratings matrix
    # A matrix with userids as rows and ISBNs as columns and the rating as the values in matrix)
    rat_mat <- acast(data_region, `UserID` ~ ISBN, value.var = 'Rating', fun.aggregate = sum)
    
  }
  
  
  # Replacing 0's with NA's in matrix (Since we are using recommenderlab)
  rat_mat[rat_mat == 0] <- NA
  
  # Convert rat_mat into realRatingMatrix data structure
  # RealRatingMatrix is a recommenderlab sparse-matrix like data-structure
  real_rat_mat <- as(rat_mat, "realRatingMatrix")
  
  
  # Creating a recommender object (model)
  rec = Recommender(real_rat_mat[1:nrow(real_rat_mat)],
                    method = "IBCF",                    # Item based collaborative filtering
                    param = list(normalize = "Z-score", method="Cosine", nn = 15)) 
  #nn = 15 => No. of similar users
  
  
  # 1. Rating Prediction
  pred_rating <- predict(rec, real_rat_mat[1:nrow(real_rat_mat)], type="ratings")
  
  
  # 2. Predicting Top-n item recommendations for a user
  recc_predicted <- predict(object = rec, newdata = real_rat_mat, n = 5, type="topNList")
  recc_items <- as(recc_predicted, "list")
  
  
  return(recc_items[id])
  
}

