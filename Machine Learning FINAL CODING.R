#Clear environment
rm(list = ls())

#Set Work Dec and read CSV
cust_grp1 <- read.csv("C:/Users/selinayang/Desktop/POS Customer Group_Clean_DEV.csv", header = TRUE, sep =",", encoding = "UTF-8", stringsAsFactors = FALSE)
#Drop KOE 3PL column (Added on 06152022)
cust_grp1$KOE.3PL.CY22 <- NULL
#Rename column names
names(cust_grp1) <- c("Index","End_Customer_Name","POS_Cust_Clean","ITPs")
dup_value_table<- cust_grp1[duplicated(cust_grp1$End_Customer_Name),] #Duplicated End Customer Names Found
cust_grp_clean <- cust_grp1[!duplicated(cust_grp1$End_Customer_Name),] #Remove Duplicates
cust_grp1 <- cust_grp_clean
rm(cust_grp_clean)
rm(dup_value_table)

#Load Keywords Master List into the program
keywords_masterList <- read.csv("C:/Users/selinayang/Desktop/ITPs Keywords Master List.csv", header = TRUE, sep =",", encoding = "UTF-8", stringsAsFactors = FALSE)
names(keywords_masterList) <- c("ITPs", "1","2","3","4","5","6","7","8","9","10","11","12")

##Supervied Learning on End Customers Grouping##
#Cleaning Data
str(cust_grp1)
cust_grp1$Index <- NULL
cust_grp1$POS_Cust_Clean <- NULL
#Drop the columns containing '-' in ITPs
cust_grp_ITPs <-subset(cust_grp1, cust_grp1$ITPs!= "-")
cust_grp_ITP_Name <- as.data.frame(cust_grp_ITPs$ITPs)
# Adding ITPs List to perform a for loop
CY22_ITP <- read.csv("C:/Users/selinayang/Desktop/CY22 ITP List.csv", header = TRUE, sep =",", stringsAsFactors = FALSE)
# ITPs counts that are larger than 40 times
# Rename column names
names(CY22_ITP) <- c("End_Customer_Name")
ITP_List <- as.list(CY22_ITP$End_Customer_Name) #Transformed as a list
cust_grp1 <- as.vector(cust_grp1)
str(ITP_List)
# Load Exported new end customers as test sets (Predictors)
cust_test <- read.csv("C:/Users/selinayang/Desktop/New End Customers.csv", header = TRUE, sep =",", encoding = "UTF-8", stringsAsFactors = FALSE)
#Rename column names
names(cust_test) <- c("End_Customer_Name")
num_rows_test <- nrow(cust_test)
num_rows_train <- nrow(cust_grp1)
# Create a list to store the results
results <- data.frame(NA_col = rep(NA, num_rows_test)) #Based on the test dataset objects
w = data.frame(NA_col = rep(NA, num_rows_train)) #Based on the train dataset (Full dataset)

# Assgin new End Customers to the first column 
results[ , 1] <- cust_test$End_Customer_Name
# Rename colname
colnames(results)[1] <- ""

library(tm)
#Replace punctuations with WhiteSpace for cust_test
cust_test_clean <- gsub("[[:punct:]]", " ", cust_test$End_Customer_Name)
cust_test_clean <- gsub('MANUFACTURING|LTD|INC|SYSTEMS|ELECTRONICS|INTERNATIONAL|TECHNOLOGY|INTERGRATED|GRIDS|GMBH|PRODUCTS|TECHNOLOGIES', '', cust_test_clean)
cust_test_clean <- as.data.frame(cust_test_clean)
names(cust_test_clean) <- c("End_Customer_Name")

#Replace punctuations with WhiteSpace for cust_grp_ITPs
ITPs_clean <- gsub("[[:punct:]]", " ", cust_grp_ITPs$End_Customer_Name)
ITPs_clean <- gsub('MANUFACTURING|LTD|INC|SYSTEMS|ELECTRONICS|INTERNATIONAL|TECHNOLOGY|INTERGRATED|GRIDS|GMBH|PRODUCTS|TECHNOLOGIES', '', ITPs_clean)
ITPs_clean <- as.data.frame(ITPs_clean)
names(ITPs_clean) <- c("End_Customer_Name")
cust_grp_ITPs$End_Customer_Name <- ITPs_clean$End_Customer_Name
rows1_ITPs <- nrow(CY22_ITP)

i = 0
# Loop through ITP List and Create a dummy variable as well as labels for prediction
for (j in 1:rows1_ITPs){ #Key Word List: Convert ITPs into 1, -1 and store it in a data.frame
  a <- apply (cust_grp1, 1, function(x) {ifelse(any(x %in% ITP_List[j]), 1, 0)})
  b <- apply (cust_grp_ITP_Name, 1, function(x) {ifelse(any(x %in% ITP_List[j]), 1, 0)})
  w[ , j]<- a #For training label
  colnames(w)[j] <- CY22_ITP$End_Customer_Name[j]
  cust_grp_ITPs[ , j+1] <- b 
  colnames(cust_grp_ITPs)[j+1] <- paste("Dummy", CY22_ITP$End_Customer_Name[j], sep = " ")
  
  # Perform Down Sampling 
  Yes <- which(w[, j] =='1')
  No <-  which(w[, j] =='0')
  length(Yes)
  length(No)
  Number_DownSample<- sample(No,5*length(Yes))
  Cust_grp.down <- cust_grp1[c(Number_DownSample, Yes),]
  Cust_grp.down$Creation_Dummy <- ifelse(Cust_grp.down$ITPs == CY22_ITP$End_Customer_Name[j], 1, 0)
  Cust_grp.down$ITPs <- NULL
  
  #Replace punctuations with WhiteSpace for Cust_grp.down[training dataset]
  grp.down_clean <- gsub("[[:punct:]]", " ", Cust_grp.down$End_Customer_Name)
  grp.down_clean <- gsub('MANUFACTURING|LTD|INC|SYSTEMS|ELECTRONICS|INTERNATIONAL|TECHNOLOGY|INTERGRATED|GRIDS|GMBH|PRODUCTS|TECHNOLOGIES', '', grp.down_clean)
  grp.down_clean <- as.data.frame(grp.down_clean)
  names(grp.down_clean) <- c("End_Customer_Name")
  
  # Text Mining Pt1 (Your loaded Test Set--> text mining)
  library(tm)
  end_customer_corpus1 <- VCorpus(VectorSource(cust_test_clean$End_Customer_Name))
  # Remove Punctuation as the machine seems not to read them
  endcustomer_corpus_clean1 <- tm_map(end_customer_corpus1, removePunctuation)
  # Remove WhiteSpace
  endcustomer_corpus_clean1 <- tm_map(endcustomer_corpus_clean1, stripWhitespace)
  # Perform tokenization
  cust_test.dtm <- DocumentTermMatrix(endcustomer_corpus_clean1)
  
  # Text Mining Pt2 (All values from cust_grp1 sets)
  end_customer_corpus <- VCorpus(VectorSource(grp.down_clean$End_Customer_Name))
  # Remove Punctuation as the machine seems not to read them
  endcustomer_corpus_clean <- tm_map(end_customer_corpus, removePunctuation)
  # Remove WhiteSpace
  endcustomer_corpus_clean <- tm_map(endcustomer_corpus_clean, stripWhitespace)
  # Perform tokenization
  cust_train.dtm <- DocumentTermMatrix(endcustomer_corpus_clean)
  
  # Create training set label(Required for loop)
  cust_train_label <- Cust_grp.down$Creation_Dummy
  
  
  #Pt3 Finding customize keywords for each ITPs
  for (k in 1 : nrow(keywords_masterList)){
    if (any(CY22_ITP$End_Customer_Name[j] %in% keywords_masterList$ITPs[k])){
      ITP.keywords <- keywords_masterList[k, -1]
      ITP.keywords <- as.data.frame(t(ITP.keywords))
      names(ITP.keywords) <- c("word")
      ITP.keywords$word <- tolower(ITP.keywords$word)
    }
  }
  # Create function to look over key words distribution in testing dataset to avoid an error in prediction
  summarise_keywords_test <- function(y){
    # Convert dashes to spaces - to ensure that words like "data-focused" are separated appropriately
    y <- gsub("-", " ", y)
    # Find Frequency
    frequencies_test <- DocumentTermMatrix(endcustomer_corpus_clean1)
    # Convert to dataframe
    Test.Sparse <- as.data.frame(as.matrix(frequencies_test))
    
    # Convert variable names to make sure they are appropriate
    colnames(Test.Sparse) <- make.names(colnames(Test.Sparse))
    
    # Calculate word frequencies
    top_keywords_test <- colSums(Test.Sparse)
    top_keywords_test <- data.frame(
      word = names(top_keywords_test),
      freq = top_keywords_test
    )
    
    # Sort
    top_keywords_test <- top_keywords_test[order(top_keywords_test$freq, decreasing = TRUE), ]
    rownames(top_keywords_test) <- NULL
    
    # Output
    return(top_keywords_test)
  }
  # Find KeyWords for Test Data Set
  Test_keywords <- summarise_keywords_test(cust_test$End_Customer_Name)
  Test_keywords$freq <- NULL
  
  { 
    if (any(ITP.keywords$word %in% Test_keywords$word)){
      inner_words <- merge(x = ITP.keywords, y = Test_keywords, by = "word")
      #Sort the keyword descending to make sure it follows ITP's Keywords
      #inner_words <- inner_words[order(inner_words$freq.x, decreasing = TRUE),]
      words <- inner_words$word
      i = i + 1
    }
    else{
      next
    }
  }
  #} #previously ended here
  # Limit the training and testing dateset to only include words in the frequency vector
  cust_train_freq <- cust_train.dtm[ , words]
  cust_test_freq <- cust_test.dtm[, words]
  # Convert 1, -1 to Y/N
  convert_counts <- function(x) {
    x <- ifelse(x > 0, "Yes", "No")
  }
  
  # Apply function to the dataset
  drop = FALSE
  cust_train.dtm <- apply(cust_train_freq, MARGIN =2, convert_counts)
  cust_test.dtm <- apply(cust_test_freq, MARGIN = 2, convert_counts)
  
  
  # Perform Naive Bayes
  library(e1071)
  ITPs_classifier <- naiveBayes(cust_train.dtm, cust_train_label)
  ITPs_test_pred <- predict(ITPs_classifier, cust_test.dtm)
  # Add to new dataframe
  results[ , i+1] <- ITPs_test_pred 
  colnames(results)[i+1] <- CY22_ITP$End_Customer_Name[j] #Store the results
  
} #End for loop

library(readr)
write_excel_csv(results, "C:/Users/selinayang/Desktop/NB_Output.csv") #To prevent the removal of special characters
