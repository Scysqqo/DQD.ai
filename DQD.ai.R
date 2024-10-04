#================================================================================================
#Data Quality Dimensions: Assessment functions                                       Function #
#================================================================================================
#Function: 01
completeness <- function(dataset) {
  cleanSpace <- function(data2) {
    for (col in names(data2)) {
      result <- sapply(data2[[col]], no_Value)
      data2[[col]][result] <- NA
    }
    return(data2)
  }
  no_Value <- function(in_Value) {
    new_tr_value <- trimws(in_Value)
    no_element <- nchar(new_tr_value) == 0
    return(no_element)
  }
  countNAvalues<-0
  dataset<-cleanSpace(dataset)
  total_cells<-nrow(dataset)*ncol(dataset)
  total_matches<-total_cells-sum(is.na(dataset))
  df <- data.frame(Dimension=c("Completeness"), 
                   N_Occurrency=prettyNum(c(total_matches),big.mark=","),
                   Compliance=c(total_matches)*100/total_cells)
  df$Compliance <- round(df$Compliance, 2)
  df$Compliance <- paste0(df$Compliance, "%")
  n <- paste0("(", prettyNum(c(total_cells),big.mark=","), ")")
  df$N_Occurrency <- paste0(df$N_Occurrency, n)
  return(df)
}                                                #01
#Function: 02
uniqueness<-function(dataset, id_col,isbn_col){
  dataset=dataset[,-1]
  total_rows<-nrow(dataset)
  sum_id<- total_rows-sum(duplicated(id_col))
  sum_isbn<- total_rows-sum(duplicated(isbn_col))
  sum_row<- total_rows-sum(duplicated(dataset))
  
  df <- data.frame(Dimension=c("Uniqueness of data record","uniqueness of primary key",
                               "Uniqueness of the mapping of an object in a data file"), 
                   N_Occurrency=prettyNum(c(sum_row,sum_id,sum_isbn),big.mark=","),
                   Compliance=c(sum_row,sum_id,sum_isbn)*100/total_rows)
  df$Compliance <- round(df$Compliance, 2)
  df$Compliance <- paste0(df$Compliance, "%")
  n <- paste0("(", prettyNum(c(total_rows),big.mark=","), ")")
  df$N_Occurrency <- paste0(df$N_Occurrency, n)
  return(df)
}                                    #02
#Function: 03
accuracy<-function(dataset,col,model){
  nameSex<-read.csv("nameSex2.csv",header=TRUE,na.strings = c("", NA))
  countryDistrict<-read.csv("countryDistrict2.csv",header=TRUE,na.strings = c("", NA))
  country<-read.csv("country.csv",header=TRUE,na.strings = c("", NA))
  df1<-dataset
  colnames(df1)[which(names(df1) == "First_Name")] <- "Name"
  df1<-df1[ , c(col)]
  if(model=="c") {df2<-country[,c(col)]
  }
  if(model=="d") {district<-countryDistrict[,c(col)] 
  df2<-district}
  if(model=="n") {name<-nameSex[,c(col)] 
  df2<-name}
  total_matches<-sum(df1 %in% df2)
  total_matches<-total_matches
  total_cells<-nrow(dataset)
  df <- data.frame(Dimension=c(paste(col,"Accuracy")), 
                   N_Occurrency=prettyNum(c(total_matches),big.mark=","),
                   Compliance=c(total_matches)*100/total_cells)
  df$Compliance <- round(df$Compliance, 2)
  df$Compliance <- paste0(df$Compliance, "%")
  n <- paste0("(", prettyNum(c(total_cells),big.mark=","), ")")
  df$N_Occurrency <- paste0(df$N_Occurrency, n)
  return(df)
}                                             #03
#Function: 04
validity<-function(col=NULL, form=NULL,sep=NULL,model=NULL){
  if(model=="Date"){
    validate_Date <- function(col, form,sep) {
      text<-form
      form=paste(c(paste("%",substr(text, 1, 1),sep = ""), paste("%",substr(text, 2,2),sep = ""),
                   paste("%",substr(text, 3,3),sep = "")), collapse=sep)
      date.format = form
      tryCatch(!is.na(as.Date(col, date.format)),  
               error = function(err) {FALSE})  
    }
    total_matches<-sum(validate_Date(col,form,sep))
    total_cells<-nrow(sdataset)
  }
  if(model=="Email"){
    total_matches <-0
    total_cells <-0
    pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}(\\.[A-Za-z]{2,})?$"
    matches <- grepl(pattern, col)
    total_matches <- total_matches + sum(matches)
    total_cells <- total_cells + length(col)
    matches
  }
  df <- data.frame(Dimension=c(paste(model,"Validity")), 
                   N_Occurrency=prettyNum(c(total_matches),big.mark=","),
                   Compliance=c(total_matches)*100/total_cells)
  df$Compliance <- round(df$Compliance, 2)
  df$Compliance <- paste0(df$Compliance, "%")
  n <- paste0("(", prettyNum(c(total_cells),big.mark=","), ")")
  df$N_Occurrency <- paste0(df$N_Occurrency, n)
  return (df)
}                       #04
#Function: 05
consistency<-function(dataset,a_col,b_col,model){
  df1<-dataset
  df1<-df1[ , c(a_col, b_col)]
  if(model=="ns"){df2<-read.csv("nameSex2.csv",header=TRUE,na.strings = c("", NA))}
  if(model=="cd"){df2<-read.csv("countryDistrict2.csv",header=TRUE,na.strings = c("", NA))}
  
  df1$exists <- do.call(paste0, df1) %in% do.call(paste0, df2)
  total_matches<-sum(df1$exists)
  total_cells<-nrow(dataset)
  
  df <- data.frame(Dimension=c(paste(a_col,"-",b_col,"Consistency")), 
                   N_Occurrency=prettyNum(c(total_matches),big.mark=","),
                   Compliance=c(total_matches)*100/total_cells)
  df$Compliance <- round(df$Compliance, 2)
  df$Compliance <- paste0(df$Compliance, "%")
  n <- paste0("(", prettyNum(c(total_cells),big.mark=","), ")")
  df$N_Occurrency <- paste0(df$N_Occurrency, n)
  return(df)
}                                  #05
#------------------------------------------------------------------------------------------------
#Data Quality Dimensions: Improvement functions                                         -
#------------------------------------------------------------------------------------------------
#Function: 06
completeness_Imp <- function(dataset) {
  cleanSpace <- function(data2) {
    for (col in names(data2)) {
      result <- sapply(data2[[col]], no_Value)
      data2[[col]][result] <- NA
    }
    return(data2)
  }
  no_Value <- function(in_Value) {
    new_tr_value <- trimws(in_Value)
    no_element <- nchar(new_tr_value) == 0
    return(no_element)
  }
  dataset<-cleanSpace(dataset)
  factor_cols <- c("First_Name","Last_Name","Sex", "Country","District","Date_of_Birth",
                   "Book_ID","Price","Delivered")
  dataset[factor_cols] <- lapply(dataset[factor_cols], as.factor)
  
  library(mice)
  data_no_space_values <- dataset
  imputed_data <- mice(data_no_space_values, m = 1, method = "rf")
  print(imputed_data)
  finished_imputed_data <- complete(imputed_data, 1)
  return(finished_imputed_data)
}                                            #06
#Function: 07
uniqueness_Imp <- function(dataset,model) {
  library(dplyr)
  library(magrittr)
  df<-dataset
  if(model=="ID"){
    df$ID <- as.character(df$ID)  
    dup_indices <- duplicated(df$ID)
    if (any(dup_indices)) {
      max_id <- max(as.numeric(df$ID))
      unique_ids <- seq(max_id + 1, length.out = sum(dup_indices))
      df$ID[dup_indices] <- as.character(unique_ids)
    }
  }
  if(model=="row"){
    df<-df %>% 
      distinct(ID,First_Name,Last_Name,Sex,Country,District,Email,Date_of_Birth,Book_ID,ISBN_10,.keep_all = TRUE)
    df<-df %>% 
      distinct(First_Name,Last_Name,Sex,Country,District,Email,Date_of_Birth,Book_ID,ISBN_10,.keep_all = TRUE) 
  }
  if(model=="isbn"){
    df_filtered <- df %>%
      group_by(ISBN_10) %>%
      filter(n_distinct(Book_ID) == 1 | row_number() == 1) %>%
      ungroup()
    df<-df_filtered
  }
  return(df)
}                                        #07
#Function: 08
accuracy_Imp<-function(dataset,model){
  library(stringdist)
  dataset <- dataset[!(dataset$District == "Empty" | is.na(dataset$District)), ]
  dataset <- dataset[!(dataset$Country == "Empty" | is.na(dataset$Country)), ]
  a<-dataset
  b <- read.csv("countryDistrict2.csv", header = TRUE, na.strings = c("", NA))
  if(model=="c"){
    a_col<-a$Country
    b_col<-b$Country
  }
  if(model=="d"){
    a_col<-a$District
    b_col<-b$District
  }
  if(model=="n"){
    colnames(a)[colnames(a) == "First_Name"] <- "Name"
    b <- read.csv("nameSex2.csv", header = TRUE, na.strings = c("", NA))
    a_col<-a$Name
    b_col<-b$Name
  }
  names <- data.frame(a_name = character(), b_name = character(), stringsAsFactors = FALSE)
  for (name_a in a_col) {
    dist <- stringdist(name_a, b_col, method = "jw")
    location <- which.min(dist)
    match_name <- b_col[location]
    names <- rbind(names, data.frame(a_name = name_a, b_name = match_name, stringsAsFactors = FALSE))
  }
  if(model=="c"){a$Country <- names$b_name}
  if(model=="n"){
    a$Name <- names$b_name
    colnames(a)[colnames(a) == "Name"] <- "First_Name"}
  if(model=="d"){a$District <- names$b_name}
  return(a)
}                                             #08
#Function: 09
validity_Imp<-function(dataset,model){
  if(model=="Date"){
    library(lubridate)
    library(magrittr)  # for the %>%
    correct<-function(x){
      date_mmddyyyy <- x
      parsed_date <- tryCatch(
        parse_date_time(date_mmddyyyy, orders = c("mdy", "dmy")), 
        error = function(e) NA
      )
      formatted_date <- parsed_date %>%
        format("%d/%m/%Y")
      if(day(formatted_date)<month(formatted_date)){
        formatted_date <- formatted_date %>%
          format("%d/%m/%Y")
      }
      return(formatted_date)
    }
    dataset$Date_of_Birth <- sapply(dataset$Date_of_Birth, correct) 
    dataset$Date_of_Birth <- sapply(dataset$Date_of_Birth, correct) 
  }
  if(model=="Email"){
    
    remove_accents <- function(x) {
      return(iconv(x, to = 'ASCII//TRANSLIT'))
    }
    dataset$Email <- sapply(dataset$Email, remove_accents)
    pattern <- "[^a-zA-Z0-9.@]"
    dataset$Email <- gsub(pattern, "", dataset$Email)
    dataset$Email<-dataset$Email
  }
  return(dataset)
}                                             #09
#Function: f10
consistency_Imp<-function(dataset,model){
  if(model=="ns"){
    df1<-read.csv("nameSex2.csv",header=TRUE,na.strings = c("", NA))
    df2<-dataset
    corrected_df2 <- df2  
    for (i in 1:nrow(df2)) {
      if (df2$Sex[i] %in% df1$Sex  && df2$First_Name[i] %in% df1$Name){
        if (df2$Sex[i] != df1$Sex[df1$Name == df2$First_Name[i]]) {
          corrected_sex <- df1$Sex[df1$Name == df2$First_Name[i]]
          corrected_df2$Sex[i] <- corrected_sex
          
        }
      }
    }
  }
  if(model=="cd"){
    df1<-read.csv("countryDistrict2.csv",header=TRUE,na.strings = c("", NA))
    df2<-dataset
    district_country_map <- setNames(df1$Country, df1$District)
    df2$Country <- district_country_map[df2$District]
    corrected_df2<-df2
  }
  return(corrected_df2)
}                                          #f10
#------------------------------------------------------------------------------------------------
#Data Quality Dimensions: Main function                                                 -
#------------------------------------------------------------------------------------------------
DQD<-function(operation=NULL,dataset=NULL,col=NULL,model=NULL,                         #f11
              a_col=NULL,b_col=NULL, id_col=NULL,isbn_col=NULL,
              form=NULL,sep=NULL){
  
  
  #Assessment operations
  if(operation=="accuracy") return (accuracy(dataset,col,model))
  if(operation=="completeness") return (completeness(dataset))
  if(operation=="consistency") return (consistency(dataset,a_col,b_col,model))
  if(operation=="uniqueness") return (uniqueness(dataset,id_col,isbn_col))
  if(operation=="validity") return (validity(col=col,model=model,form=form,sep=sep))
  
  #Improvement operations
  if(operation=="accuracy_Imp") return (accuracy_Imp(dataset,model))
  if(operation=="completeness_Imp") return (completeness_Imp(dataset))
  if(operation=="consistency_Imp") return (consistency_Imp(dataset,model))
  if(operation=="uniqueness_Imp") return (uniqueness_Imp(dataset,model))
  if(operation=="validity_Imp") return (validity_Imp(dataset=dataset,model=model))
}
#================================================================================================
sdataset<-read.csv("sdataset.csv",header=TRUE,na.strings = c("", NA))                      #dataset
DQD(operation="completeness",dataset=sdataset)                                             #call 01 
DQD(operation="uniqueness",dataset=sdataset,id_col=sdataset$ID,isbn_col = sdataset$ISBN_10)#call 02
DQD(operation="accuracy",dataset=sdataset,col="District",model = "d")                      #call 03
DQD(operation="accuracy",dataset=sdataset,col="Name",model = "n")                          #call 03 
DQD(operation="accuracy",dataset=sdataset,col="Country",model = "c")                       #call 03 
DQD(operation="validity", col=sdataset$Date,form="dmy",sep="/",model="Date")               #call 04
DQD(operation="validity", col=sdataset$Email,model="Email")                                #call 04
DQD(operation="consistency",dataset=sdataset,a_col="Country",b_col="District",model="cd")  #call 05
DQD(operation="consistency",dataset=sdataset,a_col="First_Name",b_col="Sex",model="ns")    #call 05
sdataset<-DQD(operation="completeness_Imp",dataset=sdataset)                               #call 06
sdataset<-DQD(operation="uniqueness_Imp",dataset=sdataset,model="ID")                      #call 07
sdataset<-DQD(operation="uniqueness_Imp",dataset=sdataset,model="isbn")                    #call 07
sdataset<-DQD(operation="uniqueness_Imp",dataset=sdataset,model="row")                     #call 07
sdataset<-DQD(operation="accuracy_Imp",dataset=sdataset,model="c")                         #call 08
sdataset<-DQD(operation="accuracy_Imp",dataset=sdataset,model="d")                         #call 08
sdataset<-DQD(operation="accuracy_Imp",dataset=sdataset,model="n")                         #call 08
sdataset<-DQD(operation="validity_Imp", dataset=sdataset, model="Date")                    #call 09
sdataset<-DQD(operation="validity_Imp", dataset=sdataset, model="Email")                   #call 09
sdataset<-DQD(operation="consistency_Imp",dataset=sdataset,model="ns")                     #call f10
sdataset<-DQD(operation="consistency_Imp",dataset=sdataset,model="cd")                     #call f10
