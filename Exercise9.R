#Jake Fuehrmeyer
#October 30, 2020

#Exercise 9 For Extra Credit
#Prerequisites
    #set working directory to be where the script will be used a located
    #Make sure that there are only .csv files with numeric values in this directory

#Creating function that takes three variables and calculates the 
#coefficient of variation for each user specified column
#Works with NAs in line
#dir is directory name
#column is the column number that is to be analyzed
#override determines whether or not 50 lines are required in each file

coefficientcalc <- function(directory, columnnumber, override = FALSE){
  
  #set working directory according to input given from function call
  setwd(directory)
  
  #all files in directory placed into filenames dataframe
  filenames = list.files()
  
  #vector to store coefficient of variation for each file
  finalcoefficient = numeric((length(filenames)))
  #variables to keep track of column length for override and non override calls
  overrideerror = FALSE
  overridecheck = FALSE
  error = FALSE
  
  #for loop
  for (i in 1:length(filenames)){
    #if statement to complete calculations only when column exists in file
    if(dim(read.csv(filenames[i], header = TRUE))[2]>(columnnumber-1)){
      #column stores the column of importance from each file being read
      column = read.csv(filenames[i], header = TRUE)[,columnnumber]
      
      #multiple if statements that adds the coefficient value for each file into the finalcoefficient vector
      #checking for 50 items in each column and storing information in the error or doublecheck values
      if (length(column)<50 & override == TRUE){
        finalcoefficient[i] = sd(column)/mean(column)
        overrideerror = TRUE
        
      }else if (length(column)>=50 & override == TRUE){
        finalcoefficient[i] = sd(column)/mean(column)
        overrideerror = FALSE
        overridecheck = TRUE
      }else if (length(column)<50 & override == FALSE){
        finalcoefficient[i] = sd(column)/mean(column)
        return("Not enough observations in column!")
        error = TRUE
      }else if (length(column)>=50 & override == FALSE & error == FALSE){
        finalcoefficient[i] = sd(column)/mean(column)
        error = FALSE
      }
    }
    else {
      finalcoefficient[i] = NA
    }
  }
  
  #returning either error or the final coefficient vector based on whether or 
  #not 50 items were included in each variable and if there was an override or 
  #not
  
  #return for when override is given
  if (override == TRUE & overrideerror == TRUE & overridecheck == TRUE){
    combinedfinal = c(finalcoefficient, filenames)
    return(combinedfinal)
    
  }else if (override == TRUE & overrideerror == TRUE & overridecheck == FALSE){
    return("Not enough observations in column!")
    
  }else if (override == TRUE & overrideerror == FALSE){
    combinedfinal = c(finalcoefficient, filenames)
    return(combinedfinal)
    
  }else if (override == FALSE & error == FALSE){
   
     #return for when no override is given
    combinedfinal = c(finalcoefficient, filenames)
    return(combinedfinal)
  }
}

