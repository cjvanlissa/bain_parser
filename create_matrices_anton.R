create_matrices <- function(varnames, hyp){
  
  if(is.null(varnames)) stop("Please input proper strings with constraints")
  
  hyp2 <- gsub(" ", "", hyp) #removes all whitespace
  if(!grepl("^[0-9a-zA-Z><=,().-]+$", hyp2)) stop("Impermissable characters in hypotheses") #Self-explanatory. NEW parentehese
  if(grepl("[><=]{2,}", hyp2)) stop("Do not use combined comparison signs e.g., '>=' or '=='")
  
  step1 <- unlist(strsplit(hyp2, split = "[<>=,()]")) #split by special characters and unlist
  input_vars <- step1[grep("[a-zA-Z]+", step1)] #extract subunits that contain at least one letter
  if(!all(input_vars %in% varnames)) stop("Hypothesis variable(s) not in object, check spelling") #Checks if input variables exist in lm-object
  
  framer <- function(x){ #As function because same code used once more later
    pos_comparisons <- unlist(gregexpr("[<>=]", x)) #Gives the positions of all comparison signs
    leftside <- rep(NA, length(pos_comparisons) + 1) #empty vector for loop below
    rightside <- rep(NA, length(pos_comparisons) + 1) #empty vector for loop below
    pos1 <- c(-1, pos_comparisons) #positions to extract data to the leftside of comparisons
    pos2 <- c(pos_comparisons, nchar(x) + 1) #positions to extract data to the rightside of comparisons
    for(i in seq_along(pos1)){
      leftside[i] <- substring(x, pos1[i] + 1, pos1[i+1] - 1) #Extract all variables or outcomes to the leftside of a comparison sign
      rightside[i] <- substring(x, pos2[i] + 1, pos2[i+1] - 1) #Extract all variables or outcomes to the rightside of a comparison sign
    }
    leftside <- leftside[-length(leftside)] #remove last element which is a NA due to loop formatting
    rightside <- rightside[-length(rightside)] #remove last element which is a NA due to loop formatting
    comparisons <- substring(x, pos_comparisons, pos_comparisons) #Extract comparison signs
    data.frame(left = leftside, comp = comparisons, right = rightside, stringsAsFactors = FALSE) #hypotheses as a dataframe
  }
  
  framed <- framer(hyp2) #hypotheses as a dataframe
  
  if(any(grepl(",", framed$left)) || any(grepl(",", framed$right))){ #Larger loop that deals with commas if the specified hypothesis contains any
    if(nrow(framed) > 1){
      for(r in 1:(nrow(framed)-1)){ #If a hypothesis has been specified with commas e.g., "X1 > 0, X2 > 0" or "(X1, X2) > X3"
        if(all.equal(framed$right[r], framed$left[r+1])){ #The right hand side of the hypothesis df will be equal to the next row left side
          if(substring(framed$right[r], 1, 1) == "(") { #If the first row begins with a ( as when "X1 > (X2, X3)" and opposed to "(X2, X3) > X1"
            framed$right[r] <- sub("),.+", ")", framed$right[r])#If so, remove everything to the right of the parenthesis on the right hand side
            framed$left[r+1] <- sub(".+),", "", framed$left[r +1])#and everything to the left of the parenthesis on the left hand side to correct the df
            } else{
              framed$right[r] <- sub(",.+", "", framed$right[r]) #else, remove everything to the right of the comma on the right hand side
              framed$left[r+1] <- sub("[^,]+,", "", framed$left[r+1]) #and everything to the left of the comma on the left hand side to correct the df
            }
          }
        }
      } 
    
    commas_left <- framed$left[grep(",", framed$left)] #At this point all remaining elements that contain commas should also have parentheses, check this
    commas_right <- framed$right[grep(",", framed$right)] #Necessary to use is isTRUE below in case one of these contains no commas, and 'any' for several rows
    if(isTRUE(any(!grepl("\\(.+)", commas_left))) || isTRUE(any(!grepl("\\(.+)", commas_right))) || #Check so rows contain parenthesis
       isTRUE(any(grepl(").+", commas_left))) || isTRUE(any(grepl(").+", commas_right))) || #Check so parentheses are not followed by anything
       isTRUE(any(grepl(".+\\(", commas_left))) || isTRUE(any(grepl(".+\\(", commas_right)))) { #chekc so parentheses are not preceded by anything
      stop("Incorrect hypothesis syntax or extra character, check specification")
      }
    
    
    framed$left <- gsub("[()]", "", framed$left) #drop remaining parentheses
    framed$right <- gsub("[()]", "", framed$right)
    commas <- unique(c(grep(",", framed$left), grep(",", framed$right))) #Gives us the unique rows that still contain commas (multiple comparisons) from left or right columns

    if(length(commas) > 0){ #If there are any multiple comparisons e.g., (X1, X2) below loop separates these in
      multiples <- vector("list", length = length(commas)) #Empty vector to store results for each row in loop below

      for(r in seq_along(commas)){ #for each row containing commas
        several <- framed[commas,][r, ] #select row r
    
        if(several$comp == "="){ #If e.g., (X1, X2) = X3, convert to X1 = X2 = X3
            
          several <- c(several$left, several$right)
          separate <- unlist(strsplit(several, split = ",")) #split by special characters and unlist
          if(any(grepl("^$", several))) stop("Misplaced comma in hypothesis") #if empty element
          converted_equality <- paste(separate, collapse = "=") #convert to X1 = X2 = X3 shape
          multiples[[r]] <- framer(converted_equality) #hypotheses as a dataframe
          
          } else{ #If inequality comparison
          leftvars <- unlist(strsplit(several$left, split = ",")) #separate left hand var
          rightvars <- unlist(strsplit(several$right, split = ",")) #separate right hand vars
          if(any(grepl("^$", leftvars)) || any(grepl("^$", rightvars))) stop("Misplaced comma in hypothesis") #if empty element

          left <- rep(leftvars, length.out = length(rightvars)*length(leftvars)) #repeat each leftvars the number of rightvars
          right <- rep(rightvars, each = length(leftvars)) #complement for rightvars
          comp <- rep(several$comp, length(left)) #repeat the comparison a corresponding number of times
    
          multiples[[r]] <- data.frame(left = left, comp = comp, right = right, stringsAsFactors = FALSE) #save as df to be able to combine with 'framed'
          }
        }

      framed <- framed[-commas,] #remove old unfixed rows with commas
      multiples <- do.call(rbind, multiples) #make list into dataframe
      framed <- rbind(multiples, framed) #recombine into one dataframe
    }
  } #end comma loop
  
  equality <- framed[framed$comp == "=",]
  inequality <- framed[!framed$comp == "=",]
  
  #********Equality
  if(nrow(equality) == 0) { #If there are no '=' comparisons set to NULL
    augmatrix_equality <- NULL
  } else{
    outcomes <- suppressWarnings(apply(equality[, -2], 2, as.numeric)) #Convert left/right to numeric, non-numeric values (variables) coerced to NA 
    outcomes <- matrix(outcomes, ncol = 2, byrow = FALSE) #Conversion to matrix in case there was only one row in outcomes
    if(any(rowSums(is.na(outcomes)) == 0)) stop("Value compared with value rather than variable, e.g., '2 = 2', check hypotheses")
    rows <- which(rowSums(is.na(outcomes)) < 2) #which rows contain a numeric value (comparing variable to value), that is not two NA-values
    specified <- t(outcomes[rows,]) #transpose so that specified comparison values are extracted in correct order below, e.g, in case when "X1 = 0, 2 = X2"
    specified <- specified[!is.na(specified)] #extract specified comparison values
    r_e <- ifelse(rowSums(is.na(outcomes)) == 2, 0, specified) #If variable = variable -> 0, if variable = value -> value
    r_e <- matrix(r_e) #convert to matrix
    
    var_locations <- apply(equality[, -2], 2, function(x) ifelse(x %in% varnames, match(x, varnames), 0)) #convert non-variables to 0 and others are given their locations in varnames
    var_locations <- matrix(var_locations, ncol = 2) #Necessary if only one comparison row
    
    R_e <- matrix(rep(0, nrow(equality)*length(varnames)), ncol = length(varnames)) #Create empty variable matrix
    
    for(i in seq_along(r_e)){ # for each row i in R_e, replace the columns specified in var_locations row i
      if(!all(var_locations[i, ] > 0)){ #If only one variable is specified (i.e., other one is set to zero)
        R_e[i, var_locations[i,]] <- 1 #Set this variable to 1 in R_e row i
      } else{ #If two variables specified
        R_e[i, var_locations[i,]] <- c(1, -1) #Set one column to 1 and the other to -1 in R_e row i
      }
    }
    augmatrix_equality <- cbind(R_e,r_e) #list_equality
  }
  
  #********Inequality
  if(nrow(inequality) == 0) { #If there are no '>' or '<' comparisons set to NULL
    augmatrix_inequality <- NULL 
  } else{
    outcomes <- suppressWarnings(apply(inequality[, -2], 2, as.numeric)) #Convert left/right to numeric, non-numeric values (variables) coerced to NA 
    outcomes <- matrix(outcomes, ncol = 2, byrow = FALSE) #Conversion to matrix in case there was only one row in outcomes
    if(any(rowSums(is.na(outcomes)) == 0)) stop("Value compared with value rather than variable, e.g., '2 > 2', check hypotheses")
    cols <- which(rowSums(is.na(outcomes)) < 2) #which columns contain a numeric value (comparing variable to value), that is not two NA-values
    specified <- t(outcomes[cols,]) #transpose so that specified comparison values are extracted in correct order below
    specified <- specified[!is.na(specified)] #extract specified comparison values
    r_i <- ifelse(rowSums(is.na(outcomes)) == 2, 0, specified) #If variable = variable -> 0, if variable = value -> value
    r_i <- matrix(r_i) #convert to matrix
    
    leq <- which(inequality$comp == "<") #gives the rows that contain '<' (lesser or equal) comparisons
    var_locations <- apply(inequality[, -2], 2, function(x) ifelse(x %in% varnames, match(x, varnames), 0)) #convert non-variables to 0 and others are given their locations
    var_locations <- matrix(var_locations, ncol = 2) #Necessary if only one comparison row
    
    R_i <- matrix(rep(0, nrow(inequality)*length(varnames)), ncol = length(varnames)) #Create empty variable matrix
    
    for(i in seq_along(r_i)){ # for each row i in R_i, replace the columns specified in var_locations row i
      if(!all(var_locations[i, ] > 0)){ #If only one variable is specified (i.e., other one is set to zero)
        
        value <- if(i %in% leq) -1 else 1 #If comparison is 'lesser or equal' set to -1, if 'larger or equal' set to 1
        R_i[i, var_locations[i,]] <- value #Set this variable to 1 in R_i row i
        
      } else{ #If two variables specified
        value <- if(i %in% leq) c(-1, 1) else c(1, -1) #If comparison is 'leq' take var2 - var1, if 'larger or equal' take var1 - var2
        R_i[i, var_locations[i,]] <- value #Set one column to 1 and the other to -1 in R_i row i
      }
    }
    
    augmatrix_inequality <- cbind(R_i,r_i)
  }
  
  matrices <- list(equality = augmatrix_equality, inequality = augmatrix_inequality); matrices #final output
  
}

varnames <- c("strength","intelligence","wisdom","dexterity","constitution","charisma")
hyp1 <- "(strength, intelligence) > (wisdom, dexterity)"
create_matrices(varnames, hyp1)
hyp2 <- "strength > charisma > intelligence = .5"
create_matrices(varnames, hyp2)
hyp3 <- "wisdom > 0, (strength, intelligence) > dexterity = constitution"
create_matrices(varnames, hyp3)

varnames <- c("a","b","c","d","e","f")
hyp1 <- "e=f<a=b=c"
create_matrices(varnames, hyp1)
varnames <- c("a","b","c","d","e","f")
hyp1 <- "f>e>0,c>0,d=0,a=b=0"
create_matrices(varnames, hyp1)
varnames <- c("a","b","c","d","e","f")
hyp1 <- "f>e<0,c>0,d=0,a=b=0"
create_matrices(varnames, hyp1)
varnames <- c("a","b","c","d","e","f")
hyp1 <- "f<e>0,c>0,d=0,a=b=0"
create_matrices(varnames, hyp1)
varnames <- c("a","b","c","d","e","f")
hyp1 <- "a>b<c>d"
create_matrices(varnames, hyp1)
varnames <- c("a","b","c","d","e","f")
hyp1 <- "d<c>b<a"
create_matrices(varnames, hyp1)

varnames <- c("a","b","c","d","e","f")
hyp1 <- "a>2"
create_matrices(varnames, hyp1)
varnames <- c("a","b","c","d","e","f")
hyp1 <- "a>2, b=3, b>c=d,e=f=4"
create_matrices(varnames, hyp1)

varnames <- c("a","b","c","d","e","f")
hyp1 <- "(a,b)>c,d=0"
create_matrices(varnames, hyp1)

varnames <- c("a","b","c","d","e","f")
hyp1 <- "a>-1,a<1"
create_matrices(varnames, hyp1)

# searching for error messages
varnames <- c("a","b","c","d","e","f")
hyp1 <- "a>b,b>c,c>a"
create_matrices(varnames, hyp1)

varnames <- c("a","b","c","d","e","f")
hyp1 <- "a+b>2"
create_matrices(varnames, hyp1)

varnames <- c("a","b","c","d","e","f")
hyp1 <- "(a+b)>2"
create_matrices(varnames, hyp1)

varnames <- c("a","b","c","d","e","f")
hyp1 <- "a,b>c,d"
create_matrices(varnames, hyp1)
















