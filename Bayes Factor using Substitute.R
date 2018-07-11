library(MASS)
# Data

pilot      <- subset.data.frame(nlschools[,-6],  subset = nlschools$COMB==1)

# To prove that it works with multiple parameters (which do not need to be included in all the hypotheses)
random3 <- rnorm(nrow(pilot))


# Mean and covariance estimations 
fit <- lm(lang~IQ+SES+random3, data = pilot)
estimates <- coef(fit)[-1]
covar <- vcov(fit)[-1,-1]


# Bayes factor function (naming your parameters is key!)
BF <- function(estimations = c(b1 = 2, b2 =3), CovarM, iteration = 10000, j, samplesize, 
               Hypotheses = list(H1 = substitute(abs(b1-b2)<.1), H2 = substitute(b1>b2 & b2>0))){
  
  library(mvtnorm,quietly = T, verbose = F)
  
  bfract         <- j/samplesize
  
  cimean         <- rep(0,length(estimations))
  names(cimean)  <- names(estimations)
  
  randomfi       <- rmvnorm(n = iteration, mean = estimations, sigma = CovarM)
  randomci       <- rmvnorm(n = iteration, mean = cimean     , sigma = CovarM/bfract)
  results        <- matrix(NA, ncol = 7, nrow = length(Hypotheses), 
                           dimnames = list(names(Hypotheses),c("fit"        , "complexity"            , "complement"          , "BF unconstr.",
                                                               "BF complem.", 'PMPa (excl. unconstr.)', "PMPb (incl. unconstr.)")))
  
  lis_randomfi   <- split(randomfi, colnames(randomfi))                                 # splits the matrix in a list with a vector of values for each parameter
  lis_randomci   <- split(randomci, colnames(randomci))                                 # this is then used in the evaluation later on
  
  
  for (i in 1:length(Hypotheses)) {
    
    results[i,1] <- mean(eval(Hypotheses[[i]], lis_randomfi ))                          # Fit    
    results[i,2] <- mean(eval(Hypotheses[[i]], lis_randomci ))                          # Complexity
    results[i,3] <- (1-results[i,1])/(1-results[i,2])                                   # Complement
    results[i,4] <-    results[i,1] /   results[i,2]                                    # BF Unconstrained
    results[i,5] <-    results[i,4] /   results[i,3]                                    # BF complement
  }
  
  for (i in 1:length(Hypotheses)) {
    results[i,6] <- results[i,5]/   sum(results[,5])                                    # PMPa
    results[i,7] <- results[i,4]/(1+sum(results[,4]))                                   # PMPb
  }
  
  PMPbu          <- 1/(1+sum(results[,4]))
  results        <-    rbind(results, "Hu" = c(NA ,NA ,NA ,NA ,NA ,NA ,PMPbu))
  
  print(round(results, digits = 4),na.print = "-")}

BF(estimations = c(b1 = estimates[[1]], b2 = estimates[[2]], b3 = estimates[[3]]) ,CovarM = covar,j = 3,samplesize = nrow(pilot),
    Hypotheses = list(H1 = substitute(abs(b1-b2)<2), H2 = substitute(b1>b2& b2>.11&b3>0) ,H3 = substitute(b3>b1& b1>.11)))

