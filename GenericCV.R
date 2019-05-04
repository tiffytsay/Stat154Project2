#Function is reliant on having a "fold" column being present in the dataframe
#CVfold is the specific fold function that is used for either creating the blocks or x-axis
genericCV <- function(classifier, trainingfeatures, traininglabel, K = 10, 
                      loss = "mse", data) {
  CVmodels = list()
  CVloss = list()
  
  # create a loss function
  lossfun <- function(type = loss, mod) {
    if (type == "mse"){
      a = mean( predict(mod, newdata = data) != data[,traininglabel] )
      return(a)
    }
    else { #depends on which loss function to use
      return(NA)
    }
  }
  
  for (i in 1:K) { # create a model trained on 9 different folds
    fxn <- as.formula(paste(traininglabel,
                            "~", 
                            paste(trainingfeatures, collapse = " + ")))
    print(fxn)
    mod = train(fxn, 
                data = data[which(data$folds == i),],
                method = classifier,
                preProcess = c("center", "scale")
    )
    CVmodels[[paste0("fold",i,".model", collapse = "")]] = mod
    CVloss[[paste0("fold",i,".loss",collapse = "")]] = lossfun(mod=CVmodels[[i]])
  }
  
  return(list("models" = CVmodels, "loss" = CVloss))
}


#Create folds based off the passed in training set 
#Folds are saved into the original dataframe with their fold number
CVfold = function(train, K, block) {
  lengths = c()
  folds = c()
  #Block method 
  if (block) {
    divisions = seq(1,56,1)
    #Saving the size of each division
    for(division in divisions) {
      lengths[division] <- length(which(train$regions==division))
    }
    set.seed(1)
    #Repeatedly sample 1/K from each region
    #Remove that row once it has been selected
    for (division in divisions) {
      subset = which(train$regions==division)
      for (fold in 1:K) {
        sample_idx = sample(subset, 
                            size = floor(lengths[division] / K))
        for (i in sample_idx){
          folds[i] = fold
        }
        subset = subset[!subset %in% sample_idx]
      }
    }
    #X-axis method
  } else {
    divisions = range(train$x)
    divisions = seq(divisions[1], divisions[2], 1)
    for(division in divisions) {
      lengths[division] <- length(which(train$x==division))
    }
    set.seed(1)
    #Repeatedly sample 1/K from each region
    #Remove that row once it has been selected
    for (division in divisions) {
      subset = which(train$x==division)
      for (fold in 1:K) {
        sample_idx = sample(subset, 
                            size = floor(lengths[division] / K))
        for (i in sample_idx){
          folds[i] = fold
        }
        subset = subset[!subset %in% sample_idx]
      }
    }
  }
  folds[is.na(folds)] = K
  return(folds)