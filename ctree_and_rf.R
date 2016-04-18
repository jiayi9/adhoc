output$ctree = renderPlot({
  withProgress(message = 'Running Conditional Tree', {
    X = data.frame(lapply(attr_ranked_p_re(), as.factor))
    D = data.frame(data = X,STATUS = ifelse(STATUS()=="F","F","P"))
    library(party)
    library(partykit)
    
    fit = ctree(STATUS~.,data=D)
    p=plot(fit)
    
    p
  })
})


output$randomforest = renderPlot({
  withProgress(message = 'Running Random Forest', {
    library(randomForest)
    
    X = data.frame(lapply(attr_ranked_p_re(), as.factor))
    D = data.frame( X,STATUS = ifelse(STATUS()=="F","F","P"))
    
    set.seed(415)
    RANK = randomForest(STATUS~., data=D, importance=TRUE, ntree=2000)
    p=varImpPlot(RANK, main = "Variable Importance Rank")
    p
    
  })
})