library(ggplot2)
library(gtable)
library(ClustOfVar)
library(ggdendro)
library(gridExtra)
library(scales)
library(plyr)
library(ggvis)
options(shiny.maxRequestSize=100*1024^2)
options(scipen=999)

server <- function(input, session,output) {
  source("FUNCTIONS.R",local = TRUE)
  source("chooser.R", local = TRUE)
  source("global.R",local = TRUE)
  
  temp = reactive({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    R = read.csv(inFile$datapath, header=TRUE, sep=",")
    R
  })
  
  
  upload = reactive({
    R = temp()
    validate(need(!is.null(R)," "))
    R
  })
  
  STATUS = reactive({
    R = upload()$STATUS
    validate(need(sum(R=="F")>0,"No failures in data."))
    R
  })
  
  output$info = renderText({
    paste("The file contains",nrow(upload()),"rows",ncol(upload()),"cols.")
  })
  
  RAW = reactive({
    R = upload()
    R[,!(names(R) %in% "STATUS")]
  })
  
  ATTR = reactive({
    X = RAW()
    X[] = lapply(X,as.character)
    X[is.na(X)|X==""] = "N/A"
    
    LEVELS = sapply(X,function(x) length(unique(x)))
    R = X[,LEVELS>1&LEVELS<120,drop=FALSE]
    R
  })
  
  attr_names = reactive({

    X = ATTR()
    STATUS = STATUS()
    
    LEVELS = sapply(X,function(x) length(unique(x)))
    X2 = X[,LEVELS>1&LEVELS<=MAXLEVEL(),drop=FALSE]
    
    pvalues = sapply(X2, function(x) chisq_test(x,STATUS, floor(nrow(X)*0.05)))
    X3 = X2[,as.numeric(pvalues)<=as.numeric(MAXP()),drop=FALSE]
    
    
    R = list()
    R$accepted = names(X3)
    
    rejected_names = setdiff(names(X),names(X3))
    TEMP = data_ranked_Chisq(X[,rejected_names],STATUS,5)$DATA
    
    
    R$rejected = names(TEMP)
    R
  })
  
  #output$L = renderPrint( sapply(ATTR(),function(x) length(unique(x))))
  
  #output$L = renderPrint( sapply(ATTR()[,attr_names()$rejected],function(x) nlevels(factor(x))))
  output$L = renderPrint(attr_names()$rejected)

  output$CHOOSER = renderUI({

    choices = attr_names()$rejected

    chooserInput("chooser",
                 leftLabel="In",
                 rightLabel="Out",
                 leftChoices = choices,
                 rightChoices =NULL,
                 size=length(choices),
                 multiple=TRUE)
  })
  
  SELECTED_NAMES = reactive({
    R = input$chooser$right
    R = union( attr_names()$accepted,  input$chooser$right)
    validate(need(length(R)>0,"Select at least one variable."))
    R
  })
  
  SELECTED = reactive({
    ATTR()[,SELECTED_NAMES()]
  })
  
  UNSELECTED = reactive({
    validate(need(!is.null(upload()),"Please upload data."))
    validate(need(!is.null(input$chooser$left),""))
    R = ATTR()[,input$chooser$left]

    R
  })
  
  attr_ranked_fit_2 = reactive({
    X = UNSELECTED()
    
    LEVELS = sapply(X,function(x) length(unique(x)))
    temp = sum(LEVELS<2)
    validate(need(temp==0,"FUCK"))
    
    STATUS = STATUS()
    X[] = lapply(X,as.character)
    fit=data_ranked_Chisq(X,STATUS,5)
    fit
  })
  
  attr_ranked_p_2 = reactive({
      attr_ranked_fit_2()$DATA
  })
  
  selected_attr_names_2 = reactive({names(attr_ranked_p_2())})
  selected_attr_pvalue_2 = reactive({
    pvalues = attr_ranked_fit_2()$P
    round(pvalues[1:ncol(attr_ranked_p_2())],3)
  })
  selected_attr_levels_2 = reactive({
    sapply(attr_ranked_p_2(),function(x) length(unique(x)))
  })
  output$selected_attr_names_2 = renderTable({
    cbind(ATTRs = selected_attr_names_2(),
          P_value = selected_attr_pvalue_2(),
          Levels = as.vector(selected_attr_levels_2())
    )
  })
  
  
  attr_ranked_fit = reactive({
    withProgress(message = 'Ranking Attributes...', {

      
      X = SELECTED()
      STATUS = STATUS()
      X[] = lapply(X,as.character)

      
      R = X
      
      validate(need(ncol(R) != 0, "All attributes fail to survive.\nTry to increase p-value or increase # of max levels."))
      
      fit=data_ranked_Score(R,STATUS, floor(nrow(R))*0.05)
      fit
    })
  })
  
  attr_ranked_p = reactive({
    withProgress(message = 'Filtering Attributes...', {
      DATA = attr_ranked_fit()$DATA
      pvalues = attr_ranked_fit()$P
      X = DATA
      validate(need(ncol(X) > 2, "No more than 3 attributes survived.\nClustering will fail.\nTry to increase p-value."))
      X
    })
  })
  
  selected_attr_names = reactive({names(attr_ranked_p())})
  selected_attr_pvalue = reactive({
    pvalues = attr_ranked_fit()$P
    round(pvalues[1:ncol(attr_ranked_p())],3)
  })
  selected_attr_levels = reactive({
    sapply(attr_ranked_p(),function(x) length(unique(x)))
  })
  
  labelx = eventReactive(input$go,{
    validate(need(
      ncol(attr_ranked_p())>=NUM_CLUST_A(),"The number of attributes is less than the number of clusters.\nTry decreasing the number of attributes clusters."
    ))
    withProgress(message = 'Generating labels', {
      D = attr_ranked_p()
      fit = ClustOfVar::hclustvar(X.quali = Random_Sample_prop(D,1))
      #fit = ClustOfVar::hclustvar(X.quali = D)
      labels = cutree(fit,NUM_CLUST_A())
      labelx = data.frame(Names=names(labels),group = paste("Group",as.vector(labels)),num=as.vector(labels))
      labelx
    })
  })
  
  attr_ranked_p_re = eventReactive(input$go,{
    withProgress(message = 'Rearranging Attributes...', {
      R = rearrange(X = attr_ranked_p(), clust = labelx())
      R
    })
  })
  
  
  output$selected_attr_names = renderTable({
    cbind(ATTRs = selected_attr_names(),
          P_value = selected_attr_pvalue()
          ,
          Levels = as.vector(selected_attr_levels())
          )
  })
  
  
  para_ranked_fit = eventReactive(input$go,{
    withProgress(message = 'Ranking Parametrics...', {
    X = upload()
    STATUS = STATUS()
    LEVELS = sapply(X,function(x) length(unique(x)))
    R = X[, sapply(X,is.numeric)&LEVELS>0.2*nrow(X) ]
    
    validate(need(ncol(R)>2,"No parametrics."))
    fit=data_ranked_logist(R,STATUS=="F")
    fit
    })
  })
  
  
  para_ranked_top = reactive({
    X = para_ranked_fit()$DATA
    validate(need(ncol(X)>=18,"This tab needs at least 18 parametrics."))
    X[,1:18]
  })
  
  output$ranked_para_names = renderTable({
    cbind(PARAs = names(para_ranked_fit()$DATA),
          P_value = para_ranked_fit()$P
          )
  })
  
  MAXP = eventReactive(input$update,{
    input$maxp
  })
  NUM_CLUST_A = eventReactive(input$go,{
    x = ncol(attr_ranked_p())
    n = ceiling(x/3)
    n  })
  NUM_CLUST_P = eventReactive(input$go,{
    5
  })
  MAXLEVEL = eventReactive(input$update,{
    input$maxlevel
  })
  

  
  
  output$rawdata = renderDataTable({
    upload()
  })
  
  source("BarPlots.R",local=TRUE)
  
  source("CrossTab.R",local=TRUE)
  
  source("ctree_and_rf.R",local=TRUE)
  
  source("OverView.R",local=TRUE)
  
  source("ScatterPlot.R",local=TRUE)
  
  source("Interaction.R",local=TRUE)
}

