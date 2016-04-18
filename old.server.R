library(ggplot2)
library(gtable)
library(ClustOfVar)
library(ggdendro)
library(gridExtra)
library(scales)
library(plyr)
library(ggvis)
options(shiny.maxRequestSize=100*1024^2)

source("www/FUNCTIONS.R",local = TRUE)

server <- function(input, session,output) {
  
  upload = reactive({

    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    R = read.csv(inFile$datapath, header=TRUE, sep=",")
    validate(need(!is.null(R), "Please Upload Data!"))
    R
    
  })
  
  output$info = renderPrint({
    cat("The file contains",nrow(upload()),"rows",ncol(upload()),"cols.")
  })
  
  
  temp = reactive({
    withProgress(message = 'Processing uploaded data 1', {
    validate(need(!is.null(upload()), "Please Upload Data!"))
    
    RAW = upload()
    STATUS = as.character(RAW$STATUS)
    RAW = data.frame(sapply(RAW,as.character),stringsAsFactors = FALSE)
    RAW[is.na(RAW)|RAW==""] = "N/A"
    LEVELS = sapply(RAW,function(x) length(unique(x)))
    raw = RAW[,LEVELS>1&LEVELS<input$maxlevel,drop=FALSE]
    X = raw[,!(names(raw) %in% "STATUS")]
    validate(need(ncol(X) != 0, "All attributes fail to survive.\n Try to increase p-value."))
    validate(need( nrow(X) == length(STATUS), "differing number of rows \n Try to increase p-value."))
    fit=data_ranked_Chisq(X,STATUS,5)
    fit
    })
  })
  
  temp2 = eventReactive(input$update,{
    withProgress(message = 'Processing uploaded data 2', {
    DATA = temp()$DATA
    pvalues = temp()$P
    X = DATA[,pvalues<input$maxp]
    validate(need(ncol(X) != 0, "All attributes fail to survive.\n Try to increase p-value."))
    validate(need(ncol(X) > 2, "No more than 3 attributes survived.\n Clustering will fail.\n Try to increase p-value."))
    X
    })
  })
  
  labelx = eventReactive(input$update,{
    validate(need(
      ncol(temp2())>input$num_clust_A,"The number of attributes is less than the number of clusters.\nTry decreasing the number of attributes clusters."
                  ))
    withProgress(message = 'Generating labels', {
    D = temp2()
    fit = ClustOfVar::hclustvar(X.quali = Random_Sample_prop(D,1))
    #fit = ClustOfVar::hclustvar(X.quali = D)
    labels = cutree(fit,input$num_clust_A)
    labelx = data.frame(Names=names(labels),group = paste("Group",as.vector(labels)),num=as.vector(labels))
    labelx
    })
  })
  
  ATTR = reactive({
    withProgress(message = 'Processing uploaded data 3', {
    R = rearrange(X = temp2(), clust = labelx())
    validate(need(!is.null(R),"Please Upload Data!"))
    R
    })
  })
  
  output$L = renderPrint({
    cbind(LEVELS = sapply(temp2(),function(x) length(unique(x))))
  })
  
  output$LL = renderPrint({
    names(ATTR())
  })
  
  output$pvalues = renderPrint({
    temp()$P
  })
  
  output$LLL = renderPlot({
    index = 1
    y = upload()$STATUS
    grid.draw(barplot_1(ATTR()[,index],
                        y,
                        names(ATTR())[index],
                        temp()$P[index],
                        labelx(),
                        4
    ))
  })
  
  
  observeEvent(input$update,{
    withProgress(message = 'Making barcharts', {
    validate(need(ncol(ATTR()) != 0, "All attributes fail to survive."))
    validate(need( !is.null(upload()), "Need data"))
    
    Ncol = ncol(ATTR())
    ROWS = floor( Ncol/5 )
    
    output$hist = renderUI({
      if (ROWS>0) {
      table_output_list = lapply(1:ROWS,function(i){
        
        name = paste("row",i,sep="")
        tags$div(class = "group-output",
                 plotOutput(name),
                 br()  
        )
      })
      do.call(tagList,table_output_list)
      } else{
        NULL
      }
    })
    
    if(ROWS>0){
    for(j in 1:ROWS){
      local({
        my_i = j
        
        name = paste("row",my_i,sep="")
        
        output[[name]] = renderPlot({
          y=upload()$STATUS
          for(i in 1:5){
            index=my_i*5-5+i
            assign(paste("p",i,sep=""),
                   barplot_1(ATTR()[,index],
                             y,
                             names(ATTR())[index],
                             #temp()$P[index], 
                             chisq_test(ATTR()[,index],y,50),
                             labelx(),
                             input$num_clust_A
                   ),envir = globalenv() 
            )
          }
          g = cbind(p1,p2,p3,p4,p5, size="first")
          g$heights = grid::unit.pmax(p1$heights, p2$heights,p3$heights,p4$heights,p5$heights)
          grid::grid.newpage()
          grid::grid.draw(g)
          
        })
      })
    }
    }
    
    Nleft = Ncol%%5
    
    output$hist2 = renderPlot({
      
      X3 = ATTR()
      STATUS = upload()$STATUS
      labelx = labelx()
      eachRow=5
      N=input$num_clust_A
      Nrow=ROWS
      if(Nleft>0){
        pvalues=c()
        pvalues[(Nrow*eachRow+1):(Nrow*eachRow+Nleft)] = sapply(1:Nleft, function(i){chisq_test(X3[,Nrow*eachRow+i],STATUS,5)})
      }
      

      
      p0=ggplot(mtcars, aes(x = wt, y = mpg)) + geom_blank()+theme(
        panel.background = element_rect(fill = NA), 
        panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank()
      )
      p0 = ggplotGrob(p0)
      
      if(Nleft==1){
        pp1 = barplot_1( X3[,Nrow*eachRow+1],
                         STATUS,
                         names(X3)[Nrow*eachRow+1],
                         pvalues[Nrow*eachRow+1],
                         labelx,
                         N)
        gx = cbind(pp1, p0,p0,p0,p0, size="first")
        gx$heights = grid::unit.pmax(pp1$heights)
        
      }
      if(Nleft==2){
        pp1 = barplot_1( X3[,Nrow*eachRow+1],
                         STATUS,
                         names(X3)[Nrow*eachRow+1],
                         pvalues[Nrow*eachRow+1],
                         labelx,
                         N)
        
        pp2 = barplot_1( X3[,Nrow*eachRow+2],
                         STATUS,
                         names(X3)[Nrow*eachRow+2],
                         pvalues[Nrow*eachRow+2],
                         labelx,
                         N)
        
        gx = cbind(pp1, pp2,p0,p0,p0, size="first")
        gx$heights = grid::unit.pmax(pp1$heights,pp2$heights)
        
      }
      if(Nleft==3){
        pp1 = barplot_1( X3[,Nrow*eachRow+1],
                         STATUS,
                         names(X3)[Nrow*eachRow+1],
                         pvalues[Nrow*eachRow+1],
                         labelx,
                         N)
        
        pp2 = barplot_1( X3[,Nrow*eachRow+2],
                         STATUS,
                         names(X3)[Nrow*eachRow+2],
                         pvalues[Nrow*eachRow+2],
                         labelx,
                         N)
        
        pp3 = barplot_1( X3[,Nrow*eachRow+3],
                         STATUS,
                         names(X3)[Nrow*eachRow+3],
                         pvalues[Nrow*eachRow+3],
                         labelx,
                         N)  
        gx = cbind(pp1, pp2,pp3,p0,p0, size="first")
        gx$heights = grid::unit.pmax(pp1$heights,pp2$heights,pp3$heights)
        
      }
      if(Nleft==4){
        pp1 = barplot_1( X3[,Nrow*eachRow+1],
                         STATUS,
                         names(X3)[Nrow*eachRow+1],
                         pvalues[Nrow*eachRow+1],
                         labelx,
                         N)
        
        pp2 = barplot_1( X3[,Nrow*eachRow+2],
                         STATUS,
                         names(X3)[Nrow*eachRow+2],
                         pvalues[Nrow*eachRow+2],
                         labelx,
                         N)
        
        pp3 = barplot_1( X3[,Nrow*eachRow+3],
                         STATUS,
                         names(X3)[Nrow*eachRow+3],
                         pvalues[Nrow*eachRow+3],
                         labelx,
                         N)  
        pp4 = barplot_1( X3[,Nrow*eachRow+4],
                         STATUS,
                         names(X3)[Nrow*eachRow+4],
                         pvalues[Nrow*eachRow+4],
                         labelx,
                         N)  
        gx = cbind(pp1, pp2,pp3,pp4,p0, size="first")
        gx$heights = grid::unit.pmax(pp1$heights,pp2$heights,pp3$heights,pp4$height)
        
      }
      grid::grid.newpage()
      if(Nleft ==0 ){NULL}else(grid::grid.draw(gx))
      
    })
    })
  })
  
  output$clust_1 = renderPlot({
    validate(need(
      ncol(temp2())>input$num_clust_A,"The number of attributes is less than the number of clusters.\nTry decreasing the number of attributes clusters."
    ))
    withProgress(message = 'Clustering attributes', {
    N=input$num_clust_A
    D = temp2()
    fit = ClustOfVar::hclustvar(X.quali = Random_Sample_prop(D,1))
    
    p1 = ggdendrogram(as.dendrogram(fit), rotate=TRUE)
    
    df2<-data.frame(cluster=cutree(fit,N),states=factor(fit$labels,levels=fit$labels[fit$order]))
    df3<-ddply(df2,.(cluster),summarise,pos=mean(as.numeric(states)))
    p2 = ggplot(df2,aes(states,y=1,fill=factor(cluster)))+geom_tile()+
      scale_y_continuous(expand=c(0,0))+
      theme(axis.title=element_blank(),
            axis.ticks=element_blank(),
            axis.text=element_blank(),
            legend.position="none")+coord_flip()+
      geom_text(data=df3,aes(x=pos,label=cluster))
    gp1<-ggplotGrob(p1) 
    gp2<-ggplotGrob(p2)  
    maxHeight = grid::unit.pmax(gp1$heights[2:5], gp2$heights[2:5])
    gp1$heights[2:5] <- as.list(maxHeight)
    gp2$heights[2:5] <- as.list(maxHeight)
    grid.arrange(gp2, gp1, ncol=2,widths=c(1/6,5/6))
    })
  })
  
  #---------------------------------------------PARA----------------------------------  
  
  temp_para = reactive({
    validate(need(!is.null(upload()) , "Please Upload Data!"))
    
    RAW = upload()
    STATUS = as.character(RAW$STATUS)
    LEVELS = sapply(RAW,function(x) length(unique(x)))
    raw = RAW[, sapply(RAW,is.numeric)&LEVELS>0.2*nrow(RAW) ]
    
    validate(need(ncol(raw)>17 , "Not enough parameterics!"))
    
    fit=data_ranked_logist(raw,STATUS=="F")
    fit
  })
  
  output$LLLLL = renderPrint({names(temp_para())})
  
  output$para_hint = renderText({
    validate(need(ncol(temp_para()) != 0, "No parametrics"))
    if(ncol(temp_para())>=18) {x = " "} else {x = "Note: Less than specified number of variables."}
    x
  })
  
  
  
  output$clust2 = renderPlot({
    withProgress(message = 'Clustering parametrics', {
    fit = ClustOfVar::hclustvar(X.quanti =  temp_para()[,1:18])
    p1 = ggdendrogram(as.dendrogram(fit), rotate=TRUE)
    
    df2<-data.frame(cluster=cutree(fit,input$num_clust_P),
                    states=factor(fit$labels,levels=fit$labels[fit$order]))
    df3<-ddply(df2,.(cluster),summarise,pos=mean(as.numeric(states)))
    p2 = ggplot(df2,aes(states,y=1,fill=factor(cluster)))+geom_tile()+
      scale_y_continuous(expand=c(0,0))+
      theme(axis.title=element_blank(),
            axis.ticks=element_blank(),
            axis.text=element_blank(),
            legend.position="none")+coord_flip()+
      geom_text(data=df3,aes(x=pos,label=cluster))
    gp1<-ggplotGrob(p1)
    gp2<-ggplotGrob(p2)  
    maxHeight = grid::unit.pmax(gp1$heights[2:5], gp2$heights[2:5])
    gp1$heights[2:5] <- as.list(maxHeight)
    gp2$heights[2:5] <- as.list(maxHeight)
    grid.arrange(gp2, gp1, ncol=2,widths=c(1/6,5/6))
    })
  })
  
  scatter_color = reactive({
    
    X =   temp_para()[,1:18]
    fit = ClustOfVar::hclustvar(X.quanti = Random_Sample_prop(X,1))
    temp = cutree(fit,input$num_clust_P)
    data.frame(NAME = names(temp),  group = as.vector(temp))
  })
  
  #output$para = renderPrint({temp_para()})
  
  
  
  
  output$scatter1 = renderPlot({
    withProgress(message = 'Making scatterplots', {
    
    D = temp_para()[,1:18]
    FAIL = upload()$STATUS
    highlight =   (FAIL == "F")
    color = scatter_color()
    for ( i in 1:9){
      x = D[,2*i-1]
      y = D[,2*i]
      namex = names(D)[2*i-1]
      namey = names(D)[2*i]
      temp = data.frame(x,y)
      temp2 = Random_Sample_prop(temp,0.4)
      temp3 = temp[highlight,]
      
      xcol = ggplotColours(input$num_clust_P)[  color[color[,1]==namex,2]      ]
      ycol = ggplotColours(input$num_clust_P)[  color[color[,1]==namey,2]      ]
      
      p = ggplot(data=temp2,aes(x=x,y=y)) +
        geom_point()+
        xlab(namex)+ ylab(namey)+
        geom_point(data=temp3,aes(x=x,y=y),color="red",size=4)+
        theme(legend.position="none")+
        
        theme(axis.title.x=element_text(colour=xcol))+
        theme(axis.title.y=element_text(colour=ycol))
      
      assign(paste("ph",i,sep=""),p,envir = globalenv() )
      
    }
    grid.arrange(ph1,ph2,ph3,ph4,ph5,ph6,ph7,ph8,ph9)
    })
  })
  
  
  #------------ 2-way bar chart
  output$factor1 <- renderUI({
    selectInput("factor_1", "Factor 1:",  choices = names(ATTR()),selected = names(ATTR())[1])
  }) 
  
  output$factor2 <- renderUI({
    selectInput("factor_2", "Factor 2:",  choices = names(ATTR()),selected = names(ATTR())[2])
  }) 
  
  output$interaction = renderPlot({
    withProgress(message = 'Making 2-way barchart', {
    if(is.null(input$factor_1) & is.null(input$factor_2)){
      x1 = ATTR()[,1]
      x2 = ATTR()[,2]
      FAIL = upload()$STATUS
      xname1 = names(ATTR())[1]
      xname2 = names(ATTR())[2]
    } else{
      x1 = ATTR()[,input$factor_1]
      x2 = ATTR()[,input$factor_2]
      FAIL = upload()$STATUS
      xname1 = input$factor_1
      xname2 = input$factor_2
    }
    barplot_2(x1,x2,FAIL,xname1,xname2)
    })
  })
  
  
  #------------number of clusters updates------------------------------
#   observeEvent(input$update,{
#     validate(need(ncol(ATTR()) != 0, "All attributes fail to survive."))
#     data <- ATTR()
#     n1 = ncol(data)
#     n_attr = ceiling(n1/5)
#     if(n_attr<3) n_attr=3
#     updateNumericInput(session, 'num_clust_A', value = n_attr)
#     
#     #       data <- temp_para()
#     #       n2 = ncol(data)
#     #       n_para = ceiling(n2/5)
#     #       if(n_para<3) n_para=3
#     #       
#     #       updateNumericInput(session, 'num_clust_P', value = n_para)
#   })
#   
  #------------ggvis scatterplot---------
  
  observe({
    data <- temp_para()
    var<-sapply(data,function(n) mode(n)=="numeric" & length(unique(n))>15)
    updateSelectInput(session, 'x', choices = names(data[,var]))
    updateSelectInput(session, 'y', choices = names(data[,var]))
  })
  
  yVarName<-reactive({
    input$y
  })
  
  xVarName<-reactive({
    input$x
  })
  
  
  filteredData<-reactive({
    data<-isolate(upload())
    # if there is no input, make a dummy dataframe
    if(input$x=="x" && input$y=="y"){
      if(is.null(data)){
        data<-data.frame(x=0,y=0,STATUS=0)
      }
    }
    else{
      data<-data[,c(input$x,input$y,"STATUS")]
      names(data)<-c("x","y","STATUS")
    }
    data
  })
  
  #--scatter
  vis<-reactive({
    
    plotData<-filteredData()
    plotData = na.omit(plotData)
    plotData$id = 1:nrow(plotData)
    
    y = plotData$STATUS
    X1 = plotData[y!="F",]
    X2 = plotData[y=="F",]
    
    
    all_values <- function(x) {
      if(is.null(x)) return(NULL)
      row <- plotData[plotData$id == x$id, ]
      paste0(c(xVarName(),yVarName()), ": ", format(row[,c(xVarName(),yVarName())]), collapse = "<br />")
      #paste0(c("A","B"), ": ", format(row[1:2]), collapse = "<br />")
      #paste0(names(row), ": ", format(row), collapse = "<br />")
    }
    
    
    
    
    plotData %>%
      ggvis(~x, ~y, opacity:=0.3, size:=30,opacity.hover:=0.9,size.hover:=200,key:=~id) %>%
      layer_points() %>%
      layer_points(data=X2,~x, ~y, size:=60, opacity:=1, opacity.hover:=1,size.hover:=200,fill:="red") %>%
      add_axis("y", title = yVarName()) %>%
      #add_axis("x", title = xVarName()) %>%
      add_tooltip(all_values,"hover") %>%
      add_axis("x",title = xVarName(), properties = axis_props(
        #           axis = list(stroke = "red", strokeWidth = 5),
        #           grid = list(stroke = "blue"),
        #           ticks = list(stroke = "blue", strokeWidth = 2),
        labels = list(angle = -90, align = "right")
      ))
    
    
    
    
    
    #       mtc <- mtcars
    #       mtc$id <- 1:nrow(mtc)
    #       
    #       all_values <- function(x) {
    #         if(is.null(x)) return(NULL)
    #         row <- mtc[mtc$id == x$id, ]
    #         paste0(c("x","y"), ": ", format(row[c(1,6)]), collapse = "<br />")
    #       }
    #       
    #       mtc %>%
    #         ggvis(x = ~wt, y = ~mpg, size.hover := 200, key:=~id,
    #               stroke := NA, stroke.hover := "red", strokeWidth := 3) %>%
    #         layer_points()%>%
    #         add_tooltip(all_values, "hover")
    
    
  })%>%bind_shiny("scatterD", "scatterD_ui")
  
  output$scatterD2 = renderPlot({
    
    D = filteredData()
    FAIL = upload()$STATUS
    
    highlight =   (FAIL == "F")
    color = scatter_color()
    
    x = D$x
    y = D$y
    namex = xVarName()
    namey = yVarName()
    temp = data.frame(x,y)
    temp2 = Random_Sample_prop(temp,1)
    temp3 = temp[highlight,]
    
    p = ggplot(data=temp2,aes(x=x,y=y)) +
      geom_point()+
      xlab(namex)+ ylab(namey)+
      geom_point(data=temp3,aes(x=x,y=y),color="red",size=4)+
      theme(legend.position="none")
    
    p
  })
  
  output$rawdata = renderDataTable({
    withProgress(message = 'Generating Raw Data', {
    upload()
    })
  })
  
  output$ctree = renderPlot({
    withProgress(message = 'Running Conditional Tree', {
    X = data.frame(lapply(ATTR(), as.factor))
    D = data.frame(data = X,STATUS = ifelse(upload()$STATUS=="F","F","P"))
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
    
    #       RAW = upload()
    #       STATUS = as.factor(RAW$STATUS)
    #       
    #       RAW = data.frame(sapply(RAW,as.character),stringsAsFactors = FALSE)
    #       
    #       RAW[is.na(RAW)|RAW==""] = "N/A"
    #       RAW = data.frame(sapply(RAW,as.factor))
    #       
    #       LEVELS = sapply(RAW,function(x) length(unique(x)))
    #       r#       RAW = upload()
    #       STATUS = as.factor(RAW$STATUS)
    #       
    #       RAW = data.frame(sapply(RAW,as.character),stringsAsFactors = FALSE)
    #       
    #       RAW[is.na(RAW)|RAW==""] = "N/A"
    #       RAW = data.frame(sapply(RAW,as.factor))
    #       
    #       LEVELS = sapply(RAW,function(x) length(unique(x)))
    #       raw = RAW[,LEVELS>1&LEVELS<input$maxlevel,drop=FALSE]
    #       X = raw[,!(names(raw) %in% "STATUS")]
    #       
    #       aw = RAW[,LEVELS>1&LEVELS<input$maxlevel,drop=FALSE]
    #       X = raw[,!(names(raw) %in% "STATUS")]
    #       
    #       
    #       
    #       X = data.frame(lapply(temp_para(), as.factor))
    #       D = data.frame(data = X,STATUS = upload()$STATUS)
    #       
    X = data.frame(lapply(ATTR(), as.factor))
    D = data.frame( X,STATUS = ifelse(upload()$STATUS=="F","F","P"))
    
    set.seed(415)
    RANK = randomForest(STATUS~., data=D, importance=TRUE, ntree=2000)
    p=varImpPlot(RANK, main = "Variable Importance Rank")
    p
    
  })
  })
}

