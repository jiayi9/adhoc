# attr_ranked_p_re()
# para_ranked_fit()$DATA

source("global.R",local = TRUE)
Interaction_DF = eventReactive(input$interaction,{
  withProgress(message = "Interaction!",{
    ATTR = attr_ranked_p_re()
    ATTR = ATTR[,toupper(names(ATTR)) %in% attr_list_inter ,drop = FALSE]
    validate(need(ncol(ATTR)>0,"No Attributes in the list exist."))
    PARA = para_ranked_fit()$DATA
    STATUS = STATUS()
    M = InteractionMatrix(ATTR,PARA,STATUS)
    validate(need(nrow(M)>0,"All pvalues big."))
    M
  })
})


observeEvent(input$interaction,{
  
  N = 40
  top_n = ifelse(  nrow(Interaction_DF())>N, N, nrow(Interaction_DF()))
  X = Interaction_DF()[1:top_n,]
  
  
  PARA_UNIQUE = unique(X$PARA)
  n = length(PARA_UNIQUE)
  


  output$L = renderText(PARA_UNIQUE)
  

  
  
  output$boxplots_para = renderUI({
    
    boxplots_para_output_list = lapply(1:n,function(i){
      
      current_PARA = PARA_UNIQUE[i]
      temp = X[X$PARA == current_PARA, ]
      m = ceiling(nrow(temp)/3)
      HEIGHT = paste0(m*450 ,"px")
      
      name = paste("boxplot_para",i,sep="")
      text_name = paste("boxplot_para_text",i,sep="")
      tags$div(class = "group-output",
               hr(),
               h3(textOutput(text_name)),
               plotOutput(name,height = HEIGHT),
               br()  
      )
    })
    do.call(tagList,boxplots_para_output_list)
    
  })
  
  for(j in 1:n){
    local({
      my_i = j
      
      X = Interaction_DF()[1:top_n,]
      
      STATUS = STATUS()
      
      current_PARA = PARA_UNIQUE[my_i]
      
      temp = X[X$PARA == current_PARA, ]
      
      m = nrow(temp)
      
      name = paste("boxplot_para",my_i,sep="")
      
      P = lapply(1:m, function(i){
        attr_name = temp$ATTR[i]
        
        para_name = current_PARA
        
        attr = attr_ranked_p_re()[,   attr_name ]
        
        para = para_ranked_fit()$DATA[, para_name]
        PlotLogistGroup(attr,para,STATUS,
                        attr_name=attr_name,
                        para_name=para_name,
                        cutoff = 0.4,hide=1)        
      })
      
      
      output[[name]] = renderPlot({
        
        do.call(grid.arrange,c(P,ncol=3))
        
        
      })
      
      text_name = paste("boxplot_para_text",my_i,sep="")
      output[[text_name]] = renderText(current_PARA)
    })
  }  
})


# 
# observeEvent(input$interaction,{
#   
#   output$Interaction_DF = renderTable({
#     n = 20
#     X = Interaction_DF()
#     if(nrow(X)>n){
#       R = X[1:n,]
#     } else {
#       R = X
#     }
#     R = R[,1:3]
#     colnames(R) = c("Attributes","Parametrics","LR P value")
#     R
#   })
#   
#   n = ifelse(  nrow(Interaction_DF())>20, 20, nrow(Interaction_DF()))
#   
#   output$boxplots = renderUI({
#     
#     boxplots_output_list = lapply(1:n,function(i){
#       
#       name = paste("boxplot",i,sep="")
#       tags$div(class = "group-output",
#                plotOutput(name),
#                br()  
#       )
#     })
#     do.call(tagList,boxplots_output_list)
#     
#   })
#   
#   
#   for(j in 1:n){
#     local({
#       my_i = j
#       
#       X = Interaction_DF()
#       
#       attr_name = X$ATTR[my_i]
#       
#       para_name = X$PARA[my_i]
#       
#       attr = attr_ranked_p_re()[,   attr_name ]
#       
#       para = para_ranked_fit()$DATA[, para_name]
#       
#       STATUS = STATUS()
#       
#       
#       name = paste("boxplot",my_i,sep="")
#       
#       output[[name]] = renderPlot({
#         PlotLogistGroup(attr,para,STATUS,
#                         attr_name=attr_name,
#                         para_name=para_name,
#                         cutoff = 0.4,hide=1)
#         
#         
#       })
#     })
#   }
#   
#   
# })



