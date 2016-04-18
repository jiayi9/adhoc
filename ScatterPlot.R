
output$para_select <- renderUI({
  selectInput("uni_para", "Parameter:",  choices = names(para_ranked_fit()$DATA),selected = names(para_ranked_fit()$DATA)[1])
}) 

output$para_scatter_1 <- renderUI({
  selectInput("para_scatter_1", "X parameter:",  choices = names(para_ranked_fit()$DATA),selected = names(para_ranked_fit()$DATA)[1])
}) 

output$para_scatter_2 <- renderUI({
  selectInput("para_scatter_2", "Y parameter:",  choices = names(para_ranked_fit()$DATA),selected = names(para_ranked_fit()$DATA)[2])
}) 

#single chart
# output$single_para = renderPlot({
#   withProgress(message = 'Making boxplot', {
#     FAIL = STATUS()
#     WEEK = upload()$CEE_TEST_WORK_WEEK
#     highlight =   (FAIL == "F")
#     
#     if(is.null(input$uni_para)) {
#       temp = data.frame(WEEK=WEEK,y=para_ranked_fit()$DATA[,1])
#     } else {
#       temp = data.frame(WEEK=WEEK,y=para_ranked_fit()$DATA[,input$uni_para] )
#     }
#     
#     temp2 = Random_Sample_prop(  temp, 1)
#     temp3 = subset(temp,highlight)
#     
#     p = ggplot(data=temp2,aes(WEEK,y))+
#       geom_boxplot()+
#       geom_jitter()+
#       xlab("Fiscal Week")+
#       ylab("Value")+
#       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#       geom_point(
#         data = temp3,aes(WEEK,y),color="red",size=4
#       ) 
#     p
#   })
# })

output$scatterplot = renderPlot({
  withProgress(message = 'Making scatterplot', {
    FAIL = STATUS()
    highlight =   (FAIL == "F")
    
    if(is.null(input$para_scatter_1) & is.null(input$para_scatter_2)){
      temp = data.frame(x = para_ranked_fit()$DATA[,1], y = para_ranked_fit()$DATA[,2])
      namex = names(para_ranked_fit()$DATA)[1]
      namey = names(para_ranked_fit()$DATA)[2]
    } else {
      temp = data.frame(x = para_ranked_fit()$DATA[,input$para_scatter_1], y = para_ranked_fit()$DATA[,input$para_scatter_2])
      namex = input$para_scatter_1
      namey = input$para_scatter_2
    }
    
    temp2 = Random_Sample_prop(  temp, 1)
    temp3 = subset(temp,highlight)
    
    ggplot(data=temp2,aes(x=x,y=y)) +
      geom_point()+
      #geom_smooth(method=lm) +
      stat_ellipse(type = "norm",linetype = 2)+
      xlab(namex)+ ylab(namey)+
      geom_point(data=temp3,aes(x=x,y=y),color="red",size=4)+theme(legend.position="none")
  })
})

output$parametrics = renderUI({
  div(

    fluidRow(
      column(3,
             uiOutput("para_scatter_1"),
             uiOutput("para_scatter_2")
      ),
      column(9,
             plotOutput("scatterplot",height="500px")
      )
    )
#    hr(),
#     fluidRow(
#       column(3,uiOutput("para_select")),
#       column(9,plotOutput("single_para"))
#     )
  )
})

# output$fails_table = renderTable({
#   withProgress(message = 'Making failure table', {
#     FAIL = FAIL()
#     highlight =   (FAIL == "F")
#     
#     if(is.null(input$para_scatter_1) & is.null(input$para_scatter_2)){
#       temp = data.frame(x = para_ranked_fit()$DATA[,1], y = para_ranked_fit()$DATA[,2])
#       namex = names(para_ranked_fit()$DATA)[1]
#       namey = names(para_ranked_fit()$DATA)[2]
#     } else {
#       temp = data.frame(x = para_ranked_fit()$DATA[,input$para_scatter_1], y = para_ranked_fit()$DATA[,input$para_scatter_2])
#       namex = input$para_scatter_1
#       namey = input$para_scatter_2
#     }
#     names(temp)=c(namex,namey)
#     
#     R = subset(temp,highlight)
#     SN = subset(ID()[,c(1,3)],highlight)
#     
#     min1 = min(temp[,1],na.rm = TRUE)
#     min2 = min(temp[,2],na.rm = TRUE)
#     max1 = max(temp[,1],na.rm = TRUE)
#     max2 = max(temp[,2],na.rm = TRUE)
#     
#     v1 = sapply(R[,1], function(i) sum(i>=temp[,1],na.rm=TRUE)/nrow(temp))
#     v2 = sapply(R[,2], function(i) sum(i>=temp[,2],na.rm=TRUE)/nrow(temp))
#     
#     
#     R2 = data.frame(SN,round(v1,2),round(v2,2))
#     names(R2)=c("DRIVE_SN","HEAD_SN",namex,namey)
#     R2 = R2[order(R2[,3]),]
#     R2
#   })
# },include.rownames=FALSE)
