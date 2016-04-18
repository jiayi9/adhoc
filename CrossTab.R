
output$factor1 <- renderUI({
  selectInput("factor_1", "Factor 1:",  choices = names(attr_ranked_p_re()),selected = names(attr_ranked_p_re())[1])
}) 

output$factor2 <- renderUI({
  selectInput("factor_2", "Factor 2:",  choices = names(attr_ranked_p_re()),selected = names(attr_ranked_p_re())[2])
}) 

output$interaction = renderPlot({
  withProgress(message = 'Making two way barchart', {
    if(is.null(input$factor_1) & is.null(input$factor_2)){
      x1 = attr_ranked_p_re()[,1]
      x2 = attr_ranked_p_re()[,2]
      FAIL = STATUS()
      xname1 = names(attr_ranked_p_re())[1]
      xname2 = names(attr_ranked_p_re())[2]
    } else{
      x1 = attr_ranked_p_re()[,input$factor_1]
      x2 = attr_ranked_p_re()[,input$factor_2]
      FAIL = STATUS()
      xname1 = input$factor_1
      xname2 = input$factor_2
    }
    barplot_2(x1,x2,FAIL,xname1,xname2)
  })
})

output$two_way = renderUI({
  tags$div(class = "group-output",
           fluidRow(
             column(3,
                    uiOutput("factor1"),
                    uiOutput("factor2")
                    ),
             column(9,
                    plotOutput("interaction")
                    )
           ),


           br(),br(),br(),br(),br()
  )
  
})