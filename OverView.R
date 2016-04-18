scatterData = eventReactive(list(input$flush,input$go), {
  #validate(need(ncol(para_ranked_top())>0,""))
  
  para_ranked_top()[,sample(1:18)]
},ignoreNULL=TRUE)

scatter_color = reactive({
  #validate(need(ncol(para_ranked_top())>0,""))
  
  X = para_ranked_top()
  fit = ClustOfVar::hclustvar(X.quanti =  X[,1:18])
  
  temp = cutree(fit,NUM_CLUST_P())
  data.frame(NAME = names(temp),  group = as.vector(temp))
})

observeEvent(list(input$flush,input$go),{
  withProgress(message = 'Drawing top 18 parametrics', {
  D = scatterData()
  FAIL = STATUS()
  highlight =   (FAIL == "F")
  
  color = scatter_color()
  
  for ( i in 1:9){
    x = D[,2*i-1]
    y = D[,2*i]
    namex = names(D)[2*i-1]
    namey = names(D)[2*i]
    temp = data.frame(x,y)
    temp2 = Random_Sample_prop(temp,0.1)
    temp3 = temp[highlight,]
    
    xcol = ggplotColours(NUM_CLUST_P())[  color[color[,1]==namex,2]      ]
    ycol = ggplotColours(NUM_CLUST_P())[  color[color[,1]==namey,2]      ]
    
    p = ggplot(data=temp2,aes(x=x,y=y)) +
      geom_point()+
      xlab(namex)+ ylab(namey)+
      geom_point(data=temp3,aes(x=x,y=y),color="red",size=4)+
      theme(legend.position="none")+
      theme(axis.title.x=element_text(colour=xcol))+
      theme(axis.title.y=element_text(colour=ycol))
    
    assign(paste("p",i,sep=""),p)
    
  }
  output$scatter1 =renderPlot({
    grid::grid.newpage()
    g=cbind(ggplotGrob(p1),ggplotGrob(p2),ggplotGrob(p3),size="first")
    grid::grid.draw(g)
    
    })
  
  output$scatter2 =renderPlot({
    grid::grid.newpage()
    g=cbind(ggplotGrob(p4),ggplotGrob(p5),ggplotGrob(p6),size="first")
    grid::grid.draw(g)
  })
  
  output$scatter3 =renderPlot({
    grid::grid.newpage()
    g=cbind(ggplotGrob(p7),ggplotGrob(p8),ggplotGrob(p9),size="first")
    grid::grid.draw(g)
    
  })
  
#   output$scatter1 = renderPlot({p1})
#   output$scatter2 = renderPlot({p2})
#   output$scatter3 = renderPlot({p3})
#   output$scatter4 = renderPlot({p4})
#   output$scatter5 = renderPlot({p5})
#   output$scatter6 = renderPlot({p6})
#   output$scatter7 = renderPlot({p7})
#   output$scatter8 = renderPlot({p8})
#   output$scatter9 = renderPlot({p9})
  })
  
})


# output$para_tops = renderPlot({
#   withProgress(message = 'Making scatterplots', {
# 
#     grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol=3)
#   })
# })

output$para_clust_chart = renderPlot({
  withProgress(message = 'Clustering parametrics', {
    fit = ClustOfVar::hclustvar(X.quanti =  para_ranked_top()[,1:18])
    p1 = ggdendrogram(as.dendrogram(fit), rotate=TRUE)
    
    df2<-data.frame(cluster=cutree(fit,NUM_CLUST_P()),
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


output$para = renderUI({
  tags$div(class = "group-output",
           actionButton("flush","FLUSH ORDER", icon = icon('fa fa-refresh')),
           #plotOutput("para_tops",height="900px"),
           plotOutput("scatter1"),
           plotOutput("scatter2"),
           plotOutput("scatter3"),
#            plotOutput("scatter4"),
#            plotOutput("scatter5"),
#            plotOutput("scatter6"),
#            plotOutput("scatter7"),
#            plotOutput("scatter8"),
#            plotOutput("scatter9"),
           hr(),
           plotOutput("para_clust_chart")
  )
})