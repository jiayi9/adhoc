output$barplot_text = renderText({
  n = ncol(attr_ranked_p_re())
  m = ceiling(n/3)
  paste("There are ",n," attributes and ",m," clusters.")
})

observeEvent(input$go,{
  
  withProgress(message = 'Making barcharts', {
    
    X = attr_ranked_p_re()
    Ncol = ncol(X)
    ROWS = floor( Ncol/5 )
    
    
    output$hist_1 = renderUI({
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
            X = attr_ranked_p_re()
            y=STATUS()
            for(i in 1:5){
              index=my_i*5-5+i
              assign(paste("p",i,sep=""),
                     barplot_1(X[,index],
                               y,
                               names(X)[index],
                               chisq_test(X[,index],y,floor(nrow(X)*0.05)),
                               labelx(),
                               NUM_CLUST_A()
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

    output$hist_2 = renderPlot({
      
      X3 = attr_ranked_p_re()
      STATUS = STATUS()
      labelx = labelx()
      eachRow=5
      N=NUM_CLUST_A()
      Nrow=ROWS
      if(Nleft>0){
        pvalues=c()
        pvalues[(Nrow*eachRow+1):(Nrow*eachRow+Nleft)] = sapply(1:Nleft, function(i){chisq_test(X3[,Nrow*eachRow+i],STATUS,floor(nrow(X3)*0.05))})
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
  
  if(Nleft == 0){
    output$hist_2_x = renderUI(

        plotOutput("hist_2",height="0px")

    )
  } else {
    output$hist_2_x = renderUI(
      
        plotOutput("hist_2")
  
    )
  }
  

  output$attr_clust_chart = renderPlot({
    withProgress(message = 'Drawing attr clustering dendrogram', {
      X = attr_ranked_p_re()
      fit = ClustOfVar::hclustvar(X.quali = X)
      
      p1 = ggdendrogram(as.dendrogram(fit), rotate=TRUE)
      
      df2<-data.frame(cluster=cutree(fit,NUM_CLUST_A()),states=factor(fit$labels,levels=fit$labels[fit$order]))
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
  
  
  
})


