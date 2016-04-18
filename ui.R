


ui <- fluidPage(

  tags$head(tags$style(
  "
      
      .sidebar{height:1000px;background-color: #FAFAFA;
               font-family: 'Arial Narrow', Arial, sans-serif;}
      .grey{background-color: #FAFAFA; margin:10px;height:300px;
-webkit-border-radius: 10px;-moz-border-radius: 10px;border-radius: 10px;}

      .grey2{background-color: #FAFAFA; }

      .red{background-color: #F6CECE;padding:10px;margin:10px;
-webkit-border-radius: 10px;-moz-border-radius: 10px;border-radius: 10px;}
      .darkgrey{background-color: #A4A4A4;padding:10px;margin:5px;
-webkit-border-radius: 10px;-moz-border-radius: 10px;border-radius: 10px;}
  "
  )),
  

           navbarPage(
             "",
             tabPanel(
               "Upload & Define",icon = icon("th"),
               br(),br(),br(),

               fluidRow(

                 column(6,class="grey",
                        h3("#1 Upload and Set Criteria"),
                        fluidRow(
                          column(7,
                                 helpText("Please await when loading and processing."),
                                 fileInput('file1', '',
                                           accept=c('text/csv', 
                                                    'text/comma-separated-values,text/plain', 
                                                    '.csv'),width = "1200px"),
                                 textOutput("info"),
                                 br(),
                                 actionButton("update","OK & Reset",class="btn btn-primary btn-sm")#,icon=icon("fa fa-check"))
                                 
                                 
                          ),
                          column(5,
                                 numericInput(inputId="maxp",label="Cut-off P value for ATTRs",value=0.1,min=0.01,max=1,step=0.01),
                                 numericInput(inputId="maxlevel",label="Max number of Levels for ATTRs",value=15,min=3,max=50,step=1) 
                                 
                          )
                        ),
                        br(),
                        p(tags$b("↓"),align = "center")
                  ),
                 
                 
                 column(5,class="grey",
                        h3("#3"),
#                        numericInput(inputId="num_clust_A",label="Number of clusters Attr",value=5,min=2,max=50,step=1),
#                        numericInput(inputId="num_clust_P",label="Number of clusters Para",value=5,min=2,max=50,step=1),
                        actionButton("go","Ready & Analyze!",class="btn btn-primary btn-sm"),
                        helpText("Find results in tabs 'Attributes', 'Parametrics' and 'Interaction'."),
                  br(),br(),br(),br(),br(),br(),br(),
                        p(tags$b("↑"),align = "center")
                        
                 )
               ),
               
               
   
               

               hr(),
               h3("#2 Define Attributes"),
               #p(tags$b("→"),align = "center"),
               
              # verbatimTextOutput("L"),
               
               fluidRow(class="grey2",
                 column(4,class="grey2",
                        h4("Attributes Not Used"),
                        tableOutput("selected_attr_names_2")
                 ),

                 column(5,
                        br(),
                        p("Add  unused attributes to → the right box for analysis"),

                        uiOutput("CHOOSER")
                        ),
                 column(3,class="grey2",
                        h4("Attributes Used"),
                        tableOutput("selected_attr_names")
                 )


               ),
              

              br(),br(),br(),br(),              br(),br(),br(),br()

             ),
             tabPanel(
               "DataView",icon = icon("file"),
               br(),br(),br(),br(),
               dataTableOutput("rawdata")
             ),
             navbarMenu(
               "Attributes",icon = icon("bar-chart"),
               tabPanel("1-way BarPlots & Clustering",
                        br(),br(),br(),
                        h4("1-way BarPlots & Clustering"),
                        hr(),
                        textOutput("barplot_text"),
                        uiOutput("hist_1"),
                        uiOutput("hist_2_x"),
                        hr(),
                        plotOutput("attr_clust_chart",height="600px")),
               tabPanel("2-way BarPlot",
                        br(),br(),br(),
                        
                        h4("2-way BarPlot (Attributes Interaction)"),
                        hr(),
                        uiOutput("two_way")),
               tabPanel("Conditional Tree",
                        br(),br(),br(),
                        h4("Conditional Tree  (Tentative segmentation)"),
                        hr(),
                        plotOutput("ctree",height = "600px")),
               tabPanel("Random Forest",
                        br(),br(),br(),
                        h4("Random Forest  (Variable Importance Rank)"),
                        hr(),
                        plotOutput("randomforest",height = "600px"))
             ),
             navbarMenu(
               "Parametrics",icon = icon("line-chart"),
               #tabPanel("Ranked BoxPlots"),
               tabPanel("Overview & Clustering",
                        br(),br(),br(),
                        h4("Parametrics (Top 18) Overview & Clustering "),
                        helpText("This Tab is prone to occasional crash due to limited system charting capability. Refresh in case of errors."),
                        hr(),
                        uiOutput("para")),
                       #p("This page is currently not usable for web version of ADHOC.\nUser can download the app use this page.")),
               tabPanel("ScatterPlot",
                        br(),br(),br(),
                        h4("Parametrics Ranking(P value) & ScatterPlot"),
                        hr(),
                        fluidRow(
                          column(3,
                                 tableOutput("ranked_para_names")
                                 ),
                          column(9,
                                 uiOutput("parametrics")
                                 )

                        )
               )
             ),
            tabPanel("Interaction",
                     br(),br(),br(),
                     #textOutput("L"),
                     h4("Attribute X Parametric Interaction"),
                     hr(),
                     actionButton("interaction","Run",class="btn btn-primary btn-sm"),
                     uiOutput("boxplots_para")
                     
                     
#                      fluidRow(
#                        column(4,
#                               actionButton("interaction","Run",class="btn btn-primary btn-sm"),
#                               br(),br(),
#                               tableOutput("Interaction_DF")),
#                        column(8,
#                               uiOutput("boxplots_para"))
#                      )
         
         
),

             windowTitle = "Ad Hoc Analysis",
             position = "fixed-top"
           ),

  hr(),
  helpText("ning.h.he@seagate.com"),
  helpText("jiayi.l.lu@seagate.com")
)

