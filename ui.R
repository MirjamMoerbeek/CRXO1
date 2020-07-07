library(shinythemes)
library(plotly)
navbarPage(theme = shinytheme("cerulean"),"Unanticipated attrition in the cluster randomized crossover design",
           tabPanel("Subject attrition in a cohort design",
                      fluidRow(
                        column(3,
                               h4("Sample size and effect size"),
                               numericInput("kk1", label = h6("Number of clusters per treatment sequence"), value = 10,min=2),
                               numericInput("delta1", label = h6("Standardized effect size"), value = 0.2,min=0,max=1,step=0.1)
                            
                        ),    
                        column(3,
                               h4("Attrition rates"),
                               numericInput("pp.AB1", label = h6("Attrition rate in sequence AB"), value = 0.3,min=0.1,max=0.9,step=0.1),
                               numericInput("pp.BA1", label = h6("Attrition rate in sequence BA"), value = 0.3,min=0.1,max=0.9,step=0.1)
                        ),
                        column(3, 
                               h4("Correlation coefficients"),
                               numericInput("eta1", label = h6("Within-cluster between-period correlation (eta)"), value = 0.1,min=0,max=1,step=0.05),
                               numericInput("rho1", label = h6("Within-cluster within-period correlation (rho)"), value = 0.2,min=0,max=1,step=0.05),
                               numericInput("xi1", label = h6("Within-cluster within-subject correlation (xi)"), value = 0.3,min=0,max=1,step=0.05)
                        ),
                        column(3, 
                               h4("Additional subjects"),
                               HTML("<br><br>"),
                               h6("All subjects who drop out are replaced by other subjects from their cluster."),
                               HTML("<br><br><br><br><br><br><br>"),
                               submitButton("Submit")
                        )
                        ),hr(),hr(),
                        fluidRow(
                          column(5,
                                 h4("Efficiency graph"),
                                 plotlyOutput("plot1a", width = 400, height = 500)),
                          column(5,
                                 h4("Power graph"),     
                                 plotlyOutput("plot1b", width = 400, height = 500))
                          )
                    ),
                    
                    
           tabPanel("Cluster attrition in a cohort design",
                    fluidRow(
                      column(3,
                             h4("Sample size and effect size"),
                             numericInput("kk2", label = h6("Number of clusters per treatment sequence"), value = 10,min=2),
                             numericInput("delta2", label = h6("Standardized effect size"), value = 0.2,min=0,max=1,step=0.1)
                             
                      ),    
                      column(3,
                             h4("Attrition rates"),
                             numericInput("pp.AB2", label = h6("Attrition rate in sequence AB"), value = 0.3,min=0.1,max=0.9,step=0.1),
                             numericInput("pp.BA2", label = h6("Attrition rate in sequence BA"), value = 0.3,min=0.1,max=0.9,step=0.1)
                      ),
                      column(3, 
                             h4("Correlation coefficients"),
                             numericInput("eta2", label = h6("Within-cluster between-period correlation (eta)"), value = 0.1,min=0,max=1,step=0.05),
                             numericInput("rho2", label = h6("Within-cluster within-period correlation (rho)"), value = 0.2,min=0,max=1,step=0.05),
                             numericInput("xi2", label = h6("Within-cluster within-subject correlation (xi)"), value = 0.3,min=0,max=1,step=0.05)
                      ),
                      column(3, 
                             h4("Additional subjects"),
                             numericInput("mm.extra2", label = h6("Increase number of subjects cluster-period 2 in those clusters that do not drop out with (give percentage):"), value = 100,min=0,max=100,step=1),
                             HTML("<br><br><br><br><br><br>"),
                             submitButton("Submit")
                      )
                    ),hr(),hr(),
                    fluidRow(
                      column(5,
                             h4("Efficiency graph"),
                             plotlyOutput("plot2a", width = 400, height = 500)),
                      column(5,
                             h4("Power graph"),     
                             plotlyOutput("plot2b", width = 400, height = 500))
                    )
           ),

           tabPanel("Cluster attrition in a cross-sectional design",
                    fluidRow(
                      column(3,
                             h4("Sample size and effect size"),
                             numericInput("kk3", label = h6("Number of clusters per treatment sequence"), value = 10,min=2),
                             numericInput("delta3", label = h6("Standardized effect size"), value = 0.2,min=0,max=1,step=0.1)
                             
                      ),    
                      column(3,
                             h4("Attrition rates"),
                             numericInput("pp.AB3", label = h6("Attrition rate in sequence AB"), value = 0.3,min=0.1,max=0.9,step=0.1),
                             numericInput("pp.BA3", label = h6("Attrition rate in sequence BA"), value = 0.3,min=0.1,max=0.9,step=0.1)
                      ),
                      column(3, 
                             h4("Correlation coefficients"),
                             numericInput("eta3", label = h6("Within-cluster between-period correlation (eta)"), value = 0.1,min=0,max=1,step=0.05),
                             numericInput("rho3", label = h6("Within-cluster within-period correlation (rho)"), value = 0.2,min=0,max=1,step=0.05)
                      ),
                      column(3, 
                      h4("Additional subjects"),
                      numericInput("mm.extra3", label = h6("Increase number of subjects cluster-period 2 in those clusters that do not drop out with (give percentage):"), value = 100,min=0,max=100,step=1),
                      HTML("<br>"),
                      submitButton("Submit")
                      )
                    ),hr(),hr(),
                    fluidRow(
                      column(5,
                             h4("Efficiency graph"),
                             plotlyOutput("plot3a", width = 400, height = 500)),
                      column(5,
                             h4("Power graph"),     
                             plotlyOutput("plot3b", width = 400, height = 500))
                    )
           )
           
)