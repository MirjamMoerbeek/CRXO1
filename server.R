library(shiny)
library(plotly)
library(tidyr)
library(plyr)
source("functions.R")

function(input, output, session) {

######################################################################################################################
######################################################################################################################
######################################################################################################################
### Part 1: Subject attrition in a cohort design
######################################################################################################################
######################################################################################################################
######################################################################################################################
  
################################################################################################################################
### Part 1: Efficiency graph 
################################################################################################################################  
output$plot1a <- renderPlotly({
    
  kk=2*input$kk1 # total number of clusters = 2* number of clusters per arm
  pp.AB=input$pp.AB1
  pp.BA=input$pp.BA1
  eta=input$eta1
  rho=input$rho1
  xi=input$xi1

  mm=seq(10,100,by=10)
  var1=rep(0,length(mm))
  for(ii in 1:length(mm))
    {
    var1[ii]=f.var1(mm[ii],kk,eta,rho,xi)
    }
  
  var2=rep(0,length(mm))
  for(ii in 1:length(mm))
    {
    var2[ii]=f.var2(mm[ii],kk,eta,rho,xi,pp.AB,pp.BA)
  }
  
  var3=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
     var3[ii]=f.var2a(mm[ii],kk,eta,rho,xi,pp.AB,pp.BA)
  }



  RE1=var1/var1
  RE2=var1/var2
  RE3=var1/var3


  min.yaxis=0.95*min(c(RE1,RE2,RE3))
  max.yaxis=1.05*max(c(RE1,RE2,RE3))

  data=data.frame(mm,RE1,RE2,RE3)
  key <- row.names(data)
  plot_ly(x = ~data[,1], y = ~data[,2], name = 'No attrition', key = ~key, mode ='lines+markers',type="scatter",width=1,showlegend=TRUE,marker = list(color ='black'),line = list(color = 'black')) %>%
    add_trace(y = ~data[,3], name = 'Attrition', key = ~key, mode = 'lines+markers',marker = list(color ='red'),line = list(color = 'red')) %>%
    add_trace(y = ~data[,4], name = 'Attrition, replacement with other subjects', key = ~key, mode = 'lines+markers',marker = list(color ='limegreen'),line = list(color = 'limegreen')) %>%
    layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,y=-0.25)) %>% 
    layout(dragmode = "select",
           xaxis = list(title = "Number of subjects per cluster-period", range = c(0, 110), showgrid = F,zeroline=TRUE,showline = TRUE),
           yaxis = list(title = "Relative Efficiency", range=c(min.yaxis,max.yaxis),showgrid = T,zeroline=TRUE,showline = TRUE))

       })
  
################################################################################################################################
### Part 1: Power graph
################################################################################################################################  
output$plot1b <- renderPlotly({
  
  kk=2*input$kk1 # total number of clusters = 2* number of clusters per arm
  pp.AB=input$pp.AB1
  pp.BA=input$pp.BA1
  eta=input$eta1
  rho=input$rho1
  xi=input$xi1
  ES=input$delta1
  alpha=0.05
  
  mm=seq(10,100,by=10)
  var1=rep(0,length(mm))
  
  for(ii in 1:length(mm))
  {
    var1[ii]=f.var1(mm[ii],kk,eta,rho,xi)
  }
  
  var2=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var2[ii]=f.var2(mm[ii],kk,eta,rho,xi,pp.AB,pp.BA)
  }
  
  var3=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var3[ii]=f.var2a(mm[ii],kk,eta,rho,xi,pp.AB,pp.BA)
  }
  

  power1=pnorm(ES/sqrt(var1)-qnorm(1-alpha/2))
  power2=pnorm(ES/sqrt(var2)-qnorm(1-alpha/2))
  power3=pnorm(ES/sqrt(var3)-qnorm(1-alpha/2))

  data=data.frame(mm,power1,power2,power3)

  key <- row.names(data)
  plot_ly(x = ~data[,1], y = ~data[,2], name = 'No attrition', key = ~key, mode ='lines+markers',type="scatter",width=1,showlegend=TRUE,marker = list(color ='black'),line = list(color = 'black')) %>%
    add_trace(y = ~data[,3], name = 'Attrition', key = ~key, mode = 'lines+markers',marker = list(color ='red'),line = list(color = 'red')) %>%
    add_trace(y = ~data[,4], name = 'Attrition, replacement with other subjects', key = ~key, mode = 'lines+markers',marker = list(color ='limegreen'),line = list(color = 'limegreen')) %>%
    layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,y=-0.25)) %>% 
    layout(dragmode = "select",
           xaxis = list(title = "Number of subjects per cluster-period", range = c(0, 110), showgrid = F,zeroline=TRUE,showline = TRUE),
           yaxis = list(title = "Power", range=c(0,1),showgrid = T,zeroline=TRUE,showline = TRUE))
  
})

######################################################################################################################
######################################################################################################################
######################################################################################################################
### Part 2: Cluster attrition in a cohort design
######################################################################################################################
######################################################################################################################
######################################################################################################################
    
################################################################################################################################
### Part 2: Efficiency graph 
################################################################################################################################  
output$plot2a <- renderPlotly({
  
  kk=2*input$kk2 # total number of clusters = 2* number of clusters per arm
  pp.AB=input$pp.AB2
  pp.BA=input$pp.BA2
  eta=input$eta2
  rho=input$rho2
  xi=input$xi2
  mm.extra=input$mm.extra2
  
  mm=seq(10,100,by=10)
  var1=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var1[ii]=f.var1(mm[ii],kk,eta,rho,xi)
  }
  
  var2=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var2[ii]=f.var3(mm[ii],kk,eta,rho,xi,pp.AB,pp.BA)
  }
  

  var3=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var3[ii]=f.var3a(mm[ii],kk,eta,rho,xi,pp.AB,pp.BA,mm.extra)
  } 
  

  RE1=var1/var1
  RE2=var1/var2
  RE3=var1/var3

  min.yaxis=0.95*min(c(RE1,RE2,RE3))
  max.yaxis=1.05*max(c(RE1,RE2,RE3))
  
  data=data.frame(mm,RE1,RE2,RE3)
  key <- row.names(data)
  plot_ly(x = ~data[,1], y = ~data[,2], name = 'No attrition', key = ~key, mode ='lines+markers',type="scatter",width=1,showlegend=TRUE,marker = list(color ='black'),line = list(color = 'black')) %>%
    add_trace(y = ~data[,3], name = 'Attrition', key = ~key, mode = 'lines+markers',marker = list(color ='red'),line = list(color = 'red')) %>%
    add_trace(y = ~data[,4], name = 'Attrition, extra subjects', key = ~key, mode = 'lines+markers',marker = list(color ='limegreen'),line = list(color = 'limegreen')) %>%
    layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,y=-0.25)) %>% 
    layout(dragmode = "select",
           xaxis = list(title = "Number of subjects per cluster-period", range = c(0, 110), showgrid = F,zeroline=TRUE,showline = TRUE),
           yaxis = list(title = "Relative Efficiency", range=c(min.yaxis,max.yaxis),showgrid = T,zeroline=TRUE,showline = TRUE))
  
})

################################################################################################################################
### Part 2: Power graph
################################################################################################################################  
output$plot2b <- renderPlotly({
  
  kk=2*input$kk2 # total number of clusters = 2* number of clusters per arm
  pp.AB=input$pp.AB2
  pp.BA=input$pp.BA2
  eta=input$eta2
  rho=input$rho2
  xi=input$xi2
  ES=input$delta2
  alpha=0.05
  mm.extra=input$mm.extra2
  
  mm=seq(10,100,by=10)
  var1=rep(0,length(mm))
  
  for(ii in 1:length(mm))
  {
    var1[ii]=f.var1(mm[ii],kk,eta,rho,xi)
  }
  
  var2=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var2[ii]=f.var3(mm[ii],kk,eta,rho,xi,pp.AB,pp.BA)
  }
  
  var3=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var3[ii]=f.var3a(mm[ii],kk,eta,rho,xi,pp.AB,pp.BA,mm.extra)
  } 
  

  power1=pnorm(ES/sqrt(var1)-qnorm(1-alpha/2))
  power2=pnorm(ES/sqrt(var2)-qnorm(1-alpha/2))
  power3=pnorm(ES/sqrt(var3)-qnorm(1-alpha/2))

  data=data.frame(mm,power1,power2,power3)
  
  key <- row.names(data)
  plot_ly(x = ~data[,1], y = ~data[,2], name = 'No attrition', key = ~key, mode ='lines+markers',type="scatter",width=1,showlegend=TRUE,marker = list(color ='black'),line = list(color = 'black')) %>%
    add_trace(y = ~data[,3], name = 'Attrition', key = ~key, mode = 'lines+markers',marker = list(color ='red'),line = list(color = 'red')) %>%
    add_trace(y = ~data[,4], name = 'Attrition, extra subjects', key = ~key, mode = 'lines+markers',marker = list(color ='limegreen'),line = list(color = 'limegreen')) %>%
    layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,y=-0.25)) %>% 
    layout(dragmode = "select",
           xaxis = list(title = "Number of subjects per cluster-period", range = c(0, 110), showgrid = F,zeroline=TRUE,showline = TRUE),
           yaxis = list(title = "Power", range=c(0,1),showgrid = T,zeroline=TRUE,showline = TRUE))
  
})
   
######################################################################################################################
######################################################################################################################
######################################################################################################################
### Part 3: Cluster attrition in a cross-sectional design
######################################################################################################################
######################################################################################################################
######################################################################################################################

################################################################################################################################
### Part 3: Efficiency graph 
################################################################################################################################  
output$plot3a <- renderPlotly({
  
  kk=2*input$kk3 # total number of clusters = 2* number of clusters per arm
  pp.AB=input$pp.AB3
  pp.BA=input$pp.BA3
  eta=input$eta3
  rho=input$rho3
  mm.extra=input$mm.extra3

  mm=seq(10,100,by=10)
  var1=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var1[ii]=f.var4(mm[ii],kk,eta,rho)
  }
  
  var2=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var2[ii]=f.var5(mm[ii],kk,eta,rho,pp.AB,pp.BA)
  }
  
  var3=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var3[ii]=f.var5a(mm[ii],kk,eta,rho,pp.AB,pp.BA,mm.extra)
  }

  


  RE1=var1/var1
  RE2=var1/var2
  RE3=var1/var3

  min.yaxis=0.95*min(c(RE1,RE2,RE3))
  max.yaxis=1.05*max(c(RE1,RE2,RE3))
  
  data=data.frame(mm,RE1,RE2,RE3)
  key <- row.names(data)
  plot_ly(x = ~data[,1], y = ~data[,2], name = 'No attrition', key = ~key, mode ='lines+markers',type="scatter",width=1,showlegend=TRUE,marker = list(color ='black'),line = list(color = 'black')) %>%
    add_trace(y = ~data[,3], name = 'Attrition', key = ~key, mode = 'lines+markers',marker = list(color ='red'),line = list(color = 'red')) %>%
    add_trace(y = ~data[,4], name = 'Attrition, extra subjects', key = ~key, mode = 'lines+markers',marker = list(color ='limegreen'),line = list(color = 'limegreen')) %>%
    layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,y=-0.25)) %>% 
    layout(dragmode = "select",
           xaxis = list(title = "Number of subjects per cluster-period", range = c(0, 110), showgrid = F,zeroline=TRUE,showline = TRUE),
           yaxis = list(title = "Relative Efficiency", range=c(min.yaxis,max.yaxis),showgrid = T,zeroline=TRUE,showline = TRUE))
  
})

################################################################################################################################
### Part 3: Power graph
################################################################################################################################  
output$plot3b <- renderPlotly({
  
  kk=2*input$kk3 # total number of clusters = 2* number of clusters per arm
  pp.AB=input$pp.AB3
  pp.BA=input$pp.BA3
  eta=input$eta3
  rho=input$rho3
  ES=input$delta3
  alpha=0.05
  mm.extra=input$mm.extra3
  
  mm=seq(10,100,by=10)
  var1=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var1[ii]=f.var4(mm[ii],kk,eta,rho)
  }
  
  var2=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var2[ii]=f.var5(mm[ii],kk,eta,rho,pp.AB,pp.BA)
  }
  
  var3=rep(0,length(mm))
  for(ii in 1:length(mm))
  {
    var3[ii]=f.var5a(mm[ii],kk,eta,rho,pp.AB,pp.BA,mm.extra)
  }
  
  
  power1=pnorm(ES/sqrt(var1)-qnorm(1-alpha/2))
  power2=pnorm(ES/sqrt(var2)-qnorm(1-alpha/2))
  power3=pnorm(ES/sqrt(var3)-qnorm(1-alpha/2))

  data=data.frame(mm,power1,power2,power3)
  
  key <- row.names(data)
  plot_ly(x = ~data[,1], y = ~data[,2], name = 'No attrition', key = ~key, mode ='lines+markers',type="scatter",width=1,showlegend=TRUE,marker = list(color ='black'),line = list(color = 'black')) %>%
    add_trace(y = ~data[,3], name = 'Attrition', key = ~key, mode = 'lines+markers',marker = list(color ='red'),line = list(color = 'red')) %>%
    add_trace(y = ~data[,4], name = 'Attrition, extra subjects', key = ~key, mode = 'lines+markers',marker = list(color ='limegreen'),line = list(color = 'limegreen')) %>%
    layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,y=-0.25)) %>% 
    layout(dragmode = "select",
           xaxis = list(title = "Number of subjects per cluster-period", range = c(0, 110), showgrid = F,zeroline=TRUE,showline = TRUE),
           yaxis = list(title = "Power", range=c(0,1),showgrid = T,zeroline=TRUE,showline = TRUE))
  
})

   
  
}

