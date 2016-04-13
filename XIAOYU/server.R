library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(dygraphs)
library(xts)
library(leaflet)
library(shinydashboard)

##
movie <- readRDS("data.rds")
score <- readRDS("score.rds")
TYPE <- readRDS("TYPE.Rds")
Final <- readRDS("Final.Rds")
time <- readRDS("time.Rds")
data2d <- readRDS("2ddata.Rds")
table<-readRDS("tablescore.Rds")

#shinyserver
shinyServer(function(input, output,session) {
#################### xiaoyu s Menu Item ####################
  
  ######## Section one  ########
  #pie chart
  output$case0 <- renderPlotly({
    
    if(input$rank == 1) {
      
      summary <- sort(summary(movie$Type),decreasing = T)
      B <- data.frame(Type = names(summary), number = summary)
      rownames(B) <- NULL
      B <- B[c(1:15),]
      plot_ly(B, labels = Type, values = number, type = "pie",marker=list(colors=c(
        "#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462" ,"#B3DE69","#FCCDE5" ,"#D9D9D9",
        "#BC80BD","#CCEBC5","#FBB4AE", "#B3CDE3" ,"#CCEBC5" ,"#DECBE4" ,"#FED9A6"
      ))) %>% 
        layout(title = "Number of Movies by Type",paper_bgcolor='rgba(0,0,0,0)')
      
    }
    else {
      
      summary <- sort(summary(movie$Director),decreasing = T)
      A <- data.frame(Director = names(summary), number = summary)
      rownames(A) <- NULL
      A <- A[-c(1,2),]
      A <- A[c(1:30),]
      plot_ly(A, labels = Director, values = number, type = "pie",marker=list(colors=c(
        "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6" ,"#FFFFCC","#E5D8BD", "#FDDAEC", "#F2F2F2",
        "#66C2A5", "#FC8D62" ,"#8DA0CB", "#E78AC3" ,"#A6D854", "#FFD92F" ,"#E5C494" ,"#B3B3B3"
      ))) %>% 
        layout(title = "Number of Movies by Director",paper_bgcolor='rgba(0,0,0,0)')
      
    }
    
  })
  
  ######## Section two  ########

  output$reveiw0 <- renderPlotly({
    
    if(input$review == 1){
      
      plot_ly(score, x = title.y, y = summary, color = summary, size = summary, mode = "markers")%>%
        layout(xaxis = list(title = "", tickfont = list(size = 7), tickangle = 30), 
               yaxis = list(title = "The total number of reviews"),title = "Top 50 Movies")
    
      }
    
    else{
    
      plot_ly(score, x = review, y = n, size = n, color = n, mode = "markers") %>%
        layout(xaxis = list(title = "", tickfont = list(size = 7), tickangle = 30),
               yaxis = list(title = "The total number of reviews"),title = "Top 50 Active Users")
    
      }
    
    })
  
  output$reveiw1 <- renderPlotly({
    
    if(input$more == 1){
      plot_ly(Final, x = Type, y = summary, mode = "markers",color = summary) %>%
        layout(xaxis = list(title = "", tickfont = list(size = 12), tickangle = 30),
               title = "Overall Ranking")
    }
    
    else {
      plot_ly(Final, x = Type,  y = summary, type = "box")  %>%
        layout(xaxis = list(title = "", tickfont = list(size = 12), tickangle = 30),
               title = "Summary")
      
      }
    
    })
  
  #input product id
  output$reveiw2 <- renderDygraph({
    
    text <- input$text_id

    if(text != ""){
      sub <-  time[time$product_productid == text,]
      sub <- sub[order(sub$newtime),]
      
      mean <- c()
    
      for (i in 1: dim(sub)[1]){
      
      mean[i] <- mean(sub[1:i,3])
      
      }
    
      sub <- cbind(sub,mean)
      data <- xts(sub[,-4], order.by = sub[,4])
      name = paste("Average Score of ",sub[1,7])
      
      dygraph(data[,7], main = name, ylab = "Average Score") %>% dyRangeSelector() %>%  dyOptions(stackedGraph = TRUE)
    }
  
    })
  
  
  ######## Section three  ########
    output$dygraph <- renderDygraph({
      data <- switch(input$time, 
                     "1" = TYPE[,1],
                     "2" = TYPE[,2],
                     "3" = TYPE[,3],
                     "4" = TYPE[,4],
                     "5" = TYPE[,5],
                     "6" = TYPE[,6],
                     "7" = TYPE[,7])
      
      data2 <- switch(input$compare, 
                      "1" = NULL,
                      "2" = TYPE[,2],
                      "3" = TYPE[,3],
                      "4" = TYPE[,4],
                      "5" = TYPE[,5],
                      "6" = TYPE[,6],
                      "7" = TYPE[,7])
      
      data3 <- cbind(data,data2)
      name <- paste(names(data),"    ", names(data2))
      dygraph(data3, main = "Between-Types Comparison") %>% dyRangeSelector() 
      
    })
    
    #input timeline
    output$case3 <- renderPlotly({
      
      text <- input$text_year
      search <- sort(summary(movie$Type[movie$Year == text]),decreasing = T)
      search <- data.frame(names = names(search), search)
      rownames(search) <- NULL
      search <- search[1:15,]
      
      plot_ly(search, x = names, y = search, size = search, color = search, mode = "markers") %>%
        layout(xaxis = list(title = "", tickfont = list(size = 11), tickangle = 20),
               yaxis = list(title = "The total number of movies"))
      
    })
  #################### end of  xiaoyu s Menu Item #################### 
    
  #####################Similarity######################################


    
    output$gmap<-renderPlotly({
      
      film<-data2d[1:input$topmovies,]
      
      if(input$factor == 1){
      plot_ly(x=film$x1,y=film$x2,text=film$title.y,
              color=film$Year,
              marker=list(size=10,opacity=0.8),
              mode="markers")
      }
      
      else if(input$factor == 2){
        plot_ly(x=film$x1,y=film$x2,text=film$title.y,
                color=film$Type, marker=list(size=10,opacity=0.8),
                mode="markers")
      }
      
      else if(input$factor == 3){
        plot_ly(x=film$x1,y=film$x2,text=film$title.y,
                color=film$summary, marker=list(size=10,opacity=0.8),
                mode="markers")
      }
      
      else if (input$factor==4){
        plot_ly(x=film$x1,y=film$x2,text=film$title.y,
                color=film$countries, marker=list(size=10,opacity=0.8),colors="Spectral",
                mode="markers")
      }
      
      else{
        plot_ly(x=film$x1,y=film$x2,text=film$title.y,
                color=film$recommendation, marker=list(size=10,opacity=0.8), colors=c("coral","black"),
                mode="markers")
        
      }
      
    
    })
    
    output$interestmoviename<-renderText({
      paste("You select: ",data2d[which(data2d$id==input$interestid),]$title.y)
    })
    
    output$interestmovieimg<-renderUI({
      image_file <- data2d[which(data2d$id==input$interestid),]$image_url
      tags$img(src= image_file,height=250)
    })
    
    output$interestmoviehist<-renderPlotly({
      plot_ly(x=table[input$interestid,],ocapacity=0.6,color="aquamarine",type="histogram")%>%
      layout(paper_bgcolor='rgba(245,245,245,1)',plot_bgcolor='rgba(245,245,245,1)',title="Histogram of review score",xaxis = list(title = "Score", tickfont = list(size = 11), 
                                                            tickangle = 20,range=c(0,6) ),
             yaxis = list(title = "Number of reviewers" ))
        
    })
    
    datable<-reactive({
      group=data2d[which(data2d$id==input$interestid),]$km.cluster
      f<-data2d[data2d$km.cluster==group,]
      f[,c(1,4,5,6,7,8)]
      
    })
    
    
    output$interestmovietable<-renderDataTable({
      datable()})
    
    output$interestmoviemap<-renderPlotly({
      film<-data2d[data2d$km.cluster==data2d[which(data2d$id==input$interestid),]$km.cluster,]
      plot_ly(x=film$x1,y=film$x2,text=film$title.y,mode="markers",
               marker=list(size=10,opacity=0.8))
    })
    
    output$interestmovieinfo<-renderUI({
      str0<-"Movie Infomation:"
      str1<-paste("Movie title: ",data2d[which(data2d$id==input$interestid),]$title.y)
      str2<-paste("Total review: ",data2d[which(data2d$id==input$interestid),]$summary)
      str3<-paste("Country: ",data2d[which(data2d$id==input$interestid),]$countries)
      str4<-paste("Year: ",data2d[which(data2d$id==input$interestid),]$Year)
      HTML(paste(h3(str0),h5(str1), h5(str2),h5(str3),h5(str4), sep = '<br>'))
      
      
      
    })
    
    
    
  #######################################################################    
  })