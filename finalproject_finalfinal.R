require(ggplot2)
require(dplyr)
require(RColorBrewer)
require(ggrepel)
require(lubridate)
require(ggridges)
require(shiny)
require(shinythemes)
require(leaflet)
require(sp)
require(plotly)
require(tidyverse)
require(shinydashboard)
require(DT)


#Prepping Data#
onedirection<-read.csv("https://raw.githubusercontent.com/sarahrizkyy/qac251/main/onedirection.csv")
onedirection%>%
  mutate(album_release_date=as.Date(album_release_date, format="%Y-%m-%d")) %>%
  arrange(album_release_date)-> onedirection
onedirection%>%
  mutate(album_name_ordered=factor(album_name, levels = rev(unique(onedirection$album_name)))) %>% 
  mutate(artist_name_ordered=factor(artist_name, levels = rev(unique(onedirection$artist_name))))-> onedirection


#Visualization 1: One Direction's Brief Background and History#
ggplot(data=onedirection)+
  stat_summary(aes(x=album_release_date, y=popularity, color=artist_name_ordered), fun="mean", geom="point")+
  stat_summary(aes(x=album_release_date, y=popularity, color=artist_name_ordered), fun="mean", geom="line")+
  ggtitle("Popularity of Artists Over the Years (Based on Album Releases)")+
  ylab("Popularity")+
  xlab("Year (Based on Album Releases)")+
  scale_color_brewer("Artist Name", palette = "Set2") +
  scale_x_date(date_labels = "%Y") +
  expand_limits(x = as.Date("2010-12-10")) +
  geom_vline(xintercept = as.Date("2010-12-10"), linetype=4)+
  geom_vline(xintercept = as.Date("2015-12-31"), linetype=4) +
  annotate("label", x=as.Date("2011-02-01"), y=55, label="One Direction was introduced \nto the world through X-Factor Series 7. \nThey won third place in Dec 2010",
           hjust=0, vjust=1, size=2.5, fontface="bold", color="black")+
  annotate("label", x=as.Date("2016-02-20"), y=69, label="Zayn left the band in Mar 2015. \nOne Direction dissolved in Dec 2015.",
           hjust=0, vjust=1, size=2.5, fontface="bold", color="black") 


#Visualization 2-5: VERY Big Shiny App#
#Layout is Heavily Inspired by https://www.youtube.com/watch?v=tlOBVZx8Hy0#
ui <- fluidPage (
  
  dashboardPage(
    dashboardHeader(title = "An Analysis on One Direction: Pre and Post Split",
                    titleWidth = 650,
    tags$li(class="dropdown",tags$a(href="https://github.com/sarahrizkyy/qac251", icon("github"), "Source Code"))),
    
    dashboardSidebar(
      sidebarMenu(id="sidebar",
                  menuItem("Dataset", tabName = "data", icon=icon("database")),
                  menuItem("Exploring Changes", tabName = "viz", icon=icon("map")),
                  conditionalPanel("input.sidebar == 'viz' && input.t2 == 'discographies'", 
                                   selectInput(inputId = "artistchoice",
                                               label = "Choose an artist to analyze:",
                                               choices = c(unique(onedirection$artist_name)))),
                  conditionalPanel("input.sidebar == 'viz' && input.t2 == 'eras' ", 
                                   selectInput(inputId = "sentimentchoice",
                                               label = "Choose a sentiment type to analyze:",
                                               choices = c("danceability","energy","loudness","speechiness","acousticness",
                                                           "instrumentalness","liveness","valence","tempo"))),
                  conditionalPanel("input.sidebar == 'viz' && input.t2 == 'sentiments' ", 
                                   selectInput(inputId = "sent_artistchoice",
                                               label="Choose an artist to analyze:",
                                               choices=c(unique(onedirection$artist_name))),
                                   selectInput(inputId="sent_sentimentchoice",
                                               label="Choose a sentiment type to analyze:",
                                               choices=c("danceability","energy","loudness","speechiness","acousticness",
                                                         "instrumentalness","liveness","valence","tempo"))),
                  menuItem("Predicting Popularity", tabName = "pop", icon=icon("chart-line")),
                  conditionalPanel("input.sidebar == 'pop' && input.t3 == 'tool'",
                                   selectInput(inputId = "artistchoice",
                                               label="Choose an artist to analyze:",
                                               choices=c(unique(onedirection$artist_name)),
                                               multiple = TRUE),
                                   selectInput(inputId="sentimentx",
                                               label="Choose a sentiment type as your x axis",
                                               choices=c("danceability","energy","loudness","speechiness","acousticness",
                                                         "instrumentalness","liveness","valence", "tempo")),
                                   selectInput(inputId="sentimenty",
                                               label="Choose a sentiment type as your y axis",
                                               choices=c("danceability","energy","loudness","speechiness","acousticness",
                                                         "instrumentalness","liveness","valence", "tempo"))))),
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName="data",
                tabBox(id="t1", width = 12,
                       tabPanel("About", icon=icon("address-card"), 
                                textOutput(outputId = "about1")),
                       tabPanel("Data", icon=icon("table"), 
                                dataTableOutput(outputId = "dataT")))),
        tabItem(tabName="viz",
                tabBox(id="t2", width = 12,
                       tabPanel("About", icon=icon("address-card"), 
                                textOutput(outputId = "about2")),
                       tabPanel("Individual Discographies", value="discographies", 
                                plotlyOutput(outputId = "viz_plotly"),
                                dataTableOutput(outputId = "viz_track_table")),
                       tabPanel("Pre and Post Split Changes", value="eras", 
                                plotOutput(outputId = "dumbell")),
                       tabPanel("Sentiment Distributions", value="sentiments", 
                                plotlyOutput(outputId = "sent_plotly")))),
        tabItem(tabName="pop",
                tabBox(id="t3", width = 12,
                       tabPanel("About", icon=icon("address-card"), 
                                textOutput(outputId = "about3")),
                       tabPanel("Popularity Tool", value="tool", 
                                plotlyOutput(outputId = "pop_plotly"),
                                tableOutput(outputId = "pop_table")))))

)))


server <- function(input, output, session){
  output$dataT <-renderDataTable({onedirection},
                                 options = list( scrollX = TRUE, scrollY = "400px"))
  
  output$viz_plotly <- renderPlotly({
    
    onedirection %>%
      filter(artist_name %in% input$artistchoice) %>% 
      group_by(album_release_date, album_name_ordered) %>%
      mutate(n=n()) %>% 
      mutate(mean_popularity = mean(popularity)) ->  onedirectionsub         
    
    ggplot(data=onedirectionsub)+
      geom_point(aes(x=album_release_date, y=mean_popularity, 
                     text=paste("Album name:", album_name_ordered, "<br>Total tracks:", n))) +
      geom_line(aes(x=album_release_date, y=mean_popularity)) +
      ylab("Popularity") +
      xlab("Album Release Years") -> g1
    
    ggplotly(g1, tooltip="text")})
  
  
  output$viz_track_table <- renderDataTable({
    
    selected_data <- event_data("plotly_selected")
    
    if (is.null(selected_data)) {
      return(NULL)
    }
    
    onedirection %>%
      arrange(track_number) %>% 
      filter(artist_name == input$artistchoice,
             album_release_date == selected_data$x) %>%
      select(track_name, key, time_signature, duration_ms, explicit) -> track_table
    
    track_table
  }, options = list(scrollX = TRUE, scrollY = "150px"))
  
  output$dumbell <- renderPlot({
    
    onedirection %>% 
      mutate(era=if_else(artist_name=="One Direction", "Band_Era", "Solo_Era")) %>% 
      group_by(era, artist_name) %>% 
      summarize(avgsentiment=mean(eval(as.symbol(input$sentimentchoice)))) %>% 
      pivot_wider(names_from = era, values_from =avgsentiment) -> onedirectionwide
    
    onedirectionwide %>% 
      filter(artist_name == "One Direction") %>% 
      pull(Band_Era) -> bandavgsentiment
    
    onedirectionwide %>% 
      filter(artist_name != "One Direction") %>%
      mutate(Band_Era = ifelse(is.na(Band_Era), bandavgsentiment, Band_Era)) -> onedirectionwide
    
    ggplot(data=onedirectionwide)+
      geom_segment(aes(x=Band_Era, xend=Solo_Era, y=artist_name, yend=artist_name))+
      geom_point(aes(x=Band_Era, y=artist_name , color="Band Era"))+
      geom_point(aes(x=Solo_Era, y=artist_name , color="Solo Era"))+
      scale_color_manual("Era", values=c("red","blue"))+
      xlab("Sentiment Scores")+
      ylab("Artist Name")})
  
  output$sent_plotly <- renderPlotly({
    onedirection%>%
      filter(artist_name==input$sent_artistchoice) ->onedirectionsub
    
    ggplot(data=onedirectionsub)+
      geom_violin(aes(x=album_name_ordered, y=eval(as.symbol(input$sent_sentimentchoice))))+
      geom_jitter(aes(x=album_name_ordered, y=eval(as.symbol(input$sent_sentimentchoice)), color=eval(as.symbol(input$sent_sentimentchoice)),
                      text=paste("Track name:", track_name, "<br>Score:", eval(as.symbol(input$sent_sentimentchoice)))))+
      ylab(paste(input$sent_sentimentchoice))+
      xlab("Album Name")+
      scale_color_viridis_c(name = input$sent_sentimentchoice, option = "C", direction=-1) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))+
      stat_summary(aes(x=album_name_ordered, y=eval(as.symbol(input$sent_sentimentchoice))), fun="mean", geom="point") -> g1
    
    ggplotly(g1, tooltip="text", source="main")})
  
  output$pop_plotly <- renderPlotly({
    onedirection%>%
      filter(artist_name %in% c(input$artistchoice)) -> onedirectionsub
    
    ggplot(data=onedirectionsub)+
      geom_point (aes(x=eval(as.symbol(input$sentimentx)), y=eval(as.symbol(input$sentimenty)), 
                      color=popularity, text=paste("Track name:", track_name,"<br>Artist name:",artist_name)))+
      geom_smooth(aes(x=eval(as.symbol(input$sentimentx)), y=eval(as.symbol(input$sentimenty))), 
                  se=FALSE, color="black")+
      scale_color_distiller("Popularity", palette="Spectral")+
      ylab(input$sentimenty)+
      xlab(input$sentimentx) ->g1
    
    ggplotly(g1, tooltip="text", source="main")})
  
  output$pop_table <- renderTable({
    onedirection%>%
      filter(artist_name %in% c(input$artistchoice)) %>%
      arrange(desc(popularity)) %>%
      select(artist_name, track_name, popularity, input$sentimentx, input$sentimenty) %>%
      head(n=5)-> table1
    table1})
  
  output$about1 <-renderText({"Data was sourced directly from the Spotify API.
    Then, some additional variables were created to properly visualize the data.
    In total, the dataset includes 255 observations of 23 variables"})
  
  output$about2 <- renderText({"This section explores changes between eras and within artists.
                                The 'Individual Discographies' tab takes a closer look at track information by artist by album. Please use the select tool to generate detailed tables.
                                The 'Pre and Post Changes' tab analyzes sentiment changes between the two eras, using Band Era as time 0.
                                Lastly, the 'Sentiment Distributions' tab investigates detailed sentiment distributions by artist by album"})
  
  output$about3 <- renderText({"This section is created for the curious minds. 
    Play around with the sentiment variables and find what may predict popularity the most."})

}
  
shinyApp(ui = ui, server = server)
