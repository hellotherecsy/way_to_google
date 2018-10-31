 

server <- function(input, output) {

  ####### start React ###### 
  reset_conti_reactive <- reactive({
    continent_info  %>% filter(location == input$conti )
  })
  
  position_reactive <- reactive({
    if(input$position == 'All') {
      jobs
    }else {
      jobs  %>% filter(positions == input$position )
    }
  })
  
  domain_reactive <- reactive({
    if(input$req_domain == 'All') {
      jobs
    }else {
      jobs  %>% filter(domain == input$req_domain )
    }
  })
  
  exp_reactive <- reactive({
    if(input$exp == 0 ) {
      jobs
    }else {
      jobs  %>% filter(experience >= input$exp )
    }
  })
  
  map_reactive <- reactive({
    if(is.null(input$mymap_marker_click )) {
      jobs_map
    }else {
      jobs_map  %>% filter(lat == input$mymap_marker_click$lat )
    }
  })
  
  wc_reactive <- reactive({
    
    if(input$wc_in == 'Responsibilities') {
      jobs %>% select( ., Responsibilities ) 
    } else if (input$wc_in == 'Minimum.Qualifications') {
      jobs %>% select( ., Minimum.Qualifications ) 
    } else (
      jobs %>% select( ., Preferred.Qualifications ) 
    )  
  })
  
  
  ######## start output ######
  
  ## (1)
  output$mymap <- renderLeaflet({
    
    continent_info <- reset_conti_reactive()
 
    leaflet(jobs_map_sum) %>% addTiles() %>% 
      addProviderTiles("Stamen.TonerHybrid") %>%
      setView(lng = continent_info$lon, lat =continent_info$lat, zoom = 4) %>%
      addMarkers(~lon, ~lat, popup = ~paste(sep = "<br/>",
                                            paste0('<b>',city,'</b>') ,
                                            paste0('Job count : ',sum_of_jobs) 
      ))
  })
  
  # Show popup on click
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click
    text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
  })
  
  output$map_table <- DT::renderDataTable(DT::datatable({
    jobs_map  <- map_reactive()
    data.table(jobs_map) %>% select(., city,positions, count)
  }, options = list(
    pageLength=5, scrollX = TRUE )  )   ) 
  
  
  
  # (1) Example
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # (2) Example 
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # (2) Language 
  output$languages <- renderPlotly({
  
    occurence_mini <- c()
    occurence_prefer <- c()
    
    languages <- c( 'matlab','hadoop', 'mysql','spark', 'pig', 
                   'python', 'java','java,',  'c[++]', 'php', 'javascript', 'objective-c', 
                   'ruby', 'perl','c ','c#', ' r,','go,','ajax',
                   'angular','sas','sql','ios','spss','shell','js,','nodejs','html' )
    
    jobs$Minimum.Qualifications <- tolower(jobs$Minimum.Qualifications)
    jobs$Preferred.Qualifications <- tolower(jobs$Preferred.Qualifications)
    
    lan_temp <- position_reactive()
    
    for(i in languages){
      temp_mimi   <- str_count(lan_temp$Minimum.Qualifications, i)
      temp_prefer <- str_count(lan_temp$Preferred.Qualifications, i)
      occurence_mini <- c(occurence_mini, sum(temp_mimi, na.rm = TRUE))
      occurence_prefer <- c(occurence_prefer, sum(temp_prefer, na.rm = TRUE))
    }
    lanquage_cnt <- data.frame(cbind(languages, as.numeric(occurence_mini)))
    
    names(lanquage_cnt) <- c("language", "cnt")
    lanquage_cnt <- lanquage_cnt %>% 
      arrange(desc(as.numeric(as.character(cnt))))
    
   
    lanquage_cnt$cnt <- as.numeric(as.character(lanquage_cnt$cnt))
    lanquage_cnt$language <- factor(lanquage_cnt$language,
                                    levels = lanquage_cnt$language[order(-lanquage_cnt$cnt,decreasing = TRUE)])
 
    lang <- ggplot(data = head(lanquage_cnt, n = 10), aes( language, as.numeric(cnt), fill = language)) +
      geom_bar(stat = "identity") + 
      labs(x= 'occurence', y= 'language' ) + 
      geom_text(aes(label= cnt ), color="black", size=3) +
      ggtitle("Languages in job description") #+ 
     # guides(fill=FALSE) +
      #coord_flip() 
    lang <- ggplotly(lang)
    lang
    
  })
  
  ############### Education    ############### 
  output$edu <- renderPlotly({
     
    edu_temp <- position_reactive()
    
    edu_temp <- edu_temp %>%
      select(., positions,  education) %>%
      group_by(., positions,  education) %>% count()
    
    #counts <- as.data.frame(table(edu_temp$positions, edu_temp$education))
    colnames(edu_temp) <- c("Positions", "Education", "Freq")  
    
    # edu_temp$Education <- factor(edu_temp$Education,
    #                                 levels = edu_temp$Education[order(-edu_temp$Freq,decreasing = TRUE)])
     
    edu1 <- ggplot(edu_temp, aes(Positions, Freq)) +
      geom_bar(aes(fill = Education), position = "dodge",stat = "identity") +
      ggtitle("Positions distribution based on Education") 
 
    edu1 <- ggplotly(edu1)
    edu1
    
  })
  
  ############### job type ############### 
  output$job_type_pie <- renderPlotly({ 
    job_type <- jobs %>% count(job_type)
    ## pie chart count
    pie <- plot_ly(job_type, labels = ~job_type, values = ~n, type = 'pie') %>%
      layout(title = 'Google Job Composition',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    pie <- ggplotly(pie, height = 350, width=600)  
    })
  
  
  
  ############### languae_type_pie ############### 
  output$languae_type_pie <- renderPlotly({ 
    
    #jobs <- domain_reactive()
    #job_type <- jobs %>% count(jobs$)
    occurence_mini <- c()
    occurence_prefer <- c()
    
    languages <- c( 'matlab','hadoop', 'mysql','spark', 'pig', 
                    'python', 'java','java,',  'c[++]', 'php', 'javascript', 'objective-c', 
                    'ruby', 'perl','c ','c#', ' r,','go,','ajax',
                    'angular','sas','sql','ios','spss','shell','js,','nodejs','html' )
    
    jobs$Minimum.Qualifications <- tolower(jobs$Minimum.Qualifications)
    jobs$Preferred.Qualifications <- tolower(jobs$Preferred.Qualifications)
  
    jobs <- exp_reactive() 
    
    if(input$req_domain == 'All') {
      jobs
    } else (
      jobs <- filter(jobs, domain ==input$req_domain )
    )
    
    for(i in languages){
      temp_mimi   <- str_count(jobs$Minimum.Qualifications, i)
      temp_prefer <- str_count(jobs$Preferred.Qualifications, i)
      occurence_mini <- c(occurence_mini, sum(temp_mimi, na.rm = TRUE))
      occurence_prefer <- c(occurence_prefer, sum(temp_prefer, na.rm = TRUE))
    }
    lanquage_cnt <- data.frame(cbind(languages, as.numeric(occurence_mini)))
    
    names(lanquage_cnt) <- c("language", "cnt")
    lanquage_cnt <- lanquage_cnt %>% 
      arrange(desc(as.numeric(as.character(cnt))))
    
    
    lanquage_cnt$cnt <- as.numeric(as.character(lanquage_cnt$cnt))
    lanquage_cnt$language <- factor(lanquage_cnt$language,
                                    levels = lanquage_cnt$language[order(-lanquage_cnt$cnt,decreasing = TRUE)])

    ## pie chart count
    pie <- plot_ly(lanquage_cnt, labels = ~language, values = ~cnt, type = 'pie') %>%
      layout(title = 'Domain Unit Language Status',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    pie <- ggplotly(pie, height = 350, width=600)  
  })
  
  ############### domain exprience box plot ############### 
  output$domain_box <- renderPlotly({ 
    
    #jobs <- domain_reactive()
    jobs <- exp_reactive() 
    
    if(input$req_domain == 'All') {
      jobs
    } else (
      jobs <- filter(jobs, domain ==input$req_domain )
    )

    p2 <- ggplot(jobs, aes(x=domain, y=experience, fill=domain)) + geom_boxplot() +
      coord_flip() +
      ggtitle('Domain per Exp ')
    p2 <- ggplotly(p2)
    p2
    
  })
  
  
  output$word_cloud <- renderPlot({ 
    wc_df <- wc_reactive()
  
    #a1 <- jobs$Responsibilities 
    a2 <- as.matrix(wc_df)[,]
  
    wc <- rquery.wordcloud(a2, type=c("text", "url", "file"),
                     lang="english", excludeWords = NULL,
                     textStemming = FALSE,  colorPalette="Dark2",
                     max.words=100, min.freq =10   )
    
    wc
    } , width = "auto", height = "auto", res=130 )
 
  # (3) Table Explorer
  output$table <- DT::renderDataTable(DT::datatable({
    data <- jobs
    if (input$ctr != "All") {
      data <- data[data$country == input$ctr,]
    }
    if (input$dom != "All") {
      data <- data[data$domain == input$dom,]
    }
    if (input$pos != "All") {
      data <- data[data$positions == input$pos,]
    }
     
    data$Responsibilities <- substr(data$Responsibilities, 1, 50)
    data$Minimum.Qualifications <- substr(data$Minimum.Qualifications, 1, 50)
    data$Preferred.Qualifications <- substr(data$Preferred.Qualifications, 1, 50)
  
    data   
  }, options = list(
    pageLength=10, scrollX = TRUE ,autoWidth = T )  )   ) 
  
  
  
}