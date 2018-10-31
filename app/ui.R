## ui.R ##

ui <- dashboardPage(
  skin = 'red',
  dashboardHeader(title = "Way to Googler!" ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Position Map", tabName = "world_map", icon = icon("map")), 
      menuItem("domain", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Prefer language", tabName = "languages", icon = icon("th")), 
      menuItem("word cloud", tabName = "wc_tab", icon = icon("cloud")),
      menuItem("data Explorer", tabName = "Data_Explorer", icon = icon("th"))
    )
  ) ,
  
  dashboardBody(
    tabItems(
      
      # (1) First tab content
      tabItem(tabName = "world_map",
              fluidRow(
                box( title = "Google Hiring Map",
                  leafletOutput("mymap"), width="100%", height="100%" ) , 
               
                box(
                  title = "Select continent ", status = "warning", solidHeader = TRUE,
                  "",
                  selectInput("conti",
                              "Which continent?", 
                              c(unique(as.character(continent_info$location))))
                ) , 
                box(
                  
                  DT::dataTableOutput("map_table")
                )
              )
      ),  ## End of tabItem
      
      # (2) First tab content
      tabItem(tabName = "dashboard",
              fluidPage(
                fluidRow(
                  column(3,
                         wellPanel(h4("Filter"),
                                   selectInput("req_domain", "Domain Field", 
                                               c("All",
                                                unique(as.character(jobs$domain)))),
                                   sliderInput("exp", "requirement experience ",
                                               0, 15, 0, step = 1)
                                   )),
                         column(9,
                                plotlyOutput("domain_box")  ,
                                p( ),
                                plotlyOutput("languae_type_pie"),
                              
                                wellPanel(h4("Filter")) 
                )
              )
              
              # 
              # fluidRow(
              # box( h4('test'), width = 3
              #   ),
              # box(
              #   
              #            plotlyOutput("job_type_pie")  , width = 7,
              #            plotlyOutput("domain_box")
              #   )
              # 
             # box(plotlyOutput("job_type_pie") , width = 500 , height =500),
            #  box(plotlyOutput("domain_box") , width = 500 , height =500)
              )
      ),
      # (3) tab content
      tabItem(tabName = "wc_tab",
              
              fluidPage(
                fluidRow(
                  column(4,
                         wellPanel(h4("W Cloud"),
                                   selectInput("wc_in", "Select Field", 
                                               c('Responsibilities',
                                                 'Minimum.Qualifications',
                                                 'Preferred.Qualifications') ,
                                               selected = 'Responsibilities'
                                                 )
                         )),
                  column(7,
                         plotOutput("word_cloud", width = 700, height = 700)) 
                  )
                )
      ),
      # (4) languages
      tabItem(tabName = "languages",
              fluidRow(
                box(
                column(width = 5 , 
                       selectInput("position",
                                   "Filter by Position",
                                   c("All",
                                     unique(as.character(jobs$positions))))
                ),width = 1000 ) ,
                
                box(plotlyOutput("languages",height = 500,width=1000 ), height='100%',width = '100%' ) , 
                box(plotlyOutput("edu",height = 500,width=1000 ), height='100%',width = '100%' )  
                
              )
      ),
      
      
      # (4) Data Grid 
      tabItem(tabName = "Data_Explorer",
             fluidRow(
               column(4,
                      selectInput("ctr",
                                  "Country:",
                                  c("All",
                                    unique(as.character(jobs$country))))
               ),
               column(4,
                      selectInput("dom",
                                  "Domain:",
                                  c("All",
                                    unique(as.character(jobs$domain))))
               ),
               column(4,
                      selectInput("pos",
                                  "Position:",
                                  c("All",
                                    unique(as.character(jobs$positions))))
               )
             ),
             # Create a new row for the table.
             DT::dataTableOutput("table")  
             #column(width = 12,height=12 ,
            #        box(
            #          title = "google Jobs ", height = NULL , width = NULL, status = "primary",
            #          div(style = 'overflow-y: scroll',DT::dataTableOutput("table") )
            #        )
            # )
      )
    ) # End of tabItems
  ) # End of dashboardBody
) # End of dashboardPage