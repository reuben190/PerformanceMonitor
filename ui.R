
library(shiny)
library(shinydashboard)
library(dygraphs)

dashboardPage(
  dashboardHeader(title = "Performance Tracking"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import Data", tabName = "data", icon = icon("list-alt")),
      menuItem("Analytics", tabName = "analytics", icon = icon("area-chart"))
      
                )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(tabBox(width = "100%",
                tabPanel("Select Data",DT::dataTableOutput("table_baseData")),
                tabPanel("Prepared Data",column(width=12,selectizeInput("group","Select Criteria To Group By:",GroupBy,selected=1)),DT::dataTableOutput("table_baseDataSummary")))
                
                )
              ),
      tabItem(tabName = "analytics",
              navbarPage("Metrics",
                tabPanel("Units",
                         fluidPage(
                           sidebarLayout(
                             
                             sidebarPanel(
                               htmlOutput("data_units")
                             ),
                             
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Orders", dygraphOutput('dygraph_units'))
                               )
                             )
                           )
                          )
                         ),
                         
                tabPanel("Lender Fee",
                         fluidPage(
                           sidebarLayout(
                             
                             sidebarPanel(
                               htmlOutput("data_LenderFee")
                             ),
                             
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Lender Fee", dygraphOutput('dygraph_lenderfee'))
                               )
                             )
                           )
                         )),
                tabPanel("Appraiser Fee",
                         fluidPage(
                           sidebarLayout(
                             
                             sidebarPanel(
                               htmlOutput("data_AppraiserFee")
                             ),
                             
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Appraiser Fee", dygraphOutput('dygraph_appraiserfee'))
                               )
                             )
                           )
                         )),
                tabPanel("Margin Fee",
                         fluidPage(
                           sidebarLayout(
                             
                             sidebarPanel(
                               htmlOutput("data_Margin")
                             ),
                             
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Margin", dygraphOutput('dygraph_margin'))
                               )
                             )
                           )
                         )),
                tabPanel("Performance",
                         fluidPage(
                           sidebarLayout(
                             
                             sidebarPanel(
                               htmlOutput("data_Performance")
                             ),
                             
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Performance", dygraphOutput('dygraph_performance'))
                               )
                             )
                           )
                         )),
                tabPanel("Inspection Time",fluidPage(
                  sidebarLayout(
                    
                    sidebarPanel(
                      htmlOutput("data_InspectTAT")
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Inspection Time", dygraphOutput('dygraph_InspectTAT'))
                      )
                    )
                  )
                )),
                tabPanel("Upload Time",fluidPage(
                  sidebarLayout(
                    
                    sidebarPanel(
                      htmlOutput("data_UploadTAT")
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Upload Time", dygraphOutput('dygraph_UploadTAT'))
                      )
                    )
                  )
                )),
                tabPanel("First Delivery Time",fluidPage(
                  sidebarLayout(
                    
                    sidebarPanel(
                      htmlOutput("data_1stDeliveryTAT")
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        tabPanel("First Delivery Time", dygraphOutput('dygraph_1stDeliveryTAT'))
                      )
                    )
                  )
                )),
                tabPanel("Quality",fluidPage(
                  sidebarLayout(
                    
                    sidebarPanel(
                      htmlOutput("data_Quality")
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Quality", dygraphOutput('dygraph_Quality'))
                      )
                    )
                  )
                ))
                
                
                
              )
      )
              
    )
  )
)