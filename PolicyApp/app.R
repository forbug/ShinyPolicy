library(shiny)
library(shinydashboard)
library(ggplot2)
library(treemapify)
library(scales)
library(dplyr)

pol_d <- readRDS("data/pol_d.rds")
pol_f <- readRDS("data/pol_f.rds")
loc_d <- readRDS("data/pol_loc_d.rds")
loc_f <- readRDS("data/pol_loc_f_stg.rds")

tot_pol <- left_join(pol_d,pol_f,c("CPP_MONOLN_POL_DIMKEY"))
pol_loc <- left_join(pol_f,loc_d,c("ORIGNL_POL_DIMKEY"))


ui <- dashboardPage(
  
  dashboardHeader(title = "Policy Vision"),
  
  dashboardSidebar(
    sidebarMenu( id = "menu",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Tables", icon = icon("table"), startExpanded = TRUE,
               menuSubItem("POL_D", tabName = "DW_CPP_MONOLN_POL_D"),
               menuSubItem("POL_F", tabName = "DW_CPP_MONOLN_POL_F"),
               menuSubItem("POL_LOC_D", tabName = "DW_CPP_MONOLN_POL_LOC_D"),
               menuSubItem("POL_LOC_F", tabName = "DW_CPP_MONOLN_POL_LOC_F_STG")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Dashboard"),
              
              fluidRow(
                valueBoxOutput("costliest_state")
                
              ),
              fluidRow(
                tabBox(title = "Claims Summary",
                  id = "tabset1, height = 250px",
                  tabPanel("Top PGMs",
    
                      plotOutput("top_risk")
                    
                  ),
                  tabPanel("Top Modifications",
                      plotOutput("top_mod")
                  )
                ),
                tabBox(title = "POL_F Summary",
                       id = "tabset2, height = 250px",
                       tabPanel("Max TIV Per Claim",
                                
                                plotOutput("top_tiv")
                                
                       ),
                       tabPanel("TIV Relation",
                                
                                plotOutput("max_tiv_v_wprem")
                                
                       )
                )
                
                
              )
              
      ),
      tabItem(tabName = "DW_CPP_MONOLN_POL_D",
              
              h2("DW_CPP_MONOLN_POL_D"),
              
              tabPanel("DataTable",
                       div(style = 'overflow-x: scroll', DT::dataTableOutput("pol_d")))
              
      ),
      tabItem(tabName = "DW_CPP_MONOLN_POL_F",
              h2("DW_CPP_MONOLN_POL_F"),
              
              tabPanel("DataTable",
                       div(style = 'overflow-x: scroll', DT::dataTableOutput("pol_f")))
              
      ),
      tabItem(tabName = "DW_CPP_MONOLN_POL_LOC_D",
              h2("DW_CPP_MONOLN_POL_LOC_D"),
              
              tabPanel("DataTable",
                       div(style = 'overflow-x: scroll', DT::dataTableOutput("pol_loc_d")))
              
      ),
      tabItem(tabName = "DW_CPP_MONOLN_POL_LOC_F_STG",
              h2("DW_CPP_MONOLN_POL_LOC_F_STG"),
              
              tabPanel("DataTable",
                       div(style = 'overflow-x: scroll', DT::dataTableOutput("pol_loc_f")))
      )
      
    )
    
    
    
    
  )
)

# Server logic ----
server <- function(input, output, session) {
  
 
  
  
  #
  output$res <- renderText({
    
    req(input$sisdebarItemExpanded)
    paste("Expanded menuItem:", input$sidebarItemExpanded)
    
  })
  
  
  #output data tables
  output$pol_d <- DT::renderDataTable(
    DT::datatable(pol_d, 
        filter = list(position = 'top', clear = FALSE),
        options = list(pageLength = 10))
  )
  
  output$pol_f <- DT::renderDataTable(
    DT::datatable(pol_f, 
                  filter = list(position = 'top', clear = FALSE),
                  options = list(pageLength = 10))
  )
  
  output$pol_loc_d <- DT::renderDataTable(
    DT::datatable(loc_d, 
                  filter = list(position = 'top', clear = FALSE),
                  options = list(pageLength = 10))
  )
  
  output$pol_loc_f <- DT::renderDataTable(
    DT::datatable(loc_f, 
                  filter = list(position = 'top', clear = FALSE),
                  options = list(pageLength = 10))
  )
  
  output$top_risk <- renderPlot( 
    subset(pol_d, INDEP_RISK_PGM_DESC != " ") %>%
      group_by(INDEP_RISK_PGM_DESC) %>%
      summarise(cnt = n()) %>%
      top_n(5,cnt) %>%
      ggplot(aes(x=reorder(INDEP_RISK_PGM_DESC,cnt), y=cnt)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = -90, hjust = 0)) +
      coord_flip()
  
  )
  
  output$top_mod <- renderPlot( 
    subset(pol_d, PKG_MODFCTN_DESC != "") %>%
      group_by(PKG_MODFCTN_DESC) %>%
      summarise(cnt = n()) %>%
      ggplot(aes(area=cnt, fill = PKG_MODFCTN_DESC)) +
      geom_treemap(stat = "identity") +
      scale_fill_brewer("MODFCTNs") 
  )
  
  output$top_tiv <- renderPlot(
    subset(tot_pol, INDEP_RISK_PGM_DESC != " ") %>%
      ggplot(aes(x = INDEP_RISK_PGM_DESC, y = LARGEST_TIV_AMT)) +
      geom_boxplot() +
      coord_cartesian(ylim = c(0,5000000)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
  )
  
  output$max_tiv_v_wprem <- renderPlot(
    subset(tot_pol, INDEP_RISK_PGM_DESC != " ") %>%
      ggplot(aes(x =LARGEST_TIV_AMT, y = POL_TIV_AMT, color = INDEP_RISK_PGM_DESC)) +
      geom_point()
    
  )
  
  output$costliest_state <- renderValueBox({
    valueBox(
      sprintf("$ %s",format(max((pol_loc %>% group_by(LOC_ST_NM) %>% summarise(POL_TIV = sum(POL_TIV_AMT)))$POL_TIV, na.rm = TRUE),nsmall = 1, big.mark=",")), "Progress", icon = icon("list"),
      color = "purple"



      ) 
  })
  
}

# Run app ----
shinyApp(ui, server)
