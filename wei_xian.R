#install.packages('shiny')
library(shiny)

install.packages('shinyjs')
library(shinyjs)

#install.packages('tidyverse')
library(tidyverse)

library(readxl)



## PART-1: READING RAW-DATA

#df <- read_excel("Data/DataDownload.xls", 2)
#worksheet <- df %>% distinct(`Category Code`) %>% pull(`Category Code`)

#for (sheets in worksheet){
#  assign(paste("df", sheets, sep = "_"), read_excel("Data/DataDownload.xls", sheet = sheets))
#}

## PART-2: GENERATE Var-Lists (fixed through run-time)

varCategory <- df %>% distinct(`Category Name`) %>% pull(`Category Name`)
varStates <- df_LOCAL %>% distinct(State) %>% pull(State)
varStatesName <- setNames(state.name,state.abb)[varStates]

## NWX: exclude "DC", not in the list state.name
varStatesName <- setNames(state.name,state.abb)[varStates[varStates!="DC"]]
varStatesName <- state.name[match(state.abb,varStates[varStates!="DC"])]

## GET variables for each Category
varLOCAL <- df %>% group_by(`Category Code`) %>% filter(`Category Code`=="LOCAL") %>% distinct(`Variable Name`) %>% pull(`Variable Name`)
varLOCAL_Code <- df %>% group_by(`Category Code`) %>% filter(`Category Code`=="LOCAL") %>% distinct(`Variable Code`) %>% pull(`Variable Code`)


ui <- fluidPage(
  
  headerPanel("Food Environment Atlas in the US"),
  br(),br(),
  
  sidebarLayout(
    position = "left",
    
    sidebarPanel = sidebarPanel(
      "Please select options:",
      br(),br(),
      
      textInput(inputId = "txtInput1", label = "TMP Text Input", value = "Enter text..."),
      
      #selectInput(inputId = "Category", "Category:", choices = c("mpg","cyl"),selected = NULL, multiple = FALSE),
      selectInput(inputId = "inCategory", "Category:", choices = varCategory, selected = "Local Foods", multiple = FALSE),
      
      selectInput(inputId = "inState", "State:", choices = varStatesName, selected = "Virginia", multiple = FALSE),
      
      uiOutput("in2County"), #renderUI, based on State
      
      uiOutput("in2Indicator"), #renderUI, based on Category
      br(),
      
      wellPanel(
        #h3("This is", strong("INFO"),"label"),
        #br(),
        #p("Line1"),
        #p("Line2"),
        #p("Selected Category:", input$Category),
        #br()
        "Selected Category:", textOutput(outputId = "infoCategory"), br(),
        "Selected State:", textOutput(outputId = "infoState"), br(),
        "Selected County:", textOutput(outputId = "infoCounty"), br(),
        "Selected Indicator:", textOutput(outputId = "infoIndicator")
      )
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        
        id="tabs",
        
        # Aswadi
        tabPanel("Interactive Maps", "This is an interactive Map"),
        
        
        # WeiXin
        tabPanel(
          title = "County-Infos", 
          
          h3(textOutput(outputId = "tab2selectedCounty")), # Show selected County / State
          
          p(textOutput(outputId = "tab2selectedCategory")), # Show selected Category
          
          tableOutput(outputId = "tab2table")
          
        ),
        
        
        
        # BingShien
        tabPanel("Key Indicator", "Infos by key indicator")
      )
    )
  )
  
  
)

server <- function(input, output, session) {
  
  output$in2County <- renderUI(
    selectInput(inputId = "inCounty", label = "County:", choices = df_LOCAL$County[df_LOCAL$State==state.abb[which(state.name==input$inState)]], selected = NULL, multiple = FALSE)
  )
  
  output$in2Indicator <- renderUI(
    selectInput(inputId = "inIndicator", label = "Indicator:", choices = df$`Variable Name`[df$`Category Name`==input$inCategory], selected = NULL, multiple = FALSE)
  )
  
  output$infoCategory <- renderText(input$inCategory)
  output$infoState <- renderText(input$inState)
  output$infoCounty <- renderText(input$inCounty)
  output$infoIndicator <- renderText(input$inIndicator)
  
  
  
  # NWX: TAB-2
  observe({
    if(input$tabs == "County-Infos"){
      output$tab2selectedCounty <- renderText(paste("Infos by County:", input$inCounty, "in", input$inState))
      output$tab2selectedCategory <- renderText(paste("Selected Category:", input$inCategory))
      
      countyInfo <- df_LOCAL %>% filter(County==input$inCounty) %>% t()
      countyRownames <- df$`Variable Name`[df$`Variable Code` %in% row.names(countyInfo)]
      
      
      
      ## Test2 Beginn
      
      # 100
      #countyTMP1 <- df_LOCAL %>% filter(County=="Accomack") %>% t()
      countyTMP1 <- df_LOCAL %>% filter(County==input$inCounty) %>% t()
      rnameTMP1 <- row.names(countyTMP1)
      
      # 97 
      rCodeTMP1_Avai <- df$`Variable Code`[df$`Variable Code` %in% rnameTMP1]
      rnameTMP1_Avai <- df$`Variable Name`[df$`Variable Code` %in% rnameTMP1]
      rUnitTMP1_Avai <- df$Units[df$`Variable Code` %in% rnameTMP1]
      
      # cbind
      tmpTbl1 <- cbind(countyTMP1,rnameTMP1)
      colnames(tmpTbl1) <- c("Value","Code")
      tmpTbl2 <- cbind(rCodeTMP1_Avai,rnameTMP1_Avai,rUnitTMP1_Avai)
      colnames(tmpTbl2) <- c("Code","Name","Units")
      merge(tmpTbl1,tmpTbl2,by="Code")
      
      # Get FINAL table
      countyInfo <- cbind(tmpFinal["Name"],tmpFinal["Value"],tmpFinal["Units"])
      
      ## Test2 END
      
      # Get complete table, including state/fips/county, extract row names, cbind (with row names / units), then only filter.
      #myTmp <- cbind(rowNames2,countyInfo[rowNames2,])
      #colnames(myTmp) <- c("VarName","Value")
      
      #myRowCode <- df$`Variable Code`[df$`Variable Code` %in% row.names(countyInfo)]
      #myRowNames <- df$`Variable Name`[df$`Variable Code` %in% row.names(countyInfo)]
      
      output$tab2table <- renderTable(
        countyInfo,colnames = TRUE)
      
    } else {
      output$tab2selectedCounty <- renderText("NOTHING")
      output$tab2selectedCategory <- renderText("NOTHING")
    }
  })
  
  #output$tab2table <- renderTable(
  #countyInfo <- df_LOCAL %>% filter(County==input$inCounty) %>% t(), 
  #rownames = TRUE,
  #rownames <- df$`Variable Name`[df$`Variable Code` %in% row.names()],
  #colnames = TRUE
  #)
  
  
  
  # hiding or showing element, not yet correct
  observeEvent(input$inState, {
    if (input$inState == "Virginia"){
      #shinyjs::hide(input$inIndicator)
      shinyjs::hide(id="myID")
    }else{
      #shinyjs::hide(input$inIndicator)
      shinyjs::hide(id="myID")
    }
    
  })
  
}

shinyApp(ui, server)
