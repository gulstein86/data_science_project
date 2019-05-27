##################################################
## Project: Food Environment Atlas 
## Script purpose: 
##  - Group project for WQD7001 - Principle Data Science
## Date: 2019/05/13
## Author: Aswadi, Wei Xian, Peng Hor, Bing Shien
##################################################

library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet) # Plot maps
library(readxl)
library(rgdal)
library(dplyr)
library(plotly) # Plot graph

## PART-1: READING RAW-DATA
  
# df <- read_excel("input/DataDownload.xls", 2)
# worksheet <- df %>% distinct(`Category Code`) %>% pull(`Category Code`)

# for (sheets in worksheet){
#  assign(paste("df", sheets, sep = "_"), read_excel("input/DataDownload.xls", sheet = sheets))
# }


# US MAP - COUNTIES
# us.map.county <- readOGR(dsn= 'input/UScounties', layer = "UScounties", stringsAsFactors = FALSE)
# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60) Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map.county <- us.map.county[!us.map.county$STATE_FIPS %in% c("02", "15", "72", "66", "78", "60", "69","64", "68", "70", "74"),]
# Make sure other outling islands are removed.
us.map.county <- us.map.county[!us.map.county$STATE_FIPS %in% c("81", "84", "86", "87", "89", "71", "76","95", "79"),]

#### PART-2: GENERATE Var-Lists (fixed through run-time) ####

ls(pattern = "df_[A-Z]*")
df_list <- mget(x = ls(pattern = "df_[A-Z]*"))

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
  br(),
  p("Over 250 distinct variables measured from time to time, in order to identify key indicators affecting the food environment in the US."),
  p("This tool aims to provide the public a simple and easy access to all the available information"),
  br(),
  
  sidebarLayout(
    position = "left",
    
    sidebarPanel = sidebarPanel(
      "Please select options:",
      br(),br(),
      
      conditionalPanel("input.tabs != 1",
                       selectInput(inputId = "inState", "State:", choices = varStatesName, selected = "Virginia", multiple = FALSE)
      ), 
      
      conditionalPanel("input.tabs == 2",
                       uiOutput("in2County") #renderUI, based on State
                       ), 
      
      selectInput(inputId = "inCategory", "Food-Environment Category:", choices = varCategory, selected = "Local Foods", multiple = FALSE),

      conditionalPanel("input.tabs != 2",
                       uiOutput("in2Indicator") #renderUI, based on Category
      ), 
     
      br(),
      
      wellPanel(
        #h3("This is", strong("INFO"),"label"),
        #br(),
        #p("Line1"),
        #p("Line2"),
        #p("Selected Category:", input$Category),
        #br()
        
        conditionalPanel("input.tabs != 1",
                         "Selected State:", textOutput(outputId = "infoState"), br()
                      
        ), 
        
        conditionalPanel("input.tabs == 2",
                         "Selected County:", textOutput(outputId = "infoCounty"), br()
        ), 
        
        "Selected Category:", textOutput(outputId = "infoCategory"), br(),
        
        conditionalPanel("input.tabs != 2",
                         "Selected Indicator:", textOutput(outputId = "infoIndicator")
        )
      )
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        
        id="tabs",
        
        # Aswadi
        tabPanel(
          title = "Interactive-Maps", 
          h3(textOutput(outputId = "tab1selectedCategory")), # Show selected Category
          p(textOutput(outputId = "tab1selectedIndicator")), # Show selected Indicator
          leafletOutput("mymap"),
          value=1
        ),
        
        # WeiXin
        tabPanel(
          title = "County-Infos", 
          h3(textOutput(outputId = "tab2selectedCounty")), # Show selected County / State
          p(textOutput(outputId = "tab2selectedCategory")), # Show selected Category
          tableOutput(outputId = "tab2table"),
          value=2
          
        ),
        
        # BingShien
        tabPanel(
          title = "Key-Indicator", 
          h3(textOutput(outputId = "tab3selectedCounty")), # Show selected County / State
          p(textOutput(outputId = "tab3selectedCategory")), # Show selected Category
          p(textOutput(outputId = "tab3selectedUnit")), # Show Measurement Units
          plotOutput(outputId = "tab3barplot"),
          br(),
          plotlyOutput(outputId = "tab3pie"),
          value=3
        )
      )
    )
  )
  
  
)

server <- function(input, output, session) {
  
  output$in2County <- renderUI(
    selectInput(inputId = "inCounty", label = "County:", choices = df_LOCAL$County[df_LOCAL$State==state.abb[which(state.name==input$inState)]], selected = NULL, multiple = FALSE)
  )
  
  output$in2Indicator <- renderUI(
    selectInput(inputId = "inIndicator", label = "F/E-Indicator:", choices = df$`Variable Name`[df$`Category Name`==input$inCategory], selected = "Direct farm sales, 2012", multiple = FALSE)
  )
  
  
  output$infoCategory <- renderText(input$inCategory)
  output$infoState <- renderText(input$inState)
  output$infoCounty <- renderText(input$inCounty)
  output$infoIndicator <- renderText(input$inIndicator)
  
  ## TAB-1: 
  ## INTERACTIVE MAP
  observe({
    if(input$tabs == "1"){
      
      output$tab1selectedCategory <- renderText(paste("Selected Category:", input$inCategory))
      output$tab1selectedIndicator <- renderText(paste("Information on", input$inIndicator,"in the US"))
      
      # using leaflet 
      output$mymap <- renderLeaflet({
        # print('indicator:',input$inIndicator)
        # print(paste0("test1: ", df$`Category Code`[df$`Category Name`==input$inCategory] %>% distinct(`Category Code`)))
        # print(paste0("df_", filter(df,`Category Name`==input$inCategory) %>% distinct(`Category Code`)))
        # print(paste0(filter(df,`Variable Name`==input$inIndicator) %>% distinct(`Variable Name`)))
        # print(paste0(df$`Variable Code`[df$`Variable Name`==input$inIndicator]))
        
        # print(paste0("df_", filter(df,`Category Name`==input$inCategory) %>% distinct(`Category Code`)))
        county_data <- df_list[[paste0("df_", filter(df,`Category Name`==input$inCategory) %>% distinct(`Category Code`))]]
        
        ### XXX
        dfCategory <- paste0("df_", filter(df,`Category Name`==input$inCategory) %>% distinct(`Category Code`))
        if(dfCategory=="df_ACCESS") {tmpDF <- df_ACCESS}
        else if(dfCategory=="df_ASSISTANCE") {tmpDF <- df_ASSISTANCE}
        else if(dfCategory=="df_HEALTH") {tmpDF <- df_HEALTH}
        else if(dfCategory=="df_INSECURITY") {tmpDF <- df_INSECURITY}
        else if(dfCategory=="df_LOCAL") {tmpDF <- df_LOCAL}
        else if(dfCategory=="df_PRICES_TAXES") {tmpDF <- df_PRICES_TAXES}
        else if(dfCategory=="df_RESTAURANTS") {tmpDF <- df_RESTAURANTS}
        else if(dfCategory=="df_SOCIOECONOMIC") {tmpDF <- df_SOCIOECONOMIC}
        else if(dfCategory=="df_STORES") {tmpDF <- df_STORES}
        ### XXX
        
        # county_data <- paste0("df_", filter(df,`Category Name`==input$inCategory) %>% distinct(`Category Code`))
        
        # temp_df <- select(county_data,FIPS,paste0(df$`Variable Code`[df$`Variable Name`==input$inIndicator]))
        temp_df <- select(tmpDF,FIPS,paste0(df$`Variable Code`[df$`Variable Name`==input$inIndicator]))
        names(temp_df) <- c("FIPS", "variable")
        leafmap3 <- merge(us.map.county, temp_df, by.x= 'FIPS', by.y='FIPS')
        
        popup_dat3 <- paste0("<strong>County: </strong>",
                             leafmap3$NAME,
                             "<br><strong>",paste0(filter(df,`Variable Name`==input$inIndicator) %>% distinct(`Variable Name`))," : </strong>",
                             as.character(leafmap3$variable))
        # pal <- colorQuantile("viridis", NULL, n = 5)
        pal <- colorQuantile("Spectral", NULL, n = 10)
        # pal <- colorNumeric("Spectral", NULL, n = 11)
        
        
        leaflet(data = leafmap3) %>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(variable),
                      fillOpacity = 0.8,
                      color = "#BDBDC3",
                      weight = 1,
                      popup = popup_dat3) %>%
        addLegend("bottomright", pal = pal, values = ~variable,
                  title = paste0(filter(df,`Variable Name`==input$inIndicator) %>% distinct(`Variable Name`)),
                  opacity = 0.5 )
      })
      
    }
  })
  ### TAB-1 END
  ####
  
  
  
  # TAB-2: County-Infos
  # Display all Variables for ONE particular County
  observe({
    if(input$tabs == "2"){
      
      #output$tab2selectedCounty <- renderText("NOTHING")
      #output$tab2selectedCategory <- renderText("NOTHING")
      
      output$tab2selectedCounty <- renderText(paste("Infos for", input$inCounty, "(County) in", input$inState, "(State)"))
      output$tab2selectedCategory <- renderText(paste("Selected Category:", input$inCategory))

      dfCategory <- paste0("df_", filter(df,`Category Name`==input$inCategory) %>% distinct(`Category Code`))
      
      if(dfCategory=="df_ACCESS") {tmpDF <- df_ACCESS}
      else if(dfCategory=="df_ASSISTANCE") {tmpDF <- df_ASSISTANCE}
      else if(dfCategory=="df_HEALTH") {tmpDF <- df_HEALTH}
      else if(dfCategory=="df_INSECURITY") {tmpDF <- df_INSECURITY}
      else if(dfCategory=="df_LOCAL") {tmpDF <- df_LOCAL}
      else if(dfCategory=="df_PRICES_TAXES") {tmpDF <- df_PRICES_TAXES}
      else if(dfCategory=="df_RESTAURANTS") {tmpDF <- df_RESTAURANTS}
      else if(dfCategory=="df_SOCIOECONOMIC") {tmpDF <- df_SOCIOECONOMIC}
      else if(dfCategory=="df_STORES") {tmpDF <- df_STORES}
      
      # Get Variables (Codes + Values) from respective worksheet
      # countyTMP1 <- df_LOCAL %>% filter(State==state.abb[which(state.name=="Virginia")] & County=="Accomack") %>% t()
      countyTMP1 <- tmpDF %>% filter(State==state.abb[which(state.name==input$inState)] & County==input$inCounty) %>% t()
      rnameTMP1 <- row.names(countyTMP1)
      
      # Filter for (Code + Name + Units) from the Summary Variable List
      rCodeTMP1_Avai <- df$`Variable Code`[df$`Variable Code` %in% rnameTMP1]
      rnameTMP1_Avai <- df$`Variable Name`[df$`Variable Code` %in% rnameTMP1]
      rUnitTMP1_Avai <- df$Units[df$`Variable Code` %in% rnameTMP1]
      rValTMP1_Avai <- countyTMP1[]
      
      # Merge by Code (then drop) to get the final table
      tmpTbl1 <- cbind(V1 = countyTMP1[], Code = rnameTMP1)
      tmpTbl2 <- cbind(Code = rCodeTMP1_Avai, Name = rnameTMP1_Avai, Units = rUnitTMP1_Avai)
      tmpFinal <- merge(tmpTbl2,tmpTbl1,by="Code")
      countyInfo <- tmpFinal[,!names(tmpFinal)=="Code"]
      
      # Render Table
      output$tab2table <- renderTable(countyInfo)
      
    } else {
      output$tab2selectedCounty <- renderText("NOTHING")
      output$tab2selectedCategory <- renderText("NOTHING")
    }
  })
  ### TAB-2 END
  
  # TAB-3: County-Infos
  # Display all Variables for ONE particular County
  observe({
    if(input$tabs == "3"){
      
      output$tab3selectedCounty <- renderText(paste("How is the situation in", input$inState, "(State)?"))
      output$tab3selectedCategory <- renderText(paste("Selected Category:", input$inCategory))
      
      varUnit <- df$Units[df$`Variable Name`==input$inIndicator]
      output$tab3selectedUnit <- renderText(paste('Measurement units:', varUnit))
      
      dfCategory <- paste0("df_", filter(df,`Category Name`==input$inCategory) %>% distinct(`Category Code`))
      
      if(dfCategory=="df_ACCESS") {tmpDF <- df_ACCESS}
      else if(dfCategory=="df_ASSISTANCE") {tmpDF <- df_ASSISTANCE}
      else if(dfCategory=="df_HEALTH") {tmpDF <- df_HEALTH}
      else if(dfCategory=="df_INSECURITY") {tmpDF <- df_INSECURITY}
      else if(dfCategory=="df_LOCAL") {tmpDF <- df_LOCAL}
      else if(dfCategory=="df_PRICES_TAXES") {tmpDF <- df_PRICES_TAXES}
      else if(dfCategory=="df_RESTAURANTS") {tmpDF <- df_RESTAURANTS}
      else if(dfCategory=="df_SOCIOECONOMIC") {tmpDF <- df_SOCIOECONOMIC}
      else if(dfCategory=="df_STORES") {tmpDF <- df_STORES}
      
      # Render Plot 1
      output$tab3barplot <- renderPlot({
        
        tmpIndicator <- df$`Variable Code`[df$`Variable Name`==input$inIndicator]
        print(tmpIndicator)
        
        #filter data
        test1 <- tmpDF %>% select(County, tmpIndicator) %>% filter(tmpDF[["State"]] == state.abb[which(state.name==input$inState)]) %>% arrange(desc(UQS(syms(tmpIndicator)))) %>% head(10)
        print(test1)

        # print(test1[[tmpIndicator]])
        
        #boxplot
        bp = barplot(test1[[tmpIndicator]], ylab = input$inIndicator,  xlab = "County", main = paste0("Top 10 Locations Based on ", input$inIndicator))
        text(x=bp[,1], y=-1, adj=c(1, 1), test1$County, cex=0.8, srt=45, xpd=TRUE)
      })
      
      # Render Plot 2
      output$tab3pie <- renderPlotly({
        
        tmpIndicator <- df$`Variable Code`[df$`Variable Name`==input$inIndicator]
        print(tmpIndicator)
        
        #filter data
        test1 <- tmpDF %>% select(County, tmpIndicator) %>% filter(tmpDF[["State"]] == state.abb[which(state.name==input$inState)]) %>% arrange(desc(UQS(syms(tmpIndicator)))) %>% head(10)
        print(test1)
        
        print(test1[[tmpIndicator]])
        print(test1[["County"]])
        
        #pie
        # pie(test1[[tmpIndicator]], test1[["County"]], main = paste0("Top 10 County Based on ", input$inIndicator))
        plot_ly(test1, labels = ~County, values = test1[[tmpIndicator]], type = 'pie') %>%
                layout(title = paste0("Top 10 County Based on ", input$inIndicator),
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })
      
    } else {
      output$tab3selectedCounty <- renderText("NOTHING")
      output$tab3selectedCategory <- renderText("NOTHING")
    }
  })
  ### TAB-3 END
  
}

shinyApp(ui, server)
