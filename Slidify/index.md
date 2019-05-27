---
title       : THE FOOD HUNTERS
author      : 
- "YAP PENG HOR WQD180123"
- "ASWADI ABDUL RAHMAN WQD180082"
- "NG WEI XIN WQD180102"
- "TAN BING SHIEN WQD180104"
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
github      :
  user: penghor315
  repo: WQD7001-Project
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
logo        : UM.png

---
title: "THE FOOD HUNTERS"
author: 
- "YAP PENG HOR WQD180123"
- "ASWADI ABDUL RAHMAN WQD180082"
date: "January 15, 2016"
output: slidy_presentation
logo  : UM.png
github      :
  user: penghor315
  repo: WQD7001-Project

---

## Introduction

This presentation is part of the course project for the Coursera Developing Data Products. This peer assessed assignment has two parts. 

- First, we need to create a Shiny application and deploy it on Rstudio's servers. 
- Second, we should use Slidify or Rstudio Presenter to prepare a reproducible pitch presentation about the application.
- This reproducible pitch presentation is prepared using Rstudio Presenter, address the second part of the course project.

The app (Dataset Explorer) developed for the first part of the assignment is available at: https://salimahm.shinyapps.io/DatasetExplorer/



## Dataset Explorer

This Shiny application, called Dataset Explorer provides useful functions for exploring dataset. This helps user to understand the dataset or data frame before any detailed analysis can be performed.

The application allows the user to:

- Select the dataset, user wish to explore.
-	State number of observations to view.
-	Select the function to explore the dataset.
-	Refer to supporting documentation.

Source code for ui.R and server.R code are available on the GitHub : https://github.com/SalimahM/DevelopingDataProducts



## Useful Functions for Dataset Explorer
-	The dim( ) function returns the dimensions of the data frame that is number of rows and number of columns. The output is a vector.
-	The str( ) function returns many useful pieces of information, including the above useful outputs and the types of data for each column.
-	The summary ( ) function provides basic descriptive statistics and frequencies.
-	The View( ) function opens a data frame in a new window in a spreadsheet style format that can be scrolled for viewing the data. 
-	The pairs( ) enables us to visually check possible correlated variables.

## Screenshot of Dataset Explorer

[alt text]("UM.png")






