library(shiny)
library(shinydashboard)
library(readxl)
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(mvtnorm)
library(quadprog)
library(dplyr)
library(data.table)
library(plyr)
library(matrixStats)
library(stats)
library(MASS)
library(data.table)
library(plyr)
library(psych)
library(dplyr)

source('Code/ui.R')
source('Code/server.R')

##Test Pull Request 

shinyApp(ui, server)
