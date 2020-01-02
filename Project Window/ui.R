

library(shiny)
library(shinydashboard)

# header
header = dashboardHeader(
    dropdownMenu(
        type = "messages",
        messageItem(
            from = "Daniel Deng",
            message = "View Daniel Deng's LinkedIn account!",
            href = "https://linkedin.com/in/Daniel-Deng1102"
        )
    )
)

# siderbar
sidebar  = dashboardSidebar(
    sidebarMenu(
        
        # home page
        menuItem(
            text = "Daniel's Home Page",
            tabName = "home"
        ),
        
        # project 1: data manipulation
        menuItem(
            text = "Data Preparation",
            tabName = "dataprep"
        )
    )
)

# body
body = dashboardBody(
    tabItems(
        
        # home page
        tabItem(
            tabName = "home",
            includeMarkdown("/cloud/project/homepage/homepage.Rmd")
        ),
        
        # project 1: data manipulation
        tabItem(
            tabName = "dataprep"
        )
    )
)

shinyUI(dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
))
