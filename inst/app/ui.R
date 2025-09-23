# Header
header <- shinydashboard::dashboardHeader(
  title = "Omics BioAnalytics")

# Sidebar
sidebar <- dashboardSidebar(
  conditionalPanel(
    condition = "output.analysisRan == false",
    sidebarMenu(
      id = "tabs_before_analysis",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem(
        "Data upload",
        tabName = "data",
        icon = icon("database")
      ),
      menuItem(
        "App features",
        tabName = "appfeatures",
        icon = icon("flask")
      ),
      menuItem(
        "Generate Report",
        tabName = "report",
        icon = icon("clipboard-list")
      )
    )
  ),
  conditionalPanel(
    condition = "output.analysisRan == true",
    sidebarMenu(
      id = "tabs_after_analysis",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem(
        "Data upload",
        tabName = "data",
        icon = icon("database")
      ),
      menuItem(
        "Methods",
        tabName = "methods",
        icon = icon("flask")
      ),
      menuItem(
        "Analysis",
        tabName = "analysis",
        icon = icon("bar-chart-o"),
        menuSubItem("Metadata", tabName = "subitem1"),
        menuSubItem("Exploratory Data Analysis", tabName = "subitem2"),
        menuSubItem("Differential Expression", tabName = "subitem3"),
        menuSubItem("Biomarker Panels", tabName = "subitem4")
      ),
      menuItem(
        "Generate Report",
        tabName = "report",
        icon = icon("clipboard-list")
      )
    )
  )
)

# Body
body <- shinydashboard::dashboardBody(
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),
  fluidPage(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "overview",
        fluidRow(
          HTML("<img src='app-architecture.png' id='app-architecture'/>"),
          p("Created in BioRender.com")
      )
      ),
      shinydashboard::tabItem(
        tabName = "data",
        omicsBioAnalytics::data_upload_ui("data_upload")
      ),
      shinydashboard::tabItem("subitem1",
                              omicsBioAnalytics::metadata_ui("metadata")
      ),
      shinydashboard::tabItem("subitem2",
                              omicsBioAnalytics::eda()
      ),
      shinydashboard::tabItem("subitem3",
                              omicsBioAnalytics::dea()
      ),
      shinydashboard::tabItem("subitem4",
                              omicsBioAnalytics::biomarker_discovery_analysis()
      ),
      shinydashboard::tabItem(
        tabName = "appfeatures",
        fluidRow(
          column(12,
                 tags$img(
                   src = "Fig1.png",
                   id = "app-features",
                   style = "width:100%; max-width:100%; height:auto;"
                 ),
                 p("Created in BioRender.com")
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "report",
        omicsBioAnalytics::report_ui("report")
      )
    )
  )
)

shinydashboard::dashboardPage(
  skin = "blue",
  header,
  sidebar,
  body
)
