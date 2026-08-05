# Interactive interface
dashboardPage(
  dashboardHeader(title = "Run Miles Run!"),
  
  # Sidebar Navigation
  dashboardSidebar(
    sidebarMenu(
      id = "overview",
      menuItem("Overview", tabName = "overview", icon = icon("list")),
      menuItem("Data Explorer", 
        icon = icon("compass"),
        menuSubItem("Temperature vs. Finish Time", tabName = "temp_v_time"),
        menuSubItem("Route Map", tabName = "map")
      ),
      menuItem("Results", tabName = "results", icon = icon("chart-simple")),
      menuItem("About", tabName = "about", icon = icon("circle-info"))
    )
  ),
  
  # Body Contents
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
        h2("Overview")
      ),
      tabItem(tabName = "temp_v_time",
        h2("Temperature vs. Finish Time"),
      ),
      tabItem(tabName = "map",
        h2("Course Maps")
      ),
      tabItem(tabName = "results",
        h2("Results/Findings")
      ),
      tabItem(tabName = "about",
        h2("About This Project"),
        p("These are the details of this project.")
      )
    )
  )
)