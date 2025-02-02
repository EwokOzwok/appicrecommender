#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import htmltools
#' @import DT
#' @noRd
app_ui <- function(request) {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    f7Page(
      title = "APPIC Site Recommender",
      options = list(theme=c("auto"), dark=TRUE, preloader = F,  pullToRefresh=TRUE),
      allowPWA=F,
      f7TabLayout(
        # panels are not mandatory. These are similar to sidebars
        navbar = f7Navbar(
          title= "APPIC Site Recommender"),
        # f7Tabs is a special toolbar with included navigation
        f7Tabs(
          id = "tabs",
          animated = TRUE,
          swipeable = FALSE,
          # Define tabs here
          f7Tab(
            tabName = "WelcomeTab",
            icon = f7Icon("house_fill"),
            active = TRUE,
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  title = "What does the APPIC Recommender do?",
                  f7Align(h3("The APPIC Site Recommender takes a list of APPIC site numbers, runs a recommender algorithm, and outputs the top 10 APPIC internship sites that most closely resemble the description of your preferred sites."),
                          side = c("left")),
                  hairlines = F, strong = T, inset = F, tablet = FALSE
                )
              )
            ),
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  title = "Enter AT LEAST 2 site numbers separated by commas (e.g., 1442, 1242, 1099) below...",
                  f7Text(inputId = "site_numbers", label = "Enter site numbers", placeholder = "Site Numbers Here"),
                  br(),
                  f7Button("get_recommendations", "Get Site Recommendations!"),
                  hairlines = F, strong = T, inset = F, tablet = FALSE
                )
              )
            ),
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  uiOutput("download_button_ui"),
                  DTOutput("dataTable"),
                  hairlines = F, strong = T, inset = F, tablet = FALSE
                )
              )
            )
          )
        )
      )
    )
  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @import htmltools
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  # add_resource_path(
  #   "www",
  #   app_sys("app/www")
  # )

  tags$head(
    tags$style(HTML("
    .dataTable {
      color: white !important;
    }
  ")),
    HTML('<link rel="stylesheet" type="text/css" href="https://ewokozwok.github.io/Rolodex/www/framework7.bundle.min.css">'),

    # Fix Install button to Upper right of screen
    tags$style("
    .custom-toast {
    position: fixed !important;
    top: 20px !important;
    right: 20px !important;
    z-index: 9999 !important;
    width: auto !important; /* Ensure it doesn't span unnecessarily */
    height: auto !important; /* Keep it concise */
    box-shadow: none; /* Optional: Remove toast shadow */
    background: transparent !important; /* Optional: Remove toast background */
    }

    .custom-toast .toast-button {
        background-color: green; /* Retain the button color */
        color: white;
        border: none;
        padding: 10px 15px;
        border-radius: 5px;
        cursor: pointer;
    }
    "),
  )
}
