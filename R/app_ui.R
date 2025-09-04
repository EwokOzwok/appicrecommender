#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import htmltools
#' @import DT
#' @import shinyjs
#' @noRd
app_ui <- function(request) {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    f7Page(
      useShinyjs(),
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
                  f7Align(h4("The APPIC Site Recommender takes a list of APPIC site numbers, runs a recommender algorithm,"), side = c("center")),
                  f7Align(h4("and outputs the top 10 APPIC internship sites that most closely resemble the description of your preferred sites."), side = c("center")),
                  br(),
                  f7Align(h5("Super useful recommendations got you feeling generous?"),side = c("center")),
                  f7Align(h5("A coffee in these trying times would be greatly appreciated!"),side = c("center")),

                  tags$div(
                    id = "buy_me_a_coffee",
                    style = "display: flex; justify-content: center; align-items: center; height: auto; width: 100%;",
                    tags$img(
                      src = "https://cdn.buymeacoffee.com/buttons/v2/default-blue.png",  # Replace with your image URL
                      alt = "Buy Me A Coffee",
                      style = "height: 60px !important;width: 217px !important; cursor: pointer;"
                    )
                  ),

                  hairlines = F, strong = T, inset = F, tablet = FALSE
                )
              )
            ),
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  title = "Select Program/Degree type, and at least 2 Sites.",
                  br(),
                  f7Select("programtype", "Select your program type", choice = c("select one", "Clinical", "Counseling", "School")),
                  br(),
                  br(),
                  f7Select("degreetype", "Select your degree type", choice = c("select one", "PhD", "PsyD", "EdD")),
                  br(),
                  br(),
                  f7Select("sitetype", "Select Site Type (Optional)", selected = NULL, choices = c("select one", "VAMC", "UCC", "Consortia", "Community Mental Health", "Hospitals (Non-VA)", "Child/Adolescent")),
                  br(),
                  br(),
                  uiOutput("site_selector"),
                  br(),
                  br(),
                  f7Toggle("user_recs", "Include User Recommendations"),
                  br(),
                  br(),
                  uiOutput("get_recs_button"),
                  br(),
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
              DT::DTOutput("dataTable"),
              hairlines = F, strong = T, inset = F, tablet = FALSE
              )
            )
          )
          ),


          f7Tab(
            tabName = "Map",
            icon = f7Icon("map_pin"),
            active = TRUE,
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  f7Align(h2("The APPIC Site Map"), side = c("center")),
                  uiOutput("RecommendationToggle"),
                  br(),
                  leafletOutput("map", height = "600px"),
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
    tags$script(src = "https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js"),


    tags$style(HTML("
    .dataTable {
      color: white !important;
    }
  ")),
    # HTML('<link rel="stylesheet" type="text/css" href="https://ewokozwok.github.io/Rolodex/www/framework7.bundle.min.css">'),

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
