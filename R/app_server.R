#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import httr
#' @import jsonlite
#' @import shinyjs
#' @import future
#' @import promises
#' @import DT
#' @import leaflet
#' @import tidygeocoder
#' @import dplyr
#' @noRd
app_server <- function(input, output, session) {

  site_list <- reactiveValues(appic = appic$APPICNumber, site = appic$Site...Department)
  current_site_type <- reactiveVal(NULL)
  rec_geo_df <- reactiveVal()
  edit_counter = reactiveVal(0)

  # Reactive value to store filtered sites
  filtered_sites <- reactiveVal(as.character(appic$Site...Department))

  runjs('
  $(document).ready(function() {
    // Ensure this runs only after the table is rendered
    $("#buy_me_a_coffee").click(function() {
      Shiny.setInputValue("buy_me_a_coffee", "clicked");
      // Trigger the link opening here after setting the input value
      window.open("https://www.buymeacoffee.com/Ewokozwok", "_blank");
    });
  });
')

  # Define Promises Functions -----------------------------------------------
  future::plan(multisession)
  `%...>%` <- function(promise, success) {
    promise %>% promises::then(success)
  }
  `%...!%` <- function(promise, error) {
    promise %>% promises::catch(error)
  }
  resolve_promise <- function(promise, success, error = NULL) {
    if (is.null(error)) {
      promise %...>% success
    } else {
      promise %...>% success %...!% error
    }
  }

  # Initial render of site selector - this will be updated by updateF7SmartSelect
  output$site_selector <- renderUI({
    tagList(
      f7SmartSelect("sites", "Choose at least 2 sites:",
                    openIn = 'page', searchbar = TRUE, multiple = TRUE,
                    selected = NULL, choices = as.list(as.character(appic$Site...Department)))
    )
  })

  observeEvent({
    input$programtype
    input$degreetype
    input$sitetype
  }, {
    program <- input$programtype
    degree <- input$degreetype
    sitetype <- input$sitetype

    # Validation mappings
    possible_site_types <- c("select one", "VAMC", "UCC", "Consortia",
                             "Community Mental Health", "Hospitals (Non-VA)",
                             "Child/Adolescent")

    sitetypecols <- c("AllSites", "VAMC", "UCC", "Consortia",
                      "CommunityMH", "Hospitals", "ChildAdolescent")

    # Input validation
    if (is.null(program) || is.null(degree) || is.null(sitetype)) {
      output$get_recs_button <- renderUI({})
      # Reset to all sites if inputs are invalid
      filtered_sites(as.character(appic$Site...Department))
      return()
    }

    # Check if required selections are valid
    if (program == "select one" || degree == "select one") {
      output$get_recs_button <- renderUI({})
      # Reset to all sites if selections are invalid
      filtered_sites(as.character(appic$Site...Department))
      return()
    }

    # Validate inputs against expected values
    valid_programs <- c("Clinical", "Counseling", "School")
    valid_degrees <- c("PhD", "PsyD", "EdD")

    if (!program %in% valid_programs || !degree %in% valid_degrees) {
      warning("Invalid input selection detected")
      output$get_recs_button <- renderUI({})
      filtered_sites(as.character(appic$Site...Department))
      return()
    }

    # Handle optional site type filtering
    use_site_filter <- !is.null(sitetype) && sitetype != "select one"
    selected_site_col <- NULL

    if (use_site_filter) {
      if (!sitetype %in% possible_site_types) {
        warning("Invalid site type selection detected")
        output$get_recs_button <- renderUI({})
        filtered_sites(as.character(appic$Site...Department))
        return()
      }

      # Get the corresponding site type column
      selected_site_col <- sitetypecols[match(sitetype, possible_site_types)]

      # Check if the column exists in the data
      if (is.na(selected_site_col) || !selected_site_col %in% names(appic)) {
        warning(paste("Site type column not found:", selected_site_col))
        output$get_recs_button <- renderUI({})
        filtered_sites(as.character(appic$Site...Department))
        return()
      }
    }

    print(paste("Program:", program))
    print(paste("Degree:", degree))
    print(paste("Site type:", sitetype))
    print(paste("Use site filter:", use_site_filter))
    if (use_site_filter) {
      print(paste("Site column:", selected_site_col))
    }

    current_site_type(selected_site_col)

    # Dynamic filtering using computed conditions
    tryCatch({
      # Build filter conditions dynamically
      degree_condition <- appic[[degree]] == 1
      program_condition <- appic[[program]] == 1

      # Apply site filter only if specified
      if (use_site_filter) {
        site_condition <- appic[[selected_site_col]] == 1
        print(paste("Site condition sum:", sum(site_condition, na.rm = TRUE)))
        # Check if site column has valid data
        if (any(is.na(site_condition))) {
          warning("Missing or invalid data in site filtering column")
        }
        combined_filter <- degree_condition & program_condition & site_condition
        print(paste("Combined filter sum:", sum(combined_filter, na.rm = TRUE)))
      } else {
        combined_filter <- degree_condition & program_condition
        print(paste("No site filter - combined filter sum:", sum(combined_filter, na.rm = TRUE)))
      }

      # Check if required columns have valid data
      if (any(is.na(degree_condition)) || any(is.na(program_condition))) {
        warning("Missing or invalid data in filtering columns")
      }

      # Handle case where no sites match the criteria
      if (sum(combined_filter, na.rm = TRUE) == 0) {
        warning("No sites match the selected criteria")
        site_list$site <- character(0)
        site_list$appic <- character(0)
        filtered_sites(character(0))
      } else {
        # Filter and assign results
        site_list$site <- appic[combined_filter & !is.na(combined_filter), "Site...Department"]
        site_list$appic <- appic[combined_filter & !is.na(combined_filter), "APPICNumber"]
        filtered_sites(as.character(site_list$site))

        print(paste("Found", length(site_list$site), "matching sites"))
      }

    }, error = function(e) {
      warning(paste("Error in filtering:", e$message))
      site_list$site <- character(0)
      site_list$appic <- character(0)
      filtered_sites(character(0))
    })

    # Render the recommend button since we have valid program and degree selections
    output$get_recs_button <- renderUI({
      tagList(
        f7Button("get_recommendations", "Get Site Recommendations!")
      )
    })
  })

  # Observer to update site selector when filtered_sites changes
  observeEvent({
    input$programtype
    input$degreetype
    input$sitetype
  }, {
    print(paste("Observer triggered: Updating site selector with", length(filtered_sites()), "sites"))

    output$site_selector <- renderUI({})
    # Delay for 150 milliseconds (0.15 seconds)
    shinyjs::delay(150, {
      output$site_selector <- renderUI({
        tagList(
          f7SmartSelect("sites", "Choose at least 2 sites:",
                        openIn = 'page', searchbar = TRUE, multiple = TRUE,
                        selected = NULL, choices = filtered_sites())
        )
      })
    })


  })


  # observeEvent({
  #     input$programtype
  #     input$degreetype
  #     input$sitetype
  #   }, {
  #   program = input$programtype
  #   degree = input$degreetype
  #   sitetype = input$sitetype
  #
  #
  #   possible_site_types = c("select one", "VAMC", "UCC", "Consortia", "Community Mental Health", "Hospitals (Non-VA)", "Child/Adolescent")
  #
  #   sitetypecols = c("AllSites", "VAMC",	"UCC",	"Consortia",	"CommunityMH",	"Hospitals",	"ChildAdolescent")
  #
  #   selected_site_col <- sitetypecols[match(input$sitetype, possible_site_types)]
  #
  #   print(program)
  #   print(degree)
  #   print(selected_site_col)
  #
  #   current_site_type(selected_site_col)
  #
  #   if(program == "select one" | degree == "select one"){
  #     output$get_recs_button <- renderUI({})
  #
  #
  #   } else {
  #
  #     if(degree == "PhD"){
  #       if(program == "Clinical"){
  #         site_list$site = appic[appic$PhD == 1 & appic$Clinical == 1 & appic[,selected_site_col] == 1, "Site...Department"]
  #         site_list$appic = appic[appic$PhD == 1 & appic$Clinical == 1 & appic[,selected_site_col] == 1, "APPICNumber"]
  #
  #
  #         }
  #
  #       if(program == "Counseling"){
  #         site_list$site = appic[appic$PhD == 1 & appic$Counseling == 1 & appic[,selected_site_col] == 1, "Site...Department"]
  #         site_list$appic = appic[appic$PhD == 1 & appic$Counseling == 1 & appic[,selected_site_col] == 1, "APPICNumber"]
  #
  #         }
  #
  #       if(program == "School"){
  #         site_list$site = appic[appic$PhD == 1 & appic$School == 1 & appic[,selected_site_col] == 1, "Site...Department"]
  #         site_list$appic = appic[appic$PhD == 1 & appic$School == 1 & appic[,selected_site_col] == 1, "APPICNumber"]
  #         }
  #     }
  #
  #     if(degree == "PsyD"){
  #       if(program == "Clinical"){
  #         site_list$site = appic[appic$PsyD == 1 & appic$Clinical == 1 & appic[,selected_site_col] == 1, "Site...Department"]
  #         site_list$appic = appic[appic$PsyD == 1 & appic$Clinical == 1 & appic[,selected_site_col] == 1, "APPICNumber"]
  #
  #
  #       }
  #
  #       if(program == "Counseling"){
  #         site_list$site = appic[appic$PsyD == 1 & appic$Counseling == 1 & appic[,selected_site_col] == 1, "Site...Department"]
  #         site_list$appic = appic[appic$PsyD == 1 & appic$Counseling == 1 & appic[,selected_site_col] == 1, "APPICNumber"]
  #
  #       }
  #
  #       if(program == "School"){
  #         site_list$site = appic[appic$PsyD == 1 & appic$School == 1 & appic[,selected_site_col] == 1, "Site...Department"]
  #         site_list$appic = appic[appic$PsyD == 1 & appic$School == 1 & appic[,selected_site_col] == 1, "APPICNumber"]
  #       }
  #     }
  #
  #
  #     if(degree == "EdD"){
  #       if(program == "Clinical"){
  #         site_list$site = appic[appic$EdD == 1 & appic$Clinical == 1 & appic[,selected_site_col] == 1, "Site...Department"]
  #         site_list$appic = appic[appic$EdD == 1 & appic$Clinical == 1 & appic[,selected_site_col] == 1, "APPICNumber"]
  #
  #
  #       }
  #
  #       if(program == "Counseling"){
  #         site_list$site = appic[appic$EdD == 1 & appic$Counseling == 1 & appic[,selected_site_col] == 1, "Site...Department"]
  #         site_list$appic = appic[appic$EdD == 1 & appic$Counseling == 1 & appic[,selected_site_col] == 1, "APPICNumber"]
  #
  #       }
  #
  #       if(program == "School"){
  #         site_list$site = appic[appic$EdD == 1 & appic$School == 1 & appic[,selected_site_col] == 1, "Site...Department"]
  #         site_list$appic = appic[appic$EdD == 1 & appic$School == 1 & appic[,selected_site_col] == 1, "APPICNumber"]
  #       }
  #     }
  #
  #     current_df = as.data.frame(matrix(data=NA, nrow = length(site_list$site), ncol = 2))
  #     current_df[,1]=site_list$site
  #     current_df[,2]=site_list$appic
  #     print(head(current_df)
  #
  #     updateF7SmartSelect("sites", choices = site_list$site, selected = NULL, session = session)
  #
  #     # current_count = edit_counter()
  #     # if(current_count > 0){
  #     #
  #     # }
  #
  #     edit_counter(edit_counter()+1)
  #
  #     output$get_recs_button <- renderUI({
  #       tagList(
  #         f7Button("get_recommendations", "Get Site Recommendations!")
  #       )
  #     })
  #
  #
  #
  #
  #   }
  #
  #
  # })



  observeEvent(input$buy_me_a_coffee,{
    runjs('window.open("https://buymeacoffee.com/Ewokozwok", "_blank");')


  })

  clean_json_string <- function(json_string) {
    json_string <- gsub("NaN", "null", json_string)  # Replace NaN with null
    return(json_string)
  }









# agency_type = c(
#   "Select Agency Type(s) (OPTIONAL)",
#   "Academic Health Center",
#   "Armed Forces Medical Center",
#   "Child/Adolescent Psychiatric or Pediatrics",
#   "Community Health Center",
#   "Consortium",
#   "Medical School",
#   "Prison or Correctional Facility",
#   "Private General Hospital",
#   "Private Outpatient Clinic",
#   "Private Psychiatric Hospital",
#   "Psychology Department",
#   "School District",
#   "State/County/Other Public Hospital",
#   "VA Medical Center",
#   "Other"
# )









  observeEvent(input$get_recommendations, {
    # Split and convert input numbers to numeric vector
    # sites <- as.numeric(unlist(strsplit(input$site_numbers, "[,\\s]+")))

    sites <- na.omit(as.list(input$sites))
    appic_numbers <- c()  # Initialize an empty list to store results

    for (site in sites) {
      print(site)
      # Extract numbers for each site and append to the list
      appic_numbers <- c(appic_numbers, appic[appic$Site...Department == site, "APPIC.Number"])
    }

    print(appic_numbers)

    program <- input$programtype
    degree <- input$degreetype
    sitetype <- current_site_type()
    user_rec_toggle <- input$user_recs

    if(user_rec_toggle == T){
      user_rec_status = 1
    } else {
      user_rec_status = 0
    }

    # Replace NaN values with NULL or another appropriate value (e.g., NA, "N/A")
    # appic_numbers[is.nan(sites)] <- NULL
        # Check if all sites exist in appic[[1]] - assuming APPIC numbers are in first element
    # invalid_sites <- appic_numbers[!appic_numbers %in% appic$APPIC.Number]

    # Check if all elements are numeric
    # if (any(is.na(sites))) {
    #   showNotification(
    #     "Please enter valid numeric site numbers.",
    #     type = "error",
    #     duration = 15
    #   )
    #   return()  # Stop execution if any value is invalid
    # }


#
#     if(length(invalid_sites) > 0) {
#       showNotification(
#         paste("Invalid site number(s) entered:", paste(invalid_sites, collapse=", ")),
#         type = "error",
#         duration = 15
#       )
#       return()  # This stops the execution of the rest of the observer
#     }

    if(length(sites) < 2) {
      showNotification(
        paste("Enter at least 2 sites"),
        type = "error",
        duration = 15
      )
      return()  # This stops the execution of the rest of the observer
    }


    f7Notif(
      text = "Running Recommendation Algorithm now...",
      icon = f7Icon("bolt_fill"),
      title = "Notification",
      subtitle = "Processing recommendation request",
      titleRightText = "now"
    )



    promise <- future({
      tryCatch({
        print("Starting POST request...")

        print(paste("Sending sites:", paste(sites, collapse=", ")))

        response <- POST(
          "http://localhost:9090/recommend",
          body = list(appic_numbers = appic_numbers,
                      program_type = program,
                      degree_type = degree,
                      site_type = sitetype,
                      user_rec_status = user_rec_status),
          encode = "json"
        )

        print("POST request completed.")
        print(paste("Response status:", status_code(response)))



        if (status_code(response) != 200) {
          stop(paste("Server returned error:", response))
        }
        response_content <- rawToChar(response$content)
        response_content <- clean_json_string(response_content)  # Clean the response
        # print(paste("Raw response:", response_content))
        return(response_content)  # Return the raw JSON string
      }, error = function(e) {
        print(paste("Error during POST request:", e$message))
        stop(e)
      })
    })

    resolve_promise(
      promise,
      success = function(result) {
        print("Promise resolved successfully.")
        # Parse the JSON string into a data frame
        tryCatch({
          json_data <- fromJSON(result)
          print("JSON parsed successfully")
          json_data<-as.data.frame(as.matrix(json_data))

          rec_geo_df(json_data)

          # Show the data frame to the user in a DataTable
          output$dataTable <- DT::renderDT({
            req(json_data)  # Ensure json_data is available
            clean_json_data <- json_data[, c(7,2:5)]  # Filter the necessary columns

            DT::datatable(clean_json_data)  # Render the table with DT::datatable
          })


          output$download <- downloadHandler(
            filename = function() {
              paste("APPIC Site Recommendataions - ", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(json_data, file, row.names = FALSE)
            }
          )

          output$download_button_ui <- renderUI({
            f7DownloadButton("download", "Download Recommendations")
          })

          output$RecommendationToggle <- renderUI({
            f7Toggle("rectoggle", "Show only recommended sites", checked = F)
          })




        }, error = function(e) {
          print(paste("Error parsing JSON:", e$message))
          showModal(modalDialog(
            title = "Error",
            paste("Error parsing server response:", e$message),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      },
      error = function(err) {
        print(paste("Promise rejected with error:", err))
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred while processing your request:", err$message),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    )
  })


  output$map <- renderLeaflet({
    leaflet(data = appicgeo) %>%
      addTiles() %>%
      addMarkers(
        ~long_jittered, ~lat_jittered,
        label = ~paste(City, State, Country),
        popup = ~paste0(
          "<div style='max-width: 250px;'>",
          "<h4 style='margin:0;'>", `Site...Department`, "</h4>",
          "<p style='margin:5px 0;'><b>📅 Application Due Date:</b><br/>", Application.Due.Date, "</p>",
          "<button onclick=\"window.open('", URL, "', '_blank')\" ",
          "style='background-color:#007aff; color:white; border:none; padding:6px 12px; border-radius:5px; cursor:pointer;'>",
          "View Program Info</button>",
          "</div>"
        )
      )
  })


  observeEvent(input$rectoggle,{
    if(input$rectoggle == T){
    recs <- rec_geo_df()
    filtered_df <- appicgeo[appicgeo$APPICNumber %in% recs[, 1], ]

    output$map <- renderLeaflet({
      leaflet(data = filtered_df) %>%
        addTiles() %>%
        addMarkers(
          ~long_jittered, ~lat_jittered,
          label = ~paste(City, State, Country),
          popup = ~paste0(
            "<div style='max-width: 250px;'>",
            "<h4 style='margin:0;'>", `Site...Department`, "</h4>",
            "<p style='margin:5px 0;'><b>📅 Application Due Date:</b><br/>", Application.Due.Date, "</p>",
            "<button onclick=\"window.open('", URL, "', '_blank')\" ",
            "style='background-color:#007aff; color:white; border:none; padding:6px 12px; border-radius:5px; cursor:pointer;'>",
            "View Program Info</button>",
            "</div>"
          )
        )
    })

    } else {

    output$map <- renderLeaflet({
      leaflet(data = appicgeo) %>%
        addTiles() %>%
        addMarkers(
          ~long_jittered, ~lat_jittered,
          label = ~paste(City, State, Country),
          popup = ~paste0(
            "<div style='max-width: 250px;'>",
            "<h4 style='margin:0;'>", `Site...Department`, "</h4>",
            "<p style='margin:5px 0;'><b>📅 Application Due Date:</b><br/>", Application.Due.Date, "</p>",
            "<button onclick=\"window.open('", URL, "', '_blank')\" ",
            "style='background-color:#007aff; color:white; border:none; padding:6px 12px; border-radius:5px; cursor:pointer;'>",
            "View Program Info</button>",
            "</div>"
          )
        )
    })

    }
  })



}
