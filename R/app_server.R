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
#' @noRd
app_server <- function(input, output, session) {

  site_list <- reactiveValues(appic = appic$APPICNumber, site = appic$Site...Department)


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




  observe({
    program = input$programtype
    degree = input$degreetype
    if(program == "select one" | degree == "select one"){
      output$get_recs_button <- renderUI({})
      output$site_selector <- renderUI({})


    } else {

      if(degree == "PhD"){
        if(program == "Clinical"){
          site_list$site = appic[appic$PhD == 1 & appic$Clinical == 1, "Site...Department"]
          site_list$appic = appic[appic$PhD == 1 & appic$Clinical == 1, "APPICNumber"]


          }

        if(program == "Counseling"){
          site_list$site = appic[appic$PhD == 1 & appic$Counseling == 1, "Site...Department"]
          site_list$appic = appic[appic$PhD == 1 & appic$Counseling == 1, "APPICNumber"]

          }

        if(program == "School"){
          site_list$site = appic[appic$PhD == 1 & appic$School == 1, "Site...Department"]
          site_list$appic = appic[appic$PhD == 1 & appic$School == 1, "APPICNumber"]
          }
      }

      if(degree == "PsyD"){
        if(program == "Clinical"){
          site_list$site = appic[appic$PsyD == 1 & appic$Clinical == 1, "Site...Department"]
          site_list$appic = appic[appic$PsyD == 1 & appic$Clinical == 1, "APPICNumber"]


        }

        if(program == "Counseling"){
          site_list$site = appic[appic$PsyD == 1 & appic$Counseling == 1, "Site...Department"]
          site_list$appic = appic[appic$PsyD == 1 & appic$Counseling == 1, "APPICNumber"]

        }

        if(program == "School"){
          site_list$site = appic[appic$PsyD == 1 & appic$School == 1, "Site...Department"]
          site_list$appic = appic[appic$PsyD == 1 & appic$School == 1, "APPICNumber"]
        }
      }


      if(degree == "EdD"){
        if(program == "Clinical"){
          site_list$site = appic[appic$EdD == 1 & appic$Clinical == 1, "Site...Department"]
          site_list$appic = appic[appic$EdD == 1 & appic$Clinical == 1, "APPICNumber"]


        }

        if(program == "Counseling"){
          site_list$site = appic[appic$EdD == 1 & appic$Counseling == 1, "Site...Department"]
          site_list$appic = appic[appic$EdD == 1 & appic$Counseling == 1, "APPICNumber"]

        }

        if(program == "School"){
          site_list$site = appic[appic$EdD == 1 & appic$School == 1, "Site...Department"]
          site_list$appic = appic[appic$EdD == 1 & appic$School == 1, "APPICNumber"]
        }
      }



      output$site_selector <- renderUI({
        tagList(
          f7SmartSelect("sites", "Choose at least 2 sites:", openIn = 'page', searchbar = T, multiple = T, selected = "Choose sites", choices = site_list$site),

        )
      })



      output$get_recs_button <- renderUI({
        tagList(
          f7Button("get_recommendations", "Get Site Recommendations!")
        )
      })




    }





  })



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






    promise <- future({
      tryCatch({
        print("Starting POST request...")
        print(paste("Sending sites:", paste(sites, collapse=", ")))

        response <- POST(
          "http://localhost:9090/recommend",
          body = list(appic_numbers = appic_numbers,
                      program_type = program,
                      degree_type = degree,
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
}
