#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    rclipboard::rclipboardSetup(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    suppressWarnings(
      navbarPage(
        title = "pmxCode",
        theme = bslib::bs_theme(bootswatch = "sandstone", version = 3),
        htmltools::tags$head(
          htmltools::tags$style(
            htmltools::HTML(
              paste(
                "@import url(https://use.fontawesome.com/releases/v6.2.0/css/all.css);",
                ".btn.active{",
                "  background-color: #c96674;",
                "}",
                ".btn.active:hover{",
                "  background-color: #c96674;",
                "}",
                sep = "\n"
              )
            )
          )
        ),
        new_model_ui(),
        covariates_ui(),
        help_ui(),
        header = NULL,
        footer = NULL
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
#' @import markdown
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "pmxcode"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
