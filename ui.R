library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage("iDigBio data exploration",
               tabPanel("Loading the data",
                        h3("Query"),
                        verbatimTextOutput("queryText")
                        ),

               tabPanel("Plot through time",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("plot_type",
                                            label = "Plot type",
                                            choices = c("Barplot", "Cumulative")),

                                dateRangeInput("date_range",
                                               label = "Date Range",
                                               start = min(as.Date(hol$datecollected), na.rm = TRUE),
                                               end = max(as.Date(hol$datecollected), na.rm = TRUE)),

                                selectInput("color_by",
                                            label = "Color by:",
                                            choices = c("none", "institutioncode")),

                                selectizeInput('institution_code', 'Institution Code',
                                               choices = unique(hol$institutioncode),
                                               multiple = TRUE,
                                               selected = unique(hol$institutioncode))

                                ),

                            ## Show a plot of the generated distribution
                            mainPanel(
                                tabPanel("Samples through time", plotOutput("distPlot"))
                                )
                            )
                        ),
               tabPanel("Missing data",
                        sidebarLayout(
                            sidebarPanel(
                                selectizeInput('institution_code_missing', 'Institution Code',
                                               choices = unique(hol$institutioncode),
                                               multiple = TRUE,
                                               selected = unique(hol$institutioncode)),
                                checkboxInput("only_missing",
                                              "Remove fields with only missing data",
                                              FALSE),
                                checkboxInput("no_missing",
                                              "Remove fields with no missing data",
                                              FALSE),
                                selectizeInput('fields_to_show', 'Fields to show',
                                               choices = names(hol),
                                               multiple = TRUE,
                                               selected = names(hol))
                                ),
                            mainPanel(
                                plotOutput("missingData"))
                            )
                        ),

               tabPanel("Unique values",
                        sidebarLayout(
                            sidebarPanel(
                                selectizeInput('institution_code_unique', 'Institution Code',
                                               choices = unique(hol$institutioncode),
                                               multiple = TRUE,
                                               selected = unique(hol$institutioncode)),
                                 checkboxInput("fully_unique",
                                              "Remove fields with only unique values",
                                               FALSE),
                                checkboxInput("only_one",
                                              "Remove fields with only 1 value",
                                              FALSE),
                                selectizeInput('fields_to_show_unique', 'Fields to show',
                                               choices = names(hol),
                                               multiple = TRUE,
                                               selected = names(hol))

                                ),
                            mainPanel(
                                plotOutput("unique_values")
                                #verbatimTextOutput("unique_text_values")
                                )
                            )
                        )
               )
    )
