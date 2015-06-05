library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)
library(ridigbio)
library(jsonlite)

# Define server logic required to draw a histogram
shinyServer(
    function(input, output, session) {
        ## # Expression that generates a histogram. The expression is
        ## # wrapped in a call to renderPlot to indicate that:
        ## #
        ## #  1) It is "reactive" and therefore should
        ## #     re-execute automatically when inputs change
        ## #  2) Its output type is a plot

        output$queryText <- renderText({
            qry <- parseQueryString(session$clientData$url_search)
            qry <- jsonlite::fromJSON(qry$rq)
            message("Query: ", qry)
            hol <<- idig_search_records(rq = qry, fields = "all")
            if (nrow(hol) > 1) paste("Number of records:", nrow(hol)) else "problem"
        })

        output$distPlot <- renderPlot({

            idig_data <- subset(hol, institutioncode %in% input$institution_code)
            idig_data$datecollected <- as.Date(idig_data$datecollected)

            if (identical(input$plot_type, "Barplot")) {
                 p <- ggplot(idig_data, aes(x = datecollected)) +
                   ylab("Number of records") + xlab("Date of collection") +
                   xlim(input$date_range[1], input$date_range[2]) + geom_histogram()

                if (input$color_by != "none") {
                    p <- p + geom_histogram(aes_string(fill = input$color_by))
                } else {
                    p <- p + geom_histogram()
                }
            } else {
                p <- ggplot(idig_data, aes_string(x = "datecollected")) +
                  xlab("Date of collection") + ylab("Proportion of specimens")
                if (input$color_by != "none") {
                    p <- p + stat_ecdf(aes_string(colour = input$color_by))
                } else {
                    p <- p + stat_ecdf()
                }
            }
            print(p)

        })

        output$missingData <- renderPlot({

            idig_data <- hol

            n_row <- nrow(idig_data)

            idig_data_tmp <- idig_data %>%
              filter(institutioncode %in% input$institution_code_missing) %>%
              select(one_of(input$fields_to_show)) %>%
              group_by(institutioncode) %>%
              summarise_each(funs(sum(is.na(.)/n_row))) %>%
              gather(institutioncode, percent_missing) %>%
              as.data.frame %>% setNames(c("institution_code", "field", "percent_missing"))

            idig_data_tmp$field <- reorder(idig_data_tmp$field, idig_data_tmp$percent_missing,
                                           sum)

            total_percent <- idig_data_tmp %>% group_by(field) %>%
              summarise(total_missing = sum(percent_missing))

            idig_data_tmp %<>%  left_join(total_percent)

            if (input$only_missing)
                idig_data_tmp %<>% filter(total_missing != 1)

            if (input$no_missing)
                idig_data_tmp %<>% filter(total_missing != 0)

            p <- ggplot(idig_data_tmp, aes(x = field, y = percent_missing, fill = institution_code)) +
              geom_bar(stat = "identity") + coord_flip() + ylim(c(0, 1)) +
              xlab("Data Fields") + ylab("Percent missing data")

            print(p)

        })
})
