library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
wpd_complete <- readRDS("data/wpd.rds")
countries <- wpd_complete |>
  distinct(COUNTRY, REGION)

draw_key_area <- function(data, params, size) {
  grid::rectGrob(
    gp = grid::gpar(
      col = "grey20",
      fill = alpha(data$fill, data$alpha),
      lty = 1
    )
  )
}

indicators <- c(
  "OTH_POLCLASS" = "Does the party/candidate claim that the political class - incumbent politicians, parties, bureaucrats, judges, and other holders of state office - are the \"elite\" or \"others\" to ordinary people and their interests?",
  "OTH_ECONOMIC" = "Does party/candidate claim that economic elites - big business, the wealthy, financiers, oligarchs or other economic powerbrokers - are the \"elite\" or \"others\" to ordinary people and their interests?",
  "OTH_MILITARY" = "Does party/candidate claim that the national military are the \"elite\" or \"others\" to ordinary people and their interests?",
  "OTH_IMMIGRANTS" = "Does the party/candidate claim that immigrants are the \"elite\" or \"others\" to ordinary people and their interests?",
  "OTH_ETHNIC" = "Does party/candidate claim that specific ethnic, racial or religious groups are the \"elite\" or \"others\" to ordinary people and their interests?",
  "OTH_FOREIGN" = "Does party/candidate claim foreign entities - states, institutions or other entities - are the \"elite\" or \"others\" to ordinary people and their interests?",
  "LRPOSITION" = "Where does the candidate/party stand on the left-right ideological spectrum?",
  "LIBDEMNORMS" = "Does the party/candidate's behaviour threaten established liberal democratic norms - engaging in political violence, resisting constitutional rules and institutional processes, showing hostility to judicial oversight, or attacking on media scrutiny - compared with predecessors and contemporaries?",
  "CHARISMA" = "Is the personal charisma of the candidate or party leader essential to their campaign's political appeal?",
  "INSIDER" = "Has party leader or candidate been in government in the past?",
  "INC_PARL" = "Is the legislative party incumbent in government at the time of the election?",
  "INC_PRES"= "Is the presidential candidate incumbent in government at the time of the election?"
)

server <- function(input, output, session) {
  observe({
    regions <- na.omit(unique(wpd_complete$REGION))
    types <- na.omit(unique(wpd_complete$TYPE_BROAD))
    updateSelectizeInput(inputId = "overall_region", choices = regions, selected = regions)
    updateSelectizeInput(inputId = "overall_indicator", choices = c("None", names(indicators)))
    updateSelectizeInput(inputId = "overall_type", choices = types, selected = types)
  })

  observe({
    req(input$overall_region)
    region_countries <- countries$COUNTRY[countries$REGION %in% input$overall_region]
    updateSelectizeInput(inputId = "overall_country", choices = unique(wpd_complete$COUNTRY), selected = region_countries)
  })

  overall_country <- reactive({
    input$overall_country
  })
  country_t <- debounce(overall_country, 300)

  indicator <- reactive({
    req(input$overall_indicator)
    if(input$overall_indicator == "None") return("Overall")
    return(sym(input$overall_indicator))
  })

  wpd_filtered <- reactive({
    req(country_t())
    req(input$overall_type)
    wpd_complete |>
      filter(
        COUNTRY %in% !!country_t(),
        TYPE_BROAD %in% !!input$overall_type,
        ROUND == 1
      )
  })

  plot_data <- reactive({
    # Aggregate votes over countries
    x <- if(length(country_t()) > 1) {
      # if(!identical(indicator(), "Overall")) browser()
      wpd_filtered() |>
        group_by(COUNTRY, INDICATOR = !!indicator(), TYPE_BROAD, YEAR = year(Date)) |>
        filter(Date == max(Date)) |>
        # Use 0% for <5% populism elections
        replace_na(list(VOTE = 0)) |>
        summarise(VOTE = sum(VOTE)) |>
        group_by(COUNTRY, TYPE_BROAD) |>
        complete(INDICATOR, YEAR, fill = list(VOTE = 0)) |>
        filter(!is.na(INDICATOR)) |>
        group_by(COUNTRY, TYPE_BROAD, INDICATOR) |>
        complete(YEAR = min(YEAR):2020) |>
        fill(VOTE) |>
        replace_na(list(VOTE = 0)) |>
        group_by(INDICATOR, TYPE_BROAD, YEAR) |>
        summarise(VOTE = mean(VOTE, na.rm = TRUE))
    } else {
      wpd_filtered() |>
        group_by(COUNTRY, INDICATOR = !!indicator(), TYPE_BROAD, YEAR = Date) |>
        summarise(VOTE = sum(VOTE)) |>
        group_by(COUNTRY, TYPE_BROAD) |>
        complete(YEAR, INDICATOR, fill = list(VOTE = 0)) |>
        filter(!is.na(INDICATOR))
    }

    ungroup(x)
  })

  plot_data_tot <- reactive({
    plot_data() |>
      group_by(YEAR, TYPE_BROAD) |>
      summarise(VOTE = sum(VOTE, na.rm = TRUE))
  })

  output$plot_overall <- renderPlot({
    req(plot_data_tot())
    req(plot_data())
    pal <- if(input$overall_indicator == "LRPOSITION") {
      c("#A30C0C", "white", "#0D6EFD")
    } else {
      c("white", "#0D6EFD")
    }
    p <- plot_data() |>
      ggplot(aes(y = replace_na(VOTE, 0), x = YEAR)) +
      scale_y_continuous(limits = c(-2, 100), breaks = c(5, 25, 50, 75, 100), labels = c("< 5%", "25%", "50%", "75%", "100%"), expand = expansion()) +
      scale_shape_manual(values = c("TRUE" = 1, "FALSE" = 16)) +
      theme_minimal() +
      # theme(legend.position = "bottom", legend.direction = "vertical") +
      labs(x = "Year", y = "Vote (%)", linetype = NULL) +
      guides(shape = "none") +
      facet_wrap(vars(TYPE_BROAD), ncol = 1)
    if(input$overall_indicator != "None") {
      codebook_indicator <- paste0(input$overall_indicator, ": ", indicators[input$overall_indicator])
      caption_wrap <- session$clientData$output_plot_overall_width/12

      p <- p +
        geom_area(aes(fill = INDICATOR), key_glyph = draw_key_area) +
        # guides(fill = guide_legend(title.position = "left")) +
        scale_fill_ordinal(type = pal) +
        labs(
          caption = paste0(strwrap(codebook_indicator, caption_wrap), collapse = "\n"),
          fill = input$overall_indicator
        ) +
        theme(plot.caption = element_text(hjust = 0))
    }
    p +
      geom_line(linetype = "dashed", data = plot_data_tot()) +
      geom_line(aes(linetype = "Total populism", y = ifelse(VOTE == 0, NA_real_, VOTE)), data = plot_data_tot(), key_glyph = "timeseries") +
      geom_point(size = 1.5, stroke = 0.8, data = plot_data_tot(), colour = "white") +
      geom_point(aes(shape = VOTE == 0), size = 1.5, stroke = 0.8, data = plot_data_tot()) +
      geom_hline(yintercept = 5, linetype = "dashed")
  }, height = function(){max(300, length(input$overall_type)*600)}, res = 144)
}

# Read HTML UI and replace navbar links
ui <- readLines("www/app.html")
ui_nav_links <- grepl("nav-link", ui)
ui <- gsub("href=\"\\.", "href=\"https://world-populism-dataset.github.io/website", ui)
ui <- sub("quarto-title column-body", "quarto-title column-page", ui)

shinyApp(ui = htmlTemplate(text_ = ui), server)
