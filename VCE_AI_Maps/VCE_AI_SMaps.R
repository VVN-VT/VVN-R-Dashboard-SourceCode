# ============================================================================
# Virginia Extension AI Data Tool
# ============================================================================
# The Virginia slice of the VCE AI tool project. Upstream is an AI assistant
# that routes a plain-language question to the right Virginia dataset.
#
# Tab 1 is the assistant itself -- the published screenshot of the chat window,
# and what the tool is doing when it answers. Tab 2 is the catalogue behind the
# routing: which datasets exist and which agency publishes each.
#
# This is the one dashboard in the suite with no map: the subject is data
# availability, not geography, and a choropleth of "number of datasets" per
# locality would imply a spatial pattern the catalogue does not have.
#
# Virginia-only data is extracted by prepare_data.R.
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(htmltools)
library(DT)

source("vvn_ui.R")

DATA_DIR   <- "VCE_AI_Data"
stopifnot(dir.exists(DATA_DIR))

catalogue  <- readRDS(file.path(DATA_DIR, "catalogue.rds"))
agencies   <- readRDS(file.path(DATA_DIR, "agencies.rds"))
localities <- readRDS(file.path(DATA_DIR, "localities.rds"))

# The alias table no longer has a tab of its own, but it is the reason the
# assistant can resolve "Richmond" at all, so its scale is reported on tab 1.
N_ALIASES    <- nrow(localities)
N_LOCALITIES <- dplyr::n_distinct(localities$official_name)

# A data-catalogue palette, distinct from the subject-specific map scales.
AI_AGENCY_PALETTE <- c("#BDD9E1", "#005575")
AI_LOCALITY_COLORS <- c("County" = "#1C7C8C", "Independent city" = "#E6A34A")

#' One screenshot with its caption, framed as a figure.
#' @param file image in www/
#' @param alt  description for screen readers -- it carries the figures in the
#'   image, because a reader who cannot see it still needs the numbers
#' @param ... caption content
vvn_shot <- function(file, alt, ...) {
  tags$figure(
    class = "vvn-shot",
    tags$img(src = file, alt = alt),
    tags$figcaption(...)
  )
}

# ===== UI ==================================================================

ui <- page_fluid(
  title = "Virginia Extension AI Data Tool",
  theme = vvn_theme(),
  tags$head(vvn_css()),

  vvn_header(
    "Virginia Extension AI Data Tool",
    "An AI assistant that answers Virginia questions from named public datasets, and the catalogue behind it."
  ),

  navset_tab(
    # ---------------------------------------------------------------- tab 1
    nav_panel(
      "The assistant",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          vvn_group("About the tool"),
          vvn_help(
            "Extension staff field questions that span health, agriculture,",
            "education, housing and income. The answers exist, but in different",
            "places. This tool's job is knowing which source answers which",
            "question."
          ),
          vvn_help(
            "You ask in plain language. It picks the dataset, answers from it,",
            "and links the source so the figure can be checked."
          ),
          vvn_help("The catalogue it draws on is on the second tab.")
        ),

        card(
          full_screen = TRUE,
          card_header("The chat window"),
          card_body(
            padding = 0,
            vvn_shot(
              "vce-ai-chatbot.png",
              paste("Chat window. A user asks whether there is data on diseases; the",
                    "assistant names the reportable_disease_surveillance_virginia_geography",
                    "dataset from the Virginia Department of Health and links it. The user",
                    "then asks the incidence rate of Lyme disease in Accomack County in 2024,",
                    "and the assistant answers 12 cases per 100,000 population, again with",
                    "the source link."),
              "Two exchanges with the tool. Each answer names the dataset it came from and links it —",
              "that dataset is one row of the catalogue on the next tab."
            )
          )
        ),

        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Why it is built this way — a conventional chatbot vs the VCE AI Tool"),
            card_body(
              padding = 0,
              vvn_shot(
                "vce-ai-hallucination.png",
                paste("The same question put to two systems. Asked which school in Fairfax",
                      "County had the most students with special education needs in 2023,",
                      "ChatGPT answers Kilmer Middle School with 326 students, marked with a",
                      "red cross. The VCE AI Tool answers Lake Braddock Secondary with 643",
                      "students for 2022-2023, marked with a green check and linked to the",
                      "special-education-child-count dataset on data.virginia.gov."),
                strong("The same question, two answers."), "This is the comparison the project leads with, and it",
                "is the reason the tool exists: a general chatbot produces a confident school name and number from",
                "nothing in particular, while the tool answers from a named dataset and links it. Both answers look",
                "equally plausible on the page — only one can be checked."
              )
            )
          )
        ),

        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Example questions and answers"),
            card_body(
              padding = 0,
              vvn_shot(
                "vce-ai-followup.png",
                paste("Chat window. Asked the incidence rate of chickenpox in Montgomery",
                      "County, the assistant answers 1 case per 100,000 population for 2024",
                      "with a link to the VDH reportable disease surveillance dataset. The",
                      "user then asks only 'what about for fairfax county?' and the assistant",
                      "answers 3.9 cases per 100,000 population for 2024, same source."),
                strong("A follow-up keeps its context."), "The second question is just “what about for fairfax",
                "county?” — the tool carries the subject and year across, and answers 3.9 against",
                "Montgomery's 1.0 from the same VDH dataset."
              ),
              vvn_shot(
                "vce-ai-schools.png",
                paste("Chat window. Asked for the total number of students with special",
                      "education needs in Montgomery County by school, the assistant returns",
                      "a per-school list for the 2021-2022 school year: Auburn Elementary 60,",
                      "Auburn High 42, Auburn Middle 59, Belview Elementary 36, Blacksburg",
                      "High 128, Blacksburg Middle 116, Christiansburg Elementary 44,",
                      "Christiansburg High 118, Christiansburg Middle 113, Christiansburg",
                      "Primary 49, Eastern Montgomery Elementary 66, Eastern Montgomery High",
                      "48, Falling Branch Elementary 76, and more below the visible area."),
                strong("Answers are not limited to one number."), "Asked for Montgomery County special education",
                "counts by school, it returns the full per-school breakdown — the list continues past the",
                "bottom of the screenshot. A follow-up can narrow it to elementary, middle or high."
              ),
              vvn_shot(
                "vce-ai-census.png",
                paste("Chat window. Asked for the total count of males 75 years and over with",
                      "a vision difficulty in Accomack County in 2020, the assistant answers",
                      "that Accomack County, FIPS 51001, had 168 such males, citing American",
                      "Community Survey table B18103_019 with a link to the Census data",
                      "developers page."),
                strong("Narrow demographic queries work too."), "Males 75 and over reporting a vision difficulty in",
                "Accomack County, 2020:", strong("168"), "— answered from ACS table B18103_019, with the FIPS",
                "code and the table identifier both given so the query can be reproduced."
              )
            )
          )
        ),

        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Saving and resuming a session"),
            card_body(
              padding = 0,
              vvn_shot(
                "vce-ai-session.png",
                paste("Top of the chat window with two arrows. One points at a 'Save Chat",
                      "Session' download link, the other at an 'Upload Previous Chat Session'",
                      "file chooser."),
                "A session exports to a small JSON file and can be uploaded again later to pick up where it left",
                "off — so a line of enquiry can be put down and resumed, or handed to a colleague."
              )
            )
          )
        ),

        vvn_notes(
          "How the chatbox works",
          p(
            "A question goes in as ordinary English —", em("“Do you have any data on diseases?”"), "— and the tool",
            "matches it against the keywords attached to each registered dataset. The match picks a source; the",
            "answer is then drawn from that source rather than from the model's own memory."
          ),
          p(
            "That distinction is the whole design. In the screenshot the assistant does not simply assert that Lyme",
            "incidence in Accomack County was", strong("12 cases per 100,000"), "— it names the dataset that says so",
            "and links it. An Extension agent can follow the link and confirm the number before repeating it, which",
            "is what makes the answer usable in the field."
          ),
          p(
            "The two exchanges also show the tool working at different levels: the first asks what", em("exists"), "and",
            "gets a description of coverage, columns and years; the second asks for a specific", em("value"), "and gets",
            "one figure. The same catalogue serves both."
          ),
          p(
            sprintf("Resolving a question to a place is a real obstacle in Virginia, so the tool carries %d locality name aliases mapping to %d official localities.",
                    N_ALIASES, N_LOCALITIES),
            "Virginia has 95 counties and 38 independent cities. The locality reference uses official names and aliases",
            "to resolve places; FIPS codes provide an unambiguous key."
          ),
          p(
            class = "vvn-help",
            "All six screenshots are reproduced from the project's own repository and documentation — they are",
            "its published examples, not mock-ups. Some have had empty space trimmed so the conversation and the",
            "input box sit together; no content inside any chat panel has been altered. This dashboard has no map",
            "by design — the subject is which data exists, not where."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 2
    nav_panel(
      "Data sources",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          vvn_group("Agency"),
          selectInput("agency", NULL,
                      choices = c("All agencies" = "", sort(unique(catalogue$agency)))),
          vvn_help(
            "Every dataset the assistant can route a question to, grouped by the",
            "agency that publishes it."
          ),
          vvn_help(
            "The keywords are what the tool matches a question against, so they",
            "show what each dataset is expected to answer."
          )
        ),

        uiOutput("cat_kpi"),
        card(
          full_screen = TRUE,
          card_header("Datasets per agency"),
          card_body(padding = 0, plotlyOutput("agency_chart", height = "320px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Locality aliases by locality type"),
            card_body(padding = 0,
                      plotlyOutput("locality_alias_chart", height = "300px"))
          )
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header(textOutput("cat_title", inline = TRUE)),
            card_body(DT::DTOutput("cat_table"))
          )
        ),

        vvn_notes(
          "Reading the catalogue",
          p(
            "This is the directory the assistant reasons over. The answers live in different places — the Census,",
            "County Health Rankings, the Virginia Department of Health, NASS and others — and knowing which source",
            "answers which question is the tool's actual contribution."
          ),
          p(
            "The", strong("keywords"), "column is the routing surface. A question mentioning “broadband” or",
            "“insurance” gets matched against these terms to pick a dataset, so the catalogue is both a",
            "directory of what exists and a description of what each source is expected to cover."
          ),
          p(
            class = "vvn-help",
            "Catalogue read from the project's master registry. Counts are datasets rather than files, so one",
            "registered dataset may be several files on disk."
          )
        )
      )
    )
  )
)

# ===== SERVER ==============================================================

server <- function(input, output, session) {

  # ---- tab 2 -------------------------------------------------------------

  cat_sel <- reactive({
    if (!nzchar(input$agency %||% "")) catalogue
    else filter(catalogue, agency == input$agency)
  })

  output$cat_title <- renderText({
    n <- nrow(cat_sel())
    if (nzchar(input$agency %||% "")) paste0(input$agency, " — ", n, " datasets")
    else paste0("All registered datasets — ", n)
  })

  output$cat_kpi <- renderUI({
    vvn_kpi_row(
      vvn_tile("Registered datasets", nrow(catalogue),
               "routable by the assistant", accent = TRUE),
      vvn_tile("Agencies", dplyr::n_distinct(catalogue$agency), "publishing bodies"),
      vvn_tile("Locality aliases", N_ALIASES,
               sprintf("resolving to %d localities", N_LOCALITIES)),
      vvn_tile("Largest source", agencies$agency[1],
               sprintf("%d files", agencies$files[1]))
    )
  })

  output$agency_chart <- renderPlotly({
    d <- catalogue %>%
      count(agency, name = "datasets") %>%
      arrange(desc(datasets)) %>%
      mutate(agency = factor(agency, levels = rev(agency)))

    gg <- ggplot(d, aes(x = datasets, y = agency,
                        text = paste0(agency, "<br>", datasets, " datasets"))) +
      geom_col(aes(fill = datasets), width = 0.68) +
      geom_text(aes(label = datasets), hjust = -0.35,
                size = 3.1, colour = VVN_INK) +
      scale_fill_gradient(low = AI_AGENCY_PALETTE[1],
                          high = AI_AGENCY_PALETTE[2], guide = "none") +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(x))),
                         expand = ggplot2::expansion(mult = c(0, 0.18))) +
      labs(x = "Datasets", y = NULL) +
      vvn_ggtheme()

    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>%
      layout(showlegend = FALSE)
  })

  output$locality_alias_chart <- renderPlotly({
    d <- localities %>%
      count(locality_type, name = "aliases") %>%
      mutate(
        locality_type = dplyr::recode(
          locality_type,
          county = "County",
          independent_city = "Independent city"
        ),
        locality_type = factor(locality_type,
                               levels = names(AI_LOCALITY_COLORS))
      )

    plot_ly(
      data = d,
      labels = ~locality_type,
      values = ~aliases,
      type = "pie",
      hole = 0.62,
      sort = FALSE,
      textinfo = "label+percent",
      hovertemplate = "%{label}<br>%{value} alias records (%{percent})<extra></extra>",
      marker = list(
        colors = unname(AI_LOCALITY_COLORS[as.character(d$locality_type)]),
        line = list(color = "#ffffff", width = 2)
      )
    ) %>%
      layout(
        showlegend = FALSE,
        annotations = list(list(
          x = 0.5, y = 0.5,
          text = paste0(format(sum(d$aliases), big.mark = ","), "<br>aliases"),
          showarrow = FALSE,
          font = list(size = 15, color = VVN_INK)
        )),
        margin = list(l = 20, r = 20, t = 16, b = 16),
        font = list(family = "Source Sans 3, system-ui, sans-serif",
                    size = 12, color = VVN_INK),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE)
  })

  output$cat_table <- DT::renderDT({
    cat_sel() %>%
      transmute(Agency = agency, Topic = topic, `Matched on` = keywords) %>%
      DT::datatable(
        rownames = FALSE,
        options = list(pageLength = 10, dom = "ftip", scrollX = TRUE),
        class = "compact stripe hover"
      )
  })
}

shinyApp(ui = ui, server = server)
