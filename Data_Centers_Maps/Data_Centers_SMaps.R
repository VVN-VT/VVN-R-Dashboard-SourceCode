# ============================================================================
# Virginia Data Centers
# ============================================================================
# Where Virginia's data centres are, and how the count has changed. The
# upstream DSPG project is a national study of heat-island and night-lights
# effects; this keeps only the distribution and the growth, for Virginia.
#
# Virginia-only data is extracted by prepare_data.R; county boundaries come
# from tigris at run time, as in every other app in the suite.
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(sf)
library(tigris)
library(leaflet)
library(htmltools)
library(scales)
library(DT)
library(scales)

source("vvn_ui.R")

# This dashboard's own sequential ramp. Every dashboard in the suite uses a
# different hue on the same lightness ladder, so the seven are distinguishable
# at a glance while still reading as one family -- see VVN_RAMPS.
DC_RAMP     <- vvn_ramp("green")
DC_RAMP_ALT <- vvn_ramp("teal")

options(tigris_use_cache = TRUE)

DATA_DIR <- "Data_Centers_Data"
stopifnot(dir.exists(DATA_DIR))

facilities <- readRDS(file.path(DATA_DIR, "va_data_centers.rds"))

# Development stages, ordered from built to speculative. This ordering is the
# whole point of the second tab: it is the pipeline, read left to right.
STAGE_LEVELS <- c("Operational", "Under Construction", "Planned",
                  "Land Banked", "Canceled")
facilities <- facilities %>%
  mutate(stage = factor(stage, levels = intersect(STAGE_LEVELS, unique(stage))))

STAGES <- levels(facilities$stage)
CAPACITY_TYPES <- sort(unique(facilities$capacity_type))

# A cool steel-blue map scale separates the site footprint from arts and health.
# The site footprint uses the shared water-blue ramp.
DC_PALETTE <- c("#BDD9E1", "#91BECC", "#5EA4BA", "#008BA9", "#007195", "#005575")
DC_STAGE_COLORS <- c(
  "Operational" = "#154B6A",
  "Under Construction" = "#E5751F",
  "Planned" = "#D7A629",
  "Land Banked" = "#7E8B97",
  "Canceled" = "#B95B63"
)

# How many facilities carry an operational year. Only about 7% do, which is
# small enough that the growth curve must be labelled as a sample rather than
# presented as the history of the industry -- see the second tab's notes.
DATED <- facilities %>% filter(!is.na(year_operational))
DATED_SHARE <- nrow(DATED) / nrow(facilities)

# The cumulative curve starts in 2000. Only three dated Virginia facilities
# predate that -- 1949, 1996 and 1999 -- and plotting them stretched the axis
# across five near-empty decades for three points, compressing the period that
# actually matters. They are not discarded: they are carried in as an opening
# balance, so the line starts at their count rather than resetting to zero and
# understating the running total at every year after.
GROWTH_START <- 2000L
GROWTH_BASELINE <- sum(DATED$year_operational < GROWTH_START)
DATED_SINCE <- nrow(DATED) - GROWTH_BASELINE

#' Virginia boundaries, fetched lazily so a Census outage cannot stop start-up.
load_boundaries <- local({
  value <- NULL; done <- FALSE
  function() {
    if (!done) {
      value <<- tryCatch(
        counties(state = "VA", cb = TRUE, class = "sf", progress_bar = FALSE) %>%
          st_transform(4326) %>%
          vvn_locality_key(),
        error = function(e) NULL
      )
      done <<- TRUE
    }
    value
  }
})

# Counts per locality are extremely skewed -- Loudoun alone holds roughly half
# the state's facilities -- so a linear colour ramp would paint one county dark
# and flatten everything else. Quantile breaks over the non-zero counties keep
# the rest of the distribution readable.
locality_counts <- function(stages, types) {
  facilities %>%
    filter(stage %in% stages, capacity_type %in% types) %>%
    count(GEOID, name = "n")
}

# ===== UI ==================================================================

ui <- page_fluid(
  title = "Virginia Data Centers",
  theme = vvn_theme(),
  tags$head(vvn_css()),

  vvn_header(
    "Virginia Data Centers",
    "Where Virginia's data centres are, and how the count has grown."
  ),

  navset_tab(
    # ---------------------------------------------------------------- tab 1
    nav_panel(
      "Distribution",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          vvn_group("Development stage"),
          checkboxGroupInput("stages", NULL, choices = STAGES,
                             selected = "Operational"),
          vvn_group("Capacity type"),
          checkboxGroupInput("types", NULL, choices = CAPACITY_TYPES,
                             selected = CAPACITY_TYPES),
          vvn_help(
            "Counties are shaded by facility count and each site is drawn as a",
            "point, so a single large locality does not hide the sites inside it."
          ),
          vvn_help(
            "Shading uses quantile breaks: Loudoun holds about half the state's",
            "data centres, and a linear scale would flatten every other locality."
          )
        ),

        uiOutput("dist_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("dist_title", inline = TRUE)),
          card_body(padding = 0, leafletOutput("dist_map", height = "560px"))
        ),

        vvn_notes(
          "What this map shows",
          p(
            "Virginia hosts the densest concentration of data centres anywhere in the world, and it is not evenly",
            "spread. ", strong("Loudoun County"), "alone accounts for roughly half of the state's facilities — the",
            "corridor known as Data Center Alley — with", strong("Prince William"), "and", strong("Fairfax"),
            "forming the rest of the Northern Virginia cluster."
          ),
          p(
            "Filter by stage to separate what exists from what is coming.",
            strong("Operational"), "sites are built and running;", strong("under construction"), "and",
            strong("planned"), "show where the next wave lands, which is increasingly outside Northern Virginia."
          ),
          p(
            class = "vvn-help",
            "Facility locations from the DSPG data centre inventory, selected to Virginia by spatial join.",
            "Counts are facilities, not capacity: a hyperscale campus and a small colocation site each count once."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 2
    nav_panel(
      "Change over time",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          vvn_group("Capacity type"),
          checkboxGroupInput("growth_types", NULL, choices = CAPACITY_TYPES,
                             selected = CAPACITY_TYPES),
          vvn_help(
            "The pipeline chart uses every Virginia facility. The cumulative",
            "curve runs from 2000 and uses only those with a recorded",
            "operational year —",
            sprintf("%d of %d, about %.0f%%.", nrow(DATED), nrow(facilities),
                    100 * DATED_SHARE)
          ),
          vvn_help(
            "Treat the curve as a sample of the growth pattern, not a count of",
            "the industry. The pipeline is complete data; the dates are not."
          )
        ),

        uiOutput("growth_kpi"),
        card(
          full_screen = TRUE,
          card_header("Development pipeline — every Virginia facility"),
          card_body(padding = 0, plotlyOutput("pipeline", height = "300px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header(textOutput("growth_title", inline = TRUE)),
            card_body(padding = 0, plotlyOutput("growth", height = "340px"))
          )
        ),

        vvn_notes(
          "Reading these charts",
          p(
            "The", strong("pipeline"), "is the honest measure of change here, because it covers every facility.",
            "Reading it from left to right gives the trajectory: what is already running, what is being built now,",
            "and what has been announced but not started. A large planned count against a small operational one",
            "means the visible footprint today understates what is committed."
          ),
          p(
            strong("The cumulative curve is a sample."), "Only",
            sprintf("%d of the %d", nrow(DATED), nrow(facilities)), "Virginia facilities carry an operational year",
            "in the source inventory, so the curve shows the shape of growth rather than its true level — the real",
            "counts are higher at every point. The curve opens in", strong("2000"), "because only three dated",
            "facilities predate it; those three are carried in as the starting count rather than dropped, so the",
            sprintf("line begins at %d and the %d dated facilities from 2000 onward build on it.",
                    GROWTH_BASELINE, DATED_SINCE),
            "Some dated entries are in the future, which is why the line continues past the present — those are",
            "commissioning dates for sites not yet running, marked by the dashed line."
          ),
          p(
            class = "vvn-help",
            "Counts are facilities rather than megawatts or floor area, so a hyperscale campus and a small",
            "colocation suite weigh the same."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 3
    nav_panel(
      "Pipeline detail",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          vvn_group("Capacity type"),
          checkboxGroupInput("pipe_types", NULL, choices = CAPACITY_TYPES,
                             selected = CAPACITY_TYPES),
          vvn_help(
            "The same pipeline as the previous tab, broken out two ways: by",
            "capacity type, and by the localities carrying it."
          ),
          vvn_help(
            "Colocation sites rent space to many tenants; hyperscalers are",
            "single-operator campuses. They move through the pipeline",
            "differently."
          )
        ),
        uiOutput("pipe_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("pipe_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("pipe_stage_type", height = "360px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Pipeline by locality"),
            card_body(padding = 0, plotlyOutput("pipe_locality", height = "480px"))
          )
        ),
        vvn_notes(
          "Reading the pipeline",
          p(
            "Stage is ordered from built to speculative:", strong("operational"), "sites exist today, those",
            strong("under construction"), "are committed,", strong("planned"), "sites are announced but not started, and",
            strong("land banked"), "means the site is held without a published plan. Reading left to right is reading",
            "forward in time."
          ),
          p(
            strong("The two capacity types behave differently."), "Comparing their stage profiles shows whether the",
            "growth still to come looks like the footprint already built, or like a shift in what kind of facility",
            "Virginia is getting."
          ),
          p(
            class = "vvn-help",
            "The locality chart shows every locality with at least one facility, stacked by stage. One canceled",
            "record is retained rather than dropped, so the counts reconcile with the source inventory."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 4
    nav_panel(
      "Concentration",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          vvn_group("Include stages"),
          checkboxGroupInput("conc_stages", NULL, choices = STAGES,
                             selected = STAGES),
          vvn_help(
            "How unevenly data centres are spread across Virginia, and how much",
            "of the total sits in the handful of localities at the top."
          ),
          vvn_help(
            "Switching stages on and off shows whether the concentration already",
            "built is being reinforced or spread out by what is planned."
          )
        ),
        uiOutput("conc_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("conc_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("conc_curve", height = "420px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Share of all Virginia data centres, by locality"),
            card_body(padding = 0, plotlyOutput("conc_share", height = "440px"))
          )
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header(
              div(
                class = "vvn-card-bar",
                span("The data behind these charts"),
                downloadLink("conc_csv", "Download CSV", class = "vvn-dl")
              )
            ),
            card_body(DT::DTOutput("conc_table"))
          )
        ),
        vvn_notes(
          "Reading the concentration",
          p(
            "The map shows that data centres cluster; this tab says", strong("how much"), ". The curve plots localities",
            "from most to fewest against the running share of the state total, so the height at any point answers",
            "\u201cwhat fraction sits in the top N localities?\u201d."
          ),
          p(
            "A straight diagonal would mean facilities are spread evenly across the localities that have them. The",
            "sharper the curve bends toward the top-left, the more the state total is carried by a few places — and",
            "the more a change in one locality moves the Virginia picture."
          ),
          p(
            strong("Only 33 of Virginia\u2019s 133 localities host a facility at all."), "The curve is drawn across those",
            "33, not all 133, so it understates the true concentration relative to the state as a whole: the other",
            "100 localities sit at zero and are not on the axis."
          ),
          p(
            class = "vvn-help",
            "Counts are facilities, not capacity. A locality with several small colocation suites can outrank one",
            "with a single very large hyperscale campus."
          )
        )
      )
    )
  )
)

# ===== SERVER ==============================================================

server <- function(input, output, session) {

  # ---- distribution ------------------------------------------------------

  dist_sel <- reactive({
    # req() on the control, a separate check on the result: an empty tick-list
    # and an empty combination are different problems and deserve different
    # messages.
    validate(
      need(length(input$stages) > 0, "Select at least one development stage."),
      need(length(input$types) > 0,  "Select at least one capacity type.")
    )
    facilities %>%
      filter(stage %in% input$stages, capacity_type %in% input$types)
  })

  output$dist_title <- renderText({
    n <- nrow(dist_sel())
    paste0(format(n, big.mark = ","), " data centres — ",
           paste(input$stages, collapse = ", "))
  })

  output$dist_kpi <- renderUI({
    d <- dist_sel()
    if (nrow(d) == 0) return(NULL)
    by_loc <- count(d, GEOID, NAME, sort = TRUE)
    top_share <- by_loc$n[1] / nrow(d)

    vvn_kpi_row(
      vvn_tile("Facilities", format(nrow(d), big.mark = ","),
               paste(input$stages, collapse = " + "), accent = TRUE),
      vvn_tile("Localities", nrow(by_loc), "with at least one"),
      vvn_tile("Largest concentration", format(by_loc$n[1], big.mark = ","),
               sprintf("%s — %.0f%% of the total", by_loc$NAME[1], 100 * top_share)),
      vvn_tile("Hyperscale share",
               sprintf("%.0f%%", 100 * mean(d$capacity_type == "Hyperscaler")),
               "of the selected facilities")
    )
  })

  output$dist_map <- renderLeaflet({
    shapes <- load_boundaries()
    validate(need(!is.null(shapes),
                  "County boundaries could not be downloaded from the Census TIGER service."))
    d <- dist_sel()
    validate(need(nrow(d) > 0,
                  "No facility matches this combination of stage and capacity type."))

    counts <- count(d, GEOID, name = "n")
    dat <- shapes %>% left_join(counts, by = "GEOID")
    values <- dat$n

    nz <- values[!is.na(values) & values > 0]
    breaks <- unique(c(0, stats::quantile(nz, probs = seq(0.2, 1, 0.2), na.rm = TRUE)))
    pal <- if (length(breaks) >= 3) {
      colorBin(DC_PALETTE, domain = range(breaks), bins = breaks,
               na.color = VVN_NO_DATA)
    } else {
      colorNumeric(DC_PALETTE, domain = values, na.color = VVN_NO_DATA)
    }

    lab <- sprintf(
      "<strong>%s</strong><br>%s data centre%s",
      dat$locality,
      ifelse(is.na(values), "no", format(values, big.mark = ",")),
      ifelse(!is.na(values) & values == 1, "", "s")
    )

    leaflet(dat) %>%
      vvn_basemap() %>%
      addPolygons(
        fillColor   = ~ifelse(is.na(values) | values == 0, VVN_NO_DATA, pal(values)),
        fillOpacity = 0.92,
        color = VVN_STROKE_COLOUR, weight = VVN_STROKE_WEIGHT,
        opacity = VVN_STROKE_OPACITY,
        label = lapply(lab, HTML), labelOptions = vvn_labelopts(),
        highlightOptions = vvn_highlight()
      ) %>%
      vvn_outline() %>%
      # Individual sites on top: the choropleth shows concentration, the points
      # show that a single county can hold hundreds of separate facilities.
      addCircleMarkers(
        data = d, lng = ~longitude, lat = ~latitude,
        radius = 3, stroke = FALSE, fillColor = DC_PALETTE[6], fillOpacity = 0.78,
        label = ~lapply(sprintf("<strong>%s</strong><br>%s<br>%s", NAME,
                                capacity_type, stage), HTML),
        labelOptions = vvn_labelopts()
      ) %>%
      addLegend(
        position = "bottomright", pal = pal, values = range(breaks),
        title = "Data centres<br>per locality", opacity = 1, na.label = "None"
      )
  })

  # ---- change over time --------------------------------------------------

  growth_sel <- reactive({
    facilities %>% filter(capacity_type %in% req(input$growth_types))
  })

  output$growth_kpi <- renderUI({
    d <- growth_sel()
    if (nrow(d) == 0) return(NULL)
    ops <- sum(d$stage == "Operational", na.rm = TRUE)
    pipe <- sum(d$stage %in% c("Under Construction", "Planned"), na.rm = TRUE)

    vvn_kpi_row(
      vvn_tile("Operational", format(ops, big.mark = ","),
               "built and running", accent = TRUE),
      vvn_tile("In the pipeline", format(pipe, big.mark = ","),
               "under construction or planned"),
      vvn_tile("Pipeline vs built",
               if (ops > 0) sprintf("%.0f%%", 100 * pipe / ops) else "—",
               "more coming, relative to today's footprint"),
      vvn_tile("Dated records", sprintf("%d of %d", sum(!is.na(d$year_operational)), nrow(d)),
               "carry an operational year")
    )
  })

  output$pipeline <- renderPlotly({
    d <- growth_sel() %>% filter(!is.na(stage)) %>% count(stage)
    validate(need(nrow(d) > 0, "Select at least one capacity type."))

    gg <- ggplot(d, aes(x = stage, y = n,
                        fill = stage,
                        text = paste0(stage, "<br>", format(n, big.mark = ","), " facilities"))) +
      geom_col(width = 0.68) +
      geom_text(aes(label = format(n, big.mark = ",")), vjust = -0.45,
                size = 3.4, colour = VVN_INK) +
      scale_fill_manual(values = DC_STAGE_COLORS, guide = "none") +
      scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.16))) +
      labs(x = NULL, y = "Facilities") +
      vvn_ggtheme()

    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>%
      layout(showlegend = FALSE)
  })

  output$growth_title <- renderText({
    y <- growth_sel()$year_operational
    sprintf("Cumulative data centres since %d — %d dated facilities, %d carried in",
            GROWTH_START,
            sum(y >= GROWTH_START, na.rm = TRUE),
            sum(y < GROWTH_START, na.rm = TRUE))
  })

  output$growth <- renderPlotly({
    dated <- growth_sel() %>% filter(!is.na(year_operational))

    # Facilities already standing when the window opens become the starting
    # count, so the running total stays correct instead of restarting at zero.
    opening <- sum(dated$year_operational < GROWTH_START)

    d <- dated %>%
      filter(year_operational >= GROWTH_START) %>%
      count(year_operational, name = "opened") %>%
      arrange(year_operational) %>%
      mutate(cumulative = opening + cumsum(opened))

    validate(need(nrow(d) > 1, "Too few dated facilities to draw a curve."))

    this_year <- as.integer(format(Sys.Date(), "%Y"))

    gg <- ggplot(d, aes(x = year_operational, y = cumulative,
                        text = paste0(year_operational, "<br>",
                                      cumulative, " cumulative<br>",
                                      opened, " opened that year"))) +
      geom_line(linewidth = 0.8, colour = DC_PALETTE[5]) +
      geom_point(size = 2.1, colour = DC_STAGE_COLORS["Planned"]) +
      scale_x_continuous(limits = c(GROWTH_START, NA),
                         breaks = function(x) seq(GROWTH_START, max(x), by = 5)) +
      labs(x = NULL, y = "Cumulative facilities") +
      vvn_ggtheme()

    # Mark where the record stops being history and starts being schedule.
    if (max(d$year_operational) > this_year) {
      gg <- gg + ggplot2::geom_vline(xintercept = this_year + 0.5,
                                     linetype = "dashed", linewidth = 0.4,
                                     colour = VVN_MUTED)
    }

    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })

  # ---- tab 3: pipeline detail --------------------------------------------

  # Stage is the measure on both of this tab's charts, so one fixed colour
  # assignment is used for it throughout: built to speculative, light to dark
  # along a single ramp, because stage is ordered.
  STAGE_COLOURS <- stats::setNames(
    DC_RAMP[seq(6, 6 - length(STAGES) + 1)], STAGES
  )

  pipe_sel <- reactive({
    facilities %>% filter(capacity_type %in% req(input$pipe_types))
  })

  output$pipe_title <- renderText({
    sprintf("Stage by capacity type — %s facilities",
            scales::comma(nrow(pipe_sel())))
  })

  output$pipe_kpi <- renderUI({
    d <- pipe_sel()
    # Distinguish an empty control from an empty result: the previous message
    # told the reader to select a capacity type when they already had, and the
    # real problem was that no facility matches the combination (Canceled +
    # Hyperscaler, for instance, where the whole state has one canceled site).
    validate(need(length(input$pipe_types) > 0, "Select at least one capacity type."))
    validate(need(nrow(d) > 0,
                  "No facility matches this combination of stage and capacity type."))
    built <- sum(d$stage == "Operational", na.rm = TRUE)
    coming <- sum(d$stage %in% c("Under Construction", "Planned"), na.rm = TRUE)
    top_type <- d %>% count(capacity_type, sort = TRUE) %>% slice(1)
    vvn_kpi_row(
      vvn_tile("Facilities", scales::comma(nrow(d)), "in the selection", accent = TRUE),
      vvn_tile("Operational", scales::comma(built),
               sprintf("%.0f%% of the selection", 100 * built / nrow(d))),
      vvn_tile("Committed or announced", scales::comma(coming),
               "under construction or planned"),
      vvn_tile("Most common type", as.character(top_type$capacity_type[1]),
               sprintf("%s facilities", scales::comma(top_type$n[1])))
    )
  })

  output$pipe_stage_type <- renderPlotly({
    d <- pipe_sel() %>%
      filter(!is.na(stage)) %>%
      count(capacity_type, stage, name = "n")
    validate(need(nrow(d) > 0, "No facilities match this selection."))
    gg <- ggplot(d, aes(x = n, y = capacity_type, fill = stage,
                        text = paste0(capacity_type, "<br>", stage, ": ",
                                      scales::comma(n), " facilities"))) +
      # A 2px surface gap between stacked segments keeps the boundaries legible.
      geom_col(width = 0.6, colour = "#ffffff", linewidth = 0.8) +
      scale_fill_manual(values = STAGE_COLOURS, name = NULL) +
      scale_x_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, 0.06))) +
      labs(x = "Facilities", y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })

  output$pipe_locality <- renderPlotly({
    d <- pipe_sel() %>% filter(!is.na(stage))
    validate(need(nrow(d) > 0, "No facilities match this selection."))
    ord <- d %>% count(NAME, sort = TRUE) %>% arrange(n) %>% pull(NAME)
    d <- d %>%
      count(NAME, stage, name = "n") %>%
      mutate(NAME = factor(NAME, levels = ord))
    gg <- ggplot(d, aes(x = n, y = NAME, fill = stage,
                        text = paste0(NAME, "<br>", stage, ": ",
                                      scales::comma(n), " facilities"))) +
      geom_col(width = 0.68, colour = "#ffffff", linewidth = 0.7) +
      scale_fill_manual(values = STAGE_COLOURS, name = NULL) +
      scale_x_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, 0.06))) +
      labs(x = "Facilities", y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })

  # ---- tab 4: concentration ----------------------------------------------

  conc_data <- reactive({
    validate(need(length(input$conc_stages) > 0, "Select at least one stage."))
    d <- facilities %>% filter(stage %in% input$conc_stages)
    validate(need(nrow(d) > 0, "No facility is at the selected stage."))
    d %>%
      count(NAME, name = "n") %>%
      arrange(desc(n)) %>%
      mutate(rank = row_number(),
             share = 100 * n / sum(n),
             cumulative = cumsum(share))
  })

  output$conc_title <- renderText({
    d <- conc_data()
    sprintf("Running share of Virginia data centres — %d localities, %s facilities",
            nrow(d), scales::comma(sum(d$n)))
  })

  output$conc_kpi <- renderUI({
    d <- conc_data()
    top1 <- d %>% slice(1)
    top5 <- sum(utils::head(d$share, 5))
    # How many localities it takes to reach half of the state total.
    half <- which(d$cumulative >= 50)[1]
    vvn_kpi_row(
      vvn_tile("Localities hosting", nrow(d), "of Virginia's 133", accent = TRUE),
      vvn_tile("Largest locality", as.character(top1$NAME[1]),
               sprintf("%s facilities, %.0f%% of the total",
                       scales::comma(top1$n[1]), top1$share[1])),
      vvn_tile("Top five localities", sprintf("%.0f%%", top5), "of all facilities"),
      vvn_tile("Localities to reach half",
               if (is.na(half)) "—" else as.character(half),
               "carry 50% of the total")
    )
  })

  output$conc_curve <- renderPlotly({
    d <- conc_data()
    validate(need(nrow(d) > 1, paste(
      "Only", nrow(d), "locality has facilities at this stage, so there is no",
      "distribution to draw. Add a stage to compare across localities.")))
    # Even-spread reference: what the curve would look like if every hosting
    # locality held the same number of facilities.
    even <- data.frame(rank = c(1, nrow(d)), cumulative = c(100 / nrow(d), 100))
    gg <- ggplot() +
      geom_line(data = even, aes(x = rank, y = cumulative),
                linetype = "dashed", linewidth = 0.4, colour = "#aeb4c0") +
      geom_line(data = d, aes(x = rank, y = cumulative),
                colour = DC_RAMP[5], linewidth = 0.8) +
      geom_point(data = d, aes(x = rank, y = cumulative,
                               text = paste0("Top ", rank, " localities<br>",
                                             sprintf("%.0f%% of all facilities", cumulative),
                                             "<br>through ", NAME)),
                 colour = DC_RAMP[5], size = 1.9) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(x)))) +
      labs(x = "Localities, ranked from most to fewest facilities",
           y = "Running share of the state total") +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })

  output$conc_table <- DT::renderDT({
    conc_data() %>%
      transmute(
        Rank = rank,
        Locality = NAME,
        Facilities = n,
        `Share of state` = sprintf("%.1f%%", share),
        `Running share` = sprintf("%.1f%%", cumulative)
      ) %>%
      DT::datatable(rownames = FALSE,
                    options = list(pageLength = 10, dom = "ftip"),
                    class = "compact stripe hover")
  })

  output$conc_csv <- downloadHandler(
    filename = function() {
      sprintf("va-data-centre-concentration-%s.csv", Sys.Date())
    },
    content = function(file) {
      utils::write.csv(
        conc_data() %>%
          transmute(rank, locality = NAME, facilities = n,
                    share_of_state_pct = round(share, 2),
                    running_share_pct = round(cumulative, 2)),
        file, row.names = FALSE
      )
    }
  )

  output$conc_share <- renderPlotly({
    d <- conc_data() %>%
      arrange(share) %>%
      mutate(NAME = factor(NAME, levels = unique(NAME)))
    gg <- ggplot(d, aes(x = share, y = NAME,
                        text = paste0(NAME, "<br>", scales::comma(n), " facilities<br>",
                                      sprintf("%.1f%% of the state total", share)))) +
      geom_col(width = 0.68, fill = DC_RAMP[4]) +
      scale_x_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Share of all Virginia data centres", y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })
}

shinyApp(ui = ui, server = server)
