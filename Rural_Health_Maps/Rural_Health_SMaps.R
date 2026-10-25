# ============================================================================
# Virginia Rural Health Access
# ============================================================================
# The Virginia slice of the DSPG rural health project. Upstream is a national
# dashboard with a dozen tabs covering model diagnostics and a literature
# review; this keeps the two things a Virginia reader needs -- how far people
# have to drive for care, and what care infrastructure each locality has.
#
# Virginia-only data is extracted by prepare_data.R; county boundaries come
# from tigris at run time, as in every other app in the suite.
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(tigris)
library(leaflet)
library(htmltools)
library(ggplot2)
library(plotly)
library(scales)
library(DT)

source("vvn_ui.R")

options(tigris_use_cache = TRUE)

# The published DSPG dashboard this one distils, matching how every other app
# in the suite links out. The source repository is VVN-VT/rural_health_DSPG;
# see prepare_data.R, which extracts the Virginia slice from it.
DATA_DIR   <- "Rural_Health_Data"

# ===== DATA ================================================================

stopifnot(dir.exists(DATA_DIR))

drive_times <- readRDS(file.path(DATA_DIR, "va_drive_times.rds"))
infra       <- readRDS(file.path(DATA_DIR, "va_infrastructure.rds"))
labels_raw  <- readRDS(file.path(DATA_DIR, "indicator_labels.rds"))

# Site types, in the order a reader is likely to want them: emergency care
# first, routine care last.
SITE_TYPES <- c(
  "Acute care hospital"        = "acute_care_hospital",
  "Urgent care clinic"         = "clinic_urgent_care",
  "Doctors & specialists"      = "doctors_medical_specialists",
  "Mental health provider"     = "mental_health",
  "Pharmacy"                   = "pharmacy",
  "Dentist"                    = "dentist"
)
SITE_TYPES <- SITE_TYPES[SITE_TYPES %in% unique(drive_times$site_type)]

DRIVE_YEARS <- sort(unique(drive_times$year))

# Infrastructure indicators, restricted to those with usable Virginia coverage.
# The upstream file carries 30 columns, many of which are empty for most
# Virginia localities; offering those in a dropdown produces a blank map.
INFRA_CANDIDATES <- setdiff(names(infra), c("year", "GEOID", "COUNTY"))
INFRA_INDICATORS <- local({
  keep <- INFRA_CANDIDATES[vapply(INFRA_CANDIDATES, function(v) {
    mean(!is.na(infra[[v]])) >= 0.9
  }, logical(1))]
  lbl <- labels_raw$Variable.Label[match(keep, labels_raw$Variable.Name)]
  # Trim the long AHRF/POS phrasing to something that fits a select input.
  lbl <- ifelse(is.na(lbl), keep, lbl)
  lbl <- sub("^Total number of ", "", lbl)
  lbl <- sub("^Percentage of ", "% ", lbl)
  lbl <- sub("^Mean distance in miles to the nearest ", "Miles to nearest ", lbl)
  lbl <- sub(" per 1,000 population$", " (per 1,000)", lbl)
  lbl <- sub(",.*$", "", lbl)
  lbl <- paste0(toupper(substr(lbl, 1, 1)), substr(lbl, 2, nchar(lbl)))
  stats::setNames(keep, lbl)[order(lbl)]
})

INFRA_YEARS <- sort(unique(infra$year))

# Distance indicators read the other way round from supply indicators: more is
# worse. Used to flip the colour ramp so dark always means "harder to access".
INFRA_HIGHER_IS_WORSE <- grepl("DIST|NO_VEH", INFRA_INDICATORS)

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

# Colour domains fixed across all years, per measure. Rebuilding the scale from
# a single year re-normalises it and hides change over time -- the same defect
# that had to be fixed in the childcare and solar dashboards.
DRIVE_DOMAIN <- range(drive_times$avg_minutes, na.rm = TRUE)

# The two sources cover different spans (drive times 2019-2023, infrastructure
# 2009-2023). The bubble chart joins them, so it is limited to the overlap
# rather than showing a year where one side would be entirely blank.
SHARED_YEARS <- sort(intersect(DRIVE_YEARS, INFRA_YEARS))

# Which infrastructure indicators support a "compared with the state" view.
#
# A percentage difference needs a positive median: several of these indicators
# are zero for most of Virginia -- Rural Health Clinics and hospices are absent
# from more than half of localities -- so the median is 0, the ratio is
# undefined, and the map comes out one-sided (54 localities above, none below).
# Three hospital-bed and hospital-count rates have too few finite values in a
# single year to scale at all. Those indicators keep the plain map and lose the
# comparison, rather than offering a control that quietly produces nonsense.
INFRA_COMPARABLE <- local({
  vapply(unname(INFRA_INDICATORS), function(v) {
    ok <- vapply(INFRA_YEARS, function(y) {
      x <- suppressWarnings(as.numeric(infra[[v]][infra$year == y]))
      f <- x[is.finite(x)]
      length(f) >= 3 && stats::median(f) > 0 && any(f < stats::median(f))
    }, logical(1))
    any(ok)
  }, logical(1))
})
INFRA_COMPARABLE <- unname(INFRA_INDICATORS)[INFRA_COMPARABLE]

# Which years each indicator actually carries. The sources do not line up: the
# three AHRF hospital rates stop in 2022, while the nurse-practitioner,
# physician-assistant, dentist and trauma-distance measures do not start until
# 2010. Offering every year for every indicator meant a reader could pick a
# combination that looks valid and get a blank map -- and because the year
# control defaults to the most recent year, choosing any of the three hospital
# rates did exactly that on the first click.
INFRA_YEARS_BY_IND <- local({
  out <- lapply(unname(INFRA_INDICATORS), function(v) {
    keep <- vapply(INFRA_YEARS, function(y) {
      x <- suppressWarnings(as.numeric(infra[[v]][infra$year == y]))
      sum(is.finite(x)) >= 3
    }, logical(1))
    INFRA_YEARS[keep]
  })
  names(out) <- unname(INFRA_INDICATORS)
  out
})

# Warm travel-burden colors and a separate teal access-capacity scale make the
# two rural-health maps read as related views with different measures.
# Travel burden on the pink ramp, care capacity on the blue: the two measures
# stay visibly different in kind while sharing one lightness ladder.
RURAL_DRIVE_PALETTE <- vvn_ramp("rust")
RURAL_INFRA_PALETTE <- vvn_ramp("green")

INFRA_DOMAINS <- local({
  out <- lapply(INFRA_INDICATORS, function(v) {
    x <- infra[[v]]; x <- x[is.finite(x)]
    if (length(x) < 5) return(NULL)
    unname(stats::quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE))
  })
  names(out) <- INFRA_INDICATORS
  out
})

# ===== UI ==================================================================

ui <- page_fluid(
  title = "Virginia Rural Health Access",
  theme = vvn_theme(),
  tags$head(vvn_css()),

  vvn_header(
    "Virginia Rural Health Access",
    "How far Virginians drive to reach care, and what health infrastructure each locality has."
  ),

  navset_tab(
    # ---------------------------------------------------------------- tab 1
    nav_panel(
      "Drive time to care",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          vvn_group("Type of care"),
          selectInput("site_type", NULL, choices = SITE_TYPES,
                      selected = unname(SITE_TYPES)[1]),
          vvn_mode_control("drive_mode", "Drive time"),
          vvn_group("Year"),
          selectInput("drive_year", NULL, choices = rev(DRIVE_YEARS),
                      selected = max(DRIVE_YEARS)),
          vvn_help(
            "Average minutes by road from the census tracts in a locality to the",
            "nearest site of the selected type. Darker means further to travel."
          ),
          vvn_help(
            "The colour scale is fixed across all years, so a locality whose",
            "access worsens genuinely darkens rather than merely changing rank."
          )
        ),

        uiOutput("drive_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("drive_title", inline = TRUE)),
          card_body(padding = 0, leafletOutput("drive_map", height = "560px"))
        ),

        vvn_notes(
          "What this map shows",
          p(
            "Driving time is the measure that matters for rural health access: a county can sit beside a hospital",
            "on a map and still be an hour from it by road. Each locality is shaded by the average drive time from",
            "its census tracts to the nearest facility of the selected type, so the mountains and the Eastern Shore",
            "show up as the constraints they are."
          ),
          p(
            "Compare care types to see which services are genuinely distributed and which cluster in the metros.",
            strong("Pharmacies"), "are close to everyone;", strong("acute care hospitals"), "and",
            strong("mental health providers"), "are not, and the gap between them is the story."
          ),
          p(
            class = "vvn-help",
            "Drive times computed from census tract centroids to health site locations, 2019–2023.",
            "Source: DSPG rural health project."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 2
    nav_panel(
      "Health infrastructure",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          vvn_group("Indicator"),
          selectInput("indicator", NULL, choices = INFRA_INDICATORS,
                      selected = unname(INFRA_INDICATORS)[1]),
          uiOutput("infra_mode_ui"),
          vvn_group("Year"),
          selectInput("infra_year", NULL, choices = rev(INFRA_YEARS),
                      selected = max(INFRA_YEARS)),
          vvn_help(
            "Facility and workforce counts are per 1,000 residents, so a small",
            "rural locality is comparable with a metro one."
          ),
          vvn_help(
            "Darker always means harder to access: the ramp is reversed for",
            "distance measures, where a higher number is worse."
          )
        ),

        uiOutput("infra_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("infra_title", inline = TRUE)),
          card_body(padding = 0, leafletOutput("infra_map", height = "560px"))
        ),

        vvn_notes(
          "What this map shows",
          p(
            "The supply side of rural health: how much care capacity each locality actually contains.",
            "Federally Qualified Health Centres and Rural Health Clinics exist specifically to serve underserved",
            "areas, so their distribution is a direct read on where the health system has had to intervene."
          ),
          p(
            "Read this against the drive-time tab. A locality with few facilities",
            strong("and"), "a long drive to the nearest one is doubly constrained — and those are",
            "rarely the same places that show up in statewide averages."
          ),
          p(
            class = "vvn-help",
            "Indicators drawn from AHRF, CMS Provider of Services and ACS, 2009–2023.",
            "Only indicators covering at least 90% of Virginia localities are offered."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 3
    nav_panel(
      "Drive time over time",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          vvn_group("Type of care"),
          checkboxGroupInput("trend_sites", NULL, choices = SITE_TYPES,
                             selected = unname(SITE_TYPES)),
          vvn_group("Localities to trace"),
          selectizeInput("trend_locs", NULL, choices = NULL, multiple = TRUE,
                         options = list(maxItems = 6,
                                        placeholder = "Statewide median only")),
          vvn_help(
            "How the drive to care has changed between",
            sprintf("%d and %d.", min(DRIVE_YEARS), max(DRIVE_YEARS))
          ),
          vvn_help(
            "The statewide median is drawn for every selected care type. Adding",
            "localities traces them against it."
          )
        ),
        uiOutput("trend_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("trend_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("trend_lines", height = "420px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("How far the typical locality drives, by type of care"),
            card_body(padding = 0, plotlyOutput("trend_spread", height = "400px"))
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
                downloadLink("trend_csv", "Download CSV", class = "vvn-dl")
              )
            ),
            card_body(DT::DTOutput("trend_table"))
          )
        ),
        vvn_notes(
          "Reading these charts",
          p(
            "The map answers", em("where"), "the drive is long; this answers", em("whether it is getting longer"), ".",
            "Drive time changes when facilities open or close, so a rising line is usually a closure and a falling",
            "one a new site within reach."
          ),
          p(
            "The lower chart shows the", strong("spread"), "across localities for each care type, not just the",
            "middle. The box is the middle half of localities and the line inside it the median, so a wide box",
            "means the burden is unevenly shared even within one category of care."
          ),
          p(
            strong("Most of Virginia is close to care; the tail is what matters."), "Across all care types 85% of",
            "localities are within 50 minutes and the median is 19, but mental-health provision reaches 142",
            "minutes at the far end. The axis is gridded every 15 minutes so the range most localities occupy is",
            "readable, and the outliers are plotted rather than trimmed — a two-hour drive to a mental-health",
            "provider is the finding, not a nuisance."
          ),
          p(
            class = "vvn-help",
            sprintf("Drive times are modelled from census tracts, %d–%d, and averaged to the locality.",
                    min(DRIVE_YEARS), max(DRIVE_YEARS)),
            "Medians rather than means, so one very remote locality cannot carry a line."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 4
    nav_panel(
      "Access vs capacity",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          vvn_group("Type of care"),
          selectInput("bub_site", NULL, choices = SITE_TYPES,
                      selected = unname(SITE_TYPES)[1]),
          vvn_group("Capacity indicator"),
          selectInput("bub_indicator", NULL, choices = INFRA_INDICATORS,
                      selected = unname(INFRA_INDICATORS)[1]),
          vvn_group("Year"),
          # Animated slider, in the manner of an Our World in Data or Gapminder
          # chart: press play and the whole scatter moves through the years.
          sliderInput("bub_year", NULL,
                      min = min(SHARED_YEARS), max = max(SHARED_YEARS),
                      value = max(SHARED_YEARS), step = 1, sep = "",
                      ticks = FALSE,
                      animate = animationOptions(interval = 1100, loop = TRUE)),
          vvn_help(
            "Every locality is one bubble: drive time against care capacity,",
            "sized by the number of census tracts behind the estimate."
          ),
          vvn_help(
            "Press play on the year slider to watch the whole state move."
          )
        ),
        uiOutput("bub_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("bub_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("bub_scatter", height = "520px"))
        ),
        vvn_notes(
          "Reading this chart",
          p(
            "The two rural-health maps each show one side of the problem. This puts them on the same axes:",
            strong("how far people drive"), "against", strong("how much care the locality contains"), ". The",
            "bottom-right is the comfortable corner — short drives, ample capacity. The top-left is the",
            "constrained one."
          ),
          p(
            "A locality can be badly served in two different ways, and they call for different responses. Long",
            "drives with reasonable local capacity is a", em("distribution"), "problem; short drives with thin capacity",
            "is a", em("supply"), "problem. Separating them is the point of plotting one against the other."
          ),
          p(
            strong("Bubble size is the number of census tracts"), "evaluated for that locality, which is roughly a",
            "proxy for how populated it is. It is not a third measure of health — it indicates how much evidence",
            "sits behind each point, so small bubbles are the least certain."
          ),
          p(
            class = "vvn-help",
            sprintf("Years restricted to %d–%d, where drive-time and infrastructure data overlap.",
                    min(SHARED_YEARS), max(SHARED_YEARS)),
            "Localities missing either measure in the selected year are not plotted."
          )
        )
      )
    )
  )
)

# ===== SERVER ==============================================================

server <- function(input, output, session) {

  # ---- drive times -------------------------------------------------------

  drive_slice <- reactive({
    drive_times %>%
      filter(site_type == req(input$site_type),
             year == as.integer(req(input$drive_year)))
  })

  output$drive_title <- renderText({
    nm <- names(SITE_TYPES)[SITE_TYPES == req(input$site_type)]
    paste0("Average drive time to the nearest ", tolower(nm), " — ", input$drive_year)
  })

  output$drive_kpi <- renderUI({
    d <- drive_slice()
    if (nrow(d) == 0) return(NULL)
    mins <- function(x) paste0(round(x), " min")
    vvn_kpi_row(
      vvn_tile("Statewide median", mins(stats::median(d$avg_minutes)),
               sprintf("%d localities", nrow(d)), accent = TRUE),
      vvn_tile("Furthest locality", mins(max(d$avg_minutes)),
               d$NAME[which.max(d$avg_minutes)]),
      vvn_tile("Closest locality", mins(min(d$avg_minutes)),
               d$NAME[which.min(d$avg_minutes)]),
      vvn_tile("Over 30 minutes",
               paste0(sum(d$avg_minutes > 30), " of ", nrow(d)),
               "localities averaging a half-hour or more")
    )
  })

  output$drive_map <- renderLeaflet({
    shapes <- load_boundaries()
    validate(need(!is.null(shapes),
                  "County boundaries could not be downloaded from the Census TIGER service."))

    dat <- shapes %>% left_join(drive_slice(), by = "GEOID")
    values <- dat$avg_minutes
    validate(need(any(!is.na(values)), "No drive-time data for this selection."))

    shading <- vvn_map_shading(
      values, input$drive_mode %||% "level",
      seq_palette = RURAL_DRIVE_PALETTE,
      level_title = "Drive time<br>(minutes)",
      level_fmt   = labelFormat(suffix = " min")
    )
    shade <- shading$values
    pal <- shading$pal

    lab <- sprintf(
      "<strong>%s</strong><br>Average: <strong>%s min</strong>%s<br>Range: %s–%s min<br><span style='color:#5c6472'>%s tracts evaluated</span>",
      dat$locality,
      ifelse(is.na(values), "&mdash;", round(values)),
      vvn_deviation_text(shading$deviation, shading$compare),
      ifelse(is.na(dat$min_minutes), "&mdash;", round(dat$min_minutes)),
      ifelse(is.na(dat$max_minutes), "&mdash;", round(dat$max_minutes)),
      ifelse(is.na(dat$tracts_evaluated), "&mdash;", dat$tracts_evaluated)
    )

    leaflet(dat) %>%
      vvn_basemap() %>%
      addPolygons(
        fillColor   = ifelse(is.na(shade), VVN_NO_DATA, pal(shade)),
        fillOpacity = 0.92,
        color   = VVN_STROKE_COLOUR,
        weight  = VVN_STROKE_WEIGHT,
        opacity = VVN_STROKE_OPACITY,
        label        = lapply(lab, HTML),
        labelOptions = vvn_labelopts(),
        highlightOptions = vvn_highlight()
      ) %>%
      vvn_outline() %>%
      addLegend(
        position = "bottomright", pal = pal,
        values = shade[is.finite(shade)],
        title = shading$title, labFormat = shading$fmt,
        opacity = 1, na.label = "No data"
      )
  })

  # ---- infrastructure ----------------------------------------------------

  infra_slice <- reactive({
    v <- req(input$indicator)
    infra %>%
      filter(year == as.integer(req(input$infra_year))) %>%
      select(GEOID, value = all_of(v))
  })

  output$infra_title <- renderText({
    nm <- names(INFRA_INDICATORS)[INFRA_INDICATORS == req(input$indicator)]
    paste0(nm, " — ", input$infra_year)
  })

  output$infra_kpi <- renderUI({
    d <- infra_slice() %>% filter(!is.na(value))
    if (nrow(d) == 0) return(NULL)
    shapes <- load_boundaries()
    nm <- if (is.null(shapes)) NULL else shapes$locality[match(d$GEOID, shapes$GEOID)]
    fmt <- function(x) formatC(x, format = "f", digits = 2)

    vvn_kpi_row(
      vvn_tile("Statewide median", fmt(stats::median(d$value)),
               sprintf("%d localities reporting", nrow(d)), accent = TRUE),
      vvn_tile("Highest", fmt(max(d$value)),
               if (is.null(nm)) "" else nm[which.max(d$value)]),
      vvn_tile("Lowest", fmt(min(d$value)),
               if (is.null(nm)) "" else nm[which.min(d$value)]),
      vvn_tile("Reporting zero", sum(d$value == 0),
               "localities with none at all")
    )
  })

  # Shown only for indicators where the comparison is defined; see
  # INFRA_COMPARABLE. For the rest a short line says why it is absent, which is
  # more useful than a disabled control with no explanation.
  # Year choices follow the indicator, so an empty combination cannot be
  # selected. The current year is preserved when the new indicator also has it.
  observeEvent(input$indicator, {
    yrs <- INFRA_YEARS_BY_IND[[input$indicator]]
    req(length(yrs) > 0)
    keep <- if (!is.null(input$infra_year) && as.integer(input$infra_year) %in% yrs) {
      as.integer(input$infra_year)
    } else max(yrs)
    updateSelectInput(session, "infra_year",
                      choices = rev(yrs), selected = keep)
  }, ignoreInit = FALSE)

  output$infra_mode_ui <- renderUI({
    v <- req(input$indicator)
    if (v %in% INFRA_COMPARABLE) return(vvn_mode_control("infra_mode", "Indicator value"))
    tagList(
      vvn_group("Shading"),
      vvn_help(
        "This indicator is zero for more than half of Virginia, so there is no",
        "meaningful state median to compare against \u2014 the map shows the",
        "measured value only."
      )
    )
  })

  output$infra_map <- renderLeaflet({
    shapes <- load_boundaries()
    validate(need(!is.null(shapes),
                  "County boundaries could not be downloaded from the Census TIGER service."))

    v <- req(input$indicator)
    dat <- shapes %>% left_join(infra_slice(), by = "GEOID")
    values <- dat$value
    validate(need(any(!is.na(values)), "No data for this indicator and year."))

    breaks <- INFRA_DOMAINS[[v]]
    if (is.null(breaks)) breaks <- unname(stats::quantile(values, seq(0, 1, 0.2), na.rm = TRUE))
    breaks <- unique(breaks)

    # Dark always means harder to access, so supply measures run high-to-pale
    # while distance measures run low-to-pale.
    ramp <- if (isTRUE(INFRA_HIGHER_IS_WORSE[[which(INFRA_INDICATORS == v)]])) {
      RURAL_INFRA_PALETTE
    } else {
      rev(RURAL_INFRA_PALETTE)
    }

    nm <- names(INFRA_INDICATORS)[INFRA_INDICATORS == v]

    mode <- if (v %in% INFRA_COMPARABLE) (input$infra_mode %||% "level") else "level"
    shading <- vvn_map_shading(
      values, mode,
      seq_palette = ramp, level_title = gsub(" ", "<br>", nm)
    )
    shade <- shading$values
    pal <- shading$pal

    lab <- sprintf(
      "<strong>%s</strong><br>%s: <strong>%s</strong>%s",
      dat$locality, nm,
      ifelse(is.na(values), "no data", formatC(values, format = "f", digits = 2)),
      vvn_deviation_text(shading$deviation, shading$compare)
    )

    leaflet(dat) %>%
      vvn_basemap() %>%
      addPolygons(
        fillColor   = ifelse(is.na(shade), VVN_NO_DATA, pal(shade)),
        fillOpacity = 0.92,
        color   = VVN_STROKE_COLOUR,
        weight  = VVN_STROKE_WEIGHT,
        opacity = VVN_STROKE_OPACITY,
        label        = lapply(lab, HTML),
        labelOptions = vvn_labelopts(),
        highlightOptions = vvn_highlight()
      ) %>%
      vvn_outline() %>%
      addLegend(
        position = "bottomright", pal = pal,
        values = shade[is.finite(shade)],
        title = shading$title, labFormat = shading$fmt,
        opacity = 1, na.label = "No data"
      )
  })

  # ===== TAB 3: DRIVE TIME OVER TIME =======================================

  site_label <- function(v) {
    nm <- names(SITE_TYPES)[match(v, SITE_TYPES)]
    ifelse(is.na(nm), v, nm)
  }

  observe({
    nm <- drive_times %>%
      select(GEOID, NAME) %>%
      distinct() %>%
      arrange(NAME)
    updateSelectizeInput(session, "trend_locs",
                         choices = stats::setNames(nm$GEOID, nm$NAME),
                         server = TRUE)
  })

  trend_base <- reactive({
    sites <- req(input$trend_sites)
    d <- drive_times %>%
      filter(site_type %in% sites, !is.na(avg_minutes))
    validate(need(nrow(d) > 0, "Select at least one type of care."))
    d %>% mutate(Care = factor(site_label(site_type),
                               levels = names(SITE_TYPES)))
  })

  output$trend_title <- renderText({
    sprintf("Median drive time to care, %d–%d",
            min(DRIVE_YEARS), max(DRIVE_YEARS))
  })

  output$trend_kpi <- renderUI({
    d <- trend_base()
    med <- d %>% group_by(year) %>%
      summarise(m = stats::median(avg_minutes), .groups = "drop") %>% arrange(year)
    first <- med$m[1]; last <- med$m[nrow(med)]
    worst <- d %>% filter(year == max(year)) %>%
      group_by(Care) %>%
      summarise(m = stats::median(avg_minutes), .groups = "drop") %>%
      slice_max(m, n = 1)
    vvn_kpi_row(
      vvn_tile("Median drive", sprintf("%.0f min", last),
               sprintf("across selected care, %d", max(med$year)), accent = TRUE),
      vvn_tile("Change since start",
               sprintf("%+.1f min", last - first),
               sprintf("from %.0f min in %d", first, med$year[1])),
      vvn_tile("Longest drive", as.character(worst$Care[1]),
               sprintf("%.0f min median", worst$m[1])),
      vvn_tile("Localities", dplyr::n_distinct(d$GEOID), "with modelled drive times")
    )
  })

  output$trend_lines <- renderPlotly({
    d <- trend_base()
    med <- d %>%
      group_by(Care, year) %>%
      summarise(minutes = stats::median(avg_minutes), n = dplyr::n(), .groups = "drop")

    # Colour follows the care type, in the fixed order the sidebar lists them,
    # so adding or removing one never repaints the rest.
    lvl <- levels(droplevels(med$Care))
    # Six types of care: past the water-garden trio, so the extended set.
    pal <- vvn_series_colours(lvl)

    gg <- ggplot(med, aes(x = year, y = minutes, colour = Care, group = Care,
                          text = paste0(Care, "<br>", year, "<br>",
                                        sprintf("%.0f min median", minutes),
                                        "<br>", n, " localities"))) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 1.8)

    sel <- input$trend_locs
    if (length(sel)) {
      picks <- d %>%
        filter(GEOID %in% sel) %>%
        group_by(NAME, Care, year) %>%
        summarise(minutes = mean(avg_minutes), .groups = "drop")
      gg <- gg +
        geom_line(data = picks,
                  aes(x = year, y = minutes, colour = Care, group = interaction(NAME, Care),
                      text = paste0(NAME, " — ", Care, "<br>", year, "<br>",
                                    sprintf("%.0f min", minutes))),
                  linewidth = 0.45, linetype = "dotted")
    }

    gg <- gg +
      scale_colour_manual(values = pal, name = NULL, drop = FALSE) +
      scale_x_continuous(breaks = DRIVE_YEARS) +
      scale_y_continuous(labels = function(x) paste0(x, " min")) +
      labs(x = NULL, y = "Drive time (minutes)") +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })

  output$trend_spread <- renderPlotly({
    d <- trend_base() %>% filter(year == max(year))
    ord <- d %>% group_by(Care) %>%
      summarise(m = stats::median(avg_minutes), .groups = "drop") %>%
      arrange(m) %>% pull(Care) %>% as.character()
    d <- d %>% mutate(Care = factor(as.character(Care), levels = ord))
    # 85% of localities sit under 50 minutes and the median is 19, but a handful
    # of mental-health outliers reach 142, so the default axis gave three ticks
    # (0, 50, 100) and squeezed every box into the left third. A 15-minute grid
    # makes the range people actually experience readable; the outliers are kept
    # rather than clipped, because a two-hour drive to care is the finding, not
    # a nuisance.
    upper <- ceiling(max(d$avg_minutes, na.rm = TRUE) / 15) * 15
    gg <- ggplot(d, aes(x = avg_minutes, y = Care)) +
      geom_boxplot(outlier.size = 0.8, width = 0.55,
                   fill = RURAL_DRIVE_PALETTE[3], colour = "#7a4a3a",
                   linewidth = 0.4) +
      scale_x_continuous(breaks = seq(0, upper, by = 15),
                         minor_breaks = NULL,
                         labels = function(x) paste0(x, " min"),
                         expand = expansion(mult = c(0.01, 0.02))) +
      labs(x = sprintf("Drive time across localities, %d", max(trend_base()$year)),
           y = NULL) +
      vvn_ggtheme()
    ggplotly(gg) %>% vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })

  trend_table_data <- reactive({
    trend_base() %>%
      group_by(Care, year) %>%
      summarise(localities = dplyr::n(),
                median_minutes = round(stats::median(avg_minutes), 1),
                shortest = round(min(avg_minutes), 1),
                longest = round(max(avg_minutes), 1),
                .groups = "drop") %>%
      arrange(Care, year)
  })

  output$trend_table <- DT::renderDT({
    trend_table_data() %>%
      transmute(`Type of care` = Care, Year = year, Localities = localities,
                `Median minutes` = median_minutes,
                Shortest = shortest, Longest = longest) %>%
      DT::datatable(rownames = FALSE,
                    options = list(pageLength = 10, dom = "ftip"),
                    class = "compact stripe hover")
  })

  output$trend_csv <- downloadHandler(
    filename = function() sprintf("va-drive-time-trend-%s.csv", Sys.Date()),
    content = function(file) utils::write.csv(trend_table_data(), file, row.names = FALSE)
  )

  # ===== TAB 4: ACCESS VS CAPACITY =========================================

  bub_data <- reactive({
    yr <- req(input$bub_year)
    site <- req(input$bub_site)
    ind <- req(input$bub_indicator)

    dv <- drive_times %>%
      filter(year == yr, site_type == site, !is.na(avg_minutes)) %>%
      select(GEOID, NAME, avg_minutes, tracts_evaluated)
    inf <- infra %>%
      filter(year == yr) %>%
      transmute(GEOID, capacity = .data[[ind]]) %>%
      filter(is.finite(capacity))

    out <- inner_join(dv, inf, by = "GEOID")
    validate(need(nrow(out) > 0,
                  "No locality has both measures for this combination."))
    out
  })

  output$bub_title <- renderText({
    sprintf("%s — drive time against %s, %d",
            site_label(req(input$bub_site)),
            tolower(names(INFRA_INDICATORS)[match(req(input$bub_indicator), INFRA_INDICATORS)]),
            req(input$bub_year))
  })

  output$bub_kpi <- renderUI({
    d <- bub_data()
    far <- slice_max(d, avg_minutes, n = 1)
    # Correlation says whether long drives and thin capacity coincide at all.
    r <- suppressWarnings(stats::cor(d$avg_minutes, d$capacity,
                                     use = "complete.obs", method = "spearman"))
    vvn_kpi_row(
      vvn_tile("Localities plotted", nrow(d), "with both measures", accent = TRUE),
      vvn_tile("Median drive", sprintf("%.0f min", stats::median(d$avg_minutes)),
               sprintf("in %d", req(input$bub_year))),
      vvn_tile("Longest drive", as.character(far$NAME[1]),
               sprintf("%.0f minutes", far$avg_minutes[1])),
      vvn_tile("Rank correlation",
               if (is.na(r)) "—" else sprintf("%+.2f", r),
               "drive time vs capacity")
    )
  })

  output$bub_scatter <- renderPlotly({
    d <- bub_data()
    ind_lab <- names(INFRA_INDICATORS)[match(req(input$bub_indicator), INFRA_INDICATORS)]
    gg <- ggplot(d, aes(x = capacity, y = avg_minutes, size = tracts_evaluated,
                        text = paste0(NAME, "<br>",
                                      sprintf("%.0f min to %s", avg_minutes,
                                              tolower(site_label(input$bub_site))), "<br>",
                                      ind_lab, ": ", signif(capacity, 3), "<br>",
                                      tracts_evaluated, " tracts evaluated"))) +
      geom_point(colour = RURAL_INFRA_PALETTE[5], alpha = 0.62,
                 # A surface-coloured ring keeps overlapping bubbles separable.
                 stroke = 0.6) +
      scale_size_area(max_size = 9, guide = "none") +
      scale_y_continuous(labels = function(x) paste0(x, " min")) +
      labs(x = ind_lab, y = "Average drive time") +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })
}

shinyApp(ui = ui, server = server)
