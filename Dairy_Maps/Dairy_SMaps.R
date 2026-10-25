# ============================================================================
# Virginia Dairy Processing Industry
# ============================================================================
# All remote data (tigris boundaries, GitHub spreadsheets, USDA NASS, Open-Meteo)
# is fetched lazily inside reactives and cached for the life of the process.
# Nothing blocks app start-up, and any single source failing degrades to an
# in-app message instead of a start-up error.
# ============================================================================

library(shiny)
library(bslib)
library(tigris)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(lubridate)
library(scales)
library(rnassqs)
library(stringr)
library(httr)
library(jsonlite)
library(tidyr)

source("vvn_ui.R")

options(tigris_use_cache = TRUE)

# USDA NASS Quick Stats key. Request one (free) at
# https://quickstats.nass.usda.gov/api and put it in ~/.Renviron as:
#   NASS_API_KEY=your-key-here
#
# On shinyapps.io the key instead travels in an .Renviron shipped inside the
# bundle (see tools/set-nass-key.R). R only reads .Renviron automatically at
# STARTUP, from the home directory and the directory R was launched in -- and
# the server launches R before switching to the app directory, so a bundled
# .Renviron is never picked up on its own. Read it explicitly here. Harmless
# locally, where the file may not exist and the key comes from ~/.Renviron.
if (file.exists(".Renviron")) readRenviron(".Renviron")

NASS_KEY <- Sys.getenv("NASS_API_KEY")
HAS_NASS_KEY <- nzchar(NASS_KEY)
if (HAS_NASS_KEY) nassqs_auth(key = NASS_KEY)


MILK_URL  <- "https://github.com/yuetong233/Dairy_dspg/raw/refs/heads/main/data/VA_Milk_Production.xlsx"
ROADS_URL <- "https://github.com/yuetong233/Dairy_dspg/raw/main/tl_2023_51_prisecroads_final.zip"

# The dairy study area. Declared statically so the UI can be built without
# waiting on any network call.
TARGET_COUNTIES <- c(
  "SHENANDOAH", "ROCKINGHAM", "AUGUSTA", "WARREN",
  "PAGE", "FREDERICK", "CLARKE", "ROCKBRIDGE",
  "PITTSYLVANIA", "FRANKLIN"
)

# Composite sustainability score weights; surfaced in the sidebar copy too.
W_EFFICIENCY <- 0.4
W_INVENTORY  <- 0.3
W_ACCESS     <- 0.3

# Temperature-Humidity Index threshold above which dairy cattle show heat stress.
THI_THRESHOLD <- 68

# ===== CACHED LOADERS ======================================================
# memo_once() runs fn the first time it is asked and then returns the stored
# value, so every session in the process shares one fetch.

memo_once <- function(fn) {
  value <- NULL
  done  <- FALSE
  function() {
    if (!done) {
      value <<- tryCatch(fn(), error = function(e) NULL)
      done  <<- TRUE
    }
    value
  }
}

# Match the published dashboard's study area?
#
# TRUE reproduces it. The target list names 10 counties, but Franklin is both a
# county and an independent city and both carry NAME "Franklin", so matching on
# the upper-cased name alone returns 11 polygons -- Franklin city is shaded
# alongside the 10 counties, and the composite score is rescaled across 11
# localities instead of 10, shifting every county's score slightly.
#
# TRUE is set deliberately so this agrees with the DSPG original (same
# expression at app.R:40). FALSE filters to counties only via LSAD.
USE_ORIGINAL_STUDY_AREA <- TRUE

#' Study-area polygons, WGS84, NAME upper-cased for joining.
load_counties <- memo_once(function() {
  base <- counties(state = "VA", cb = TRUE, class = "sf", progress_bar = FALSE)
  if (!USE_ORIGINAL_STUDY_AREA) base <- vvn_counties_only(base)
  base %>%
    st_transform(4326) %>%
    mutate(NAME = toupper(NAME)) %>%
    filter(NAME %in% TARGET_COUNTIES)
})

#' Monthly milk production by county.
load_milk <- memo_once(function() {
  tmp <- tempfile(fileext = ".xlsx")
  download.file(MILK_URL, tmp, mode = "wb", quiet = TRUE)
  read_excel(tmp) %>%
    rename(`MILK (lbs)` = POUNDS_OF_MILK, PRODUCERS = NUMBER_OF_PRODUCERS) %>%
    mutate(
      COUNTY = toupper(COUNTY),
      DATE   = parse_date_time(paste(YEAR, MONTH, "1"), orders = "Y b d"),
      YEAR   = suppressWarnings(as.numeric(YEAR))
    ) %>%
    filter(!is.na(YEAR))
})

#' Road density per county, rescaled 0-100, as an accessibility proxy.
load_accessibility <- memo_once(function() {
  cty <- load_counties()
  if (is.null(cty) || nrow(cty) == 0) return(NULL)

  tmp <- tempfile(fileext = ".zip")
  download.file(ROADS_URL, tmp, mode = "wb", quiet = TRUE)
  dir <- file.path(tempdir(), "dairy_roads")
  dir.create(dir, showWarnings = FALSE)
  unzip(tmp, exdir = dir)

  shp <- list.files(dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(shp) == 0) return(NULL)

  roads <- st_read(shp[1], quiet = TRUE)
  if (!"RTTYP" %in% names(roads)) return(NULL)

  roads <- roads %>%
    filter(RTTYP %in% c("P", "S")) %>%
    st_transform(4326)

  joined <- st_join(roads, cty["NAME"], left = FALSE)
  if (nrow(joined) == 0) return(NULL)

  road_km <- joined %>%
    mutate(len_km = as.numeric(st_length(.)) / 1000) %>%
    st_drop_geometry() %>%
    group_by(NAME) %>%
    summarise(road_km = sum(len_km, na.rm = TRUE), .groups = "drop")

  areas <- cty %>%
    mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
    st_drop_geometry() %>%
    select(NAME, area_km2)

  road_km %>%
    left_join(areas, by = "NAME") %>%
    mutate(
      road_density = road_km / area_km2,
      access_score = rescale(road_density, to = c(0, 100))
    ) %>%
    select(NAME, road_density, access_score)
})

#' Milk-cow inventory by county for the last ten years, from USDA NASS.
load_dairy_cows <- memo_once(function() {
  if (!HAS_NASS_KEY) return(NULL)
  this_year <- as.integer(format(Sys.Date(), "%Y"))

  rows <- lapply(seq(this_year - 9, this_year), function(y) {
    tryCatch(
      nassqs(list(
        sector_desc       = "ANIMALS & PRODUCTS",
        group_desc        = "LIVESTOCK",
        commodity_desc    = "CATTLE",
        class_desc        = "COWS, MILK",
        statisticcat_desc = "INVENTORY",
        unit_desc         = "HEAD",
        agg_level_desc    = "COUNTY",
        state_alpha       = "VA",
        source_desc       = "SURVEY",
        year              = as.character(y)
      )),
      error = function(e) NULL
    )
  })

  out <- bind_rows(Filter(function(x) is.data.frame(x) && nrow(x) > 0, rows))
  if (nrow(out) == 0) return(NULL)

  out %>%
    mutate(
      COUNTY = toupper(county_name),
      YEAR   = suppressWarnings(as.numeric(year)),
      COWS   = suppressWarnings(as.numeric(Value))
    ) %>%
    filter(COUNTY %in% TARGET_COUNTIES, !is.na(COWS), !is.na(YEAR)) %>%
    group_by(COUNTY, YEAR) %>%
    summarise(COWS = sum(COWS, na.rm = TRUE), .groups = "drop")
})

#' County centroids, used as the weather query point for each county.
load_centroids <- memo_once(function() {
  cty <- load_counties()
  if (is.null(cty) || nrow(cty) == 0) return(NULL)
  # Centroid of a lon/lat polygon: compute in an equal-area projection, then
  # convert back, which avoids sf's planar-geometry warning.
  # suppressWarnings: sf notes that attributes are assumed constant over each
  # geometry, which is fine here -- only the point coordinates are used.
  pts <- suppressWarnings(
    cty %>%
      st_transform(5070) %>%
      st_centroid() %>%
      st_transform(4326)
  )
  coords <- st_coordinates(pts)
  tibble(
    county = str_to_title(cty$NAME),
    lon    = coords[, 1],
    lat    = coords[, 2]
  )
})

#' Hourly temperature/humidity for one county, converted to THI.
#' Fetched on demand for the selected county only, so the data is current
#' rather than frozen at app start-up.
fetch_thi <- function(county, lat, lon) {
  res <- tryCatch(
    GET(
      "https://api.open-meteo.com/v1/forecast",
      query = list(
        latitude = lat, longitude = lon,
        hourly = "temperature_2m,relative_humidity_2m",
        timezone = "America/New_York",
        past_days = 2, forecast_days = 3,
        temperature_unit = "fahrenheit"
      ),
      timeout(20)
    ),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) != 200) return(NULL)

  payload <- tryCatch(fromJSON(content(res, as = "text", encoding = "UTF-8")),
                      error = function(e) NULL)
  hourly <- payload$hourly
  if (is.null(hourly) ||
      !all(c("time", "temperature_2m", "relative_humidity_2m") %in% names(hourly))) {
    return(NULL)
  }

  tibble(
    time     = as.POSIXct(hourly$time, format = "%Y-%m-%dT%H:%M", tz = "America/New_York"),
    temp_f   = hourly$temperature_2m,
    humidity = hourly$relative_humidity_2m,
    county   = county
  ) %>%
    filter(!is.na(temp_f), !is.na(humidity)) %>%
    # NRC Temperature-Humidity Index for dairy cattle, Fahrenheit form.
    mutate(thi = temp_f - (0.55 - 0.0055 * humidity) * (temp_f - 58))
}

# ===== UI ==================================================================

ui <- page_fluid(
  title = "Virginia Dairy Processing Industry",
  theme = vvn_theme(),
  tags$head(vvn_css()),

  vvn_header(
    "Virginia Dairy Processing Industry",
    "Siting indicators for dairy processing in the Shenandoah Valley and Southern Piedmont, with live heat-stress monitoring."
  ),

  navset_tab(
    # ---------------------------------------------------------------- tab 1
    nav_panel(
      "Sustainability score",
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          vvn_group("What the score measures"),
          vvn_help(
            "A composite index across", length(TARGET_COUNTIES), "study-area counties,",
            "combining three rescaled indicators:"
          ),
          div(
            class = "vvn-help",
            tags$ul(
              style = "padding-left:1.1rem; margin-bottom:8px;",
              tags$li(strong(paste0(W_EFFICIENCY * 100, "%")), " milk output per cow"),
              tags$li(strong(paste0(W_INVENTORY * 100, "%")), " milk-cow inventory"),
              tags$li(strong(paste0(W_ACCESS * 100, "%")), " road-network accessibility")
            )
          ),
          vvn_help(
            "Each indicator is rescaled 0-100 across the study area, so the score",
            "ranks these counties against each other, not against the state."
          )
        ),

        card(
          full_screen = TRUE,
          card_header("Composite sustainability score by county"),
          card_body(padding = 0, leafletOutput("suitability_map", height = "520px"))
        ),

        vvn_notes(
          "Reading this map",
          p(
            "Darker shading marks counties better suited to dairy processing on this index.",
            "Hover a county for its score and the three underlying indicators; counties shown in grey",
            "are missing at least one input and are left unscored rather than scored as zero."
          ),
          p(
            "Milk output and cow inventory come from the DSPG dairy production dataset and the USDA NASS county survey.",
            "Accessibility is the density of primary and secondary roads per square kilometre, a proxy for how easily",
            "milk can be moved to a processing facility."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 2
    nav_panel(
      "Heat stress (THI)",
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          vvn_group("County"),
          selectInput(
            "thi_county", NULL,
            choices  = str_to_title(sort(TARGET_COUNTIES)),
            selected = str_to_title(sort(TARGET_COUNTIES))[1]
          ),
          actionButton("refresh_thi", "Refresh forecast",
                       icon = icon("rotate"), class = "btn btn-sm"),
          vvn_help(
            "Temperature-Humidity Index above", THI_THRESHOLD,
            "indicates heat stress in dairy cattle and measurably reduces milk yield.",
            "The shaded band is the comfortable range."
          ),
          vvn_help(
            "Two days of observations plus a three-day forecast, computed hourly from",
            "Open-Meteo data at the county centroid."
          )
        ),

        card(
          full_screen = TRUE,
          card_header(textOutput("thi_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("thi_plot", height = "460px"))
        ),

        vvn_notes(
          "Reading this chart",
          p(
            "The Temperature-Humidity Index combines air temperature and relative humidity into a single measure of",
            "heat load on dairy cattle. Below", strong(THI_THRESHOLD), "cows are in their comfort zone, shown as the",
            "green band. Above it they begin to show heat stress: reduced feed intake, lower milk yield and",
            "lower conception rates."
          ),
          p(
            "Observed hours are drawn in", strong("maroon"), "and forecast hours in", strong("orange"),
            ", so the break between the two is visible. Use the county selector to check conditions across the study area,",
            "and refresh to pull the latest forecast."
          )
        )
      )
    )
  )
)

# ===== SERVER ==============================================================

server <- function(input, output, session) {

  # --- composite score ----------------------------------------------------

  composite <- reactive({
    cty <- load_counties()
    if (is.null(cty) || nrow(cty) == 0) return(NULL)

    milk   <- load_milk()
    cows   <- load_dairy_cows()
    access <- load_accessibility()

    # Milk efficiency for the most recent year present in both sources.
    efficiency <- NULL
    if (!is.null(milk) && !is.null(cows)) {
      shared_years <- intersect(milk$YEAR, cows$YEAR)
      if (length(shared_years) > 0) {
        yr <- max(shared_years)
        efficiency <- milk %>%
          filter(YEAR == yr, COUNTY %in% TARGET_COUNTIES) %>%
          inner_join(cows %>% filter(YEAR == yr), by = c("COUNTY", "YEAR")) %>%
          filter(COWS > 0, !is.na(`MILK (lbs)`)) %>%
          group_by(COUNTY) %>%
          summarise(
            # Monthly rows, so divide by ~30 to get lbs per cow per day.
            milk_efficiency = mean(`MILK (lbs)` / (COWS * 30), na.rm = TRUE),
            total_cows      = mean(COWS, na.rm = TRUE),
            .groups = "drop"
          )
      }
    }

    out <- cty %>%
      st_drop_geometry() %>%
      select(NAME)

    out <- out %>%
      left_join(
        if (is.null(access)) tibble(NAME = character(), access_score = numeric())
        else access %>% select(NAME, access_score),
        by = "NAME"
      ) %>%
      left_join(
        if (is.null(efficiency)) {
          tibble(COUNTY = character(), milk_efficiency = numeric(), total_cows = numeric())
        } else efficiency,
        by = c("NAME" = "COUNTY")
      ) %>%
      mutate(
        efficiency_score = rescale(milk_efficiency, to = c(0, 100)),
        inventory_score  = rescale(total_cows, to = c(0, 100))
      )

    # A county is scored only when every input is present; partial counties stay
    # NA so they render grey rather than appearing to rank badly.
    out %>%
      mutate(composite_index = round(
        W_EFFICIENCY * efficiency_score +
        W_INVENTORY  * inventory_score +
        W_ACCESS     * access_score, 1
      ))
  })

  map_data <- reactive({
    comp <- composite()
    cty  <- load_counties()
    if (is.null(comp) || is.null(cty)) return(NULL)
    cty %>% left_join(comp, by = "NAME")
  })

  output$suitability_map <- renderLeaflet({
    dat <- map_data()

    if (is.null(dat)) {
      return(
        leaflet() %>%
          vvn_basemap() %>%
          addControl(
            HTML(paste(
              "<strong>County boundaries unavailable.</strong><br>",
              "The Census TIGER service could not be reached."
            )),
            position = "topright"
          )
      )
    }

    pal <- colorNumeric(VVN_SEQ_RATE, domain = dat$composite_index,
                        na.color = VVN_NO_DATA)

    labels <- sprintf(
      paste0(
        "<strong>%s</strong><br>",
        "Composite score: <strong>%s</strong><br>",
        "Milk output: %s lbs/cow/day<br>",
        "Inventory: %s cows<br>",
        "Accessibility: %s"
      ),
      str_to_title(dat$NAME),
      ifelse(is.na(dat$composite_index), "not scored", dat$composite_index),
      ifelse(is.na(dat$milk_efficiency), "&mdash;", round(dat$milk_efficiency, 1)),
      ifelse(is.na(dat$total_cows), "&mdash;",
             formatC(dat$total_cows, format = "d", big.mark = ",")),
      ifelse(is.na(dat$access_score), "&mdash;", round(dat$access_score, 1))
    )

    m <- leaflet(dat) %>%
      vvn_basemap() %>%
      # The study area is 10 counties, not the state. Drawing Virginia's border
      # shows the reader where in the state those counties actually are.
      vvn_outline() %>%
      addPolygons(
        fillColor    = ~ifelse(is.na(composite_index), VVN_NO_DATA, pal(composite_index)),
        fillOpacity  = 0.82,
        color        = VVN_STROKE_COLOUR,
        weight       = VVN_STROKE_WEIGHT,
        opacity      = VVN_STROKE_OPACITY,
        label        = lapply(labels, HTML),
        labelOptions = vvn_labelopts(),
        highlightOptions = vvn_highlight()
      ) %>%
      # Fit to the study area rather than the whole state.
      fitBounds(
        lng1 = as.numeric(st_bbox(dat)$xmin), lat1 = as.numeric(st_bbox(dat)$ymin),
        lng2 = as.numeric(st_bbox(dat)$xmax), lat2 = as.numeric(st_bbox(dat)$ymax)
      )

    if (any(!is.na(dat$composite_index))) {
      m <- m %>%
        addLegend(
          pal = pal, values = dat$composite_index,
          title = "Composite<br>score", position = "bottomright",
          opacity = 1, na.label = "Not scored"
        )
    } else {
      m <- m %>%
        addControl(
          HTML(paste(
            "<strong>No counties could be scored.</strong><br>",
            "Milk, inventory or road data is unavailable."
          )),
          position = "topright"
        )
    }
    m
  })

  # --- THI ----------------------------------------------------------------

  thi_data <- eventReactive(
    list(input$thi_county, input$refresh_thi),
    {
      county <- req(input$thi_county)
      pts <- load_centroids()
      if (is.null(pts)) return(NULL)
      row <- pts %>% filter(county == !!county)
      if (nrow(row) == 0) return(NULL)
      fetch_thi(county, row$lat[1], row$lon[1])
    },
    ignoreNULL = FALSE
  )

  output$thi_title <- renderText({
    paste("Temperature-Humidity Index —", req(input$thi_county))
  })

  output$thi_plot <- renderPlotly({
    dat <- thi_data()

    if (is.null(dat) || nrow(dat) == 0) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
            paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
            annotations = list(list(
              x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
              text = paste("Weather data is unavailable for this county.<br>",
                           "Open-Meteo could not be reached — try refreshing."),
              font = list(size = 14, color = VVN_MUTED)
            ))
          ) %>%
          config(displayModeBar = FALSE)
      )
    }

    now <- Sys.time()
    dat <- dat %>%
      mutate(period = if_else(time > now, "Forecast", "Observed")) %>%
      arrange(time)

    y_lo <- min(c(dat$thi, THI_THRESHOLD), na.rm = TRUE) - 2
    y_hi <- max(c(dat$thi, THI_THRESHOLD), na.rm = TRUE) + 2

    plot_ly() %>%
      # Comfort band, drawn first so the series sit on top of it.
      add_trace(
        x = c(min(dat$time), max(dat$time), max(dat$time), min(dat$time)),
        y = c(y_lo, y_lo, THI_THRESHOLD, THI_THRESHOLD),
        type = "scatter", mode = "none", fill = "toself",
        fillcolor = "rgba(26,152,80,0.10)",
        name = paste0("Comfort zone (THI < ", THI_THRESHOLD, ")"),
        hoverinfo = "skip"
      ) %>%
      add_trace(
        data = dat %>% filter(period == "Observed"),
        x = ~time, y = ~thi, type = "scatter", mode = "lines",
        name = "Observed", line = list(color = VVN_MAROON, width = 2),
        hovertemplate = "%{x|%b %d, %-I %p}<br>THI %{y:.1f}<extra>Observed</extra>"
      ) %>%
      add_trace(
        data = dat %>% filter(period == "Forecast"),
        x = ~time, y = ~thi, type = "scatter", mode = "lines",
        name = "Forecast", line = list(color = VVN_ORANGE, width = 2, dash = "dot"),
        hovertemplate = "%{x|%b %d, %-I %p}<br>THI %{y:.1f}<extra>Forecast</extra>"
      ) %>%
      # Threshold rule.
      add_trace(
        x = c(min(dat$time), max(dat$time)),
        y = c(THI_THRESHOLD, THI_THRESHOLD),
        type = "scatter", mode = "lines", showlegend = FALSE,
        line = list(color = "rgba(26,152,80,0.65)", width = 1, dash = "dash"),
        hoverinfo = "skip"
      ) %>%
      vvn_plotly_layout(ylab = "THI") %>%
      layout(
        xaxis = list(title = NULL, tickformat = "%b %d", gridcolor = VVN_RULE),
        yaxis = list(title = "THI", range = c(y_lo, y_hi), gridcolor = VVN_RULE),
        hovermode = "x unified"
      )
  })
}

shinyApp(ui = ui, server = server)
