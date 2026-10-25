# ============================================================================
# Virginia Solar Facilities Interactive Map
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(stringr)
library(sf)
library(tigris)
library(leaflet)
library(shinyWidgets)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(htmltools)

source("vvn_ui.R")

# This dashboard's own sequential ramp. Every dashboard in the suite uses a
# different hue on the same lightness ladder, so the seven are distinguishable
# at a glance while still reading as one family -- see VVN_RAMPS.
SOLAR_RAMP     <- vvn_ramp("ochre")
SOLAR_RAMP_ALT <- vvn_ramp("green")

options(tigris_use_cache = TRUE)

TARGET_CRS <- 4326
DATA_PATH  <- "Solar_Data"

# Layer sizes. The raw water file is a national extract (55k features);
# drawing it whole would stall the browser, so it is clipped to Virginia and
# then capped by a meaningful ranking rather than by a random or alphabetical
# sample.
MAX_WATER_FEATURES <- 600
MAX_CITIES         <- 150

# County indicators offered for the choropleth.
COUNTY_INDICATORS <- c(
  "Land price per acre"   = "Price_Per_Acre",
  "Corn yield"            = "CornYield",
  "Soybean yield"         = "SoyYield",
  "Population"            = "Population",
  "Median housing age"    = "HousingAge",
  "Vacant housing units"  = "VacantUnits"
)

INDICATOR_FORMAT <- list(
  Price_Per_Acre = function(x) paste0("$", formatC(x, format = "f", digits = 0, big.mark = ",")),
  CornYield      = function(x) paste0(formatC(x, format = "f", digits = 1), " bu/acre"),
  SoyYield       = function(x) paste0(formatC(x, format = "f", digits = 1), " bu/acre"),
  Population     = function(x) formatC(x, format = "f", digits = 0, big.mark = ","),
  HousingAge     = function(x) paste0(formatC(x, format = "f", digits = 0), " years"),
  VacantUnits    = function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
)

# ===== LOADERS =============================================================
# Each loader returns its data or NULL. Note the assignment style: the previous
# version used `tryCatch({ x <- ... }, error = function(e) { x <- NULL })`,
# where the handler's assignment is local to the handler, so on failure `x` was
# never defined at all and the next `if (!is.null(x))` raised "object not
# found". Assigning the result OF tryCatch is what actually makes the fallback
# take effect.

load_or_null <- function(expr) tryCatch(expr, error = function(e) NULL)

memo_once <- function(fn) {
  value <- NULL; done <- FALSE
  function() {
    if (!done) { value <<- load_or_null(fn()); done <<- TRUE }
    value
  }
}

#' Utility-scale solar facilities, as points.
load_facilities <- memo_once(function() {
  read_csv(file.path(DATA_PATH, "solar_facility.csv"), show_col_types = FALSE,
           progress = FALSE) %>%
    transmute(
      Facility_Name   = p_name,
      County          = County,
      Latitude        = ylat,
      Longitude       = xlong,
      Capacity_MW     = suppressWarnings(as.numeric(p_cap_ac)),
      Commission_Year = suppressWarnings(as.integer(Year))
    ) %>%
    filter(!is.na(Latitude), !is.na(Longitude))
})

#' Facility attributes for the fleet charts. Kept separate from
#' load_facilities(), which the map depends on and which deliberately carries
#' only what a map point needs.
load_fleet <- memo_once(function() {
  read_csv(file.path(DATA_PATH, "solar_facility.csv"), show_col_types = FALSE,
           progress = FALSE) %>%
    transmute(
      Facility = p_name,
      County   = County,
      Year     = suppressWarnings(as.integer(Year)),
      MW_ac    = suppressWarnings(as.numeric(p_cap_ac)),
      MW_dc    = suppressWarnings(as.numeric(p_cap_dc)),
      # p_axis carries a comma-joined pair on the two sites that use both
      # mounting types; those become their own category rather than being
      # assigned to one or dropped.
      Mounting = dplyr::case_when(
        is.na(p_axis) | p_axis == ""       ~ "Not recorded",
        grepl(",", p_axis, fixed = TRUE)   ~ "Mixed mounting",
        TRUE                                ~ p_axis
      ),
      SiteType = ifelse(is.na(p_type) | p_type == "", "Not recorded", p_type)
    ) %>%
    filter(!is.na(MW_ac), MW_ac > 0)
})

#' County boundaries with a disambiguated locality key.
load_counties <- memo_once(function() {
  counties(state = "VA", cb = TRUE, class = "sf", progress_bar = FALSE) %>%
    st_transform(TARGET_CRS) %>%
    vvn_locality_key()
})

load_state_border <- memo_once(function() {
  cty <- load_counties()
  if (is.null(cty)) return(NULL)
  st_union(cty)
})

#' County-year indicators, averaged over the duplicate rows per county-year.
load_county_indicators <- memo_once(function() {
  read_csv(file.path(DATA_PATH, "county_level_merged_data.csv"),
           show_col_types = FALSE, progress = FALSE) %>%
    mutate(
      locality_key = vvn_clean_locality(County),
      Year = suppressWarnings(as.integer(Year)),
      across(all_of(unname(COUNTY_INDICATORS)), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    filter(!is.na(Year)) %>%
    group_by(locality_key, Year) %>%
    summarise(
      across(all_of(unname(COUNTY_INDICATORS)), ~ mean(.x, na.rm = TRUE)),
      Treated = suppressWarnings(max(as.numeric(Treated), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(across(everything(), ~ ifelse(is.finite(.x) | !is.numeric(.x), .x, NA)))
})

#' Parcel polygons. Stored in UTM 17N, so they must be reprojected.
load_parcels <- memo_once(function() {
  p <- readRDS(file.path(DATA_PATH, "final_parcel_data.rds"))
  if (is.na(st_crs(p))) st_crs(p) <- TARGET_CRS
  p %>%
    st_transform(TARGET_CRS) %>%
    mutate(across(c(sales_price, assess, per_acre, Acres),
                  ~ suppressWarnings(as.numeric(as.character(.x)))))
})

#' Largest named water bodies, ranked by area rather than alphabetically.
load_water <- memo_once(function() {
  read_csv(file.path(DATA_PATH, "virginia_water_features.csv"),
           show_col_types = FALSE, progress = FALSE) %>%
    filter(!is.na(INTPTLON), !is.na(INTPTLAT), !is.na(FULLNAME)) %>%
    mutate(AWATER = suppressWarnings(as.numeric(AWATER))) %>%
    slice_max(AWATER, n = MAX_WATER_FEATURES, with_ties = FALSE) %>%
    st_as_sf(coords = c("INTPTLON", "INTPTLAT"), crs = TARGET_CRS)
})

#' Largest population centres.
load_cities <- memo_once(function() {
  st_read(file.path(DATA_PATH, "VA_Cities/VA_Cities.shp"), quiet = TRUE) %>%
    st_transform(TARGET_CRS) %>%
    mutate(POP_2010 = suppressWarnings(as.numeric(POP_2010))) %>%
    filter(!is.na(POP_2010), POP_2010 > 0) %>%
    slice_max(POP_2010, n = MAX_CITIES, with_ties = FALSE)
})

# Years available for the county view, read once so the UI can be built.
INDICATOR_YEARS <- local({
  d <- load_county_indicators()
  if (is.null(d) || nrow(d) == 0) return(integer(0))
  sort(unique(d$Year), decreasing = TRUE)
})

# How many localities actually report each year. Coverage swings from 85
# counties in 2015 to a single county in 2016, and a one-county year looks like
# a broken map unless the reader is told.
YEAR_COVERAGE <- local({
  d <- load_county_indicators()
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d %>% group_by(Year) %>%
    summarise(n = sum(!is.na(Price_Per_Acre)), .groups = "drop")
})

#' Colour breaks per indicator, pooled over EVERY year.
#'
#' Computing quantile breaks from one year's values re-normalises the scale each
#' time the year changes, so the cheapest county is always palest and the
#' priciest always darkest whatever the actual figures are -- the map cannot
#' show land prices rising, only each year's internal ranking. The same defect
#' was found and fixed in the childcare dashboard. Pooling the breaks means a
#' county that gets more expensive genuinely gets darker, and the legend holds
#' still between years.
#'
#' Quantiles rather than a linear span because these distributions are heavily
#' skewed: median land price sits near $4,000 while the maximum reaches
#' $139,000, and a linear ramp would paint all but a handful the same shade.
INDICATOR_BREAKS <- local({
  d <- load_county_indicators()
  if (is.null(d) || nrow(d) == 0) return(NULL)
  out <- lapply(unname(COUNTY_INDICATORS), function(v) {
    vals <- d[[v]]; vals <- vals[is.finite(vals)]
    if (length(vals) < 5) return(NULL)
    b <- unique(stats::quantile(vals, probs = seq(0, 1, 0.2), na.rm = TRUE))
    if (length(b) < 3) NULL else b
  })
  names(out) <- unname(COUNTY_INDICATORS)
  out
})

# ===== OVERLAY LAYERS ======================================================
# Each function takes a map and returns it with one group added, so the same
# code serves the initial leaflet() render, the leafletProxy() updates, and
# the tests. Defined once and reused, instead of the previous version's three
# copy-pasted blocks that had drifted out of sync with each other.
# They are at module level, not inside server(), so they are reachable from a
# test: the proxy-driven layer stack is otherwise invisible to testServer().

add_solar <- function(proxy) {
  fac <- load_facilities()
  if (is.null(fac) || nrow(fac) == 0) return(proxy)
  proxy %>%
    addCircleMarkers(
      data = fac, lng = ~Longitude, lat = ~Latitude,
      # Area-proportional radius so capacity is readable, with a floor.
      radius = ~pmax(4, sqrt(pmax(Capacity_MW, 0)) * 1.1),
      color = "#8a6d00", weight = 1, fillColor = "#F5C518", fillOpacity = 0.9,
      group = "Solar facilities",
      popup = ~paste0(
        "<strong>", Facility_Name, "</strong><br>",
        "<span style='color:#5c6472'>", County, " County</span><br><br>",
        "Capacity: <strong>", ifelse(is.na(Capacity_MW), "unknown",
                                     paste0(Capacity_MW, " MW")), "</strong><br>",
        "Commissioned: ", ifelse(is.na(Commission_Year), "unknown", Commission_Year)
      )
    )
}

add_cities <- function(proxy) {
  cities <- load_cities()
  if (is.null(cities) || nrow(cities) == 0) return(proxy)
  coords <- st_coordinates(cities)
  proxy %>%
    addCircleMarkers(
      lng = coords[, 1], lat = coords[, 2],
      radius = 4, stroke = FALSE, fillColor = "#C2436B", fillOpacity = 0.7,
      group = "Population centres",
      popup = paste0("<strong>", cities$NAME, "</strong><br>",
                     formatC(cities$POP_2010, format = "d", big.mark = ","),
                     " residents (2010)")
    )
}

add_water <- function(proxy) {
  water <- load_water()
  if (is.null(water) || nrow(water) == 0) return(proxy)
  coords <- st_coordinates(water)
  proxy %>%
    addCircleMarkers(
      lng = coords[, 1], lat = coords[, 2],
      radius = 3, stroke = FALSE, fillColor = "#2E6F9E", fillOpacity = 0.6,
      group = "Water bodies",
      popup = paste0("<strong>", water$FULLNAME, "</strong>")
    )
}

#' Shade counties by one indicator and add a matching legend.
add_county_layer <- function(map, dat, var, mode = "level") {
  values <- dat[[var]]
  if (!any(!is.na(values))) {
    return(leaflet::addControl(
      map, htmltools::HTML("<strong>No data for this indicator and year.</strong>"),
      position = "topright"
    ))
  }

  # Quantile bins, not a linear ramp. Several of these indicators are heavily
  # skewed -- population runs from ~2,000 to over 1.1 million -- so a linear
  # scale renders Fairfax dark and flattens every other locality into the same
  # pale colour.
  breaks <- INDICATOR_BREAKS[[var]]
  if (is.null(breaks)) {
    breaks <- unique(stats::quantile(values, probs = seq(0, 1, 0.2), na.rm = TRUE))
  }
  # Level mode keeps the pooled breaks (see INDICATOR_BREAKS, which hold the
  # scale steady across years); comparison mode uses a symmetric diverging
  # scale centred on the state median for the selected year.
  if (identical(mode, "vs_state")) {
    shading <- vvn_map_shading(values, "vs_state")
    shade <- shading$values
    pal <- shading$pal
  } else {
    pal <- if (length(breaks) >= 3) {
      leaflet::colorBin(SOLAR_RAMP, domain = range(breaks), bins = breaks,
                        na.color = VVN_NO_DATA)
    } else {
      leaflet::colorNumeric(SOLAR_RAMP, domain = values, na.color = VVN_NO_DATA)
    }
    shading <- list(
      title = NULL, fmt = leaflet::labelFormat(),
      deviation = vvn_pct_diff(values, stats::median(values[is.finite(values)]))
    )
    shade <- values
  }

  fmt <- INDICATOR_FORMAT[[var]]
  label <- names(COUNTY_INDICATORS)[COUNTY_INDICATORS == var]

  popups <- paste0(
    "<strong>", dat$locality, "</strong><br>",
    "<span style='color:#5c6472'>", dat$Year, "</span><br><br>",
    "Land price: ", ifelse(is.na(dat$Price_Per_Acre), "&mdash;",
                           INDICATOR_FORMAT$Price_Per_Acre(dat$Price_Per_Acre)), " / acre<br>",
    "Corn yield: ", ifelse(is.na(dat$CornYield), "&mdash;",
                           INDICATOR_FORMAT$CornYield(dat$CornYield)), "<br>",
    "Population: ", ifelse(is.na(dat$Population), "&mdash;",
                           INDICATOR_FORMAT$Population(dat$Population)), "<br>",
    "Has solar facility: ",
    ifelse(is.na(dat$Treated), "&mdash;", ifelse(dat$Treated == 1, "yes", "no"))
  )

  labels <- paste0(
    "<strong>", dat$locality, "</strong><br>",
    ifelse(is.na(values), "No data", fmt(values))
  )

  map %>%
    leaflet::addPolygons(
      data = dat,
      fillColor = ifelse(is.na(shade), VVN_NO_DATA, pal(shade)),
      fillOpacity = 0.8, weight = VVN_STROKE_WEIGHT,
      color = VVN_STROKE_COLOUR, opacity = VVN_STROKE_OPACITY,
      group = "Counties",
      popup = popups,
      label = lapply(labels, htmltools::HTML),
      labelOptions = vvn_labelopts(),
      highlightOptions = vvn_highlight()
    ) %>%
    leaflet::addLegend(
      position = "bottomright", pal = pal,
      values = shade[is.finite(shade)],
      title = shading$title %||% gsub(" ", "<br>", label),
      labFormat = shading$fmt, opacity = 1, na.label = "No data"
    )
}

#' Add parcel polygons shaded by assessed value per acre.
add_parcel_layer <- function(map, parcels) {
  if (is.null(parcels) || nrow(parcels) == 0) {
    return(leaflet::addControl(
      map, htmltools::HTML("<strong>No parcel records for this locality.</strong>"),
      position = "topright"
    ))
  }

  pal <- leaflet::colorNumeric(SOLAR_RAMP_ALT, domain = parcels$per_acre,
                               na.color = VVN_NO_DATA)

  map <- map %>%
    leaflet::addPolygons(
      data = parcels,
      fillColor = ifelse(is.na(parcels$per_acre), VVN_NO_DATA, pal(parcels$per_acre)),
      fillOpacity = 0.7, color = "#8a8f98", weight = 0.4,
      group = "Parcels",
      label = paste0("Parcel ", parcels$PARCELID),
      labelOptions = vvn_labelopts(),
      popup = paste0(
        "<strong>Parcel ", parcels$PARCELID, "</strong><br>",
        "<span style='color:#5c6472'>", parcels$LOCALITY, "</span><br><br>",
        "Acres: ", ifelse(is.na(parcels$Acres), "&mdash;", round(parcels$Acres, 2)), "<br>",
        "Value per acre: ", ifelse(is.na(parcels$per_acre), "&mdash;",
                                   paste0("$", formatC(parcels$per_acre, format = "f",
                                                       digits = 0, big.mark = ","))), "<br>",
        "Assessed: ", ifelse(is.na(parcels$assess), "&mdash;",
                             paste0("$", formatC(parcels$assess, format = "f",
                                                 digits = 0, big.mark = ",")))
      )
    )

  if (any(!is.na(parcels$per_acre))) {
    map <- leaflet::addLegend(
      map, position = "bottomright", pal = pal, values = parcels$per_acre,
      title = "Value<br>per acre", opacity = 1, na.label = "No data",
      labFormat = leaflet::labelFormat(prefix = "$", big.mark = ",")
    )
  }
  map
}

# ===== UI ==================================================================

ui <- page_fluid(
  title = "Virginia Solar Facilities Interactive Map",
  theme = vvn_theme(),
  tags$head(vvn_css()),

  vvn_header(
    "Virginia Solar Facilities",
    "Utility-scale solar sites against land values, farm productivity and the infrastructure that shapes where they get built."
  ),

  navset_tab(
  nav_panel(
  "Map",
  layout_sidebar(
    sidebar = sidebar(
      width = 320,

      vvn_group("Geographic level"),
      div(
        class = "vvn-chooser",
        radioGroupButtons(
          inputId   = "map_level",
          label     = NULL,
          choices   = c("County level", "Parcel level"),
          selected  = "County level",
          direction = "vertical",
          size      = "sm",
          checkIcon = list(yes = icon("check")),
          width     = "100%"
        )
      ),

      conditionalPanel(
        condition = "input.map_level == 'County level'",
        vvn_group("Shade counties by"),
        selectInput("indicator", NULL, choices = COUNTY_INDICATORS,
                    selected = "Price_Per_Acre"),
        vvn_mode_control("county_mode", "Indicator value"),
        vvn_group("Year"),
        selectInput("county_year", NULL,
                    choices = if (length(INDICATOR_YEARS)) INDICATOR_YEARS else "",
                    selected = if (length(INDICATOR_YEARS)) INDICATOR_YEARS[1] else ""),
        vvn_help("Click a county for its full indicator profile.")
      ),

      conditionalPanel(
        condition = "input.map_level == 'Parcel level'",
        vvn_group("Locality"),
        selectInput("parcel_locality", NULL, choices = NULL),
        vvn_help("Parcel records cover the localities studied for land-value effects, not the whole state.")
      ),

      vvn_group("Context layers"),
      div(
        class = "toggle-items",
        checkboxInput("show_solar",        "Solar facilities",  value = TRUE),
        checkboxInput("show_cities",       "Population centres", value = FALSE),
        checkboxInput("show_water",        "Water bodies",       value = FALSE)
      )
    ),

    card(
      full_screen = TRUE,
      card_header(textOutput("map_title", inline = TRUE)),
      card_body(padding = 0, leafletOutput("interactive_map", height = "560px"))
    ),

    vvn_notes(
      "Reading this map",
      p(
        "Each", strong("gold point"), "is a utility-scale solar facility, sized by generating capacity.",
        "In", strong("county level"), "view, counties are shaded by the indicator you select, so you can read solar",
        "siting against land prices, farm yields or housing conditions in the same frame."
      ),
      p(
        "In", strong("parcel level"), "view the map switches to individual parcel boundaries for the localities",
        "studied for land-value effects, shaded by assessed value per acre."
      ),
      p(
        strong("Context layers"), "add the geography that constrains siting: population centres a project must",
        "stay clear of, and water bodies that break up developable land. Water bodies show the",
        MAX_WATER_FEATURES, "largest by surface area."
      ),
      p(
        class = "vvn-help",
        "Facility locations come from the USGS/LBNL utility-scale solar database. Land values and demographics are",
        "county-year averages from the DSPG solar dataset. Basemap style can be changed with the control on the map."
      )
    )
  )
  ),

  # ------------------------------------------------------------------ tab 2
  nav_panel(
    "Solar fleet",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        vvn_group("Capacity measure"),
        radioButtons("fleet_measure", NULL,
                     choices = c("AC capacity (MW)" = "MW_ac",
                                 "DC capacity (MW)" = "MW_dc"),
                     selected = "MW_ac"),
        vvn_help(
          "Every utility-scale solar facility in the Virginia inventory, by the",
          "year it came online."
        ),
        vvn_help(
          "AC capacity is what the grid sees; DC is the panel rating and is",
          "always the larger of the two."
        )
      ),
      uiOutput("fleet_kpi"),
      card(
        full_screen = TRUE,
        card_header(textOutput("fleet_growth_title", inline = TRUE)),
        card_body(padding = 0, plotlyOutput("fleet_growth", height = "380px"))
      ),
      div(
        style = "margin-top:16px;",
        card(
          full_screen = TRUE,
          card_header("Largest facilities"),
          card_body(padding = 0, plotlyOutput("fleet_top", height = "420px"))
        )
      ),
      div(
        style = "margin-top:16px;",
        card(
          full_screen = TRUE,
          card_header("Capacity by county, and how facilities are mounted"),
          card_body(
            layout_columns(
              col_widths = c(7, 5),
              plotlyOutput("fleet_county", height = "400px"),
              plotlyOutput("fleet_mount", height = "400px")
            )
          )
        )
      ),
      vvn_notes(
        "Reading these charts",
        p(
          "The map answers", em("where"), "; these answer", em("how much"), "and", em("when"), ". Virginia went from a",
          "handful of sites to a fleet in roughly a decade, and the cumulative line is the clearest statement of",
          "that: the bars are what was added each year, the line is the total standing after it."
        ),
        p(
          strong("Capacity is heavily concentrated."), "A small number of large facilities account for most of the",
          "megawatts, so a count of sites and a sum of capacity rank counties differently — which is why the",
          "county chart is by capacity, with the site count in the tooltip."
        ),
        p(
          strong("Mounting type"), "separates single-axis tracking, which follows the sun through the day, from",
          "fixed-tilt. Two sites record both and are kept as their own category rather than forced into one."
        ),
        p(
          class = "vvn-help",
          "Counts are facilities, so a large campus and a small installation each count once. Facilities without a",
          "recorded capacity are excluded from every chart on this tab."
        )
      )
    )
  ),

  # ------------------------------------------------------------------ tab 3
  nav_panel(
    "County trends",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        vvn_group("Indicator"),
        selectInput("trend_indicator", NULL, choices = COUNTY_INDICATORS,
                    selected = "Price_Per_Acre"),
        vvn_group("Localities"),
        selectizeInput("trend_localities", NULL, choices = NULL, multiple = TRUE,
                       options = list(maxItems = VVN_SERIES_MAX,
                                      placeholder = "Choose up to 8")),
        vvn_help(
          "How each indicator has moved over time, for the localities you pick",
          "against the statewide median."
        ),
        vvn_help(
          "Land price and crop yields are not recorded for every county in every",
          "year, so a line may start late or break."
        )
      ),
      uiOutput("trend_kpi"),
      card(
        full_screen = TRUE,
        card_header(textOutput("trend_title", inline = TRUE)),
        card_body(padding = 0, plotlyOutput("trend_lines", height = "440px"))
      ),
      div(
        style = "margin-top:16px;",
        card(
          full_screen = TRUE,
          card_header("Counties with and without a solar facility, compared over time"),
          card_body(padding = 0, plotlyOutput("trend_treated", height = "380px"))
        )
      ),
      vvn_notes(
        "Reading these charts",
        p(
          "The upper chart is a straight time series for the localities you select, with the statewide median drawn",
          "behind them as a reference. It answers whether a locality is moving with the state or away from it."
        ),
        p(
          "The lower chart splits every county into those the dataset flags as", strong("treated"), "— having a solar",
          "facility — and those it does not, and plots the median of each group. This is the comparison the",
          "underlying research is built on."
        ),
        p(
          strong("It is a comparison, not a causal estimate."), "Counties that host solar were not chosen at random:",
          "they tend to have cheaper land and more of it to begin with. A gap between the two lines is consistent",
          "with an effect of solar development, and equally consistent with the siting criteria that put the",
          "facilities there. Separating the two needs the parcel-level design, not this chart."
        ),
        p(
          class = "vvn-help",
          "Medians rather than means, so a single very large county cannot carry a group line. Years with no",
          "observations for a group are left as a break in the line rather than interpolated."
        )
      )
    )
  )
  )
)

# ===== SERVER ==============================================================

server <- function(input, output, session) {

  # ---- parcel locality choices ------------------------------------------
  observe({
    parcels <- load_parcels()
    choices <- if (!is.null(parcels) && "LOCALITY" %in% names(parcels)) {
      c("All localities" = "All", sort(unique(parcels$LOCALITY)))
    } else {
      c("All localities" = "All")
    }
    updateSelectInput(session, "parcel_locality", choices = choices, selected = "All")
  })

  # ---- titles ------------------------------------------------------------
  output$map_title <- renderText({
    if (identical(input$map_level, "Parcel level")) {
      loc <- input$parcel_locality
      if (is.null(loc) || loc == "All") "Parcel boundaries — all studied localities"
      else paste("Parcel boundaries —", loc)
    } else {
      label <- names(COUNTY_INDICATORS)[COUNTY_INDICATORS == (input$indicator %||% "")]
      n <- if (!is.null(YEAR_COVERAGE)) {
        YEAR_COVERAGE$n[YEAR_COVERAGE$Year == as.integer(input$county_year %||% 0)]
      } else integer(0)
      paste0(
        if (length(label)) label else "County indicator", " — ", input$county_year,
        if (length(n) == 1) sprintf("  (%d localities reporting)", n) else ""
      )
    }
  })

  # ---- county layer data -------------------------------------------------
  county_layer <- reactive({
    cty <- load_counties()
    ind <- load_county_indicators()
    req(cty, ind, input$indicator, input$county_year)

    year_data <- ind %>%
      filter(Year == as.integer(input$county_year)) %>%
      select(locality_key, Year, all_of(unname(COUNTY_INDICATORS)), Treated)

    cty %>% left_join(year_data, by = "locality_key")
  })

  filtered_parcels <- reactive({
    parcels <- load_parcels()
    if (is.null(parcels)) return(NULL)
    loc <- req(input$parcel_locality)
    if (loc == "All") parcels else parcels %>% filter(LOCALITY == loc)
  })

  # ---- base map, created once -------------------------------------------
  output$interactive_map <- renderLeaflet({
    # All Esri: CARTO now requires an API key and serves a watermarked
    # placeholder instead of tiles (see vvn_basemap in vvn_ui.R). That removed
    # both the former "Light" and "Dark" options, so Light is Esri's grey
    # canvas and the dark choice is dropped rather than shipped broken.
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      # Fit Virginia rather than a fixed centre/zoom: the state is wide and
      # short, so a fixed zoom leaves bands of neighbouring states above and
      # below it. VVN_VA_BOUNDS is shared with every other map in the suite.
      fitBounds(VVN_VA_BOUNDS$lng1, VVN_VA_BOUNDS$lat1,
                VVN_VA_BOUNDS$lng2, VVN_VA_BOUNDS$lat2) %>%
      setMaxBounds(VVN_VA_BOUNDS$lng1 - 3, VVN_VA_BOUNDS$lat1 - 2,
                   VVN_VA_BOUNDS$lng2 + 3, VVN_VA_BOUNDS$lat2 + 2) %>%
      addLayersControl(
        baseGroups = c("Light", "Street", "Topographic", "Satellite"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })

  # ---- one observer owns the whole overlay stack ------------------------
  # Draw order matters on a leaflet proxy: later additions sit on top. Keeping
  # it in a single observer is what guarantees polygons go down first and the
  # solar points stay on top, whichever control changed. The previous version
  # split this across two observers that each cleared and re-added layers, so
  # they raced and the facilities vanished in parcel view.
  observe({
    proxy <- leafletProxy("interactive_map") %>%
      clearGroup("Counties") %>%
      clearGroup("Parcels") %>%
      clearGroup("Population centres") %>%
      clearGroup("Water bodies") %>%
      clearGroup("Solar facilities") %>%
      clearControls()

    proxy <- if (identical(input$map_level, "County level")) {
      add_county_layer(proxy, county_layer(), req(input$indicator),
                       input$county_mode %||% "level")
    } else {
      add_parcel_layer(proxy, filtered_parcels())
    }

    # Frame the parcel selection.
    if (identical(input$map_level, "Parcel level")) {
      parcels <- filtered_parcels()
      if (!is.null(parcels) && nrow(parcels) > 0) {
        bb <- sf::st_bbox(parcels)
        proxy <- flyToBounds(proxy, as.numeric(bb$xmin), as.numeric(bb$ymin),
                             as.numeric(bb$xmax), as.numeric(bb$ymax))
      }
    }

    # State border above the polygons so Virginia stays a readable shape.
    proxy <- vvn_outline(proxy)

    # Context layers on top of whichever base polygons were drawn.
    if (isTRUE(input$show_water))        proxy <- add_water(proxy)
    if (isTRUE(input$show_cities))       proxy <- add_cities(proxy)
    if (isTRUE(input$show_solar))        proxy <- add_solar(proxy)
  })

  # ===== FLEET TAB =========================================================

  fleet_measure <- reactive({
    m <- req(input$fleet_measure)
    list(col = m, label = if (m == "MW_dc") "DC capacity" else "AC capacity")
  })

  fleet_data <- reactive({
    f <- load_fleet()
    validate(need(!is.null(f) && nrow(f) > 0, "Facility data could not be loaded."))
    m <- fleet_measure()$col
    f %>% mutate(MW = .data[[m]]) %>% filter(!is.na(MW), MW > 0)
  })

  output$fleet_kpi <- renderUI({
    d <- fleet_data(); lab <- fleet_measure()$label
    yrs <- range(d$Year, na.rm = TRUE)
    biggest <- slice_max(d, MW, n = 1)
    vvn_kpi_row(
      vvn_tile("Facilities", nrow(d), "utility-scale sites", accent = TRUE),
      vvn_tile(paste("Total", lab), paste0(scales::comma(round(sum(d$MW))), " MW"),
               sprintf("%d–%d", yrs[1], yrs[2])),
      vvn_tile("Median size", paste0(round(stats::median(d$MW)), " MW"),
               sprintf("largest %s MW", scales::comma(round(biggest$MW[1])))),
      vvn_tile("Counties hosting", dplyr::n_distinct(d$County), "of 133 localities")
    )
  })

  output$fleet_growth_title <- renderText({
    d <- fleet_data()
    sprintf("%s added each year, and the running total — %d–%d",
            fleet_measure()$label, min(d$Year, na.rm = TRUE), max(d$Year, na.rm = TRUE))
  })

  output$fleet_growth <- renderPlotly({
    d <- fleet_data() %>%
      filter(!is.na(Year)) %>%
      group_by(Year) %>%
      summarise(added = sum(MW), n = dplyr::n(), .groups = "drop") %>%
      arrange(Year) %>%
      mutate(cumulative = cumsum(added))
    validate(need(nrow(d) > 1, "Too few dated facilities to draw a trend."))

    # Bars are the annual addition, the line is the running total. Both are
    # megawatts on the same axis -- never a second y-scale.
    gg <- ggplot(d, aes(x = Year)) +
      # An area for the running total with the annual additions as bars on top:
      # the filled shape reads as accumulated capacity rather than as another
      # ranked bar chart, which is what five other tabs in this suite already
      # look like.
      geom_area(aes(y = cumulative), fill = SOLAR_RAMP[2], alpha = 0.75) +
      geom_col(aes(y = added,
                   text = paste0(Year, "<br>", round(added), " MW added<br>",
                                 n, " facilities")),
               fill = SOLAR_RAMP[5], width = 0.68) +
      geom_line(aes(y = cumulative), colour = SOLAR_RAMP[6], linewidth = 0.7) +
      geom_point(aes(y = cumulative,
                     text = paste0(Year, "<br>", round(cumulative), " MW in total")),
                 colour = SOLAR_RAMP[6], size = 1.9) +
      scale_x_continuous(breaks = function(x) seq(ceiling(min(x)), floor(max(x)), by = 2)) +
      scale_y_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = NULL, y = paste(fleet_measure()$label, "(MW)")) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })

  output$fleet_top <- renderPlotly({
    d <- fleet_data() %>%
      slice_max(MW, n = 20) %>%
      arrange(MW) %>%
      mutate(Facility = factor(Facility, levels = unique(Facility)))
    gg <- ggplot(d, aes(x = MW, y = Facility,
                        text = paste0(Facility, "<br>", County, " County<br>",
                                      round(MW), " MW", "<br>online ", Year))) +
      geom_col(width = 0.68, fill = SOLAR_RAMP[4]) +
      scale_x_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = paste(fleet_measure()$label, "(MW)"), y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })

  output$fleet_county <- renderPlotly({
    d <- fleet_data() %>%
      group_by(County) %>%
      summarise(MW = sum(MW), n = dplyr::n(), .groups = "drop") %>%
      slice_max(MW, n = 15) %>%
      arrange(MW) %>%
      mutate(County = factor(County, levels = unique(County)))
    gg <- ggplot(d, aes(x = MW, y = County,
                        text = paste0(County, "<br>", round(MW), " MW<br>",
                                      n, if (identical(max(d$n), 1L)) " facility" else " facilities"))) +
      geom_col(width = 0.68, fill = SOLAR_RAMP[4]) +
      scale_x_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = paste(fleet_measure()$label, "(MW)"), y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })

  output$fleet_mount <- renderPlotly({
    d <- fleet_data() %>%
      group_by(Mounting) %>%
      summarise(MW = sum(MW), n = dplyr::n(), .groups = "drop") %>%
      arrange(MW) %>%
      mutate(Mounting = factor(Mounting, levels = unique(Mounting)))
    gg <- ggplot(d, aes(x = MW, y = Mounting,
                        text = paste0(Mounting, "<br>", round(MW), " MW<br>",
                                      n, " facilities"))) +
      geom_col(width = 0.6, fill = SOLAR_RAMP[4]) +
      scale_x_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, 0.1))) +
      labs(x = paste(fleet_measure()$label, "(MW)"), y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })

  # ===== COUNTY TRENDS TAB =================================================

  trend_label <- reactive({
    v <- req(input$trend_indicator)
    names(COUNTY_INDICATORS)[COUNTY_INDICATORS == v]
  })

  observe({
    ind <- load_county_indicators()
    req(!is.null(ind))
    cty <- load_counties()
    nm <- if (!is.null(cty)) {
      cty %>% sf::st_drop_geometry() %>% select(locality_key, locality) %>% distinct()
    } else NULL
    keys <- sort(unique(ind$locality_key))
    labels <- if (!is.null(nm)) {
      m <- nm$locality[match(keys, nm$locality_key)]
      ifelse(is.na(m), keys, m)
    } else keys
    ch <- stats::setNames(keys, labels)
    ch <- ch[order(names(ch))]
    updateSelectizeInput(session, "trend_localities", choices = ch,
                         selected = utils::head(unname(ch), 3), server = TRUE)
  })

  trend_data <- reactive({
    ind <- load_county_indicators()
    validate(need(!is.null(ind), "County indicator data could not be loaded."))
    v <- req(input$trend_indicator)
    ind %>%
      mutate(value = .data[[v]]) %>%
      filter(!is.na(value), is.finite(value))
  })

  output$trend_title <- renderText({
    sprintf("%s over time", trend_label())
  })

  output$trend_kpi <- renderUI({
    d <- trend_data()
    fmt <- INDICATOR_FORMAT[[req(input$trend_indicator)]]
    yrs <- range(d$Year)
    med <- d %>% group_by(Year) %>%
      summarise(m = stats::median(value), .groups = "drop") %>% arrange(Year)
    first <- med$m[1]; last <- med$m[nrow(med)]
    vvn_kpi_row(
      vvn_tile("Indicator", trend_label(),
               sprintf("%d–%d", yrs[1], yrs[2]), accent = TRUE),
      vvn_tile(paste("State median,", yrs[1]), fmt(first), "across reporting counties"),
      vvn_tile(paste("State median,", yrs[2]), fmt(last),
               if (is.finite(first) && first != 0)
                 sprintf("%+.0f%% over the period", 100 * (last - first) / first) else NULL),
      vvn_tile("Counties reporting", dplyr::n_distinct(d$locality_key),
               "with at least one year")
    )
  })

  output$trend_lines <- renderPlotly({
    d <- trend_data()
    sel <- input$trend_localities
    fmt <- INDICATOR_FORMAT[[req(input$trend_indicator)]]

    med <- d %>% group_by(Year) %>%
      summarise(value = stats::median(value), .groups = "drop") %>%
      mutate(locality = "Statewide median")

    gg <- ggplot() +
      geom_line(data = med, aes(x = Year, y = value, group = 1),
                colour = "#9aa2b1", linewidth = 0.8, linetype = "dashed")

    if (length(sel)) {
      cty <- load_counties()
      nm <- if (!is.null(cty)) {
        cty %>% sf::st_drop_geometry() %>% select(locality_key, locality) %>% distinct()
      } else NULL
      pick <- d %>%
        filter(locality_key %in% sel) %>%
        mutate(locality = if (!is.null(nm)) {
          m <- nm$locality[match(locality_key, nm$locality_key)]
          ifelse(is.na(m), locality_key, m)
        } else locality_key)
      validate(need(nrow(pick) > 0, "No observations for the selected localities."))
      # Colour follows the locality, assigned in a fixed order, so adding or
      # removing one never repaints the others.
      lvl <- sort(unique(pick$locality))
      pick$locality <- factor(pick$locality, levels = lvl)
      gg <- gg +
        geom_line(data = pick,
                  aes(x = Year, y = value, colour = locality, group = locality),
                  linewidth = 0.7) +
        geom_point(data = pick,
                   aes(x = Year, y = value, colour = locality, group = locality,
                       text = paste0(locality, "<br>", Year, "<br>", fmt(value))),
                   size = 1.7) +
        # Up to eight localities, so this one uses the extended set.
        scale_colour_manual(values = vvn_series_colours(lvl), name = NULL)
    }

    gg <- gg +
      scale_y_continuous(labels = function(x) vapply(x, function(v) fmt(v), character(1))) +
      labs(x = NULL, y = trend_label()) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })

  output$trend_treated <- renderPlotly({
    d <- trend_data()
    validate(need("Treated" %in% names(d), "This dataset has no treated/control flag."))
    fmt <- INDICATOR_FORMAT[[req(input$trend_indicator)]]
    g <- d %>%
      filter(!is.na(Treated)) %>%
      mutate(Group = ifelse(Treated > 0, "Hosts a solar facility", "No solar facility")) %>%
      group_by(Group, Year) %>%
      summarise(value = stats::median(value), n = dplyr::n(), .groups = "drop")
    validate(need(nrow(g) > 0, "No treated/control observations for this indicator."))
    lvl <- c("Hosts a solar facility", "No solar facility")
    g$Group <- factor(g$Group, levels = lvl)
    gg <- ggplot(g, aes(x = Year, y = value, colour = Group, group = Group,
                        text = paste0(Group, "<br>", Year, "<br>", fmt(value),
                                      "<br>", n, " counties"))) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 1.7) +
      scale_colour_manual(values = stats::setNames(VVN_SERIES_PALETTE[1:2], lvl),
                          name = NULL) +
      scale_y_continuous(labels = function(x) vapply(x, function(v) fmt(v), character(1))) +
      labs(x = NULL, y = trend_label()) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })
}

shinyApp(ui = ui, server = server)
