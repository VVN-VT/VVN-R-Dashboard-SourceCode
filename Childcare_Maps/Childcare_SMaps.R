# ============================================================================
# Virginia Childcare Cost & Female Labor Force Participation
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(readxl)
library(sf)
library(tigris)
library(leaflet)

source("vvn_ui.R")

# This dashboard's own sequential ramp. Every dashboard in the suite uses a
# different hue on the same lightness ladder, so the seven are distinguishable
# at a glance while still reading as one family -- see VVN_RAMPS.
CC_RAMP     <- vvn_ramp("pink")
CC_RAMP_ALT <- vvn_ramp("ochre")

options(tigris_use_cache = TRUE)

DATA_FILE  <- file.path("Childcare_Data", "Cleaned_Virginia_Womens_Bureau.xlsx")

COST_TYPES <- c(
  "Infant care"    = "MCINFANT",
  "Toddler care"   = "MCTODDLER",
  "Preschool care" = "MCPRESCHOOL"
)

COST_LABELS <- c(MCINFANT = "Infant", MCTODDLER = "Toddler", MCPRESCHOOL = "Preschool")

FLFPR_VAR <- "FLFPR_20to64_UNDER6"

# ===== DATA ================================================================
# The workbook is ~370 columns of Women's Bureau indicators; only the childcare
# price, participation rate, locality key and urban flag are used here.

stopifnot(file.exists(DATA_FILE))

RUdata <- suppressWarnings(read_excel(
  DATA_FILE,
  na = c("NA", "n/a", "", "-")
)) %>%
  mutate(
    FIPS = str_pad(as.character(COUNTY_FIPS_CODE), 5, pad = "0"),
    across(all_of(c(unname(COST_TYPES), FLFPR_VAR, "Urban")),
           ~ suppressWarnings(as.numeric(.x))),
    STUDYYEAR = suppressWarnings(as.integer(STUDYYEAR))
  ) %>%
  filter(!is.na(FIPS), !is.na(STUDYYEAR))

# Urban/rural is a fixed property of a locality, not a yearly measurement, so
# resolve it once per FIPS rather than averaging it.
#
# The previous version folded Urban into the same mean() as the cost columns in
# "trend" mode. That is latent rather than active: Urban is constant within
# every one of the 133 localities in the current workbook, so the mean comes
# back as exactly 0 or 1 and the `== 1` / `== 0` tests still work. It only
# breaks if a locality is ever reclassified mid-series, at which point the mean
# becomes a fraction, matches neither test, and that locality silently drops
# into the greyed-out branch. Resolving it per locality removes the trap.
urban_lookup <- RUdata %>%
  filter(!is.na(Urban)) %>%
  group_by(FIPS) %>%
  summarise(Urban = dplyr::first(Urban), .groups = "drop")

locality_names <- RUdata %>%
  distinct(FIPS, COUNTY_NAME) %>%
  filter(!is.na(COUNTY_NAME)) %>%
  group_by(FIPS) %>%
  summarise(COUNTY_NAME = dplyr::first(COUNTY_NAME), .groups = "drop")

# 2008 is withheld from both tabs. The workbook carries no childcare prices for
# that year at all, and although it does carry a participation rate, that series
# is not shown either -- excluded by project decision, not by absence of data.
# Keep this list explicit so the reason is visible; everything else about which
# years appear is derived from the data below.
EXCLUDED_YEARS <- c(2008L)

#' Years that actually carry data for a set of columns, minus EXCLUDED_YEARS.
#' Derived rather than hard-coded, so the year lists stay correct as the
#' workbook is updated. The previous version used literal `!= 2008` and
#' `%in% c(2008, 2021)` filters scattered across the server, which also meant
#' the two tabs silently disagreed about which years existed.
years_with_data <- function(cols) {
  RUdata %>%
    mutate(.has = rowSums(!is.na(pick(all_of(cols)))) > 0) %>%
    group_by(STUDYYEAR) %>%
    summarise(n = sum(.has), .groups = "drop") %>%
    filter(n > 0, !STUDYYEAR %in% EXCLUDED_YEARS) %>%
    pull(STUDYYEAR) %>%
    sort()
}

COST_YEARS  <- years_with_data(unname(COST_TYPES))
FLFPR_YEARS <- years_with_data(FLFPR_VAR)

# Colour-scale domain per variable, FIXED across every year in scope.
#
# This matters more than it looks. Building the scale from a single year's
# values re-normalises it every time the year changes: the cheapest locality is
# always the darkest and the priciest always the brightest, so the map looks
# identical in 2009 and 2022 even though infant-care prices rose 41% between
# them (median $135 -> $191, maximum $324 -> $559). Only the legend numbers
# moved, which is the opposite of what a reader expects when they step through
# years looking for change.
#
# With one domain shared by all years, a locality that gets more expensive
# genuinely gets brighter, and the legend stays put so successive years are
# directly comparable.
VALUE_RANGES <- local({
  span <- function(cols, years) {
    v <- RUdata %>%
      filter(STUDYYEAR %in% years) %>%
      select(all_of(cols)) %>%
      unlist(use.names = FALSE)
    v <- v[is.finite(v)]
    if (length(v) == 0) NULL else range(v)
  }
  out <- lapply(unname(COST_TYPES), function(c) span(c, COST_YEARS))
  names(out) <- unname(COST_TYPES)
  out[[FLFPR_VAR]] <- span(FLFPR_VAR, FLFPR_YEARS)
  out
})

#' Virginia boundaries, fetched lazily so a Census outage cannot stop start-up.
load_boundaries <- local({
  value <- NULL; done <- FALSE
  function() {
    if (!done) {
      value <<- tryCatch(
        counties(state = "VA", cb = TRUE, class = "sf", progress_bar = FALSE) %>%
          st_transform(4326) %>%
          mutate(FIPS = str_pad(as.character(GEOID), 5, pad = "0")) %>%
          vvn_locality_key(),
        error = function(e) NULL
      )
      done <<- TRUE
    }
    value
  }
})

# ===== UI ==================================================================

#' Sidebar for one of the two tabs. `metric` is "cost" or "flfpr".
metric_sidebar <- function(metric) {
  is_cost <- identical(metric, "cost")
  years   <- if (is_cost) COST_YEARS else FLFPR_YEARS

  sidebar(
    width = 310,

    vvn_mode_control(paste0(metric, "_mode"), "Measured value"),

    vvn_group("View"),
    radioButtons(
      paste0(metric, "_view"), NULL,
      choices  = c("Single year" = "year", "Trend over time" = "trend"),
      selected = "year"
    ),

    conditionalPanel(
      condition = sprintf("input.%s_view == 'year'", metric),
      vvn_group("Year"),
      selectInput(paste0(metric, "_year"), NULL,
                  choices = rev(years), selected = max(years))
    ),

    if (is_cost) {
      tagList(
        conditionalPanel(
          condition = "input.cost_view == 'year'",
          vvn_group("Care type"),
          selectInput("cost_type", NULL, choices = COST_TYPES, selected = "MCINFANT")
        ),
        conditionalPanel(
          condition = "input.cost_view == 'trend'",
          vvn_group("Care types to chart"),
          checkboxGroupInput("trend_cost_types", NULL,
                             choices = COST_TYPES, selected = unname(COST_TYPES))
        )
      )
    },

    vvn_group("Localities"),
    radioButtons(
      paste0(metric, "_urban"), NULL,
      choices  = c("All" = "all", "Urban only" = "urban", "Rural only" = "rural"),
      selected = "all"
    ),

    conditionalPanel(
      condition = sprintf("input.%s_view == 'trend'", metric),
      vvn_group("Compare localities"),
      selectizeInput(paste0(metric, "_picker"), NULL,
                     choices = NULL, multiple = TRUE,
                     options = list(
                       placeholder = "Search or click the map",
                       # Hard cap at the categorical palette size. Beyond this a
                       # 9th line would need a generated hue, which is
                       # indistinguishable from an existing one under
                       # colour-vision deficiency.
                       maxItems = VVN_SERIES_MAX
                     )),
      vvn_help("Up to", VVN_SERIES_MAX, "localities can be charted at once."),
      actionLink(paste0(metric, "_clear"), "Clear selection")
    )
  )
}

ui <- page_fluid(
  title = "Virginia Childcare Cost & Female Labor Force Participation",
  theme = vvn_theme(),
  tags$head(vvn_css()),

  vvn_header(
    "Childcare Cost & Female Labor Force Participation",
    "Weekly childcare prices and the labor force participation of mothers with young children, across every Virginia locality."
  ),

  navset_tab(
    # ---------------------------------------------------------------- tab 1
    nav_panel(
      "Childcare cost",
      layout_sidebar(
        sidebar = metric_sidebar("cost"),
        uiOutput("cost_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("cost_card_title", inline = TRUE)),
          card_body(padding = 0, leafletOutput("cost_map", height = "520px"))
        ),

        conditionalPanel(
          condition = "input.cost_view == 'trend'",
          div(
            style = "margin-top:18px;",
            conditionalPanel(
              condition = "output.cost_has_selection",
              card(
                full_screen = TRUE,
                card_header("Cost trend for selected localities"),
                card_body(padding = 0, plotlyOutput("cost_trend", height = "400px"))
              )
            ),
            conditionalPanel(
              condition = "!output.cost_has_selection",
              vvn_empty(
                "No localities selected",
                "Pick localities from the sidebar, or click them on the map, to chart how their childcare prices have changed."
              )
            )
          )
        ),

        vvn_notes(
          "What this map shows",
          p(
            "The weekly median price of full-time childcare in each Virginia locality, for the selected care type.",
            "Darker shading marks more expensive localities. Urban localities are consistently more expensive than",
            "rural ones, and the gap has widened over the period covered."
          ),
          p(
            "Use the", strong("urban/rural filter"), "to isolate one group; filtered-out localities stay on the map",
            "in grey so the geography is still readable. In", strong("trend over time"), "mode the map shows each",
            "locality's average across all years with data, and selecting localities charts them individually."
          ),
          p(
            class = "vvn-help",
            "Source: US Department of Labor Women's Bureau county childcare prices.",
            "Years without published prices are omitted from the year list."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 2
    nav_panel(
      "Female labor force participation",
      layout_sidebar(
        sidebar = metric_sidebar("flfpr"),
        uiOutput("flfpr_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("flfpr_card_title", inline = TRUE)),
          card_body(padding = 0, leafletOutput("flfpr_map", height = "520px"))
        ),

        conditionalPanel(
          condition = "input.flfpr_view == 'trend'",
          div(
            style = "margin-top:18px;",
            conditionalPanel(
              condition = "output.flfpr_has_selection",
              card(
                full_screen = TRUE,
                card_header("Participation trend for selected localities"),
                card_body(padding = 0, plotlyOutput("flfpr_trend", height = "400px"))
              )
            ),
            conditionalPanel(
              condition = "!output.flfpr_has_selection",
              vvn_empty(
                "No localities selected",
                "Pick localities from the sidebar, or click them on the map, to chart their participation rates over time."
              )
            )
          )
        ),

        vvn_notes(
          "What this map shows",
          p(
            "The", strong("labor force participation rate of women aged 20–64 with at least one child under six"),
            ", by locality. Brighter yellow-green marks higher participation; dark purple marks lower."
          ),
          p(
            "Read alongside the childcare cost tab, this is the core question the dataset was built for: whether the",
            "price of care tracks whether mothers of young children are in paid work. The relationship is not simple —",
            "high-cost Northern Virginia also shows high participation — because income and local labor markets move together."
          ),
          p(
            class = "vvn-help",
            "Source: American Community Survey, via the US Department of Labor Women's Bureau. Rates are percentages."
          )
        )
      )
    )
  )
)

# ===== SERVER ==============================================================

server <- function(input, output, session) {

  # ---- locality selection, shared logic for both tabs --------------------

  selections <- list(
    cost  = reactiveVal(character(0)),
    flfpr = reactiveVal(character(0))
  )

  observe({
    choices <- sort(locality_names$COUNTY_NAME)
    updateSelectizeInput(session, "cost_picker", choices = choices, server = TRUE)
    updateSelectizeInput(session, "flfpr_picker", choices = choices, server = TRUE)
  })

  # One observer set per tab, built from the same closure.
  lapply(c("cost", "flfpr"), function(metric) {
    sel <- selections[[metric]]

    observeEvent(input[[paste0(metric, "_picker")]], {
      sel(input[[paste0(metric, "_picker")]] %||% character(0))
    }, ignoreNULL = FALSE)

    observeEvent(input[[paste0(metric, "_clear")]], {
      sel(character(0))
      updateSelectizeInput(session, paste0(metric, "_picker"), selected = character(0))
    })

    # Clicking a locality adds it, but only while the trend view is open.
    observeEvent(input[[paste0(metric, "_map_shape_click")]], {
      if (!identical(input[[paste0(metric, "_view")]], "trend")) return()
      clicked <- input[[paste0(metric, "_map_shape_click")]]$id
      name <- locality_names$COUNTY_NAME[locality_names$FIPS == clicked]
      if (length(name) != 1) return()
      updated <- union(sel(), name)
      sel(updated)
      updateSelectizeInput(session, paste0(metric, "_picker"), selected = updated)
    })

    output[[paste0(metric, "_has_selection")]] <- reactive(length(sel()) > 0)
    outputOptions(output, paste0(metric, "_has_selection"), suspendWhenHidden = FALSE)
  })

  # ---- map data ----------------------------------------------------------

  #' Values to map: one year, or the per-locality mean across the years in
  #' scope. Restricted to this metric's year list so the trend-mode average
  #' covers exactly the years the dropdown and the trend chart use -- otherwise
  #' an excluded year would still be folded into the map's mean.
  map_values <- function(metric, cols) {
    years <- if (identical(metric, "cost")) COST_YEARS else FLFPR_YEARS
    in_scope <- RUdata %>% filter(STUDYYEAR %in% years)

    if (identical(input[[paste0(metric, "_view")]], "year")) {
      yr <- req(input[[paste0(metric, "_year")]])
      in_scope %>%
        filter(STUDYYEAR == as.integer(yr)) %>%
        select(FIPS, all_of(cols))
    } else {
      in_scope %>%
        group_by(FIPS) %>%
        summarise(across(all_of(cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
        # mean() of an all-NA group returns NaN; treat it as missing.
        mutate(across(all_of(cols), ~ ifelse(is.nan(.x), NA_real_, .x)))
    }
  }

  #' Draw one choropleth. `palette_fn` takes n and returns n colours.
  render_choropleth <- function(metric, var, palette_fn, legend_title, fmt) {
    shapes <- load_boundaries()
    validate(need(
      !is.null(shapes),
      "County boundaries could not be downloaded from the Census TIGER service. Please try again shortly."
    ))
    validate(need(!is.na(var), "Select at least one care type to map."))

    dat <- shapes %>%
      left_join(map_values(metric, var), by = "FIPS") %>%
      left_join(urban_lookup, by = "FIPS") %>%
      left_join(locality_names, by = "FIPS")

    values <- dat[[var]]
    validate(need(any(!is.na(values)), "No data available for this selection."))

    # Urban filter: non-matching localities stay visible but muted, so the
    # state outline is never broken up. NA urban flags are treated as excluded
    # rather than silently becoming NA colours.
    # Defaulted, because switch() errors on NULL and the input is briefly unset
    # on the first render before Shiny has initialised the radio group.
    filter_mode <- input[[paste0(metric, "_urban")]] %||% "all"
    in_scope <- switch(
      filter_mode,
      urban = !is.na(dat$Urban) & dat$Urban == 1,
      rural = !is.na(dat$Urban) & dat$Urban == 0,
      rep(TRUE, nrow(dat))
    )

    mode <- input[[paste0(metric, "_mode")]] %||% "level"

    if (identical(mode, "vs_state")) {
      # Comparison is against this year's state median, which is the point of
      # the view: it answers "high for Virginia right now", not "high against
      # the fixed multi-year range".
      shading <- vvn_map_shading(values, "vs_state")
      shade  <- shading$values
      pal    <- shading$pal
      domain <- shade[is.finite(shade)]
      legend_title_used <- shading$title
      legend_fmt_used   <- shading$fmt
    } else {
      # Fixed domain, not this year's values -- see VALUE_RANGES above.
      domain <- VALUE_RANGES[[var]] %||% range(values, na.rm = TRUE)
      pal <- colorNumeric(palette_fn(256), domain = domain, na.color = VVN_NO_DATA)
      shade <- values
      shading <- list(deviation = vvn_pct_diff(
        values, stats::median(values[is.finite(values)])))
      legend_title_used <- legend_title
      legend_fmt_used   <- labelFormat()
    }

    fill <- ifelse(!in_scope, "#dfe2e7",
                   ifelse(is.na(shade), VVN_NO_DATA, pal(shade)))

    labels <- paste0(
      "<strong>", dat$locality, "</strong><br>",
      ifelse(is.na(values), "No data", fmt(values)),
      vvn_deviation_text(shading$deviation, shading$compare %||% "percent"),
      "<br><span style='color:#5c6472'>",
      ifelse(is.na(dat$Urban), "Urban/rural not classified",
             ifelse(dat$Urban == 1, "Urban", "Rural")),
      "</span>",
      ifelse(in_scope, "", "<br><em>Filtered out</em>")
    )

    leaflet(dat) %>%
      vvn_basemap() %>%
      addPolygons(
        fillColor   = fill,
        fillOpacity = ifelse(in_scope, 0.85, 0.35),
        weight      = VVN_STROKE_WEIGHT,
        color       = VVN_STROKE_COLOUR,
        opacity     = VVN_STROKE_OPACITY,
        label       = lapply(labels, HTML),
        labelOptions     = vvn_labelopts(),
        highlightOptions = vvn_highlight(),
        layerId     = ~FIPS
      ) %>%
      vvn_outline() %>%
      addLegend(
        position = "bottomright", pal = pal, values = domain,
        title = legend_title_used, labFormat = legend_fmt_used,
        opacity = 1, na.label = "No data"
      )
  }

  # ---- headline tiles ----------------------------------------------------
  # Derived from exactly the same values the map shades, so a tile can never
  # disagree with the polygon under the cursor.

  #' Statistics for one metric's current selection.
  kpi_stats <- function(metric, var) {
    if (is.na(var)) return(NULL)
    dat <- map_values(metric, var) %>%
      left_join(urban_lookup, by = "FIPS") %>%
      left_join(locality_names, by = "FIPS") %>%
      filter(!is.na(.data[[var]]))
    if (nrow(dat) == 0) return(NULL)

    v <- dat[[var]]
    list(
      n        = nrow(dat),
      median   = stats::median(v),
      lo       = dat$COUNTY_NAME[which.min(v)],
      lo_val   = min(v),
      hi       = dat$COUNTY_NAME[which.max(v)],
      hi_val   = max(v),
      urban    = stats::median(v[!is.na(dat$Urban) & dat$Urban == 1]),
      rural    = stats::median(v[!is.na(dat$Urban) & dat$Urban == 0])
    )
  }

  output$cost_kpi <- renderUI({
    v <- cost_var()
    st <- kpi_stats("cost", v)
    if (is.null(st)) return(NULL)
    money <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))
    gap <- st$urban - st$rural

    vvn_kpi_row(
      vvn_tile("Statewide median", paste0(money(st$median), " / week"),
               sprintf("%s care, %d localities", COST_LABELS[[v]], st$n),
               accent = TRUE),
      vvn_tile("Urban – rural gap", paste0(money(abs(gap)), " / week"),
               sprintf("urban %s vs rural %s", money(st$urban), money(st$rural))),
      vvn_tile("Most expensive", money(st$hi_val), st$hi),
      vvn_tile("Least expensive", money(st$lo_val), st$lo)
    )
  })

  output$flfpr_kpi <- renderUI({
    st <- kpi_stats("flfpr", FLFPR_VAR)
    if (is.null(st)) return(NULL)
    pct <- function(x) paste0(formatC(x, format = "f", digits = 1), "%")
    gap <- st$urban - st$rural

    vvn_kpi_row(
      vvn_tile("Statewide median", pct(st$median),
               sprintf("mothers with children under 6, %d localities", st$n),
               accent = TRUE),
      vvn_tile("Urban – rural gap",
               paste0(formatC(abs(gap), format = "f", digits = 1), " pts"),
               sprintf("urban %s vs rural %s", pct(st$urban), pct(st$rural))),
      vvn_tile("Highest", pct(st$hi_val), st$hi),
      vvn_tile("Lowest", pct(st$lo_val), st$lo)
    )
  })

  # ---- childcare cost ----------------------------------------------------

  cost_var <- reactive({
    if (identical(input$cost_view, "year")) {
      req(input$cost_type)
    } else {
      types <- input$trend_cost_types
      if (length(types) == 0) NA_character_ else types[1]
    }
  })

  output$cost_card_title <- renderText({
    v <- cost_var()
    if (is.na(v)) return("Select a care type")
    if (identical(input$cost_view, "year")) {
      paste0(COST_LABELS[[v]], " care, weekly median price — ", input$cost_year)
    } else {
      paste0(COST_LABELS[[v]], " care, average weekly median price across all years")
    }
  })

  output$cost_map <- renderLeaflet({
    v <- cost_var()
    render_choropleth(
      "cost", v, grDevices::colorRampPalette(CC_RAMP),
      legend_title = paste0(if (is.na(v)) "" else COST_LABELS[[v]], " care<br>(USD / week)"),
      fmt = function(x) paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","), " / week")
    )
  })

  output$cost_trend <- renderPlotly({
    locs <- selections$cost()
    req(length(locs) > 0)
    types <- req(input$trend_cost_types)

    dat <- RUdata %>%
      filter(COUNTY_NAME %in% locs, STUDYYEAR %in% COST_YEARS) %>%
      select(COUNTY_NAME, STUDYYEAR, all_of(types)) %>%
      pivot_longer(all_of(types), names_to = "Type", values_to = "Cost") %>%
      mutate(Type = factor(COST_LABELS[Type], levels = unname(COST_LABELS))) %>%
      filter(!is.na(Cost))

    validate(need(nrow(dat) > 0, "No cost data for the selected localities."))

    gg <- ggplot(dat, aes(
      x = STUDYYEAR, y = Cost, colour = COUNTY_NAME, group = COUNTY_NAME,
      text = paste0(COUNTY_NAME, "<br>", STUDYYEAR, "<br>$", round(Cost), " / week")
    )) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 1.9) +
      scale_colour_manual(values = vvn_series_colours(locs)) +
      scale_y_continuous(labels = function(x) paste0("$", x)) +
      # Headroom on the right for the end labels added below.
      scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.18))) +
      labs(x = NULL, y = "Weekly median price", colour = NULL) +
      vvn_ggtheme()

    # Direct end-labels. Three of the categorical slots sit below 3:1 contrast
    # on a light surface, so identity must not rest on colour alone; the legend
    # stays as well.
    if (vvn_series_needs_labels(length(locs))) {
      ends <- dat %>%
        group_by(COUNTY_NAME, Type) %>%
        slice_max(STUDYYEAR, n = 1, with_ties = FALSE) %>%
        ungroup()
      gg <- gg + ggplot2::geom_text(
        data = ends, ggplot2::aes(label = COUNTY_NAME),
        hjust = -0.1, size = 2.9, show.legend = FALSE
      )
    }

    if (nlevels(droplevels(dat$Type)) > 1) gg <- gg + facet_wrap(~Type)

    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout()
  })

  # ---- female labor force participation ----------------------------------

  output$flfpr_card_title <- renderText({
    if (identical(input$flfpr_view, "year")) {
      paste0("Participation rate, mothers with children under 6 — ", input$flfpr_year)
    } else {
      "Participation rate, mothers with children under 6 — average across all years"
    }
  })

  output$flfpr_map <- renderLeaflet({
    render_choropleth(
      "flfpr", FLFPR_VAR, grDevices::colorRampPalette(CC_RAMP_ALT),
      legend_title = "Participation<br>rate (%)",
      fmt = function(x) paste0(formatC(x, format = "f", digits = 1), "%")
    )
  })

  output$flfpr_trend <- renderPlotly({
    locs <- selections$flfpr()
    req(length(locs) > 0)

    dat <- RUdata %>%
      filter(COUNTY_NAME %in% locs, STUDYYEAR %in% FLFPR_YEARS) %>%
      select(COUNTY_NAME, STUDYYEAR, all_of(FLFPR_VAR)) %>%
      rename(Rate = all_of(FLFPR_VAR)) %>%
      filter(!is.na(Rate))

    validate(need(nrow(dat) > 0, "No participation data for the selected localities."))

    gg <- ggplot(dat, aes(
      x = STUDYYEAR, y = Rate, colour = COUNTY_NAME, group = COUNTY_NAME,
      text = paste0(COUNTY_NAME, "<br>", STUDYYEAR, "<br>", round(Rate, 1), "%")
    )) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 1.9) +
      scale_colour_manual(values = vvn_series_colours(locs)) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.18))) +
      labs(x = NULL, y = "Participation rate", colour = NULL) +
      vvn_ggtheme()

    if (vvn_series_needs_labels(length(locs))) {
      ends <- dat %>% group_by(COUNTY_NAME) %>%
        slice_max(STUDYYEAR, n = 1, with_ties = FALSE) %>% ungroup()
      gg <- gg + ggplot2::geom_text(
        data = ends, ggplot2::aes(label = COUNTY_NAME),
        hjust = -0.1, size = 2.9, show.legend = FALSE
      )
    }

    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout()
  })
}

shinyApp(ui = ui, server = server)
