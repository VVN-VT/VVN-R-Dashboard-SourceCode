# ============================================================================
# Virginia Corn Condition & Yield Forecast
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(rnassqs)
library(tidyr)

source("vvn_ui.R")

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

CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))

# Condition seasons are bounded by explicit years, never a rolling window. A
# "last five years" rule silently drops the oldest season every January, which
# would have quietly turned the original 2021-2025 into 2022-2026 and lost 2021.
CONDITION_START_YEAR <- 2021

# Match the published dashboard's season list?
#
# TRUE pins it to 2021-2025, exactly the seasons the original offered. FALSE
# extends to the current year, so new seasons appear automatically as NASS
# publishes them (2026 has 18 weeks of data as of this writing).
USE_ORIGINAL_SEASONS <- TRUE
CONDITION_END_YEAR <- if (USE_ORIGINAL_SEASONS) 2025 else CURRENT_YEAR

CONDITION_YEARS <- seq(CONDITION_START_YEAR, CONDITION_END_YEAR)

# The same flag pins the yield and Good+Excellent queries to the year ranges the
# original requested. The original asked NASS for yield 1984-2024 and condition
# ratings 2014-2025, so the yield chart stops at 2024 and the G+E line at 2025
# even though NASS has since published more. Set USE_ORIGINAL_SEASONS to FALSE
# to follow the current year instead.
YIELD_START_YEAR <- 1984
YIELD_END_YEAR   <- if (USE_ORIGINAL_SEASONS) 2024 else CURRENT_YEAR
GE_START_YEAR    <- 2014
GE_END_YEAR      <- if (USE_ORIGINAL_SEASONS) 2025 else CURRENT_YEAR

CONDITION_LEVELS <- c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT")

# The original dashboard's palette, kept exactly: a red -> green ramp so poor
# conditions read warm and good conditions read green. Unchanged so the sidebar
# swatches and the chart stay consistent with the published version.
CONDITION_COLORS <- c(
  "VERY POOR" = "#a50026",
  "POOR"      = "#d73027",
  "FAIR"      = "#fee08b",
  "GOOD"      = "#66bd63",
  "EXCELLENT" = "#1a9850"
)

# ===== DATA ACCESS =========================================================
# Every NASS call is wrapped: a missing key, a rate limit or an outage must
# degrade to an in-app message rather than stopping the app from starting.
# Nothing here runs at load time; it is all driven from reactives below.

empty_condition_data <- function() {
  tibble(
    week      = as.Date(character()),
    value     = numeric(),
    condition = factor(levels = CONDITION_LEVELS),
    year      = character()
  )
}

#' Weekly corn condition percentages for one season.
get_corn_conditions <- function(year) {
  if (!HAS_NASS_KEY) return(empty_condition_data())

  raw <- tryCatch(
    nassqs(list(
      commodity_desc     = "CORN",
      year               = year,
      state_name         = "VIRGINIA",
      statisticcat_desc  = "CONDITION",
      agg_level_desc     = "STATE"
    )),
    error = function(e) NULL
  )

  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) {
    return(empty_condition_data())
  }

  raw %>%
    filter(unit_desc %in% paste("PCT", CONDITION_LEVELS)) %>%
    mutate(
      week      = as.Date(week_ending),
      value     = suppressWarnings(as.numeric(Value)),
      condition = factor(sub("^PCT ", "", unit_desc), levels = CONDITION_LEVELS),
      year      = as.character(year)
    ) %>%
    filter(!is.na(week), !is.na(value)) %>%
    arrange(week, condition) %>%
    select(week, value, condition, year)
}

#' Virginia state corn yield, bushels per acre, one row per year.
get_corn_yield <- function() {
  if (!HAS_NASS_KEY) return(NULL)

  raw <- tryCatch(
    nassqs(list(
      source_desc       = "SURVEY",
      sector_desc       = "CROPS",
      group_desc        = "FIELD CROPS",
      commodity_desc    = "CORN",
      statisticcat_desc = "YIELD",
      unit_desc         = "BU / ACRE",
      agg_level_desc    = "STATE",
      state_alpha       = "VA",
      year              = YIELD_START_YEAR:YIELD_END_YEAR
    )),
    error = function(e) NULL
  )

  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) return(NULL)

  raw %>%
    filter(!grepl("FORECAST|PROJECTED", short_desc, ignore.case = TRUE)) %>%
    mutate(
      Year  = suppressWarnings(as.integer(year)),
      Value = suppressWarnings(as.numeric(Value))
    ) %>%
    filter(!is.na(Year), !is.na(Value)) %>%
    group_by(Year) %>%
    slice(1) %>%
    ungroup() %>%
    select(Year, Value) %>%
    arrange(Year)
}

#' End-of-season Good+Excellent share by year, the predictor for the G+E model.
get_good_excellent <- function() {
  if (!HAS_NASS_KEY) return(NULL)

  raw <- tryCatch(
    nassqs(list(
      source_desc       = "SURVEY",
      sector_desc       = "CROPS",
      group_desc        = "FIELD CROPS",
      commodity_desc    = "CORN",
      statisticcat_desc = "CONDITION",
      agg_level_desc    = "STATE",
      state_alpha       = "VA",
      year              = GE_START_YEAR:GE_END_YEAR
    )),
    error = function(e) NULL
  )

  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) return(NULL)

  raw %>%
    filter(grepl("MEASURED IN PCT GOOD|MEASURED IN PCT EXCELLENT",
                 short_desc, ignore.case = TRUE)) %>%
    mutate(
      Year  = suppressWarnings(as.integer(year)),
      week  = as.Date(week_ending),
      value = suppressWarnings(as.numeric(Value))
    ) %>%
    filter(!is.na(Year), !is.na(week), !is.na(value)) %>%
    # Good + Excellent within a week, then the season's final reading.
    group_by(Year, week) %>%
    summarise(GE = sum(value), .groups = "drop") %>%
    group_by(Year) %>%
    slice_max(week, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(Year, GE)
}

#' Mean peak EVI over June-August, one row per year.
#'
#' Landsat vegetation indices for Virginia corn area, from the upstream DSPG
#' project (see Crop_Data/README.md). The satellite filter is theirs and is
#' deliberate: it selects exactly ONE sensor per year with no gaps and no
#' duplicates -- Landsat 5 for 1984-2011, Landsat 7 for 2012 alone to bridge the
#' L5-retirement/L8-launch gap, Landsat 8 from 2013. Landsat 4 and 9 are dropped
#' as redundant with 5 and 8.
#'
#' Caveat carried over from upstream: the sensors are not cross-calibrated.
#' Mean EVI is about 3.4% higher on Landsat 8 than Landsat 5, so a small step
#' sits at the 2012/2013 boundary. The year term in the model absorbs part of it.
load_evi <- local({
  value <- NULL; done <- FALSE
  function() {
    if (done) return(value)
    done <<- TRUE

    value <<- tryCatch({
      hist <- read.csv(file.path("Crop_Data", "VA_multi.csv"), fill = TRUE)
      cur  <- read.csv(file.path("Crop_Data", "2025_VI.csv"), fill = TRUE)
      hist <- hist[, !names(hist) %in% "has_blue_band"]

      bind_rows(hist, cur) %>%
        pivot_wider(
          id_cols     = c(satellite, year),
          names_from  = month_name,
          values_from = peak_EVI,
          names_glue  = "peak_EVI_{month_name}"
        ) %>%
        mutate(mEVI_678 = rowMeans(
          across(any_of(c("peak_EVI_Jun", "peak_EVI_Jul", "peak_EVI_Aug"))),
          na.rm = TRUE
        )) %>%
        filter(
          satellite != "Landsat4_TM",
          satellite != "Landsat9_OLI2",
          !(satellite == "Landsat7_ETM" & year != 2012)
        ) %>%
        filter(!is.na(mEVI_678)) %>%
        transmute(Year = as.integer(year), mEVI_678)
    }, error = function(e) NULL)

    value
  }
})

#' Root-mean-square error under leave-one-out cross-validation.
#' The EVI and G+E lines are regression fits, not true out-of-sample forecasts,
#' so reporting the in-sample residual would flatter them. This measures how far
#' off each prediction is when its own year is held out of the fit, which is the
#' number actually worth showing a user.
loo_rmse <- function(formula, data) {
  n <- nrow(data)
  if (n < 5) return(NA_real_)
  err <- vapply(seq_len(n), function(i) {
    fit <- tryCatch(lm(formula, data = data[-i, , drop = FALSE]), error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    as.numeric(predict(fit, newdata = data[i, , drop = FALSE])) - data$Value[i]
  }, numeric(1))
  sqrt(mean(err^2, na.rm = TRUE))
}

#' Centred message used when NASS is unreachable or returns nothing.
plotly_message <- function(text) {
  plot_ly() %>%
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      annotations = list(list(
        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        text = text, showarrow = FALSE, align = "center",
        font = list(size = 14, color = VVN_MUTED)
      ))
    ) %>%
    config(displayModeBar = FALSE)
}

NO_KEY_MESSAGE <- paste(
  "No USDA NASS API key found.<br>",
  "Request a free key at quickstats.nass.usda.gov/api<br>",
  "and add NASS_API_KEY=your-key to ~/.Renviron."
)


# ===== UI ==================================================================

ui <- page_fluid(
  title = "Virginia Corn Condition & Yield Forecast",
  theme = vvn_theme(),
  tags$head(vvn_css()),

  vvn_header(
    "Virginia Corn Condition & Yield Forecast",
    "Weekly crop condition ratings and state-level yield models, drawn live from the USDA National Agricultural Statistics Service."
  ),

  navset_tab(
    # ---------------------------------------------------------------- tab 1
    nav_panel(
      "Corn conditions",
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          vvn_group("Season"),
          selectInput(
            "condition_year", NULL,
            choices  = rev(as.character(CONDITION_YEARS)),
            selected = as.character(CONDITION_END_YEAR)
          ),
          vvn_help(
            "Each band is the share of Virginia corn acreage in that condition.",
            "Bands sum to 100% in every reporting week."
          ),

          vvn_group("Condition scale"),
          div(
            class = "vvn-help",
            lapply(rev(CONDITION_LEVELS), function(lvl) {
              div(
                style = "display:flex; align-items:center; gap:8px; margin-bottom:3px;",
                span(style = sprintf(
                  "width:12px; height:12px; border-radius:3px; background:%s; flex:none;",
                  CONDITION_COLORS[[lvl]]
                )),
                tools::toTitleCase(tolower(lvl))
              )
            })
          )
        ),

        card(
          full_screen = TRUE,
          card_header(textOutput("condition_title", inline = TRUE)),
          card_body(
            padding = 0,
            plotlyOutput("condition_plot", height = "460px")
          )
        ),

        vvn_notes(
          "Reading this chart",
          p(
            "Weekly corn condition ratings for Virginia, as a stacked area chart using the five NASS categories:",
            strong("excellent, good, fair, poor"), "and", strong("very poor"), ".",
            "The thickness of each band is the proportion of acreage in that condition, so the bands always total 100%."
          ),
          p(
            "The current season fills in week by week as NASS publishes its reports, so recent years will be shorter than",
            "completed ones. Switching seasons shows how within-season stress periods compare across years; hover any point",
            "for the exact percentage, and use the camera button to download the chart as a PNG."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 2
    nav_panel(
      "Yield forecast",
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          vvn_group("Series shown"),
          div(
            class = "vvn-help",
            div(style = "margin-bottom:6px;",
                strong("Actual yield"), br(), "Reported NASS outcome, by year."),
            div(style = "margin-bottom:6px;",
                strong("Trend yield"), br(), "Long-run linear trend in productivity."),
            div(strong("Good + Excellent model"), br(),
                "Yield predicted from the season's final Good+Excellent share.")
          ),
          vvn_help(
            "The trend line extends one year past the last reported yield to give a",
            "baseline expectation for the season in progress."
          )
        ),

        card(
          full_screen = TRUE,
          card_header("Virginia corn yield: reported, trend and condition-based model"),
          card_body(
            padding = 0,
            plotlyOutput("yield_plot", height = "460px")
          )
        ),

        vvn_notes(
          "Reading this chart",
          p(
            "Virginia state-level corn yield in bushels per acre from", strong(YIELD_START_YEAR),
            "to the most recent reported season, with two model lines for comparison:"
          ),
          tags$ul(
            tags$li(strong("Actual yield"),
                    " — reported outcomes from the USDA NASS survey."),
            tags$li(strong("Trend yield"),
                    " — a linear trend over the whole record, capturing gradual productivity gains from genetics and practice."),
            tags$li(strong("EVI model"),
                    " — yield regressed on the long-run trend plus mean peak Enhanced Vegetation Index over June, July and August, from Landsat imagery of Virginia corn area."),
            tags$li(strong("Good + Excellent model"),
                    " — yield regressed on the share of the crop rated Good or Excellent at the end of each season, fitted from 2014 onward.")
          ),
          p(
            "Gaps between the actual and trend lines mark unusually good or poor seasons, such as drought years.",
            "Satellite greenness carries real signal: adding it to the trend lifts explained variance from",
            "roughly 48% to 60%, and it still improves accuracy when each year is held out of the fit."
          ),
          p(
            strong("Read the model lines with care."), "They are fitted across the whole record rather than",
            "predicted forward, so they describe a historical relationship rather than forecasting an unseen",
            "season. The legend gives each model's leave-one-out error, and 2024 shows the limits plainly —",
            "the EVI model expected a near-average harvest and the actual yield came in far below it."
          ),
          p(
            class = "vvn-help",
            "Yields and condition ratings come live from the USDA NASS Quick Stats API.",
            "Vegetation indices are Landsat summaries from the DSPG crop project (see Crop_Data/README.md);",
            "the sensors are not cross-calibrated, leaving a small step where Landsat 5 gives way to Landsat 8 after 2012."
          )
        )
      )
    )
  )
)

# ===== SERVER ==============================================================

server <- function(input, output, session) {

  # --- condition data, fetched once per season and then cached -------------
  condition_cache <- reactiveValues()

  condition_data <- reactive({
    yr <- req(input$condition_year)
    if (is.null(condition_cache[[yr]])) {
      condition_cache[[yr]] <- get_corn_conditions(as.integer(yr))
    }
    condition_cache[[yr]]
  })

  output$condition_title <- renderText({
    paste("Virginia corn conditions,", req(input$condition_year))
  })

  output$condition_plot <- renderPlotly({
    if (!HAS_NASS_KEY) return(plotly_message(NO_KEY_MESSAGE))

    df <- condition_data()
    if (nrow(df) == 0) {
      return(plotly_message(paste0(
        "No condition reports published for ", input$condition_year, " yet.<br>",
        "NASS reports weekly through the growing season."
      )))
    }

    plot_ly(
      data       = df,
      x          = ~week,
      y          = ~value,
      color      = ~condition,
      colors     = CONDITION_COLORS,
      type       = "scatter",
      mode       = "none",
      stackgroup = "one",
      hovertemplate = paste0(
        "<b>%{fullData.name}</b><br>",
        "Week ending %{x|%b %d, %Y}<br>",
        "%{y}% of acreage<extra></extra>"
      )
    ) %>%
      vvn_plotly_layout(xlab = "Week ending", ylab = "Share of acreage (%)") %>%
      layout(
        yaxis  = list(range = c(0, 100), ticksuffix = "%", gridcolor = VVN_RULE),
        legend = list(traceorder = "reversed", orientation = "h", y = -0.2, x = 0)
      )
  })

  # --- yield models -------------------------------------------------------
  yield_models <- reactive({
    yield <- get_corn_yield()
    if (is.null(yield) || nrow(yield) < 10) return(NULL)

    # Linear trend over the full record.
    yield$year_index <- yield$Year - min(yield$Year)
    trend_model <- lm(Value ~ year_index, data = yield)
    yield$trend <- predict(trend_model, newdata = yield)

    # Extend the trend one year past the last reported yield.
    next_year <- max(yield$Year) + 1
    trend_line <- bind_rows(
      yield %>% select(Year, trend),
      tibble(
        Year  = next_year,
        trend = predict(trend_model,
                        newdata = data.frame(year_index = next_year - min(yield$Year)))
      )
    )

    # Good+Excellent regression, fitted only on years with both yield and ratings.
    ge <- get_good_excellent()
    ge_line <- NULL
    ge_err <- NA_real_
    if (!is.null(ge) && nrow(ge) > 0) {
      fit_data <- inner_join(yield %>% select(Year, Value), ge, by = "Year")
      if (nrow(fit_data) > 5) {
        ge_model <- lm(Value ~ GE, data = fit_data)
        ge_err <- loo_rmse(Value ~ GE, fit_data)
        ge_line <- ge %>%
          mutate(predicted = as.numeric(predict(ge_model, newdata = ge))) %>%
          arrange(Year)
      }
    }

    # EVI model: yield on the long-run trend plus June-August satellite
    # greenness, which is upstream's specification.
    evi <- load_evi()
    evi_line <- NULL
    evi_err <- NA_real_
    if (!is.null(evi) && nrow(evi) > 0) {
      fit_data <- yield %>%
        select(Year, Value, year_index) %>%
        inner_join(evi, by = "Year")
      if (nrow(fit_data) > 10) {
        evi_model <- lm(Value ~ year_index + mEVI_678, data = fit_data)
        evi_err <- loo_rmse(Value ~ year_index + mEVI_678, fit_data)
        evi_line <- fit_data %>%
          mutate(predicted = as.numeric(predict(evi_model, newdata = fit_data))) %>%
          select(Year, predicted) %>%
          arrange(Year)
      }
    }

    list(
      yield = yield, trend = trend_line,
      ge = ge_line, ge_err = ge_err,
      evi = evi_line, evi_err = evi_err
    )
  })

  output$yield_plot <- renderPlotly({
    if (!HAS_NASS_KEY) return(plotly_message(NO_KEY_MESSAGE))

    models <- yield_models()
    if (is.null(models)) {
      return(plotly_message(
        "Yield data could not be retrieved from USDA NASS.<br>Please try again shortly."
      ))
    }

    p <- plot_ly() %>%
      add_trace(
        data = models$trend, x = ~Year, y = ~trend,
        name = "Trend yield", type = "scatter", mode = "lines",
        line = list(color = VVN_MUTED, dash = "dash", width = 2),
        hovertemplate = "Trend %{x}<br>%{y:.1f} bu/acre<extra></extra>"
      )

    # EVI model, drawn in the original's green. The series name carries its own
    # measured out-of-sample error so the legend cannot imply more precision
    # than the model has.
    if (!is.null(models$evi)) {
      p <- p %>%
        add_trace(
          data = models$evi, x = ~Year, y = ~predicted,
          name = if (is.na(models$evi_err)) "EVI model (fitted)" else
            sprintf("EVI model (fitted, ±%.0f bu/acre)", models$evi_err),
          type = "scatter", mode = "lines+markers",
          line   = list(color = "#1a9850", dash = "dot", width = 2),
          marker = list(color = "#1a9850", size = 5),
          hovertemplate = "EVI model %{x}<br>%{y:.1f} bu/acre<extra></extra>"
        )
    }

    if (!is.null(models$ge)) {
      p <- p %>%
        add_trace(
          data = models$ge, x = ~Year, y = ~predicted,
          name = if (is.na(models$ge_err)) "Good + Excellent model (fitted)" else
            sprintf("Good + Excellent model (fitted, ±%.0f bu/acre)", models$ge_err),
          type = "scatter", mode = "lines+markers",
          line   = list(color = VVN_ORANGE, dash = "dashdot", width = 2),
          marker = list(color = VVN_ORANGE, size = 5),
          hovertemplate = "G+E model %{x}<br>%{y:.1f} bu/acre<extra></extra>"
        )
    }

    # Actual yield drawn last so it reads on top of the model lines.
    p %>%
      add_trace(
        data = models$yield, x = ~Year, y = ~Value,
        name = "Actual yield", type = "scatter", mode = "lines+markers",
        line   = list(color = VVN_MAROON, width = 2.5),
        marker = list(color = VVN_MAROON, size = 6),
        hovertemplate = "Actual %{x}<br>%{y:.1f} bu/acre<extra></extra>"
      ) %>%
      vvn_plotly_layout(xlab = NULL, ylab = "Yield (bushels per acre)") %>%
      layout(
        xaxis = list(dtick = 5, gridcolor = VVN_RULE),
        hovermode = "x unified"
      )
  })
}

shinyApp(ui = ui, server = server)
