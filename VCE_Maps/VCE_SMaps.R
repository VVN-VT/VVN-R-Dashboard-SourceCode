# ============================================================================
# Virginia Cooperative Extension Interactive Maps
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(sf)
library(tigris)
library(leaflet)
library(htmltools)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(scales)

source("vvn_ui.R")

# This dashboard's own sequential ramp. Every dashboard in the suite uses a
# different hue on the same lightness ladder, so the seven are distinguishable
# at a glance while still reading as one family -- see VVN_RAMPS.
VCE_RAMP     <- vvn_ramp("teal")
VCE_RAMP_ALT <- vvn_ramp("lavender")

options(tigris_use_cache = TRUE)

DATA_PATH  <- "VCE_data"

dpath <- function(...) file.path(DATA_PATH, ...)

# --- Participant total: CORRECTED, and deliberately unlike the original ------
#
# Every 4-H participant is recorded once under an ethnicity category AND once
# under a race category. The two blocks are two views of the same people, and
# each sums independently to the real headcount -- verified across all 110
# reporting localities, with a maximum gap of zero. Albemarle is 258 either
# way: ethnicity 12+236+0+10, race 229+0+1+11+17+0+0.
#
# The upstream dashboard totals BOTH blocks:
#
#   Total = rowSums(select(., starts_with("e"), starts_with("r")), na.rm = TRUE)
#   -- dashboard_take_2_july_28.R:372, verified against the source snapshot
#
# starts_with("e") catches the four ethnicity columns and starts_with("r") the
# seven race columns, so every participant is counted twice. The result is
# exactly 2.00x the truth for every locality: 67,612 statewide against a real
# 33,806. It also halves volunteers-per-participant, since that metric divides
# by this total.
#
# FALSE now, so the race block alone is the headcount. This is the one place
# these dashboards deliberately disagree with the published DSPG figures, and
# the disagreement is a factor of two. The notes on the 4-H tab say so, because
# a reader comparing the two dashboards will otherwise think one is broken.
USE_ORIGINAL_TOTALS <- FALSE
FOURH_ETHNICITY <- c(
  "Hispanic"                      = "eHispanic",
  "Not Hispanic"                  = "eNotHispanic",
  "Ethnicity not provided"        = "eNotProvided",
  "Prefer not to state ethnicity" = "ePreferNotToState"
)

FOURH_RACE <- c(
  "White"                              = "rWhite",
  "Black or African American"          = "rBlack",
  "American Indian or Alaska Native"   = "rIndianAlaskan",
  "Native Hawaiian or Pacific Islander" = "rHawaiianIslander",
  "Asian"                              = "rAsian",
  "Two or more races"                   = "rMoreThanOne",
  "Race undetermined"                   = "rUndetermined"
)

FOURH_GROUPS <- c(FOURH_RACE, FOURH_ETHNICITY)

VCE_PARTICIPANT_GROUPS <- c(
  "All participants" = "participants_total",
  "Hispanic"         = "participants_ethnicity_hispanic",
  "White"            = "participants_race_white",
  "Black"            = "participants_race_black",
  "Asian"            = "participants_race_asian"
)

VCE_VOLUNTEER_GROUPS <- c(
  "All volunteers" = "all",
  "Hispanic"       = "eth_hispanic",
  "White"          = "race_white",
  "Black"          = "race_black",
  "Asian"          = "race_asian"
)

# Values as they appear in the volunteer workbook.
VCE_VOLUNTEER_FILTER <- list(
  eth_hispanic = list(col = "Ethnicity", value = "Hispanic or Latino/a/x"),
  race_white   = list(col = "Race",      value = "05. White"),
  race_black   = list(col = "Race",      value = "03. Black or African American"),
  race_asian   = list(col = "Race",      value = "02. Asian")
)

VIEWS <- c(
  "Participants"            = "participant",
  "Volunteers"              = "volunteer",
  "Volunteers per participant" = "ratio"
)

# ===== DATA ================================================================
# Loaded once per process, not once per session. The previous version did all
# of this inside server(), so every connected user triggered two separate
# tigris downloads of the same boundaries plus three Excel reads.

# VCE administrative units covering a county together with the independent city
# inside it. Attributed to the county, which is the larger component.
# ("James" / "Charles" are handled by vvn_clean_locality, which repairs that
# truncation for every app.)
# Names deliberately left unmatched: "State Office" (not a locality), the
# "(INACTIVE)" record, and ~24 out-of-state counties in the participation file
# (Baltimore, District of Columbia, Tarrant, ...) which are genuinely not
# Virginia localities.
VCE_ALIASES <- c(
  "greensville/emporia"    = "greensville",
  "henry/martinsville"     = "henry",
  "york/city of poquoson"  = "york"
)

#' Resolve a locality name, applying the VCE-specific aliases first.
resolve_vce <- function(x, shapes) {
  key <- tolower(trimws(as.character(x)))
  aliased <- ifelse(key %in% names(VCE_ALIASES), VCE_ALIASES[key], x)
  vvn_resolve_locality(aliased, shapes)
}

load_boundaries <- local({
  value <- NULL; done <- FALSE
  function() {
    if (!done) {
      value <<- tryCatch(
        counties("VA", cb = TRUE, year = 2023, progress_bar = FALSE) %>%
          st_transform(4326) %>%
          vvn_locality_key(),
        error = function(e) NULL
      )
      done <<- TRUE
    }
    value
  }
})

#' Pull "Label: 12" counts out of a packed demographic string into columns.
expand_counts <- function(df, column, labels) {
  for (lbl in labels) {
    target <- paste0(gsub("[^A-Za-z]", "_", lbl), "_n")
    matched <- str_match(df[[column]], paste0(lbl, ":\\s*(\\d+)"))[, 2]
    df[[target]] <- ifelse(is.na(matched), 0, as.numeric(matched))
  }
  df
}

VOL_ETHNICITIES <- c("Hispanic", "Non-Hispanic")
VOL_RACES       <- c("White", "Black", "Asian", "Undetermined")

#' Everything the app needs, resolved against the boundary list in one place
#' so locality-matching problems surface here rather than as blank map areas.
load_tables <- local({
  value <- NULL; done <- FALSE
  function() {
    if (done) return(value)
    done <<- TRUE
    shapes <- load_boundaries()
    if (is.null(shapes)) return(value)

    res <- function(x) resolve_vce(x, shapes)

    # --- 4-H participants ---
    fourh <- tryCatch({
      read_csv(dpath("annualprogreport.csv"), show_col_types = FALSE, progress = FALSE) %>%
        mutate(
          locality_key = res(CountyArea),
          across(all_of(unname(FOURH_GROUPS)), ~ suppressWarnings(as.numeric(.x)))
        ) %>%
        filter(!is.na(locality_key)) %>%
        mutate(Total = if (USE_ORIGINAL_TOTALS) {
          # Published behaviour: both demographic blocks summed (double counts).
          rowSums(across(c(all_of(unname(FOURH_ETHNICITY)),
                           all_of(unname(FOURH_RACE)))), na.rm = TRUE)
        } else {
          rowSums(across(all_of(unname(FOURH_RACE))), na.rm = TRUE)
        }) %>%
        group_by(locality_key) %>%
        summarise(across(c(all_of(unname(FOURH_GROUPS)), Total),
                         ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    }, error = function(e) NULL)

    # --- 4-H volunteers ---
    fourh_vol <- tryCatch({
      read_excel(dpath("4H_Volunteers.xlsx")) %>%
        mutate(locality_key = res(CountyOrCity)) %>%
        filter(!is.na(locality_key)) %>%
        expand_counts("Ethnicity", VOL_ETHNICITIES) %>%
        expand_counts("Race", VOL_RACES) %>%
        rename(Volunteers = `4-H Volunteers`) %>%
        mutate(Volunteers = suppressWarnings(as.numeric(Volunteers))) %>%
        group_by(locality_key) %>%
        summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    }, error = function(e) NULL)

    # --- VCE participants ---
    vce_part <- tryCatch({
      read_excel(dpath("vce_particip_demographics.xlsx")) %>%
        mutate(
          locality_key = res(site_county),
          across(all_of(unname(VCE_PARTICIPANT_GROUPS)), ~ suppressWarnings(as.numeric(.x)))
        ) %>%
        filter(!is.na(locality_key)) %>%
        group_by(locality_key) %>%
        summarise(across(all_of(unname(VCE_PARTICIPANT_GROUPS)),
                         ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    }, error = function(e) NULL)

    # --- VCE volunteers (one row per volunteer) ---
    vce_vol <- tryCatch({
      raw <- read_excel(dpath("VCE_VolunteerCounty_Data.xlsx")) %>%
        filter(!is.na(CountyOrCity), suppressWarnings(as.numeric(HoursWorked)) > 0) %>%
        mutate(
          locality_key = res(CountyOrCity),
          Race      = `CF - Demographic Information - Race`,
          Ethnicity = `CF - Demographic Information - Ethnicity`
        ) %>%
        filter(!is.na(locality_key))
      raw
    }, error = function(e) NULL)

    # --- VCE volunteers vs participants ---
    vce_ratio <- tryCatch({
      read_excel(dpath("PEARS_BI_with_NewVolunteers.xlsx")) %>%
        mutate(
          locality_key = res(County),
          Participants = suppressWarnings(as.numeric(Participants)),
          Volunteers   = suppressWarnings(as.numeric(NewVolunteers))
        ) %>%
        filter(!is.na(locality_key)) %>%
        group_by(locality_key) %>%
        summarise(
          Participants = sum(Participants, na.rm = TRUE),
          Volunteers   = sum(Volunteers, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(ratio = ifelse(Participants > 0, 100 * Volunteers / Participants, NA_real_))
    }, error = function(e) NULL)

    value <<- list(
      shapes = shapes, fourh = fourh, fourh_vol = fourh_vol,
      vce_part = vce_part, vce_vol = vce_vol, vce_ratio = vce_ratio
    )
    value
  }
})

# ===== UI ==================================================================

#' Sidebar for one programme tab. `prog` is "fourh" or "vce".
programme_sidebar <- function(prog) {
  sidebar(
    width = 320,

    # No "compared with the state" control here. Total participants would
    # support it (median 152, a clean 55/55 split), but the demographic
    # breakdowns do not: 4-H Asian enrolment has a median of ZERO across
    # localities, so a percentage difference is undefined and the map comes out
    # one-sided -- 50 localities above, none below. A control that silently
    # stops meaning anything depending on the dropdown is worse than no control.
    vvn_group("View"),
    div(
      class = "vvn-chooser",
      # A real Shiny input, not the previous version's hand-written jQuery that
      # called $('.btn-outline-primary').removeClass('active') -- a selector
      # shared by both tabs, so switching view in one tab cleared the other's
      # highlight and the two tabs fought over the same CSS state.
      radioGroupButtons(
        inputId   = paste0(prog, "_view"),
        label     = NULL,
        choices   = VIEWS,
        selected  = "participant",
        direction = "vertical",
        size      = "sm",
        checkIcon = list(yes = icon("check")),
        width     = "100%"
      )
    ),

    conditionalPanel(
      condition = sprintf("input.%s_view == 'participant'", prog),
      vvn_group("Demographic group"),
      selectInput(
        paste0(prog, "_participant_group"), NULL,
        choices = if (prog == "fourh") FOURH_GROUPS else VCE_PARTICIPANT_GROUPS
      )
    ),

    conditionalPanel(
      condition = sprintf("input.%s_view == 'volunteer'", prog),
      vvn_group("Demographic group"),
      selectInput(
        paste0(prog, "_volunteer_group"), NULL,
        choices = if (prog == "fourh") {
          c("All volunteers" = "Volunteers",
            stats::setNames(paste0(VOL_ETHNICITIES, "_n"), VOL_ETHNICITIES),
            stats::setNames(paste0(VOL_RACES, "_n"), VOL_RACES))
        } else VCE_VOLUNTEER_GROUPS
      )
    ),

    conditionalPanel(
      condition = sprintf("input.%s_view == 'ratio'", prog),
      vvn_help(
        "Volunteers as a percentage of participants. Higher values mean more",
        "volunteer capacity behind each participant; localities with no",
        "recorded participants are left unshaded."
      )
    )
  )
}

programme_panel <- function(prog, title, notes) {
  layout_sidebar(
    sidebar = programme_sidebar(prog),
    card(
      full_screen = TRUE,
      card_header(textOutput(paste0(prog, "_title"), inline = TRUE)),
      card_body(padding = 0, leafletOutput(paste0(prog, "_map"), height = "560px"))
    ),
    notes
  )
}

ui <- page_fluid(
  title = "Virginia Cooperative Extension Interactive Maps",
  theme = vvn_theme(),
  tags$head(vvn_css()),

  vvn_header(
    "Virginia Cooperative Extension",
    "Who 4-H and Extension programmes reach across Virginia, and where volunteer capacity sits relative to demand."
  ),

  navset_tab(
    nav_panel(
      "4-H programmes",
      programme_panel(
        "fourh", "4-H",
        vvn_notes(
          "Reading these maps",
          p(
            strong("Participants"), "shades each locality by 4-H enrolment in the selected demographic group.",
            "Participants are reported once under a race category and once under an ethnicity category, so the two",
            "blocks describe the same people from different angles — the tooltip's total is the race-based count."
          ),
          p(
            strong("These totals are half the figure the original 4-H dashboard shows, and that is deliberate."),
            "Because each participant appears once in the race block and once in the ethnicity block, the two",
            "blocks each sum independently to the real headcount — identical for all 110 reporting localities.",
            "The original dashboard adds both blocks together, which counts every young person twice: 67,612",
            "statewide against a true 33,806, a factor of exactly two in every locality. The figures here count",
            "the race block only, so they are headcounts."
          ),
          p(
            "The same correction raises", strong("volunteers per participant"), "by a factor of two, because that",
            "metric divides by this total. A locality reading 4 volunteers per 100 participants on the original",
            "dashboard reads 8 here, and 8 is the real rate."
          ),
          p(
            strong("Volunteers"), "shows adult 4-H volunteers by group, and",
            strong("volunteers per participant"), "divides the two to show where volunteer capacity is thin",
            "relative to the number of young people enrolled — the clearest signal for where recruitment",
            "effort would do the most good."
          ),
          p(
            class = "vvn-help",
            "Source: VCE annual programme report and 4-H volunteer records. Localities shown in grey have no record",
            "in the underlying file."
          )
        )
      )
    ),

    nav_panel(
      "Extension programmes",
      programme_panel(
        "vce", "VCE",
        vvn_notes(
          "Reading these maps",
          p(
            strong("Participants"), "shades localities by Extension programme participation, either in total or",
            "for a single demographic group, making it easy to see which communities programmes are reaching."
          ),
          p(
            strong("Volunteers"), "counts individual volunteers with recorded hours, and",
            strong("volunteers per participant"), "expresses volunteers as a share of participants."
          ),
          p(
            class = "vvn-help",
            "Source: VCE participation demographics, county volunteer records and the PEARS business-intelligence",
            "extract. Independent cities are matched separately from the county of the same name."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 3
    nav_panel(
      "Who 4-H reaches",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          vvn_group("Locality"),
          selectInput("dem_locality", NULL, choices = NULL),
          vvn_help(
            "The demographic make-up of 4-H enrolment, for the selected locality",
            "and for the state as a whole."
          ),
          vvn_help(
            "Race and ethnicity are reported separately and each block counts",
            "every participant once, so they are shown as two charts rather",
            "than one. They are not added together."
          )
        ),
        uiOutput("dem_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("dem_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("dem_race", height = "360px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Enrolment by ethnicity"),
            card_body(padding = 0, plotlyOutput("dem_eth", height = "280px"))
          )
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Largest 4-H programmes by locality"),
            card_body(padding = 0, plotlyOutput("dem_top", height = "420px"))
          )
        ),
        vvn_notes(
          "Reading these charts",
          p(
            "Each bar pair compares the selected locality with the statewide pattern, as a", strong("share"), "of",
            "enrolment rather than a count, so a small rural locality can be compared with Fairfax without the",
            "comparison collapsing into a difference in size."
          ),
          p(
            strong("Race and ethnicity are two separate questions,"), "not one. Every participant is recorded once",
            "under a race category and once under an ethnicity category, so each block independently sums to the",
            "real headcount. Adding the two blocks together would count every child twice — which is exactly the",
            "error that doubles the published statewide total."
          ),
          p(
            class = "vvn-help",
            "Categories with no enrolment in the selected locality are omitted from its bars rather than drawn at",
            "zero. Source: VCE annual programme report."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 4
    nav_panel(
      "Volunteer capacity",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          vvn_group("Programme"),
          radioButtons("cap_prog", NULL,
                       choices = c("Extension (PEARS)" = "vce", "4-H" = "fourh"),
                       selected = "vce"),
          sliderInput("cap_n", "Localities to rank", min = 5, max = 30,
                      value = 15, step = 5, ticks = FALSE),
          vvn_help(
            "The slider sets how many localities the ranking below lists \u2014 and",
            "those same localities are highlighted in the scatter above it."
          ),
          vvn_help(
            "Volunteers against participants, one point per locality. The",
            "diagonal is where the two are equal: points below it have more",
            "participants than volunteers."
          )
        ),
        uiOutput("cap_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("cap_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("cap_scatter", height = "460px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header(textOutput("cap_rank_title", inline = TRUE)),
            card_body(padding = 0, plotlyOutput("cap_rank", height = "420px"))
          )
        ),
        vvn_notes(
          "Reading these charts",
          p(
            "The map tab shades volunteers per participant by locality; this view puts the two measures on their",
            "own axes so the", em("relationship"), "is visible. A locality can have few volunteers simply because it has",
            "few participants, and only a scatter separates that from genuinely thin capacity."
          ),
          p(
            "Both axes are on a log scale, because Virginia localities span three orders of magnitude in programme",
            "size. On a linear axis the handful of large localities would compress everything else into the corner."
          ),
          p(
            class = "vvn-help",
            "The ranking lists localities with the lowest volunteers-per-participant ratio among those with at",
            "least 25 recorded participants, so a locality with three participants and no volunteers does not",
            "dominate it. Those same localities are drawn in colour in the scatter above, so the slider moves both",
            "charts and you can see where the thinnest-capacity localities sit in the overall distribution."
          )
        )
      )
    )
  )
)

# ===== SERVER ==============================================================

server <- function(input, output, session) {

  tables <- reactive(load_tables())

  #' Draw a choropleth from a value column already joined onto the shapes.
  #' Draw a choropleth. `palette` is a sequential ramp (VCE_RAMP for head
  #' counts, VCE_RAMP_ALT for derived ratios) -- one hue, light to dark, so the
  #' reader sees magnitude in the colour. Every map in this app previously used
  #' a different ColorBrewer scheme (Purples, YlGnBu, OrRd, YlOrRd), which made
  #' the six views look unrelated and implied the measures differed in kind when
  #' they did not.
  draw_map <- function(dat, values, legend_title, labels,
                       palette = VCE_RAMP, mode = "level") {
    validate(need(any(!is.na(values)), "No data available for this selection."))

    # One call covers both shading modes and the degenerate-spread fallback that
    # colorBin would otherwise raise on.
    shading <- vvn_map_shading(values, mode, seq_palette = palette,
                               level_title = legend_title)
    shade <- shading$values
    pal <- shading$pal

    # The comparison is appended to whatever label the caller built, so every
    # view gains it without each call site repeating the formatting.
    labels <- paste0(labels, vvn_deviation_text(shading$deviation, shading$compare))

    leaflet(dat) %>%
      vvn_basemap() %>%
      addPolygons(
        fillColor = ifelse(is.na(shade), VVN_NO_DATA, pal(shade)),
        fillOpacity = 0.92,
        color = VVN_STROKE_COLOUR, weight = VVN_STROKE_WEIGHT,
        opacity = VVN_STROKE_OPACITY,
        label = lapply(labels, HTML),
        labelOptions = vvn_labelopts(),
        highlightOptions = vvn_highlight()
      ) %>%
      vvn_outline() %>%
      addLegend(
        position = "bottomright", pal = pal,
        values = shade[is.finite(shade)],
        title = shading$title, labFormat = shading$fmt,
        opacity = 1, na.label = "No record"
      )
  }

  count_label <- function(locality, group_label, value, total_label, total) {
    paste0(
      "<strong>", locality, "</strong><br>",
      group_label, ": <strong>", ifelse(is.na(value), "&mdash;", format(value, big.mark = ",")),
      "</strong><br>",
      total_label, ": ", ifelse(is.na(total), "&mdash;", format(total, big.mark = ","))
    )
  }

  # ---- 4-H ---------------------------------------------------------------

  output$fourh_title <- renderText({
    switch(req(input$fourh_view),
      participant = "4-H participants by locality",
      volunteer   = "4-H volunteers by locality",
      ratio       = "4-H volunteers as a share of participants"
    )
  })

  output$fourh_map <- renderLeaflet({
    tb <- tables()
    validate(need(!is.null(tb), "County boundaries could not be downloaded. Please try again shortly."))
    view <- req(input$fourh_view)

    if (view == "participant") {
      group <- req(input$fourh_participant_group)
      validate(need(!is.null(tb$fourh), "4-H participation data could not be loaded."))
      dat <- tb$shapes %>% left_join(tb$fourh, by = "locality_key")
      values <- dat[[group]]
      label <- names(FOURH_GROUPS)[FOURH_GROUPS == group]
      draw_map(dat, values, "4-H<br>participants",
               count_label(dat$locality, label, values, "Total participants", dat$Total),
               VCE_RAMP, input$fourh_mode %||% "level")

    } else if (view == "volunteer") {
      group <- req(input$fourh_volunteer_group)
      validate(need(!is.null(tb$fourh_vol), "4-H volunteer data could not be loaded."))
      dat <- tb$shapes %>% left_join(tb$fourh_vol, by = "locality_key")
      values <- dat[[group]]
      label <- if (group == "Volunteers") "All volunteers" else sub("_n$", "", group)
      draw_map(dat, values, "4-H<br>volunteers",
               count_label(dat$locality, label, values, "Total volunteers", dat$Volunteers),
               VCE_RAMP, input$fourh_mode %||% "level")

    } else {
      validate(need(!is.null(tb$fourh) && !is.null(tb$fourh_vol),
                    "4-H participation or volunteer data could not be loaded."))
      combined <- tb$fourh %>%
        select(locality_key, Participants = Total) %>%
        full_join(tb$fourh_vol %>% select(locality_key, Volunteers), by = "locality_key") %>%
        mutate(ratio = ifelse(!is.na(Participants) & Participants > 0,
                              100 * Volunteers / Participants, NA_real_))
      dat <- tb$shapes %>% left_join(combined, by = "locality_key")
      labels <- paste0(
        "<strong>", dat$locality, "</strong><br>",
        "Volunteers: ", ifelse(is.na(dat$Volunteers), "&mdash;", dat$Volunteers), "<br>",
        "Participants: ", ifelse(is.na(dat$Participants), "&mdash;", dat$Participants), "<br>",
        "Ratio: <strong>", ifelse(is.na(dat$ratio), "&mdash;",
                                  paste0(round(dat$ratio, 1), "%")), "</strong>"
      )
      draw_map(dat, dat$ratio, "Volunteers per<br>100 participants", labels,
               VCE_RAMP_ALT, input$fourh_mode %||% "level")
    }
  })

  # ---- VCE ---------------------------------------------------------------

  output$vce_title <- renderText({
    switch(req(input$vce_view),
      participant = "Extension participants by locality",
      volunteer   = "Extension volunteers by locality",
      ratio       = "Extension volunteers as a share of participants"
    )
  })

  output$vce_map <- renderLeaflet({
    tb <- tables()
    validate(need(!is.null(tb), "County boundaries could not be downloaded. Please try again shortly."))
    view <- req(input$vce_view)

    if (view == "participant") {
      # req() here is what the previous version was missing: on first render
      # the input is still NULL, and feeding NULL into case_when/switch raised
      # an error instead of simply waiting for the input to arrive.
      group <- req(input$vce_participant_group)
      validate(need(!is.null(tb$vce_part), "Extension participation data could not be loaded."))
      dat <- tb$shapes %>% left_join(tb$vce_part, by = "locality_key")
      values <- dat[[group]]
      label <- names(VCE_PARTICIPANT_GROUPS)[VCE_PARTICIPANT_GROUPS == group]
      draw_map(dat, values, "Participants",
               count_label(dat$locality, label, values,
                           "All participants", dat$participants_total),
               VCE_RAMP, input$vce_mode %||% "level")

    } else if (view == "volunteer") {
      group <- req(input$vce_volunteer_group)
      validate(need(!is.null(tb$vce_vol), "Extension volunteer data could not be loaded."))

      totals <- tb$vce_vol %>% count(locality_key, name = "TotalVolunteers")
      subset <- if (group == "all") {
        tb$vce_vol
      } else {
        spec <- VCE_VOLUNTEER_FILTER[[group]]
        tb$vce_vol %>% filter(.data[[spec$col]] == spec$value)
      }
      group_counts <- subset %>% count(locality_key, name = "GroupVolunteers")

      dat <- tb$shapes %>%
        left_join(totals, by = "locality_key") %>%
        left_join(group_counts, by = "locality_key") %>%
        # A locality with volunteers but none in this group is a real zero;
        # one with no volunteer record at all stays NA and renders grey.
        mutate(GroupVolunteers = ifelse(!is.na(TotalVolunteers) & is.na(GroupVolunteers),
                                        0, GroupVolunteers))

      label <- names(VCE_VOLUNTEER_GROUPS)[VCE_VOLUNTEER_GROUPS == group]
      draw_map(dat, dat$GroupVolunteers, "Volunteers",
               count_label(dat$locality, label, dat$GroupVolunteers,
                           "All volunteers", dat$TotalVolunteers),
               VCE_RAMP, input$vce_mode %||% "level")

    } else {
      validate(need(!is.null(tb$vce_ratio), "Volunteer ratio data could not be loaded."))
      dat <- tb$shapes %>% left_join(tb$vce_ratio, by = "locality_key")
      labels <- paste0(
        "<strong>", dat$locality, "</strong><br>",
        "Volunteers: ", ifelse(is.na(dat$Volunteers), "&mdash;", dat$Volunteers), "<br>",
        "Participants: ", ifelse(is.na(dat$Participants), "&mdash;",
                                 format(dat$Participants, big.mark = ",")), "<br>",
        "Ratio: <strong>", ifelse(is.na(dat$ratio), "&mdash;",
                                  paste0(round(dat$ratio, 1), "%")), "</strong>"
      )
      draw_map(dat, dat$ratio, "Volunteers per<br>100 participants", labels,
               VCE_RAMP_ALT, input$vce_mode %||% "level")
    }
  })

  # ===== CHART TABS ========================================================

  # Two series throughout: the selected locality and the state. Fixed order, so
  # the colour means the same thing in every chart on these tabs.
  SERIES_COLOURS <- stats::setNames(VVN_SERIES_PALETTE[1:2],
                                    c("This locality", "Statewide"))

  # Floor for the capacity ranking: below this, a locality with three
  # participants and no volunteers would top the list on arithmetic alone.
  MIN_PARTICIPANTS <- 25

  loc_choices <- reactive({
    tb <- tables()
    req(!is.null(tb), !is.null(tb$fourh))
    nm <- tb$shapes %>%
      sf::st_drop_geometry() %>%
      select(locality_key, locality) %>%
      distinct() %>%
      filter(locality_key %in% tb$fourh$locality_key) %>%
      arrange(locality)
    stats::setNames(nm$locality_key, nm$locality)
  })

  observe({
    ch <- loc_choices()
    req(length(ch) > 0)
    updateSelectInput(session, "dem_locality", choices = ch, selected = unname(ch[1]))
  })

  loc_name <- function(key) {
    ch <- loc_choices()
    if (!key %in% ch) key else names(ch)[match(key, ch)]
  }

  # ---- tab 3: who 4-H reaches --------------------------------------------

  #' Shares within one demographic block, locality against state.
  #' Each block counts every participant exactly once, so a share is taken
  #' within the block -- never across the two blocks combined.
  dem_block <- function(groups) {
    tb <- tables()
    validate(need(!is.null(tb$fourh), "4-H enrolment data could not be loaded."))
    key <- req(input$dem_locality)
    cols <- unname(groups)

    long <- function(df, series) {
      df %>%
        summarise(across(all_of(cols), ~ sum(.x, na.rm = TRUE))) %>%
        tidyr::pivot_longer(everything(), names_to = "col", values_to = "n") %>%
        mutate(Group = names(groups)[match(col, cols)],
               Series = series,
               Share = if (sum(n) > 0) 100 * n / sum(n) else NA_real_)
    }

    here <- long(tb$fourh %>% filter(locality_key == key), "This locality")
    validate(need(sum(here$n, na.rm = TRUE) > 0,
                  "This locality has no recorded 4-H enrolment."))
    bind_rows(here, long(tb$fourh, "Statewide")) %>%
      mutate(Series = factor(Series, levels = c("This locality", "Statewide"))) %>%
      filter(!(Series == "This locality" & n == 0))
  }

  dem_share_chart <- function(d, xlab) {
    ord <- d %>% filter(Series == "Statewide") %>% arrange(Share) %>% pull(Group)
    d <- d %>% mutate(Group = factor(Group, levels = unique(c(ord, d$Group))))
    gg <- ggplot(d, aes(x = Share, y = Group, fill = Series,
                        text = paste0(Group, "<br>", Series, ": ",
                                      sprintf("%.1f%%", Share),
                                      "<br>", scales::comma(n), " participants"))) +
      geom_col(width = 0.66, position = position_dodge(width = 0.72)) +
      scale_fill_manual(values = SERIES_COLOURS, name = NULL) +
      scale_x_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = xlab, y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  }

  output$dem_title <- renderText({
    sprintf("4-H enrolment by race — %s vs the state", loc_name(req(input$dem_locality)))
  })

  output$dem_kpi <- renderUI({
    tb <- tables()
    validate(need(!is.null(tb$fourh), "4-H enrolment data could not be loaded."))
    key <- req(input$dem_locality)
    row <- tb$fourh %>% filter(locality_key == key)
    validate(need(nrow(row) > 0, "No enrolment recorded for this locality."))

    # The race block alone is the true headcount: see USE_ORIGINAL_TOTALS.
    race_cols <- unname(FOURH_RACE)
    here <- sum(unlist(row[race_cols]), na.rm = TRUE)
    state <- sum(unlist(tb$fourh[race_cols]), na.rm = TRUE)
    biggest <- tibble(
      Group = names(FOURH_RACE),
      n = vapply(race_cols, function(c) sum(row[[c]], na.rm = TRUE), numeric(1))
    ) %>% slice_max(n, n = 1)

    vvn_kpi_row(
      vvn_tile("Participants here", scales::comma(here),
               "4-H enrolment, race block", accent = TRUE),
      vvn_tile("Share of the state",
               sprintf("%.1f%%", if (state > 0) 100 * here / state else NA_real_),
               paste(scales::comma(state), "statewide")),
      vvn_tile("Largest group", biggest$Group[1],
               sprintf("%s, %.0f%%", scales::comma(biggest$n[1]),
                       if (here > 0) 100 * biggest$n[1] / here else NA_real_)),
      vvn_tile("Localities reporting", nrow(tb$fourh), "in the programme report")
    )
  })

  output$dem_race <- renderPlotly({
    dem_share_chart(dem_block(FOURH_RACE), "Share of enrolment (race block)")
  })

  output$dem_eth <- renderPlotly({
    dem_share_chart(dem_block(FOURH_ETHNICITY), "Share of enrolment (ethnicity block)")
  })

  output$dem_top <- renderPlotly({
    tb <- tables()
    validate(need(!is.null(tb$fourh), "4-H enrolment data could not be loaded."))
    race_cols <- unname(FOURH_RACE)
    d <- tb$fourh %>%
      mutate(n = rowSums(across(all_of(race_cols)), na.rm = TRUE)) %>%
      filter(n > 0) %>%
      slice_max(n, n = 20) %>%
      left_join(tb$shapes %>% sf::st_drop_geometry() %>%
                  select(locality_key, locality) %>% distinct(),
                by = "locality_key") %>%
      mutate(locality = ifelse(is.na(locality), locality_key, locality)) %>%
      arrange(n) %>%
      mutate(locality = factor(locality, levels = unique(locality)),
             here = locality_key == req(input$dem_locality))
    gg <- ggplot(d, aes(x = n, y = locality,
                        text = paste0(locality, "<br>", scales::comma(n), " participants"))) +
      # One measure, one hue. The selected locality is marked by a darker step
      # of the same ramp rather than a second hue, so the ranking still reads
      # as a single series.
      # Lollipops rather than bars: this suite had five dashboards opening on
      # near-identical horizontal bar charts. The stem carries the magnitude
      # just as a bar does, with far less ink, and the dot gives the eye a
      # precise endpoint to compare across rows.
      geom_segment(aes(x = 0, xend = n, yend = locality, colour = here),
                   linewidth = 0.9, show.legend = FALSE) +
      geom_point(aes(colour = here), size = 2.6, show.legend = FALSE) +
      scale_colour_manual(values = c(`FALSE` = VCE_RAMP[3], `TRUE` = VCE_RAMP[6])) +
      scale_x_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = "4-H participants (race block)", y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })

  # ---- tab 4: volunteer capacity -----------------------------------------

  cap_data <- reactive({
    tb <- tables()
    prog <- req(input$cap_prog)
    if (identical(prog, "vce")) {
      validate(need(!is.null(tb$vce_ratio), "PEARS volunteer data could not be loaded."))
      d <- tb$vce_ratio %>% select(locality_key, Participants, Volunteers)
    } else {
      validate(need(!is.null(tb$fourh) && !is.null(tb$fourh_vol),
                    "4-H volunteer data could not be loaded."))
      race_cols <- unname(FOURH_RACE)
      d <- tb$fourh %>%
        transmute(locality_key,
                  Participants = rowSums(across(all_of(race_cols)), na.rm = TRUE)) %>%
        inner_join(tb$fourh_vol %>% select(locality_key, Volunteers), by = "locality_key")
    }
    d <- d %>%
      filter(!is.na(Participants), !is.na(Volunteers), Participants > 0) %>%
      left_join(tb$shapes %>% sf::st_drop_geometry() %>%
                  select(locality_key, locality) %>% distinct(),
                by = "locality_key") %>%
      mutate(locality = ifelse(is.na(locality), locality_key, locality),
             ratio = 100 * Volunteers / Participants)
    validate(need(nrow(d) > 0, "No volunteer data is available for this programme."))
    d
  })

  # The ranked subset is computed once and used by BOTH charts, so the slider
  # visibly changes the scatter as well as the ranking. It previously drove
  # only the lower chart, which made it look inert to anyone watching the
  # scatter above it.
  cap_ranked <- reactive({
    d <- cap_data() %>% filter(Participants >= MIN_PARTICIPANTS)
    validate(need(nrow(d) > 0, sprintf(
      "No locality has at least %d recorded participants.", MIN_PARTICIPANTS)))
    d %>% slice_min(ratio, n = min(req(input$cap_n), nrow(d)))
  })

  output$cap_rank_title <- renderText({
    sprintf("Thinnest volunteer capacity \u2014 %d localities with the lowest ratio",
            nrow(cap_ranked()))
  })

  output$cap_title <- renderText({
    d <- cap_data()
    sprintf("Volunteers against participants — %s, %d localities",
            if (identical(input$cap_prog, "vce")) "Extension" else "4-H", nrow(d))
  })

  output$cap_kpi <- renderUI({
    d <- cap_data()
    thin <- d %>% filter(Participants >= MIN_PARTICIPANTS) %>% slice_min(ratio, n = 1)
    vvn_kpi_row(
      vvn_tile("Participants", scales::comma(sum(d$Participants)),
               "across reporting localities", accent = TRUE),
      vvn_tile("Volunteers", scales::comma(sum(d$Volunteers)), "with recorded activity"),
      vvn_tile("Volunteers per 100",
               sprintf("%.1f", 100 * sum(d$Volunteers) / sum(d$Participants)),
               "participants, statewide"),
      vvn_tile("Thinnest capacity",
               if (nrow(thin)) thin$locality[1] else "—",
               if (nrow(thin)) sprintf("%.1f per 100 participants", thin$ratio[1]) else NULL)
    )
  })

  output$cap_scatter <- renderPlotly({
    d <- cap_data()
    ranked <- tryCatch(cap_ranked()$locality_key, error = function(e) character(0))
    lvl <- c("In the ranking below", "Other localities")
    d <- d %>%
      mutate(Highlight = factor(ifelse(locality_key %in% ranked, lvl[1], lvl[2]),
                                levels = lvl))

    gg <- ggplot(d, aes(x = Participants, y = Volunteers, colour = Highlight,
                        text = paste0(locality, "<br>",
                                      scales::comma(Participants), " participants<br>",
                                      scales::comma(Volunteers), " volunteers<br>",
                                      sprintf("%.1f per 100", ratio)))) +
      # Parity reference: above the line, volunteers outnumber participants.
      geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                  linewidth = 0.4, colour = "#aeb4c0") +
      # Unselected points first, so the selection is never drawn underneath.
      geom_point(data = function(x) dplyr::filter(x, Highlight == lvl[2]),
                 size = 2.1, alpha = 0.55) +
      geom_point(data = function(x) dplyr::filter(x, Highlight == lvl[1]),
                 size = 2.9) +
      scale_colour_manual(values = stats::setNames(
        c(VVN_SERIES_PALETTE[2], "#9aa2b1"), lvl), name = NULL, drop = FALSE) +
      scale_x_log10(labels = scales::comma) +
      scale_y_log10(labels = scales::comma) +
      labs(x = "Participants (log scale)", y = "Volunteers (log scale)") +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })

  output$cap_rank <- renderPlotly({
    d <- cap_ranked() %>%
      arrange(desc(ratio)) %>%
      mutate(locality = factor(locality, levels = unique(locality)))
    gg <- ggplot(d, aes(x = ratio, y = locality,
                        text = paste0(locality, "<br>",
                                      sprintf("%.1f volunteers per 100 participants", ratio),
                                      "<br>", scales::comma(Volunteers), " of ",
                                      scales::comma(Participants)))) +
      geom_segment(aes(x = 0, xend = ratio, yend = locality),
                   colour = VCE_RAMP_ALT[3], linewidth = 0.9) +
      geom_point(colour = VCE_RAMP_ALT[5], size = 2.6) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Volunteers per 100 participants", y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })
}

shinyApp(ui = ui, server = server)
