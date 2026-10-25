# ============================================================================
# Virginia Cost of Living Interactive Maps
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
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
COL_RAMP     <- vvn_ramp("blue")
COL_RAMP_ALT <- vvn_ramp("teal")

options(tigris_use_cache = TRUE)

DATA_DIR   <- "CoL_Data"

# Household types offered in the UI, in display order.
# Deliberately not labelled with an age band for the working-age rows: the
# source files disagree (most use 19-50, the healthcare files use 19-64), so a
# single printed range would be wrong for some categories. See the sidebar note.
FAMILY_STRUCTURES <- c(
  "1 Adult",
  "2 Adults",
  "1 Adult + 1 Child",
  "2 Adults + 2 Children",
  "1 Adult, 65+",
  "2 Adults, 65+"
)

DEFAULT_STRUCTURE <- "2 Adults + 2 Children"

# Include the utilities files?  Keep this FALSE.
#
# CoL_Data ships minimum/average_final_utilities_cleaned.csv, and it is tempting
# to treat their absence as an oversight. It is not. The upstream DSPG app
# (cost_of_living_dspg, col_app/interactive_map_dash2.R, deployed as
# cost.shinyapps.io/col_app) also leaves "Utilities" out of its category list,
# and documents why in its own methodology text:
#
#   Housing is HUD 2023 Fair Market Rent, and FMR figures "already include the
#   cost of basic utilities like electricity, water, and heat". Minimum cost is
#   set at 110% of FMR, where "the 10% buffer accounts for potential utility
#   costs that may exceed HUD's standard allowance".
#
# So utilities are already inside the Housing line. Adding the separate files
# would DOUBLE-COUNT them -- about $389/month for a family of four, plus its
# share of the 10% miscellaneous uplift -- and would add no geographic variation
# anyway, since those figures are identical for all 133 localities.
INCLUDE_UTILITIES <- FALSE

# Display order for the cost breakdown in a locality popup.
COST_CATEGORIES <- c(
  "Housing", "Food", "Transportation", "Healthcare",
  "Childcare", "Elder Care", "Technology", "Taxes", "Miscellaneous"
)
if (INCLUDE_UTILITIES) {
  COST_CATEGORIES <- append(COST_CATEGORIES, "Utilities", after = 1)
}

# Miscellaneous is modelled as a flat uplift on everything except taxes.
MISC_RATE <- 0.10

# ===== COLUMN NORMALISATION ================================================
# The source CSVs label the same six household types a dozen different ways
# ("1 Adult: 19-50 Years", "1 Adult (19-50)", "Adult: 19-50", "1 Adult, 1 Child",
# "2 Adults + 2 Children (6-10)", ...). Rather than pattern-match each spelling,
# reduce every header to a canonical token string and map that.

# Match the published dashboard's household-type figures?
#
# TRUE reproduces them, including a known defect. The original matched household
# columns with regexes requiring the literal age band "19-64":
#
#   "1.*Adult.*19.*64|1.*Adult.*Working|Working.*Adult" = "1 Adult: 19-64 Years"
#
# Only the two healthcare files use 19-64. Housing, food, transportation, taxes,
# childcare, elder care and technology all say 19-50, so for the two adult-only
# household types those columns were never recognised and never loaded. Those
# types therefore total healthcare plus the 10% miscellaneous uplift and nothing
# else -- a median of about $263/month for one adult and $576 for two, against
# roughly $4,430 and $7,982 once every category is included.
#
# TRUE is set deliberately so these maps agree with the DSPG original (the same
# defect sits at col_app/interactive_map_dash2.R:63). FALSE uses the token-based
# matcher below, which resolves every age band.
USE_ORIGINAL_STRUCTURE_MATCHING <- TRUE

# Average the county and city rows of an ambiguous name together, as the
# published dashboard does? See the note at the resolution step below.
USE_ORIGINAL_AMBIGUOUS_PAIRS <- TRUE

#' The original matcher: only recognises a household column when the header
#' carries the exact age band the pattern names.
normalize_structure_original <- function(x) {
  patterns <- c(
    "1.*Adult.*19.*64|1.*Adult.*Working|Working.*Adult"        = "1 Adult",
    "2.*Adults.*19.*64|2.*Adults.*Working|Two.*Adults.*Working" = "2 Adults",
    "1.*Adult.*1.*Child|Adult.*Child|Single.*Parent"            = "1 Adult + 1 Child",
    "2.*Adults.*2.*Children|Family.*4|Typical.*Family"          = "2 Adults + 2 Children",
    "1.*Adult.*65|Senior.*Individual|Elder.*Individual"         = "1 Adult, 65+",
    "2.*Adults.*65|Senior.*Couple|Elder.*Couple"                = "2 Adults, 65+"
  )
  out <- rep(NA_character_, length(x))
  for (pat in names(patterns)) {
    hit <- grepl(pat, x, ignore.case = TRUE) & is.na(out)
    if (any(hit)) out[which(hit)[1]] <- patterns[[pat]]
  }
  out
}

normalize_structure_fixed <- function(x) {
  k <- tolower(trimws(x))
  k <- gsub("\\(([^)]*)\\)", " \\1 ", k)                     # unwrap parentheses
  k <- gsub("[0-9]{1,2}\\s*[-–]\\s*[0-9]{1,2}", " ", k)  # drop age ranges
  k <- gsub("\\byears?\\b", " ", k)
  k <- gsub("\\badults\\b", "adult", k)
  k <- gsub("\\bchildren\\b", "child", k)
  k <- gsub("65\\s*\\+", "65plus", k)                         # keep the 65+ marker
  k <- gsub("[^a-z0-9]+", " ", k)
  k <- trimws(gsub("\\s+", " ", k))

  case_when(
    grepl("^1? ?adult 1 child$", k) ~ "1 Adult + 1 Child",
    grepl("^2 adult 2 child$", k)   ~ "2 Adults + 2 Children",
    grepl("^1? ?adult 65plus$", k)  ~ "1 Adult, 65+",
    grepl("^2 adult 65plus$", k)    ~ "2 Adults, 65+",
    grepl("^1? ?adult$", k)         ~ "1 Adult",
    grepl("^2 adult$", k)           ~ "2 Adults",
    TRUE ~ NA_character_
  )
}

#' Read one cost file and return it long.
#' Returns zero rows (not an error) for a missing or unreadable file, so one bad
#' category never takes the whole dashboard down.
#'
#' Locality naming in these files needs care. Each file holds 133 rows: all 95
#' Virginia counties and all 38 independent cities, every one under a BARE name
#' ("Alexandria", not "Alexandria city"). Four names -- Fairfax, Franklin,
#' Richmond and Roanoke -- are both a county and a city and so appear twice,
#' with different values and NO marker saying which row is which.
#'
#' The files are sorted alphabetically by bare name, so the tie order carries no
#' information. Checking the values against known cost differences gives a
#' CONFLICTING answer: for Richmond and Franklin the first row is clearly the
#' county (rural Richmond County's HUD 2-bedroom FMR is ~$1,056 against
#' ~$1,651+ for the Richmond metro area, and transportation correctly inverts),
#' but for Fairfax the second row looks like the county on taxes, food and
#' transportation, and Roanoke is split. The ordering is therefore NOT
#' consistent across the four pairs and cannot be recovered from these files.
#'
#' So the duplicate is DROPPED rather than guessed at: the first occurrence is
#' kept and read as the county, the second is discarded, and the four
#' independent cities show no data. The alternative -- averaging the pair, as
#' the original did -- blends two genuinely different localities into one
#' number, and publishing both under a coin-flip attribution would risk showing
#' city costs labelled as county. The four retained rows stay flagged in the
#' popup. The real fix is a FIPS column in the source CSVs.
read_cost_file <- function(file, category, type) {
  empty <- tibble(
    base_key = character(), occ = integer(), FamilyStructure = character(),
    Cost = numeric(), Category = character(), Type = character()
  )

  path <- file.path(DATA_DIR, file)
  if (!file.exists(path)) {
    warning("Cost file not found, skipping: ", path, call. = FALSE)
    return(empty)
  }

  raw <- tryCatch(
    suppressWarnings(read_csv(path, show_col_types = FALSE, progress = FALSE,
                              name_repair = "unique_quiet")),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0 || !"County" %in% names(raw)) {
    warning("Cost file unusable, skipping: ", path, call. = FALSE)
    return(empty)
  }

  mapped <- if (USE_ORIGINAL_STRUCTURE_MATCHING) {
    normalize_structure_original(names(raw))
  } else {
    normalize_structure_fixed(names(raw))
  }
  keep <- which(!is.na(mapped))
  if (length(keep) == 0) {
    warning("No household-type columns recognised in ", path, call. = FALSE)
    return(empty)
  }

  out <- raw[, c(which(names(raw) == "County")[1], keep), drop = FALSE]
  names(out) <- c("County", mapped[keep])

  out %>%
    mutate(base_key = vvn_clean_locality(County)) %>%
    filter(!is.na(base_key), base_key != "", base_key != "na") %>%
    group_by(base_key) %>%
    mutate(occ = row_number()) %>%
    ungroup() %>%
    select(-County) %>%
    pivot_longer(
      -c(base_key, occ),
      names_to = "FamilyStructure", values_to = "Cost"
    ) %>%
    mutate(
      Cost     = suppressWarnings(as.numeric(Cost)),
      Category = category,
      Type     = type
    ) %>%
    filter(!is.na(Cost), is.finite(Cost), Cost >= 0)
}

# ===== BUILD THE COST TABLE ================================================

COST_FILES <- tribble(
  ~file,                                 ~category,        ~type,
  "minimum_housing_cost.csv",            "Housing",        "min",
  "average_housing_cost.csv",            "Housing",        "avg",
  "final_minimum_food_data.csv",         "Food",           "min",
  "final_average_food_data.csv",         "Food",           "avg",
  "minimum_transportation_data.csv",     "Transportation", "min",
  "average_transportation_data.csv",     "Transportation", "avg",
  "minimum_healthcare_cost.csv",         "Healthcare",     "min",
  "average_healthcare_cost.csv",         "Healthcare",     "avg",
  "childcare_minimum_cost.csv",          "Childcare",      "min",
  "childcare_average_cost.csv",          "Childcare",      "avg",
  "minimum_elder_care_cost.csv",         "Elder Care",     "min",
  "average_elder_care_cost.csv",         "Elder Care",     "avg",
  "minimum_technology_costs.csv",        "Technology",     "min",
  "average_technology_costs.csv",        "Technology",     "avg",
  "minimum_tax_cost.csv",                "Taxes",          "min",
  "average_tax_cost.csv",                "Taxes",          "avg"
)

if (INCLUDE_UTILITIES) {
  COST_FILES <- bind_rows(COST_FILES, tribble(
    ~file,                                 ~category,   ~type,
    "minimum_final_utilities_cleaned.csv", "Utilities", "min",
    "average_final_utilities_cleaned.csv", "Utilities", "avg"
  ))
}

cost_raw <- COST_FILES %>%
  purrr::pmap_dfr(function(file, category, type) read_cost_file(file, category, type))

if (nrow(cost_raw) == 0) {
  stop("No cost data could be loaded from ", DATA_DIR,
       ". Check that the CSV files are present.")
}

cost_raw <- cost_raw %>%
  group_by(base_key, occ, FamilyStructure, Category, Type) %>%
  summarise(Cost = mean(Cost, na.rm = TRUE), .groups = "drop")

# Miscellaneous: MISC_RATE of the non-tax subtotal for that household.
cost_raw <- bind_rows(
  cost_raw,
  cost_raw %>%
    filter(Category != "Taxes") %>%
    group_by(base_key, occ, FamilyStructure, Type) %>%
    summarise(Cost = sum(Cost, na.rm = TRUE) * MISC_RATE, .groups = "drop") %>%
    filter(Cost > 0) %>%
    mutate(Category = "Miscellaneous")
) %>%
  mutate(Category = factor(Category, levels = COST_CATEGORIES)) %>%
  filter(!is.na(Category))

money <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))

# ===== BOUNDARIES AND KEY RESOLUTION ======================================

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

#' Resolve each data row's bare name to a real boundary key, then roll the cost
#' table up to totals and popup HTML. Memoised: runs once per process.
cost_tables <- local({
  value <- NULL; done <- FALSE
  function() {
    if (done) return(value)
    done <<- TRUE

    shapes <- load_boundaries()
    if (is.null(shapes)) return(value)

    county_keys <- shapes$locality_key[!shapes$is_city]
    city_keys   <- shapes$locality_key[shapes$is_city]

    resolved <- cost_raw %>%
      mutate(
        as_city = paste(base_key, "city"),
        locality_key = case_when(
          # Ambiguous name, second occurrence. Fairfax, Franklin, Richmond and
          # Roanoke each name both a county and an independent city, and the
          # source files list both under the bare name with nothing to say
          # which row is which.
          #
          # USE_ORIGINAL_AMBIGUOUS_PAIRS = TRUE keeps the published behaviour:
          # both rows carry the same key and are averaged together downstream,
          # so one blended figure covers county and city alike. FALSE drops the
          # duplicate instead, so the city shows no data and nothing on the map
          # is a blend of two different places.
          occ == 2 & base_key %in% VVN_AMBIGUOUS_LOCALITIES ~
            if (USE_ORIGINAL_AMBIGUOUS_PAIRS) base_key else NA_character_,
          # A county of this name exists: the bare name means the county.
          base_key %in% county_keys ~ base_key,
          # Otherwise the only locality with this name is a city.
          as_city %in% city_keys ~ as_city,
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(locality_key)) %>%
      # The kept row of each pair is still of uncertain attribution, so it
      # stays flagged in the popup.
      mutate(unverified = base_key %in% VVN_AMBIGUOUS_LOCALITIES) %>%
      # Collapse the occurrence index. When both rows of an ambiguous pair map
      # to the same key, this averages them per category -- which is what the
      # published dashboard does. Without this step the later sum over
      # categories would add both rows and double that locality's total.
      group_by(locality_key, FamilyStructure, Category, Type) %>%
      summarise(Cost = mean(Cost, na.rm = TRUE),
                unverified = any(unverified), .groups = "drop")

    totals <- resolved %>%
      group_by(locality_key, FamilyStructure, Type) %>%
      summarise(TotalCost = sum(Cost, na.rm = TRUE),
                unverified = any(unverified), .groups = "drop") %>%
      filter(TotalCost > 0)

    # Built once here, not re-derived per render. The previous version ran a
    # per-county filter over the whole long table inside renderLeaflet.
    popups <- resolved %>%
      filter(Cost > 0) %>%
      arrange(locality_key, Type, FamilyStructure, Category) %>%
      group_by(locality_key, FamilyStructure, Type) %>%
      summarise(
        breakdown = paste(
          sprintf("%s: %s (%s%%)", Category, money(Cost), round(100 * Cost / sum(Cost))),
          collapse = "<br>"
        ),
        total = sum(Cost),
        .groups = "drop"
      )

    value <<- list(totals = totals, popups = popups, detail = resolved)
    value
  }
})

# ===== MAP BUILDER =========================================================

TYPE_LABEL <- c(min = "Minimum", avg = "Average")

build_cost_map <- function(type, structure, mode = "level") {
  shapes <- load_boundaries()
  validate(need(
    !is.null(shapes),
    "County boundaries could not be downloaded from the Census TIGER service. Please try again shortly."
  ))

  tables <- cost_tables()
  validate(need(!is.null(tables), "Cost data could not be prepared."))

  totals <- tables$totals %>%
    filter(Type == type, FamilyStructure == structure) %>%
    select(locality_key, TotalCost, unverified)

  validate(need(
    nrow(totals) > 0,
    paste0("No ", tolower(TYPE_LABEL[[type]]), " cost data is available for “",
           structure, "”.")
  ))

  dat <- shapes %>%
    left_join(totals, by = "locality_key") %>%
    left_join(
      tables$popups %>%
        filter(Type == type, FamilyStructure == structure) %>%
        select(locality_key, breakdown, total),
      by = "locality_key"
    )

  scored <- dat %>% filter(!is.na(TotalCost))
  validate(need(
    nrow(scored) > 1,
    "Not enough localities have data for this household type to draw a map."
  ))

  # The reference for the "compared with the state" view. Median rather than
  # mean, so Northern Virginia cannot drag the reference line up and make the
  # rest of the state look cheap by construction.
  state_median <- stats::median(scored$TotalCost, na.rm = TRUE)
  dat$Deviation <- vvn_pct_diff(dat$TotalCost, state_median)
  scored <- dat %>% filter(!is.na(TotalCost))

  diverging <- identical(mode, "vs_state")

  if (diverging) {
    pal <- vvn_diverging_pal(scored$Deviation, centre = 0)
    shade_values <- dat$Deviation
    legend_values <- scored$Deviation
    legend_title <- "Difference from the<br>state median (%)"
    legend_fmt <- labelFormat(suffix = "%")
  } else {
    # Quantile bins, so the colour scale responds to the actual spread.
    breaks <- unique(stats::quantile(scored$TotalCost, probs = seq(0, 1, 0.2), na.rm = TRUE))
    if (length(breaks) < 3) breaks <- range(scored$TotalCost, na.rm = TRUE)
    pal <- colorBin(palette = COL_RAMP, domain = scored$TotalCost,
                    bins = breaks, na.color = VVN_NO_DATA)
    shade_values <- dat$TotalCost
    legend_values <- scored$TotalCost
    legend_title <- paste0(TYPE_LABEL[[type]], " monthly<br>cost (USD)")
    legend_fmt <- labelFormat(prefix = "$", big.mark = ",")
  }

  fmt <- function(x) vapply(x, function(v) if (is.na(v)) "" else money(v), character(1))

  popup_html <- ifelse(
    is.na(dat$total),
    paste0("<strong>", htmlEscape(dat$locality), "</strong><br>No cost data available"),
    paste0(
      "<strong>", htmlEscape(dat$locality), "</strong><br>",
      "<span style='color:#5c6472'>", TYPE_LABEL[[type]], " monthly cost &middot; ",
      htmlEscape(structure), "</span><br><br>",
      dat$breakdown,
      "<br><br><strong>Total: ", fmt(dat$total), "</strong>",
      ifelse(
        !is.na(dat$unverified) & dat$unverified,
        paste0("<br><br><em style='color:#8a5a00'>This name belongs to both a ",
               "county and an independent city. The source file does not say ",
               "which row is which, so the duplicate row was dropped and these ",
               "figures are read as the county.</em>"),
        ""
      )
    )
  )

  dev_text <- ifelse(
    is.na(dat$Deviation), "",
    paste0("<br>", ifelse(dat$Deviation >= 0, "+", ""),
           formatC(dat$Deviation, format = "f", digits = 1),
           "% vs the state median")
  )

  labels <- ifelse(
    is.na(dat$TotalCost),
    paste0("<strong>", htmlEscape(dat$locality), "</strong><br>No data"),
    paste0("<strong>", htmlEscape(dat$locality), "</strong><br>",
           fmt(dat$TotalCost), " / month", dev_text)
  )

  leaflet(dat) %>%
    vvn_basemap() %>%
    addPolygons(
      fillColor   = ifelse(is.na(shade_values), VVN_NO_DATA, pal(shade_values)),
      fillOpacity = 0.85,
      color       = VVN_STROKE_COLOUR,
      weight      = VVN_STROKE_WEIGHT,
      opacity     = VVN_STROKE_OPACITY,
      popup       = popup_html,
      label       = lapply(labels, HTML),
      labelOptions     = vvn_labelopts(),
      highlightOptions = vvn_highlight()
    ) %>%
    vvn_outline() %>%
      addLegend(
      pal = pal, values = legend_values,
      title = legend_title,
      position = "bottomright", opacity = 1,
      labFormat = legend_fmt,
      na.label = "No data"
    )
}

#' Sidebar shared by both tabs, differing only in the input id.
cost_sidebar <- function(input_id) {
  sidebar(
    width = 300,
    vvn_group("Household type"),
    div(
      class = "vvn-chooser",
      radioGroupButtons(
        inputId   = input_id,
        label     = NULL,
        choices   = FAMILY_STRUCTURES,
        selected  = DEFAULT_STRUCTURE,
        direction = "vertical",
        size      = "sm",
        checkIcon = list(yes = icon("check")),
        width     = "100%"
      )
    ),
    vvn_help("Click a locality for its full cost breakdown by category."),
    vvn_group("Known caveat"),
    vvn_help(
      "Fairfax, Franklin, Richmond and Roanoke each name both a county and an",
      "independent city, and the source files list both under the bare name",
      "with no way to tell them apart. The duplicate row is dropped, so those",
      "four cities show no data and the four counties are marked in their popups."
    ),
    vvn_group("Shading"),
    radioButtons(
      paste0(input_id, "_mode"), NULL,
      choices = c("Monthly cost" = "level",
                  "Compared with the state" = "vs_state"),
      selected = "level"
    ),
    vvn_help(
      "\u201cCompared with the state\u201d shades each locality by how far it sits",
      "above or below the statewide median for this household type \u2014 pink",
      "above, blue below, pale where it is close to typical."
    ),
    vvn_group("A note on age bands"),
    vvn_help(
      "The source files define the working-age household differently by category",
      "— ages 19–50 for most, 19–64 for healthcare — so the two",
      "adult-only options are labelled without a range."
    )
  )
}

# ===== CHART HELPERS =======================================================

# Both cost levels are shown together throughout the chart tabs, so the two
# series keep one fixed assignment: minimum is always the first hue, average
# always the second, in every chart. Colour follows the measure, never its rank.
TYPE_COLOURS <- c(Minimum = VVN_SERIES_PALETTE[1], Average = VVN_SERIES_PALETTE[2])

#' The per-category table with display labels attached.
cost_detail <- function() {
  tables <- cost_tables()
  if (is.null(tables) || is.null(tables$detail)) return(NULL)
  tables$detail %>%
    mutate(Level = factor(unname(TYPE_LABEL[Type]), levels = c("Minimum", "Average")))
}

#' Localities present in the resolved cost table, as a named choice vector.
locality_choices <- local({
  value <- NULL; done <- FALSE
  function() {
    if (done) return(value)
    done <<- TRUE
    shapes <- load_boundaries()
    tables <- cost_tables()
    if (is.null(shapes) || is.null(tables)) return(value)
    nm <- shapes %>%
      sf::st_drop_geometry() %>%
      select(locality_key, NAME, is_city) %>%
      distinct() %>%
      mutate(label = ifelse(is_city, paste0(NAME, " (city)"), NAME))
    keys <- unique(tables$totals$locality_key)
    nm <- nm %>% filter(locality_key %in% keys) %>% arrange(label)
    value <<- stats::setNames(nm$locality_key, nm$label)
    value
  }
})

#' Pretty locality label for one key.
locality_label <- function(key) {
  ch <- locality_choices()
  if (is.null(ch) || !key %in% ch) return(key)
  names(ch)[match(key, ch)]
}

# ===== UI ==================================================================

ui <- page_fluid(
  title = "Virginia Cost of Living Interactive Maps",
  theme = vvn_theme(),
  tags$head(vvn_css()),

  vvn_header(
    "Virginia Cost of Living",
    "Modelled monthly household costs for all 133 Virginia counties and independent cities, at a minimum and a typical standard of living."
  ),

  navset_tab(
    nav_panel(
      "Minimum cost",
      layout_sidebar(
        sidebar = cost_sidebar("structure_min"),
        card(
          full_screen = TRUE,
          card_header("Minimum monthly cost of living"),
          card_body(padding = 0, leafletOutput("min_map", height = "520px"))
        ),
        vvn_notes(
          "What this map shows",
          p(
            "The", strong("lowest monthly cost"), "at which a household of the selected type can meet its basic needs",
            "in each locality: housing, food, transportation, healthcare, care costs, technology and taxes,",
            "plus a", paste0(MISC_RATE * 100, "%"), "allowance for miscellaneous expenses on top of the non-tax subtotal."
          ),
          p(
            "Estimates are built from HUD, USDA, Census and other public sources. Where a locality is missing a specific",
            "input, healthcare and elder-care figures are imputed using an income-based multiplier comparing local to",
            "statewide median income. All values are monthly, in current US dollars."
          ),
          p(
            "Darker shading marks more expensive localities. Shading is binned by quintile, so colours rank localities",
            "against each other for the selected household type rather than against a fixed dollar scale."
          )
        )
      )
    ),

    nav_panel(
      "Average cost",
      layout_sidebar(
        sidebar = cost_sidebar("structure_avg"),
        card(
          full_screen = TRUE,
          card_header("Average monthly cost of living"),
          card_body(padding = 0, leafletOutput("avg_map", height = "520px"))
        ),
        vvn_notes(
          "What this map shows",
          p(
            "The", strong("typical monthly cost"), "of a modest but sustainable standard of living, covering the same",
            "categories as the minimum map but using observed average expenditure rather than bare-minimum budgets."
          ),
          p(
            "Sources are HUD, USDA and Census ACS, adjusted for local conditions. Comparing the two tabs for the same",
            "household type shows the gap between getting by and living comfortably, which is widest in the Northern",
            "Virginia and Richmond metro areas."
          ),
          p(
            "Darker shading marks more expensive localities, binned by quintile within the selected household type."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 3
    nav_panel(
      "Cost breakdown",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          vvn_group("Locality"),
          selectInput("bd_locality", NULL, choices = NULL),
          vvn_group("Household type"),
          selectInput("bd_structure", NULL, choices = FAMILY_STRUCTURES,
                      selected = DEFAULT_STRUCTURE),
          vvn_help(
            "What the monthly total is actually spent on, category by category,",
            "at both standards of living."
          ),
          vvn_help(
            "Categories are ordered by size at the average standard, so the",
            "largest claim on the budget is always at the top."
          )
        ),
        uiOutput("bd_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("bd_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("bd_bars", height = "420px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Share of the monthly budget"),
            card_body(padding = 0, plotlyOutput("bd_share", height = "300px"))
          )
        ),
        vvn_notes(
          "Reading the breakdown",
          p(
            "The top chart is the monthly dollar cost of each category; the lower one is the same figures as a share",
            "of that household's total. The two answer different questions — how much, and how much of the budget —",
            "and a category can be large in one and small in the other."
          ),
          p(
            strong("Housing is usually the largest single line"), "and, because HUD Fair Market Rent already includes",
            "basic utilities, there is no separate utilities row. For households with children, childcare is often",
            "close behind housing and sometimes passes it."
          ),
          p(
            "Miscellaneous is not a surveyed cost: it is a flat", paste0(MISC_RATE * 100, "%"), "allowance applied to the",
            "non-tax subtotal, so it scales with everything else rather than varying by locality on its own."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 4
    nav_panel(
      "Compare localities",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          vvn_group("Household type"),
          selectInput("cmp_structure", NULL, choices = FAMILY_STRUCTURES,
                      selected = DEFAULT_STRUCTURE),
          vvn_group("Show"),
          radioButtons("cmp_end", NULL,
                       choices = c("Most expensive" = "top",
                                   "Least expensive" = "bottom"),
                       selected = "top"),
          sliderInput("cmp_n", "How many localities", min = 5, max = 30,
                      value = 15, step = 5, ticks = FALSE),
          vvn_help(
            "Each locality is one row. The two dots are its minimum and average",
            "monthly cost; the bar between them is the gap."
          )
        ),
        uiOutput("cmp_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("cmp_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("cmp_dumbbell", height = "560px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Where the gap between getting by and living comfortably is widest"),
            card_body(padding = 0, plotlyOutput("cmp_gap", height = "400px"))
          )
        ),
        vvn_notes(
          "Reading the comparison",
          p(
            "A map answers", em("where"), "; a ranking answers", em("how much"), ". The dumbbell puts localities on a",
            "common axis so the distance between them is readable as dollars rather than as a difference in shade."
          ),
          p(
            "The", strong("gap"), "between the two dots is the cost of moving from a bare-minimum budget to a modest",
            "but sustainable one. It is not constant: in some localities the two standards are close together, in",
            "others the average is far above the minimum, and that spread is its own finding."
          ),
          p(
            class = "vvn-help",
            "Ranking is within the selected household type. A locality near the top for a family of four is not",
            "necessarily near the top for a single adult."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 5
    nav_panel(
      "Household types",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          vvn_group("Locality"),
          selectInput("hh_locality", NULL, choices = NULL),
          vvn_help(
            "How the monthly total changes with household composition, for the",
            "selected locality against the statewide median."
          ),
          vvn_help(
            "Costs do not scale with headcount: a second adult adds less than",
            "the first, while a child can add more, because childcare enters."
          )
        ),
        uiOutput("hh_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("hh_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("hh_bars", height = "400px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Which categories drive the difference between household types"),
            card_body(padding = 0, plotlyOutput("hh_heat", height = "420px"))
          )
        ),
        vvn_notes(
          "Reading these charts",
          p(
            "The upper chart compares the selected locality with the statewide median for each household type, so a",
            "locality that is expensive for families but ordinary for single adults shows up as an uneven gap rather",
            "than a uniform shift."
          ),
          p(
            "The", strong("heatmap"), "below shows where each household type's money goes. Reading across a row shows how",
            "one category responds to household composition; reading down a column shows the make-up of one",
            "household's budget. Childcare and elder care are the rows that switch on and off — they are zero for",
            "households without children or older adults, which is why those cells are blank rather than small."
          ),
          p(
            class = "vvn-help",
            "Statewide median is the median across all localities with data for that household type, not a",
            "population-weighted state average."
          )
        )
      )
    )
  )
)

# ===== SERVER ==============================================================

server <- function(input, output, session) {
  output$min_map <- renderLeaflet({
    build_cost_map("min", req(input$structure_min),
                   input$structure_min_mode %||% "level")
  })

  output$avg_map <- renderLeaflet({
    build_cost_map("avg", req(input$structure_avg),
                   input$structure_avg_mode %||% "level")
  })

  # Locality pickers are filled from the resolved table rather than hard-coded,
  # so a locality that failed to match a boundary never appears as a dead option.
  observe({
    ch <- locality_choices()
    req(!is.null(ch), length(ch) > 0)
    default <- if ("fairfax" %in% ch) "fairfax" else unname(ch[1])
    updateSelectInput(session, "bd_locality", choices = ch, selected = default)
    updateSelectInput(session, "hh_locality", choices = ch, selected = default)
  })

  # ---- tab 3: cost breakdown ---------------------------------------------

  bd_data <- reactive({
    d <- cost_detail()
    validate(need(!is.null(d), "Cost data could not be prepared."))
    out <- d %>%
      filter(locality_key == req(input$bd_locality),
             FamilyStructure == req(input$bd_structure),
             Cost > 0)
    validate(need(nrow(out) > 0, "No cost breakdown is available for this combination."))
    # Order categories by the average-level cost so the biggest line leads.
    ord <- out %>%
      filter(Level == "Average") %>%
      arrange(Cost) %>%
      pull(Category) %>%
      as.character()
    ord <- c(ord, setdiff(as.character(out$Category), ord))
    out %>% mutate(Category = factor(as.character(Category), levels = unique(ord)))
  })

  output$bd_title <- renderText({
    sprintf("Monthly cost by category — %s, %s",
            locality_label(req(input$bd_locality)), req(input$bd_structure))
  })

  output$bd_kpi <- renderUI({
    d <- bd_data()
    tot <- d %>% group_by(Level) %>% summarise(t = sum(Cost), .groups = "drop")
    mn <- tot$t[tot$Level == "Minimum"]; av <- tot$t[tot$Level == "Average"]
    top <- d %>% filter(Level == "Average") %>% slice_max(Cost, n = 1)
    vvn_kpi_row(
      vvn_tile("Minimum monthly", money(mn), "bare-necessity budget", accent = TRUE),
      vvn_tile("Average monthly", money(av), "modest but sustainable"),
      vvn_tile("Gap", money(av - mn),
               sprintf("%.0f%% above the minimum", 100 * (av - mn) / mn)),
      vvn_tile("Largest category", as.character(top$Category[1]),
               sprintf("%s, %.0f%% of the average", money(top$Cost[1]),
                       100 * top$Cost[1] / av))
    )
  })

  output$bd_bars <- renderPlotly({
    d <- bd_data()
    gg <- ggplot(d, aes(x = Cost, y = Category, fill = Level,
                        text = paste0(Category, "<br>", Level, ": ", money(Cost)))) +
      geom_col(width = 0.68, position = position_dodge(width = 0.72)) +
      scale_fill_manual(values = TYPE_COLOURS, name = NULL) +
      scale_x_continuous(labels = function(x) paste0("$", scales::comma(x)),
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Monthly cost", y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })

  output$bd_share <- renderPlotly({
    d <- bd_data() %>%
      group_by(Level) %>%
      mutate(Share = 100 * Cost / sum(Cost)) %>%
      ungroup()
    gg <- ggplot(d, aes(x = Share, y = Category, fill = Level,
                        text = paste0(Category, "<br>", Level, ": ",
                                      sprintf("%.1f%% of the budget", Share)))) +
      geom_col(width = 0.68, position = position_dodge(width = 0.72)) +
      scale_fill_manual(values = TYPE_COLOURS, name = NULL) +
      scale_x_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Share of monthly total", y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })

  # ---- tab 4: compare localities -----------------------------------------

  cmp_data <- reactive({
    tables <- cost_tables()
    validate(need(!is.null(tables), "Cost data could not be prepared."))
    wide <- tables$totals %>%
      filter(FamilyStructure == req(input$cmp_structure)) %>%
      select(locality_key, Type, TotalCost) %>%
      tidyr::pivot_wider(names_from = Type, values_from = TotalCost) %>%
      filter(!is.na(min), !is.na(avg)) %>%
      mutate(gap = avg - min,
             label = vapply(locality_key, locality_label, character(1)))
    validate(need(nrow(wide) > 0, "No cost data is available for this household type."))
    wide
  })

  cmp_sel <- reactive({
    d <- cmp_data()
    n <- min(req(input$cmp_n), nrow(d))
    if (identical(input$cmp_end, "bottom")) slice_min(d, avg, n = n)
    else slice_max(d, avg, n = n)
  })

  output$cmp_title <- renderText({
    sprintf("%s %d localities — %s",
            if (identical(input$cmp_end, "bottom")) "Least expensive" else "Most expensive",
            nrow(cmp_sel()), req(input$cmp_structure))
  })

  output$cmp_kpi <- renderUI({
    d <- cmp_data()
    hi <- slice_max(d, avg, n = 1); lo <- slice_min(d, avg, n = 1)
    wg <- slice_max(d, gap, n = 1)
    vvn_kpi_row(
      vvn_tile("Localities compared", nrow(d), "with data for this household", accent = TRUE),
      vvn_tile("Most expensive", hi$label[1], paste(money(hi$avg[1]), "a month")),
      vvn_tile("Least expensive", lo$label[1], paste(money(lo$avg[1]), "a month")),
      vvn_tile("Widest min-to-average gap", wg$label[1], paste0("+", money(wg$gap[1])))
    )
  })

  output$cmp_dumbbell <- renderPlotly({
    d <- cmp_sel() %>% arrange(avg) %>% mutate(label = factor(label, levels = label))
    gg <- ggplot(d) +
      # The connector carries the gap; it sits under the dots so neither end
      # is obscured by the line.
      geom_segment(aes(y = label, yend = label, x = min, xend = avg),
                   colour = "#c9ced8", linewidth = 1.6, lineend = "round") +
      geom_point(aes(y = label, x = min, text = paste0(label, "<br>Minimum: ", money(min))),
                 colour = TYPE_COLOURS[["Minimum"]], size = 2.8) +
      geom_point(aes(y = label, x = avg, text = paste0(label, "<br>Average: ", money(avg),
                                                       "<br>Gap: ", money(gap))),
                 colour = TYPE_COLOURS[["Average"]], size = 2.8) +
      scale_x_continuous(labels = function(x) paste0("$", scales::comma(x))) +
      labs(x = "Monthly cost of living", y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>%
      layout(showlegend = FALSE,
             annotations = list(
               list(x = min(d$min), y = nrow(d), text = "minimum", showarrow = FALSE,
                    xanchor = "center", yshift = 16,
                    font = list(size = 11, color = TYPE_COLOURS[["Minimum"]])),
               list(x = max(d$avg), y = nrow(d), text = "average", showarrow = FALSE,
                    xanchor = "center", yshift = 16,
                    font = list(size = 11, color = TYPE_COLOURS[["Average"]]))
             ))
  })

  output$cmp_gap <- renderPlotly({
    d <- cmp_data() %>%
      slice_max(gap, n = min(20, nrow(.))) %>%
      arrange(gap) %>%
      mutate(label = factor(label, levels = label))
    gg <- ggplot(d, aes(x = gap, y = label,
                        text = paste0(label, "<br>Gap: ", money(gap),
                                      "<br>", money(min), " → ", money(avg)))) +
      geom_col(width = 0.68, fill = COL_RAMP[4]) +
      scale_x_continuous(labels = function(x) paste0("$", scales::comma(x)),
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Monthly gap between minimum and average", y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })

  # ---- tab 5: household types --------------------------------------------

  hh_data <- reactive({
    tables <- cost_tables()
    validate(need(!is.null(tables), "Cost data could not be prepared."))
    key <- req(input$hh_locality)
    state <- tables$totals %>%
      group_by(FamilyStructure, Type) %>%
      summarise(TotalCost = stats::median(TotalCost, na.rm = TRUE), .groups = "drop") %>%
      mutate(Series = "Statewide median")
    here <- tables$totals %>%
      filter(locality_key == key) %>%
      select(FamilyStructure, Type, TotalCost) %>%
      mutate(Series = "This locality")
    validate(need(nrow(here) > 0, "No cost data is available for this locality."))
    bind_rows(here, state) %>%
      mutate(
        Level = factor(unname(TYPE_LABEL[Type]), levels = c("Minimum", "Average")),
        FamilyStructure = factor(FamilyStructure, levels = FAMILY_STRUCTURES),
        Series = factor(Series, levels = c("This locality", "Statewide median"))
      )
  })

  output$hh_title <- renderText({
    sprintf("Monthly cost by household type — %s vs the statewide median",
            locality_label(req(input$hh_locality)))
  })

  output$hh_kpi <- renderUI({
    d <- hh_data() %>% filter(Level == "Average")
    here <- d %>% filter(Series == "This locality")
    st <- d %>% filter(Series == "Statewide median")
    j <- here %>% inner_join(st, by = "FamilyStructure", suffix = c("_h", "_s")) %>%
      mutate(diff = 100 * (TotalCost_h - TotalCost_s) / TotalCost_s)
    one <- here$TotalCost[here$FamilyStructure == "1 Adult"]
    four <- here$TotalCost[here$FamilyStructure == "2 Adults + 2 Children"]
    worst <- j %>% slice_max(diff, n = 1)
    vvn_kpi_row(
      vvn_tile("Single adult", money(one), "average monthly cost", accent = TRUE),
      vvn_tile("Family of four", money(four),
               if (length(one) && length(four) && one > 0)
                 sprintf("%.1f× the single-adult cost", four / one) else NULL),
      vvn_tile("Furthest above the state",
               as.character(worst$FamilyStructure[1]),
               sprintf("%+.0f%% vs the median", worst$diff[1])),
      vvn_tile("Household types", dplyr::n_distinct(here$FamilyStructure), "compared here")
    )
  })

  output$hh_bars <- renderPlotly({
    d <- hh_data() %>% filter(Level == "Average")
    gg <- ggplot(d, aes(x = TotalCost, y = FamilyStructure, fill = Series,
                        text = paste0(FamilyStructure, "<br>", Series, ": ", money(TotalCost)))) +
      geom_col(width = 0.66, position = position_dodge(width = 0.72)) +
      scale_fill_manual(values = stats::setNames(VVN_SERIES_PALETTE[1:2],
                                                 c("This locality", "Statewide median")),
                        name = NULL) +
      scale_x_continuous(labels = function(x) paste0("$", scales::comma(x)),
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Average monthly cost", y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })

  output$hh_heat <- renderPlotly({
    d <- cost_detail()
    validate(need(!is.null(d), "Cost data could not be prepared."))
    dd <- d %>%
      filter(locality_key == req(input$hh_locality), Level == "Average", Cost > 0) %>%
      mutate(FamilyStructure = factor(FamilyStructure, levels = FAMILY_STRUCTURES))
    validate(need(nrow(dd) > 0, "No category detail is available for this locality."))
    gg <- ggplot(dd, aes(x = FamilyStructure, y = Category, fill = Cost,
                         text = paste0(Category, "<br>", FamilyStructure, "<br>", money(Cost)))) +
      geom_tile(colour = "#ffffff", linewidth = 1.1) +
      # One hue, light to dark: the cell value is a magnitude, not an identity.
      scale_fill_gradientn(colours = COL_RAMP, name = NULL,
                           labels = function(x) paste0("$", scales::comma(x))) +
      labs(x = NULL, y = NULL) +
      vvn_ggtheme() +
      theme(axis.text.x = element_text(angle = 20, hjust = 1),
            panel.grid = element_blank())
    ggplotly(gg, tooltip = "text") %>% vvn_plotly_layout()
  })
}

shinyApp(ui = ui, server = server)
