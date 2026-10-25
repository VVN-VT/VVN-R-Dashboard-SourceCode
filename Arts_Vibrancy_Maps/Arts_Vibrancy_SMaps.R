# ============================================================================
# Virginia Arts Vibrancy
# ============================================================================
# The Virginia slice of the DSPG arts vibrancy project: a composite index of
# arts activity for every locality, and the measures it is built from.
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

source("vvn_ui.R")

# This dashboard's own sequential ramp. Every dashboard in the suite uses a
# different hue on the same lightness ladder, so the seven are distinguishable
# at a glance while still reading as one family -- see VVN_RAMPS.
ARTS_RAMP     <- vvn_ramp("indigo")

options(tigris_use_cache = TRUE)

DATA_DIR   <- "Arts_Vibrancy_Data"
stopifnot(dir.exists(DATA_DIR))

index      <- readRDS(file.path(DATA_DIR, "va_vibrancy_index.rds"))
components <- readRDS(file.path(DATA_DIR, "va_vibrancy_components.rds"))

# Arts gets its own plum ramp within the shared VVN frame, distinct from the
# rose/maroon count scale and the amber rate scale used elsewhere.
#
# Built on an even lightness ladder (HCL L 87 -> 30 in equal 11.4 steps) rather
# than taken off the shelf. The previous ColorBrewer RdPu ramp stepped
# -7.2, -9.1, -11.6, -18.6, -16.8: its light end was crowded into shades a
# reader cannot separate while its dark end was spread wide, so the colour was
# not a fair read of magnitude. Hue is held inside a 33-degree plum band
# instead of travelling 69 degrees from red to purple, and the lightest step
# sits deltaE 12.8 from the no-data grey so "a little" never reads as "none".
# Arts uses the shared water-lily-pink ramp.
ARTS_PALETTE <- ARTS_RAMP

# One fill for every single-series chart in this app. Using two adjacent steps
# of the ramp -- as this app previously did, [4] for bars and [5] for points --
# makes two charts look deliberately different for no reason; the two shades
# are only deltaE 10.8 apart, which reads as an inconsistency rather than a
# distinction.
ARTS_FILL <- ARTS_PALETTE[5]

# Three weightings of the same underlying measures. They are offered side by
# side rather than one being picked, because the choice of weighting is a
# judgement and the rankings genuinely disagree -- Norfolk sits 3rd on equal
# weights and 5th on PCA.
METHODS <- c(
  "Equal weights"    = "ew",
  "Factor analysis"  = "fa",
  "Principal components" = "pca"
)

#' Component measures, labelled. The upstream column names are terse codes.
#'
#' Every one of these is a PER-RESIDENT rate, not a count or a dollar total,
#' and the stored values make that unambiguous: federal grants top out at
#' 3.1e-05, arts organisations at 7.3e-05 and arts employment at 1.1e-03 --
#' magnitudes that are only meaningful once divided by population. State arts
#' funding peaks at 6.27, which is dollars per resident (Bath County, with
#' about 4,200 people, so roughly $26,000 in total).
#'
#' The labels previously read "Federal arts grants (count)" and "Arts
#' organisations", which invited the reader to take 7e-05 as a number of
#' organisations. They now name the unit.
COMPONENT_LABELS <- c(
  feddol    = "Federal arts funding ($ per resident)",
  fedno     = "Federal arts grants (per resident)",
  stgdol    = "State arts funding ($ per resident)",
  stgno     = "State arts grants (per resident)",
  ArtOrg    = "Arts organisations (per resident)",
  cfirm     = "Creative firms (per resident)",
  cfrev     = "Creative firm revenue ($ per resident)",
  cfcrev    = "Creative firm contributed revenue ($ per resident)",
  cfprev    = "Creative firm programme revenue ($ per resident)",
  cfcomp    = "Creative firm compensation ($ per resident)",
  cfexp     = "Creative firm expenses ($ per resident)",
  cfgov     = "Creative firm government support ($ per resident)",
  ArtEmp    = "Arts employment (per resident)",
  IndArtPay = "Independent artist pay ($ per resident)"
)
# The component extract is NOT 62 Virginia localities. It is 40 counties, 7
# independent cities and 15 metropolitan/micropolitan statistical areas, and
# the three are different kinds of geography that must not share a ranking:
#
#  * A metro area AGGREGATES localities. Richmond, VA MSA contains Henrico,
#    Chesterfield and more, each also present as its own row, so a ranking that
#    mixes the two counts those counties twice and puts the MSA on top by
#    construction rather than by performance.
#  * Four of the metro areas are not even Virginia-only -- the Washington MSA
#    spans DC, Maryland and West Virginia, and Kingsport-Bristol, Bluefield and
#    Virginia Beach-Chesapeake-Norfolk each cross a state line. Their totals
#    include arts activity outside Virginia entirely.
#
# So the geography type is derived here and surfaced as a filter, with
# localities (counties + cities) shown by default.
GEO_TYPES <- c("County", "Independent city", "Metro area")
components$geo_type <- dplyr::case_when(
  grepl("MSA$|MicroSA$", components$LOCALITY) ~ "Metro area",
  grepl(" city$", components$LOCALITY)         ~ "Independent city",
  TRUE                                          ~ "County"
)
components$geo_type <- factor(components$geo_type, levels = GEO_TYPES)

#' A chart-axis label for a locality or metro area.
#'
#' Metro names in this file run to 48 characters -- "Washington-Arlington-
#' Alexandria, DC-VA-MD-WV MSA" -- which no amount of margin makes readable on
#' a bar chart. The state-code tail and the MSA/MicroSA suffix carry nothing the
#' reader needs once the row is marked as a metro area, and the hyphenated city
#' list is the bulk of what is left, so only the lead city is kept.
#'
#' "(metro)" is retained rather than dropped entirely: without it "Richmond"
#' would be three different rows -- Richmond County, Richmond city and the
#' Richmond metro area -- and the shortened label would silently collide. The
#' full official name stays in the hover text.
vvn_short_area <- function(x) {
  out <- sub(",[ ]*[A-Z]{2}(-[A-Z]{2})*[ ]*(MSA|MicroSA)$", "", x)
  is_metro <- grepl("MSA$|MicroSA$", x)
  out[is_metro] <- paste0(sub("-.*$", "", out[is_metro]), " (metro)")
  out
}

N_LOCALITY_ROWS <- sum(components$geo_type != "Metro area")
N_METRO_ROWS    <- sum(components$geo_type == "Metro area")

# Which measures are actually reported below the metro level. This matters more
# than it looks: of the fourteen component measures, only state arts funding and
# the state grant count carry a non-zero value for counties and cities (46 of
# 47). Federal funding, arts organisations and arts employment are zero for
# every single county and city, and the creative-firm measures are non-zero for
# between two and five of them. They are metro-area statistics that happen to be
# stored in the same table.
#
# Computed rather than hard-coded so it stays true if the extract is refreshed.
LOCALITY_LEVEL_MEASURES <- local({
  loc <- components$geo_type != "Metro area"
  vapply(intersect(names(COMPONENT_LABELS), names(components)), function(v) {
    x <- suppressWarnings(as.numeric(components[[v]]))
    sum(x[loc] > 0, na.rm = TRUE) >= 0.5 * sum(loc)
  }, logical(1))
})
LOCALITY_LEVEL_MEASURES <- names(LOCALITY_LEVEL_MEASURES)[LOCALITY_LEVEL_MEASURES]

COMPONENTS <- intersect(names(COMPONENT_LABELS), names(components))
COMPONENT_CHOICES <- stats::setNames(COMPONENTS, COMPONENT_LABELS[COMPONENTS])

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

#' The index uses bare locality names, which collide for Fairfax, Franklin,
#' Richmond and Roanoke. Resolved through the shared helper so a county is never
#' confused with the independent city of the same name.
index_keyed <- local({
  value <- NULL; done <- FALSE
  function() {
    if (done) return(value)
    done <<- TRUE
    shapes <- load_boundaries()
    if (is.null(shapes)) return(value)
    value <<- index %>%
      mutate(locality_key = vvn_resolve_locality(locality, shapes)) %>%
      filter(!is.na(locality_key))
    value
  }
})

# ===== UI ==================================================================

ui <- page_fluid(
  title = "Virginia Arts Vibrancy",
  theme = vvn_theme(),
  tags$head(vvn_css()),

  vvn_header(
    "Virginia Arts Vibrancy",
    "A composite index of arts activity across Virginia's localities, and the measures behind it."
  ),

  navset_tab(
    # ---------------------------------------------------------------- tab 1
    nav_panel(
      "Vibrancy index",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          # No "compared with the state" control here. The vibrancy index is a
          # standardised score centred near zero and running negative (median
          # -0.23), so it has no true zero and no meaningful ratio: a percentage
          # difference from a negative reference inverts the sign and put every
          # locality on one side of the scale. The three weightings already
          # provide the comparison this tab needs.
          vvn_group("Weighting method"),
          selectInput("method", NULL, choices = METHODS, selected = "ew"),
          vvn_help(
            "Three ways of combining the same measures. Equal weights treats",
            "every component alike; factor analysis and principal components",
            "let the data decide what matters."
          ),
          vvn_help(
            "The methods disagree, which is informative rather than a flaw —",
            "switching between them shows which localities rank high regardless",
            "of how the index is built."
          )
        ),

        uiOutput("index_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("index_title", inline = TRUE)),
          card_body(padding = 0, leafletOutput("index_map", height = "560px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header("Top 15 localities"),
            card_body(padding = 0, plotlyOutput("top_chart", height = "380px"))
          )
        ),

        vvn_notes(
          "What this map shows",
          p(
            class = "vvn-help",
            strong("How the score is built."), "Each of the thirteen component measures is first divided by county",
            "population, then cost-of-living adjusted by metropolitan area for the eight money measures, then",
            "converted to a z-score — centred on zero with a standard deviation of one. The standardised values",
            "are", strong("winsorised"), "above +5 standard deviations (the excess divided by ten, or by five for",
            "federal grants and independent artist pay) so a single extreme locality cannot dominate an indicator.",
            "That is why the index runs negative and why a score of 0 means \u201ctypical\u201d rather than \u201cnone\u201d."
          ),
          p(
            "A composite score of arts activity for each Virginia locality, built from arts funding, creative firms,",
            "arts organisations and arts employment. Darker shading means more vibrant.",
            strong("Charlottesville"), "ranks first on all three weightings, with", strong("Richmond"), "and",
            strong("Staunton"), "close behind — independent cities dominate the top of the table, which is",
            "largely a density effect: the index is not population-adjusted in the same way across every component."
          ),
          p(
            "Switch weighting methods to test how robust a ranking is. A locality that stays near the top under",
            "equal weights, factor analysis", strong("and"), "principal components is genuinely vibrant; one that",
            "moves sharply is an artefact of how the components were combined."
          ),
          p(
            class = "vvn-help",
            "Index values are standardised scores, not counts — the absolute number is only meaningful relative",
            "to other Virginia localities."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 2
    nav_panel(
      "Component measures",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          vvn_group("Measure"),
          selectInput(
            "component", NULL, choices = COMPONENT_CHOICES,
            # Default to a measure that is actually reported for counties and
            # cities; most of the others are metro-area only.
            selected = if (length(LOCALITY_LEVEL_MEASURES)) LOCALITY_LEVEL_MEASURES[1]
                       else COMPONENTS[1]
          ),
          vvn_help(
            "The individual inputs to the index, before any weighting.",
            "Useful for seeing whether a locality's score is driven by funding,",
            "by employment, or by the number of organisations."
          ),
          vvn_group("Geography"),
          checkboxGroupInput(
            "comp_geo", NULL, choices = GEO_TYPES, selected = GEO_TYPES
          ),
          vvn_help(
            sprintf("The file holds %d counties and cities plus %d metro areas, for %s.",
                    N_LOCALITY_ROWS, N_METRO_ROWS,
                    paste(unique(components$YEAR), collapse = ", "))
          ),
          vvn_help(
            "Metro areas overlap the counties listed beside them, so a ranking",
            "that mixes the two counts those counties twice. They are included",
            "by default only because most measures are reported at no finer",
            "level than this."
          ),
          vvn_help(
            sprintf("Reported for counties and cities: %s. Every other measure is metro-area only.",
                    paste(unname(COMPONENT_LABELS[LOCALITY_LEVEL_MEASURES]), collapse = ", "))
          )
        ),

        uiOutput("comp_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("comp_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("comp_chart", height = "560px"))
        ),

        vvn_notes(
          "Reading these measures",
          p(
            "Each bar is one locality's value for the selected measure. Unlike the index these are not standardised,",
            "so they can be compared directly — but they are", strong("rates per resident"), "rather than counts or",
            "dollar totals, and they are heavily skewed, because arts funding and creative employment concentrate",
            "in a handful of places."
          ),
          p(
            "Comparing measures shows that vibrancy is not one thing. A locality can rank well on",
            strong("arts organisations"), "while receiving little", strong("federal funding"),
            ", or carry substantial", strong("arts employment"), "with few separate organisations. The index",
            "collapses that into a single number; this tab is where it comes apart again."
          ),
          p(
            class = "vvn-help",
            sprintf("Covers %d of Virginia's 133 counties and independent cities, plus %d metropolitan and",
                    N_LOCALITY_ROWS, N_METRO_ROWS),
            "micropolitan areas that aggregate them; metro areas are off by default. Localities absent are not",
            "necessarily without arts activity — they are simply not in this extract."
          )
        )
      )
    ),

    # ---------------------------------------------------------------- tab 3
    nav_panel(
      "Funding and employment",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          vvn_group("Horizontal axis"),
          selectInput("fe_x", NULL, choices = COMPONENT_CHOICES,
                      selected = "feddol"),
          vvn_group("Vertical axis"),
          selectInput("fe_y", NULL, choices = COMPONENT_CHOICES,
                      selected = "ArtEmp"),
          vvn_help(
            "Both measures are already per resident, so there is no per-capita",
            "toggle \u2014 dividing by population again would normalise twice."
          ),
          checkboxInput("fe_log", "Log scales", value = TRUE),
          vvn_help(
            "Each bubble is a locality, sized by population. Two component",
            "measures against each other, in the manner of a Gapminder chart."
          ),
          vvn_help(
            "Most arts measures scale with population, so the per-capita toggle",
            "is usually the more informative view."
          )
        ),
        uiOutput("fe_kpi"),
        card(
          full_screen = TRUE,
          card_header(textOutput("fe_title", inline = TRUE)),
          card_body(padding = 0, plotlyOutput("fe_bubble", height = "520px"))
        ),
        div(
          style = "margin-top:16px;",
          card(
            full_screen = TRUE,
            card_header(textOutput("fe_rank_title", inline = TRUE)),
            card_body(padding = 0, plotlyOutput("fe_rank", height = "440px"))
          )
        ),
        vvn_notes(
          "Reading these charts",
          p(
            "The component tab maps one measure at a time. This plots two against each other, which is where the",
            "relationships live: whether federal funding follows arts employment, whether creative-firm revenue",
            "tracks the number of firms, and which localities sit well off the pattern."
          ),
          p(
            strong("These are rates, not totals."), "Every component measure is already expressed per resident, so",
            "a small county with one well-funded organisation can outrank a large metro area — and does. Bath",
            "County tops state arts funding per resident at $6.27, which is roughly $26,000 across its 4,200",
            "people. Reading these as totals would invert the picture entirely."
          ),
          p(
            strong("This extract is sparse, and that is the first thing to know about it."), "Most component",
            "measures are zero or unreported for a majority of the 62 localities: federal arts funding and arts",
            "employment each have a positive value for fewer than ten. The count in the chart title is the honest",
            "sample size for whatever pair of axes you choose, and it is often small."
          ),
          p(
            "Log scales are available because these measures span orders of magnitude, but they are",
            strong("off by default"), "because a logarithm of zero is undefined: switching them on silently drops",
            "every locality reporting zero, which here can be most of the state. The title says how many remain."
          ),
          p(
            class = "vvn-help",
            sprintf("Component measures cover %d of Virginia's 133 counties and cities, for %s.",
                    N_LOCALITY_ROWS, paste(sort(unique(components$YEAR)), collapse = ", ")),
            "Localities with a zero or missing value on either axis drop out when log scales are on, because a",
            "logarithm of zero is undefined."
          )
        )
      )
    )
  )
)

# ===== SERVER ==============================================================

server <- function(input, output, session) {

  # ---- index -------------------------------------------------------------

  index_values <- reactive({
    m <- req(input$method)
    idx <- index_keyed()
    req(idx)
    # `locality` is deliberately dropped: vvn_locality_key() already puts a
    # column of that name on the boundaries, and joining both sides yields
    # locality.x / locality.y, leaving dat$locality NULL -- which then blows up
    # inside sprintf() with "invalid 'type' (list) of argument". The boundary
    # copy is the canonical display name, so keep that one.
    idx %>%
      transmute(locality_key,
                value = .data[[paste0("index_", m)]],
                rank  = .data[[paste0("rank_",  m)]])
  })

  output$index_title <- renderText({
    nm <- names(METHODS)[METHODS == req(input$method)]
    paste0("Arts vibrancy index — ", tolower(nm))
  })

  #' Index values with the boundary file's display name attached.
  index_named <- reactive({
    d <- index_values(); shapes <- load_boundaries()
    req(d)
    if (is.null(shapes)) return(mutate(d, locality = locality_key))
    d %>% left_join(
      sf::st_drop_geometry(shapes)[, c("locality_key", "locality")],
      by = "locality_key"
    )
  })

  output$index_kpi <- renderUI({
    d <- index_named()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    top <- d %>% arrange(rank) %>% head(1)
    bot <- d %>% arrange(desc(rank)) %>% head(1)

    vvn_kpi_row(
      vvn_tile("Most vibrant", top$locality,
               sprintf("index %.2f", top$value), accent = TRUE),
      vvn_tile("Localities ranked", nrow(d), "of Virginia's 133"),
      vvn_tile("Median index", sprintf("%.2f", stats::median(d$value)),
               "half of localities score below this"),
      vvn_tile("Least vibrant", bot$locality, sprintf("index %.2f", bot$value))
    )
  })

  output$index_map <- renderLeaflet({
    shapes <- load_boundaries()
    validate(need(!is.null(shapes),
                  "County boundaries could not be downloaded from the Census TIGER service."))
    d <- index_values()
    validate(need(!is.null(d) && nrow(d) > 0, "No index data available."))

    dat <- shapes %>% left_join(d, by = "locality_key")
    values <- dat$value
    validate(need(any(!is.na(values)), "No index data for this method."))

    # Quantile bins in level mode: the index has a long upper tail, with
    # Charlottesville far above the rest, so a linear ramp would compress the
    # other 132 localities.
    shading <- vvn_map_shading(values, "level",
                               seq_palette = ARTS_PALETTE,
                               level_title = "Vibrancy<br>index")
    shade <- shading$values
    pal <- shading$pal

    lab <- sprintf(
      "<strong>%s</strong><br>Index: <strong>%s</strong><br>Rank: %s of %d",
      dat$locality,
      ifelse(is.na(values), "&mdash;", sprintf("%.2f", values)),
      ifelse(is.na(dat$rank), "&mdash;", dat$rank), nrow(d)
    )

    leaflet(dat) %>%
      vvn_basemap() %>%
      addPolygons(
        fillColor = ifelse(is.na(shade), VVN_NO_DATA, pal(shade)),
        fillOpacity = 0.92,
        color = VVN_STROKE_COLOUR, weight = VVN_STROKE_WEIGHT,
        opacity = VVN_STROKE_OPACITY,
        label = lapply(lab, HTML), labelOptions = vvn_labelopts(),
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

  output$top_chart <- renderPlotly({
    d <- index_named()
    validate(need(!is.null(d) && nrow(d) > 0, "No index data available."))
    top <- d %>% arrange(rank) %>% head(15) %>%
      mutate(locality = factor(locality, levels = rev(locality)))

    gg <- ggplot(top, aes(y = locality)) +
      geom_segment(aes(x = 0, xend = value, yend = locality),
                   colour = ARTS_PALETTE[2], linewidth = 1.1) +
      geom_point(aes(x = value, colour = value,
                     text = paste0(locality, "<br>index ", sprintf("%.2f", value),
                                   "<br>rank ", rank)),
                 size = 3.2) +
      scale_colour_gradientn(colours = ARTS_PALETTE, guide = "none") +
      scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.06))) +
      labs(x = "Index score", y = NULL) +
      vvn_ggtheme()

    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>%
      layout(showlegend = FALSE)
  })

  # ---- components --------------------------------------------------------

  output$comp_title <- renderText({
    nm <- names(COMPONENT_CHOICES)[COMPONENT_CHOICES == req(input$component)]
    d <- comp_values()
    scope <- if ("Metro area" %in% input$comp_geo) {
      if (length(input$comp_geo) == 1) "metro areas" else "areas, mixing localities and metro areas"
    } else "counties and cities"
    sprintf("%s \u2014 %d %s", nm, nrow(d), scope)
  })

  comp_values <- reactive({
    v <- req(input$component)
    keep <- req(input$comp_geo)
    components %>%
      filter(geo_type %in% keep) %>%
      transmute(locality = LOCALITY, geo_type, value = .data[[v]]) %>%
      filter(!is.na(value))
  })

  output$comp_kpi <- renderUI({
    d <- comp_values()
    if (nrow(d) == 0) return(NULL)
    fmt <- function(x) {
      if (max(d$value, na.rm = TRUE) > 1e5) {
        paste0("$", formatC(x, format = "d", big.mark = ","))
      } else formatC(x, format = "f", digits = 0, big.mark = ",")
    }
    top <- d %>% arrange(desc(value)) %>% head(1)

    vvn_kpi_row(
      vvn_tile("Highest", fmt(top$value), top$locality, accent = TRUE),
      vvn_tile("Median", fmt(stats::median(d$value)),
               sprintf("across %d areas shown", nrow(d))),
      vvn_tile("Largest share",
               sprintf("%.0f%%", 100 * top$value / sum(d$value, na.rm = TRUE)),
               "of the total shown"),
      vvn_tile("Reporting zero", sum(d$value == 0), "of those shown")
    )
  })

  output$comp_chart <- renderPlotly({
    d <- comp_values() %>%
      arrange(desc(value)) %>%
      head(25) %>%
      mutate(full_name = locality,
             locality = vvn_short_area(locality)) %>%
      mutate(locality = factor(locality, levels = rev(locality)))
    validate(need(nrow(d) > 0, "No data for this measure."))

    # Every value zero is the case that previously drew 25 invisible bars and
    # looked like a broken chart. Say what is actually wrong and what to do.
    v <- req(input$component)
    if (all(d$value == 0, na.rm = TRUE)) {
      metro_has <- sum(suppressWarnings(as.numeric(
        components[[v]][components$geo_type == "Metro area"])) > 0, na.rm = TRUE)
      validate(need(FALSE, paste0(
        COMPONENT_LABELS[[v]], " is zero for every area currently shown.",
        if (!("Metro area" %in% input$comp_geo) && metro_has > 0) {
          paste0(" This measure is reported only for metropolitan areas in this extract (",
                 metro_has, " of ", N_METRO_ROWS,
                 " have a value). Tick \u201cMetro area\u201d under Geography to see it.")
        } else {
          " No area in this extract reports a value for it."
        }
      )))
    }

    gg <- ggplot(d, aes(x = value, y = locality,
                        text = paste0(full_name, "<br>", geo_type, "<br>",
                                      formatC(value, format = "f", digits = 4, big.mark = ",")))) +
      geom_col(aes(fill = value), width = 0.7) +
      scale_fill_gradientn(colours = ARTS_PALETTE, guide = "none") +
      # Metro-area rows are marked, so a reader who switches them on can see at
      # a glance which bars aggregate the counties listed below them.
      geom_point(
        data = function(x) dplyr::filter(x, geo_type == "Metro area"),
        aes(x = 0, y = locality), inherit.aes = FALSE,
        shape = 18, size = 2.2, colour = VVN_MUTED
      ) +
      scale_x_continuous(labels = function(x) formatC(x, format = "d", big.mark = ",")) +
      labs(x = NULL, y = NULL) +
      vvn_ggtheme()

    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>%
      layout(showlegend = FALSE)
  })

  # ===== TAB 3: FUNDING AND EMPLOYMENT =====================================

  comp_label <- function(v) {
    nm <- COMPONENT_LABELS[[v]]
    if (is.null(nm) || is.na(nm)) v else nm
  }

  fe_data <- reactive({
    xv <- req(input$fe_x); yv <- req(input$fe_y)

    d <- components %>%
      transmute(
        locality = LOCALITY,
        population = suppressWarnings(as.numeric(POPULATION)),
        x = suppressWarnings(as.numeric(.data[[xv]])),
        y = suppressWarnings(as.numeric(.data[[yv]]))
      ) %>%
      filter(is.finite(x), is.finite(y))

    # No per-capita conversion here: the component measures are already rates
    # per resident, so dividing by population again would normalise twice and
    # produce values with no interpretation. Population is kept only to size
    # the bubbles.
    if (isTRUE(input$fe_log)) {
      # log of zero is undefined; dropping those rows is stated in the notes.
      d <- d %>% filter(x > 0, y > 0)
    }
    validate(need(nrow(d) > 0, "No locality has both measures for this combination."))
    d
  })

  fe_axis <- function(v) comp_label(v)

  output$fe_title <- renderText({
    d <- fe_data()
    n <- nrow(d); total <- nrow(components)
    sprintf("%s against %s — %d of %d localities plotted%s",
            comp_label(req(input$fe_y)), tolower(comp_label(req(input$fe_x))),
            n, total,
            if (isTRUE(input$fe_log) && n < total)
              ", zeros excluded by the log scale" else "")
  })

  output$fe_kpi <- renderUI({
    d <- fe_data()
    r <- suppressWarnings(stats::cor(d$x, d$y, use = "complete.obs", method = "spearman"))
    tx <- slice_max(d, x, n = 1); ty <- slice_max(d, y, n = 1)
    vvn_kpi_row(
      vvn_tile("Localities plotted", nrow(d), "with both measures", accent = TRUE),
      vvn_tile(paste("Highest", tolower(comp_label(input$fe_x))),
               as.character(tx$locality[1]), scales::comma(signif(tx$x[1], 3))),
      vvn_tile(paste("Highest", tolower(comp_label(input$fe_y))),
               as.character(ty$locality[1]), scales::comma(signif(ty$y[1], 3))),
      vvn_tile("Rank correlation",
               if (is.na(r)) "—" else sprintf("%+.2f", r),
               "between the two measures")
    )
  })

  output$fe_bubble <- renderPlotly({
    d <- fe_data()
    gg <- ggplot(d, aes(x = x, y = y, size = population,
                        text = paste0(vvn_short_area(locality), "<br>",
                                      fe_axis(input$fe_x), ": ", scales::comma(signif(x, 3)), "<br>",
                                      fe_axis(input$fe_y), ": ", scales::comma(signif(y, 3)), "<br>",
                                      "population ", scales::comma(population)))) +
      geom_point(colour = ARTS_FILL, alpha = 0.6) +
      scale_size_area(max_size = 10, guide = "none") +
      labs(x = fe_axis(input$fe_x), y = fe_axis(input$fe_y)) +
      vvn_ggtheme()
    if (isTRUE(input$fe_log)) {
      gg <- gg +
        scale_x_log10(labels = scales::comma) +
        scale_y_log10(labels = scales::comma)
    } else {
      gg <- gg +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::comma)
    }
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })

  output$fe_rank_title <- renderText({
    sprintf("Top localities by %s", tolower(fe_axis(req(input$fe_y))))
  })

  output$fe_rank <- renderPlotly({
    d <- fe_data() %>%
      slice_max(y, n = 20) %>%
      arrange(y) %>%
      mutate(full_name = locality, locality = vvn_short_area(locality)) %>%
      mutate(locality = factor(locality, levels = unique(locality)))
    gg <- ggplot(d, aes(x = y, y = locality,
                        text = paste0(full_name, "<br>",
                                      fe_axis(input$fe_y), ": ",
                                      scales::comma(signif(y, 3))))) +
      geom_col(width = 0.68, fill = ARTS_FILL) +
      scale_x_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = fe_axis(input$fe_y), y = NULL) +
      vvn_ggtheme()
    ggplotly(gg, tooltip = "text") %>%
      vvn_plotly_layout() %>% layout(showlegend = FALSE)
  })
}

shinyApp(ui = ui, server = server)
