# ============================================================================
# Entrypoint for Posit Connect Cloud
# ============================================================================
# Connect Cloud requires a Shiny app's primary file to be named app.R. It does
# not honour rsconnect's appPrimaryDoc the way shinyapps.io did: deploying with
# a <Name>_SMaps.R entrypoint uploads fine and then fails at publish with
# "Unable to locate Rural_Health_SMaps.R".
#
# source() returns list(value = <last expression>, visible = ...), and
# Rural_Health_SMaps.R ends in shinyApp(), so $value is the shiny.appobj.
# It is not printed, so sourcing does not start a second server.
# ============================================================================

source("Rural_Health_SMaps.R")$value
