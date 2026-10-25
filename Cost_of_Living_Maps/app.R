# ============================================================================
# Entrypoint for Posit Connect Cloud
# ============================================================================
# Connect Cloud requires a Shiny app's primary file to be named app.R. It does
# not honour rsconnect's appPrimaryDoc the way shinyapps.io did: deploying with
# a <Name>_SMaps.R entrypoint uploads fine and then fails at publish with
# "Unable to locate Cost_of_Living_SMaps.R".
#
# The dashboard itself stays in Cost_of_Living_SMaps.R so the repository layout,
# the README and the shinyapps.io deployment records all keep working. This
# file just hands Connect Cloud the app object.
#
# source() returns list(value = <last expression>, visible = ...), and
# Cost_of_Living_SMaps.R ends in shinyApp(), so $value is the shiny.appobj.
# It is not printed, so sourcing does not start a second server.
# ============================================================================

source("Cost_of_Living_SMaps.R")$value
