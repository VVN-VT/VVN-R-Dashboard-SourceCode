# ============================================================================
# Entrypoint for Posit Connect Cloud
# ============================================================================
# Connect Cloud requires a Shiny app's primary file to be named app.R; it
# ignores rsconnect's appPrimaryDoc and fails at publish with "Unable to locate
# VCE_AI_SMaps.R" otherwise.
#
# source() returns list(value = <last expression>, visible = ...), and
# VCE_AI_SMaps.R ends in shinyApp(), so $value is the shiny.appobj.
# It is not printed, so sourcing does not start a second server.
# ============================================================================

source("VCE_AI_SMaps.R")$value
