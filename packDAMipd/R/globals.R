## Required to avoid the note/warning with RMD check "no visible binding for
## global variable"
#' @importFrom utils globalVariables
utils::globalVariables(
  names = c(
    "cost_data", "trace_data", "utility_data",
    "table_param", "trial_data", "adl_scoring", "promis3a_scoring", "blank"
  ),
  package = "packDAMipd", add = TRUE
)
