## ----echo = FALSE-------------------------------------------------------------

header1 <- c("Scale", "Linear", "Linear", "Linear", "Linear", "Log", "Log", 
"Log", "Log", "Logit", "Logit", "Logit", "Logit", "Linear", "Linear", 
"Linear", "Linear", "Log", "Log", "Log", "Log", "Logit", "Logit", 
"Logit", "Logit")
header2 <- c("Variability", "No*", "Additive", "Exponential", "Logit", "No*", 
"Additive", "Exponential", "Logit", "No*", "Additive", "Exponential", 
"Logit", "No*", "Additive", "Exponential", "Logit", "No*", "Additive", 
"Exponential", "Logit", "No*", "Additive", "Exponential", "Logit"
)
header3 <- c( 
  "", 
  rep("No MU-referencing or\nMU-referencing and no time-varying covariates", 12), 
  rep("MU-referencing and time-varying covariates", 12) 
)
rel <- data.frame(
  V1 = c("Linear", "Power", "Exponential", "Additive", "Proportional", "Direct proportional"), 
  V2 = c("Y", "Y", "Y", "Y", "Y", "Y"),
  V3 = c("Y", "Y", "Y", "Y", "Y", "Y"), 
  V4 = c("Y", "Y", "Y", "Y", "Y", "Y"), 
  V5 = c("Y", "Y", "Y", "Y", "Y", "Y"), 
  V6 = c("-", "Y", "-", "-", "-", "Y"), 
  V7 = c("-", "Y", "-", "-", "-", "Y"),
  V8 = c("-", "Y", "-", "-", "-", "Y"), 
  V9 = c("-", "-", "-", "-", "-", "-"), 
  V10 = c("Y", "Y", "Y", "Y", "Y", "Y"), 
  V11 = c("-", "-", "-", "-", "-", "-"),
  V12 = c("-", "-", "-", "-", "-", "-"), 
  V13 = c("Y", "Y", "Y", "Y", "Y", "Y"), 
  V14 = c("Y", "Y", "Y", "Y", "Y", "Y"), 
  V15 = c("Y", "-", "-", "Y", "-", "-"),
  V16 = c("Y", "Y", "Y", "Y", "Y", "Y"), 
  V17 = c("Y", "-", "-", "Y", "-", "-"),
  V18 = c("-", "Y", "-", "-", "-", "Y"), 
  V19 = c("-", "-", "-", "-", "-", "-"),
  V20 = c("-", "Y", "-", "-", "-", "Y"), 
  V21 = c("-", "-", "-", "-", "-", "-"), 
  V22 = c("Y", "Y", "Y", "Y", "Y", "Y"), 
  V23 = c("-", "-", "-", "-", "-", "-"), 
  V24 = c("-", "-", "-", "-", "-", "-"),
  V25 = c("Y", "-", "-", "Y", "-", "Y")
)

cor_color <- function(x){ c("#F46D43", "#1A9850")[ as.numeric(x=="Y") + 1 ] }

flextable::flextable(rel) |>
  # Add headers
  flextable::delete_rows( 1, part = "header" ) |>
  flextable::add_header_row( values = header3 ) |>
  flextable::add_header_row( values = header2 ) |>
  flextable::add_header_row( values = header1 ) |> 
  flextable::rotate(i = 2, j = 2:25, rotation = "btlr", part = "header" ) |>
  flextable::merge_h( i = 1:3, part = "header" ) |>
  flextable::align( i = 1:3, j = 2:25, align = "center", part = "header" ) |>
  flextable::align( i = 1:6, j = 2:25, align = "center", part = "body" ) |>
  # Format content
  flextable::border_outer( border = officer::fp_border( color = "grey"), part = "all" ) |>
  flextable::border_inner( border = officer::fp_border( color = "grey"), part = "all" ) |>
  flextable::color( j = ~ . - V1, color = cor_color ) |>
  # Add footer
  flextable::add_footer_row(
    value = "* In absence of variability, parameters are never MU-referenced in code.",
    colwidths = 25
  ) |>
  # flextable::width(width = 1) |>
  flextable::fontsize( i = 1:3, j = 1:25, part = "header", size = 9 ) |>
  flextable::fontsize( i = 1:6, j = 1:25, part = "body", size = 9 ) |> 
  flextable::fontsize( i = 1, part = "footer", size = 8 ) |>
  flextable::set_table_properties(
    width = 1,
    layout = "autofit"
  )

## ----echo = FALSE-------------------------------------------------------------
data.frame(
  Scale = c( rep("Linear/\nLogit", 7), rep("Log", 2) ),
  Covariate = c( rep("Continuous", 3), rep("Discrete", 4), "Continuous", "Discrete"),
  Function = c( 
    "Linear", "Power", "Exponential", "Additive", "Proportional", 
    "Direct\nproportional", "Exponential", "Power", "Direct\nproportional"
  ),
  Code = c(
    "COV1 = THETA(2) * (CONT-25)",
    "COV1 = (CONT/25)**THETA(2)",
    "COV1 = EXP(THETA(2)*(CONT-25))",
    "COV1 = CAT2 * THETA(2) + CAT3 * THETA(3)",
    "COV1 = 1 + CAT2*THETA(2) + CAT3*THETA(3)",
    "COV1 = THETA(2)**CAT2 * THETA(3)**CAT3",
    "COV1 = EXP(THETA(2)*CAT2 + THETA(3)*CAT3)",
    "COV1 = LOG(CONT/25)*THETA(2)",
    "COV1 = THETA(2)*CAT2 + THETA(3)*CAT3"
  ),
  Applies = c( 
    "ACOV_X", "MCOV_X", "MCOV_X", "ACOV_X", "MCOV_X", "MCOV_X", "MCOV_X", 
    "ACOV_X", "ACOV_X"
  )
) |> 
  flextable::flextable() |>
  flextable::set_header_labels( Applies = "Applies to" ) |>
  flextable::merge_v( j = 1, part = "body") |>
  # Format content
  flextable::border_outer( border = officer::fp_border( color = "grey"), part = "all" ) |>
  flextable::border_inner( border = officer::fp_border( color = "grey"), part = "all" ) |>
  flextable::compose( 
    i = 1:9, j = 4, 
    value = flextable::as_paragraph( flextable::as_highlight(Code, color = "#f1f3f5") ),
    part = "body"
  ) |>
  # flextable::width(width = 1) |>
  flextable::fontsize( i = 1, j = 1:5, part = "header", size = 9 ) |>
  flextable::fontsize( i = 1:9, j = 1:5, part = "body", size = 9 ) |>
  flextable::set_table_properties(
    width = 1,
    layout = "autofit"
  )

## ----echo = FALSE-------------------------------------------------------------
table <- read.csv("../inst/resources/covariate_code.csv")
table |> 
  dplyr::filter( mu == FALSE ) |>
  dplyr::select( -mu, -time ) |>
  flextable::flextable() |>
  flextable::merge_v( j = 1, part = "body" ) |>
  # Format content
  flextable::border_outer( border = officer::fp_border( color = "grey"), part = "all" ) |>
  flextable::border_inner( border = officer::fp_border( color = "grey"), part = "all" ) |>
  # flextable::width(width = 1) |>
  flextable::fontsize( i = 1, j = 1:3,  part = "header", size = 9 ) |>
  flextable::fontsize( i = 1:12, j = 1:3, part = "body", size = 9 ) |>
  flextable::set_table_properties(
    width = 1,
    layout = "autofit"
  )

## ----echo = FALSE-------------------------------------------------------------
table |> 
  dplyr::filter( mu == TRUE & time == FALSE ) |>
  dplyr::select( -mu, -time ) |>
  flextable::flextable() |>
  flextable::merge_v( j = 1, part = "body" ) |>
  # Format content
  flextable::border_outer( border = officer::fp_border( color = "grey"), part = "all" ) |>
  flextable::border_inner( border = officer::fp_border( color = "grey"), part = "all" ) |>
  # flextable::width(width = 1) |>
  flextable::fontsize( i = 1, j = 1:3,  part = "header", size = 9 ) |>
  flextable::fontsize( i = 1:12, j = 1:3, part = "body", size = 9 ) |>
  flextable::set_table_properties(
    width = 1,
    layout = "autofit"
  )

## ----echo = FALSE-------------------------------------------------------------
table |> 
  dplyr::filter( mu == TRUE & time == TRUE ) |>
  dplyr::select( -mu, -time ) |>
  flextable::flextable() |>
  flextable::merge_v( j = 1, part = "body" ) |>
  # Format content
  flextable::border_outer( border = officer::fp_border( color = "grey"), part = "all" ) |>
  flextable::border_inner( border = officer::fp_border( color = "grey"), part = "all" ) |>
  # flextable::width(width = 1) |>
  flextable::fontsize( i = 1, j = 1:3,  part = "header", size = 9 ) |>
  flextable::fontsize( i = 1:12, j = 1:3, part = "body", size = 9 ) |>
  flextable::set_table_properties(
    width = 1,
    layout = "autofit"
  )

