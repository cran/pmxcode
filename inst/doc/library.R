## ----echo = FALSE-------------------------------------------------------------
data.frame(
  PD = rep("Pharmacodynamic model", 9),
  types = c(
    "None", "Exposure-Response", "Direct effect", "Biophase / Link", 
    "Indirect response", "Defined by ODEs", "Defined by explicit solutions",
    "Logistic regression", "Ordered categorical model"
  ),
  None = c("-", "Y", "-", "-", "-", "Y", "Y", "Y", "Y"),
  Subroutine = c("Y", "-", "Y", "Y", "Y", "Y", "-", "-", "-"),
  First = c("Y", "-", "Y", "Y", "Y", "Y", "-", "-", "-"),
  ODE = c("Y", "-", "Y", "Y", "Y", "Y", "-", "-", "-"),
  Explicit = c("-", "Y", "-", "-", "-", "-", "Y", "-", "-")
) |>
  flextable::flextable() |>
  flextable::set_header_labels(
    PD = "",
    types = "",
    Subroutine = "Defined by\nsubroutines",
    First = "Defined by\nfirst-order rates",
    ODE = "Defined by\nODEs",
    Explicit = "Defined by\nexplicit solutions"
  ) |>
  flextable::add_header_row( values = c(rep("", 2), rep("Pharmacokinetic model", 5) ) ) |>
  flextable::rotate( i = 1:9, j = 1 , rotation = "btlr" ) |>
  flextable::merge_h( i = 1, part = "header") |>
  flextable::merge_at( i = 1:2, j = 1:2, part = "header") |>
  flextable::merge_v( j = 1, part = "body") |>
  flextable::align( i = 1:2, j = 1:7, align = "center", part = "header" ) |>
  flextable::align( i = 1:9, j = 1:7, align = "center", part = "body" ) |>
  # Format content
  flextable::border_outer( border = officer::fp_border( color = "grey"), part = "all" ) |>
  flextable::border_inner( border = officer::fp_border( color = "grey"), part = "all" ) |>
  flextable::fontsize( i = 1:2, j = 1:7, part = "header", size = 9 ) |>
  flextable::fontsize( i = 1:9, j = 1:7, part = "body", size = 9 ) |> 
  flextable::set_table_properties(
    width = 1,
    layout = "autofit"
  )

