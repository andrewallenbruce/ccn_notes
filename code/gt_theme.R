gt_obj <- ccn::states |>
  collapse::fgroup_by(abbr, name) |>
  collapse::fsummarise(code = toString(code)) |>
  gt::gt()

gt_chamber_theme <- function(gt_obj, ...) {
  stopifnot(`'gt_obj' must be a 'gt_tbl'` = "gt_tbl" %in% class(gt_obj))

  table_id <- subset(gt_obj[["_options"]], parameter == "table_id")$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_obj[["_options"]][["parameter"]])[[
      1
    ]]
    gt_obj[["_options"]][["value"]][[opt_position]] <- table_id
  }

  gt_obj |>
    gt::opt_table_font(
      font = list(gt::google_font("Spline Sans Mono"), gt::default_fonts()),
      weight = 500
    ) |>
    gt::tab_style(
      locations = gt::cells_column_labels(columns = gt::everything()),
      style = gt::cell_text(
        font = gt::google_font("JetBrains Mono"),
        weight = 650,
        size = gt::px(12),
        transform = "uppercase"
      )
    ) |>
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        font = gt::google_font("Work Sans"),
        weight = 650,
        size = gt::px(22)
      )
    ) |>
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(
        font = gt::google_font("Work Sans"),
        weight = 500,
        size = gt::px(14)
      )
    ) |>
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(weight = 650, size = gt::px(12), color = "white"),
        gt::cell_fill(color = "black")
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "left",
        weight = gt::px(0.5),
        color = "black"
      ),
      locations = gt::cells_body(columns = c(-names(gt_obj[["_data"]])[1]))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        color = "black",
        weight = gt::px(1.5),
        style = "dotted"
      ),
      locations = gt::cells_body(rows = gt::everything())
    ) |>
    # gt::cols_align(align = "center", columns = gt::everything()) |>
    gt::tab_options(
      table.font.size = 12,
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(0.5),
      column_labels.border.bottom.color = "black",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      table_body.border.top.style = "none",
      heading.border.bottom.style = "none",
      heading.align = "left",
      heading.title.font.size = gt::px(26),
      source_notes.border.lr.style = "none",
      source_notes.font.size = 10,
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = gt::px(1),
      row_group.border.bottom.color = "black",
      row_group.border.bottom.style = "solid",
      row_group.padding = gt::px(1.5),
      ...
    ) |>
    gt::opt_css(
      c(
        paste0(
          "#",
          table_id,
          " tbody tr:last-child {border-bottom: 2px solid #ffffff00;}"
        ),
        paste0(
          "#",
          table_id,
          " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"
        ),
        paste0(
          "#",
          table_id,
          " .gt_sourcenote {border-bottom-color: #FFFDF5 !important;}"
        ),
        paste0(
          "#",
          table_id,
          " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"
        )
      )
    )
}

gt_chamber_theme(gt_obj)
