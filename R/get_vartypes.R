#'Get Variable Type for downloading data
#'
#'This function uses the project meta information to determine a vector of
#'variable classes. This vector can be used to passed into read_csv when
#'downloading data via API to make sure that the classes are read correctly.
#'Checkbox, radio, dropdown and yesno items are set as integer, text fields
#'are set to integer, numeric or character depending on their field validation,
#'calculated fields are set to numeric. Every other variable type are set as
#'character.
#'
#'
#'
#'@param meta project meta information as downloaded by redcap_export_meta
#'
#'@return Character vector of variable classes.
#'@export
#'@importFrom dplyr select filter mutate case_when if_else group_by summarise pull bind_rows row_number slice slice_head
#'@importFrom tidyr uncount
#'@importFrom purrr list_rbind map
#'@importFrom stringr str_count
#'
#' @examples
#' # data(proj)
#' # get_vartypes(proj)
#'
#' # if using local data:
#' # token <- "xxxxx"
#' # url <- "xxxxx"
#' # meta <- redcap_export_meta(token,url)
#' # get_vartypes(meta)


get_vartypes <- function(meta){

  field_name <- form_name <- field_type <- text_validation_type_or_show_slider_number <- select_choices_or_calculations <- choices <- n <- form_name <- NULL

  # if missing codes are used one more row needs to be added for checkbox fields
  # (pipe is used to parse the choices string which is why 1 needs to be added either way)
  if (is.na(meta$project$missing_data_codes)) {
    cb <- 1
  } else if (!is.na(meta$project$missing_data_codes)) {
    cb <- 2
  }

  dd <- meta$metadata |>
    select(field_name,
           form_name,
           field_type,
           field_val = text_validation_type_or_show_slider_number,
           choices = select_choices_or_calculations) |>
    # descr fields are not in data export
    filter(field_type != 'descriptive') |>
    # single-choice & checkbox fields are converted to integer, validated text
    # to integer or numeric, calc to numeric
    mutate(class = case_when(field_type == 'yesno' ~ 'integer',
                             field_type == 'dropdown' ~ 'integer',
                             field_type == 'radio' ~ 'integer',
                             field_type == 'checkbox' ~ 'integer',
                             field_type == 'calc' ~ 'numeric',
                             field_type == 'text' & field_val == 'integer' ~ 'integer',
                             field_type == 'text' & grepl("number",field_val) ~ 'numeric',
                             TRUE ~ 'character'
    ),
    # checkbox fields need to be copied according to number of choices (and
    # whether the missing option is activated)
    n = if_else(field_type == 'checkbox',str_count(choices,pattern = '\\|')+cb,1)
    ) |>
    uncount(n)

  # additional row needs to be added at the end of each form for the complete
  # item:
  dd2 <- map(unique(dd$form_name), function(formname){
      dd |>
      filter(form_name == formname) |>
      bind_rows(data.frame(field_name  = paste0(formname,"_complete"),
                             form_name = formname,
                             class = "integer"))
      }) |>
    list_rbind() |>
    select(field_name,form_name,class) |>
    mutate(form_name = if_else(row_number() == 1,NA,form_name))

  # additional rows need to be added if project is longitudinal and/or has
  # repeating instruments
  std <- data.frame()
  if (meta$project$is_longitudinal == '1') {
    std <- std |> rbind(c("redcap_event_name",NA,"character"))
  }
  if (meta$project$has_repeating_instruments_or_events == '1') {
    std <- std |>
      rbind(c("redcap_repeat_instrument",NA,"character")) |>
      rbind(c("redcap_repeat_instance",NA,"character"))
  }

  if (nrow(std) > 0) {
    colnames(std) <- c("field_name","form_name","class")
    out <- bind_rows(dd2 |> slice_head(),
                       std,
                       dd2 |> slice(-1))
  } else {
    out <- dd2
  }

  return(out)
}
