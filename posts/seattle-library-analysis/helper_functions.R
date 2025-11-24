filter_by_condition <- function(df, condition, ...){
  #filters the data based on supplied conditions
  #will filter on multiple conditions separated by comma OR supplied in a vector
  df |> 
    filter({{condition}}, ...)
  
}

select_by_variable <- function(df, variable, ...){
  df |> 
    select({{variable}}, ...) 
}

add_dates <- function(df){
  df |> 
    mutate(
      dow = wday(CheckoutDateTime, label = TRUE),
      doy = yday(CheckoutDateTime),
      dom = mday(CheckoutDateTime),
      month = month(CheckoutDateTime),
      hour = hour(CheckoutDateTime),
      week = week(CheckoutDateTime)
    )
}

add_age_group <- function(df){
  df |> 
    mutate(age = case_when(
      Collection %in% (type_data |> filter(age_group == 'Teen') |> pull(code)) ~ "Teen",
      Collection %in% (type_data |> filter(age_group == 'Juvenile') |> pull(code)) ~ "Juvenile",
      Collection %in% (type_data |> filter(age_group == 'Adult') |> pull(code)) ~ "Adult"),
    )
}

# format the numbers displayed in a table
format_table_numbers <- function(table, is_percent = FALSE, n = 'Total'){
  if(is_percent){
    table |> 
      fmt_percent(
        columns = c(n)
      )
  }else {
    table |>
      fmt_number(
        columns = c(n),
        decimals = 0
      ) 
  }
  
}
# format the header of a table
format_table_header <- function(table) {
  table |>  
    tab_style(
      style = list(
        cell_text(weight = 'bold',
                  transform = 'capitalize'
        )
      ),
      locations = cells_column_labels(everything())
    )
}

add_category_subgroup <- function(df, df2){
  #takes the Collection of the df and uses value to join with the type_data to get
  #a category subgroup
  df |> 
    left_join(df2, join_by(Collection == code)) |> 
    select(-description, -code_type, -format_group, -format_subgroup,
           -category_group, -age_group)
    
}

get_year_and_add_age_group <- function(df, condition){
  df |> 
    filter_by_condition({{condition}}) |> 
    add_age_group()
}


#' Title
#'
#' @param df 
#'
#' @returns
#' @export
#'
#' @examples
zero_bound_predictions <- function(df){
  stopifnot(is.data.frame(df))
  df |> 
    mutate(.pred = if_else(.pred < 0, 0, .pred))
}




#Summing the total count of books when a book is labeled with multiple ItemTypes

#Function to build dataset for the model
build_base_dataset <- function(df, condition, variable, ...){
  #@df is a duckdb connection; must be collected at the end
   df |> 
    filter_by_condition({{condition}}) |> 
    select_by_variable({{variable}}) |> 
    collect() |> 
    janitor::clean_names()
  
}


#function to filter base data set down to the n most checked out books
select_top_n_books <- function(df, .n){
  #need to determine the most popular books
  #Popular = the most checked out book summarized over the entire dataset
  
  #get a character vector of the top n books
  top_n_book_titles <- df |> 
    group_by(title) |> 
    summarise(total_checkouts = sum(checkouts)) |> 
    arrange(desc(total_checkouts)) |> 
    slice_head(n = .n) |> 
    pull(title)
  
  #use a semi-join to filter the top n books from the input df
  df |> 
    semi_join(data.frame(title = top_n_book_titles), join_by(title))
  
}

#
calculate_lags <- function(.df, .value, .group_var, .n) {

  grouped_data <- .df |> 
    group_by(across(all_of(.group_var)))
  
  lagged_cols <- map(.n, function(lag_val){
    grouped_data|> 
      mutate(!!(paste0("lag_", lag_val)) := lag({{.value}}, n = lag_val)) |> 
      ungroup() |> 
      select(starts_with('lag_'))
  })
  
  bind_cols(.df, list_cbind(lagged_cols))
  
}

calculate_rolling_averages <- function(.df, .value, .group_var, .n){
  
  #group the data frame
  grouped_data <- .df |> 
    group_by(across(all_of(.group_var)))
  
  rolling_avg_cols <- map(.n, function(avg_val){
    grouped_data |> 
      mutate(!!(paste0("rolling_avg_", avg_val, "_month")) := zoo::rollmean({{.value}}, k = avg_val,
                                                                            fill = NA, align = 'right')) |> 
      ungroup() |> 
      select(starts_with('rolling_avg'))
  })
  bind_cols(.df, list_cbind(rolling_avg_cols))
  
}



#Function to add features to dataset
#' Title
#'
#' @param df 
#' @param add_lagged_vars 
#' @param add_rolling_avg_vars 
#' @param .n_lags 
#' @param .n_avgs 
#' @param g_vars 
#'
#' @returns
#' @export
#'
#' @examples
add_features <- function(df, add_lagged_vars = TRUE, 
                         add_rolling_avg_vars = TRUE,
                         .n_lags = NULL,
                         .n_avgs = NULL, 
                         g_vars = NULL){
  
  
  # this works when you provide a single unquoted grouping variable, but not with multiple
  # can't figure out how to make it work, so just using quoted variables for now
  # g_vars <- enquos(g_vars) |>
  #   map(~rlang::quo_get_expr(.x)) |> 
  #   list_c()
  
  temp_df <- df |> 
    mutate(across(where(is.character), factor)) |> 
    mutate(year_month = tsibble::make_yearmonth(year = checkout_year, month = checkout_month),
           childrens_book = if_else(grepl("Children", publisher), TRUE, FALSE)) |> 
    group_by(title, publisher, publication_year) |> 
    arrange(year_month) |> 
    mutate(checkout_book_sd = sd(checkouts),
           is_new = as.factor(!duplicated(title)),
           months_old = rank(year_month),
           one_month_diff = checkouts - lag(checkouts, 1))
  
  if(add_lagged_vars)
    temp_df <- calculate_lags(temp_df, .value = checkouts,
                               .group_var = g_vars,
                               .n = .n_lags)
  
  if(add_rolling_avg_vars)
    temp_df <- calculate_rolling_averages(temp_df, .value = checkouts,
                                          .group_var = g_vars,
                                          .n = .n_avgs)
  temp_df |> 
    ungroup() |> 
    group_by(year_month) |>
    mutate(avg_book_checkouts = mean(checkouts),
           median_book_checkouts = median(checkouts),
           month_sd = sd(checkouts),
           is_summer = if_else(checkout_month %in% c(6, 7, 8), 1, 0),
           is_december = if_else(checkout_month == 12, 1, 0),
           is_childrens_book = if_else(childrens_book, 1, 0)) |> 
    ungroup()
  
}
