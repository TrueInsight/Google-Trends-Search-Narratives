library(bs4Dash)
library(collapsibleTree)
library(data.tree)
library(dplyr)
library(fresh)
library(gganimate)
library(ggpubr)
library(ggplot2)
library(ggraph)
library(httr) # Should be updated to library(httr2) with changes to code below
library(igraph)
library(jsonlite)
library(lubridate)
library(plotly)
library(R.utils)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
# The installation of rnaturalearthhires is required on the first run of the app, please ensure you are connected to wifi when doing so
remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
library(shiny)
library(shinyFiles)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
options(shiny.fullstacktrace=TRUE)
options(shiny.maxRequestSize=3000*1024^2)
library(tidyr)
library(utf8)
library(wordcloud2)

# column(10,(
# ),column(2,actionButton(inputId = "quit", label = "Quit"))
ui <- fluidPage(

    # fluidRow(column(6,label=""),column(6,actionButton(inputId = "quit", label = "Quit"))),

    # fluidRow(
    tags$head(
      tags$link(rel = "stylesheet", href = "www/custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://unpkg.com/pattern.css")
    ),

  # Include custom CSS
  includeCSS("www/custom.css"),
  div(class="bodymain",
      column(11,
  actionButton("filter_button", "", class = "custom-action-button"),
  tags$header(
    tags$ul(
      )
    ),

  conditionalPanel(
    condition = "output.show_filter",
    div(id = "custom-sidebar",
        sidebarPanel(
          fileInput("file1",
                        multiple = FALSE,
                        accept = c("text/txt", ".txt"),
                        label = NULL, buttonLabel = 'Import API Key'),

          # Get a folder where the downloaded data will be stored
          shinyDirButton(id='downloadPath', label='Data Folder', title='Folder for downloaded data'),
          textOutput('path'),

          # Set minimum date to Jan 2008, as experience shows GT data prior to that are unreliable
          # Use Month-year format as minimum analysis level for narratives is by month
          dateRangeInput("date_range", "Date Range:",
                         start = Sys.Date() - months(6),
                         end = Sys.Date(),
                         min = "2008-01-01",
                         max = Sys.Date(),
                         format = "mm/yyyy"),
          radioButtons("splittype", "Criterion of extracting next level terms:", choices = c("All sub-levels", "Y/Q/M sublevels")),
          conditionalPanel(
            condition = "input. splittype== 'Y/Q/M sublevels'",
            checkboxGroupInput("yqmsubdivision", "Y/Q/M subdivision",
                               c("12 months at a time from start date" = "year",
                                 "3 months at a time from start date" = "quarter",
                                 "1 month at a time from start date" = "month")),
          ),
          selectInput("country", "Country", ""),
          uiOutput("region_ui"),
          sliderInput("iterations", "Depth (Number of Iterations)", min = 1, max = 5, value = 1),
          radioButtons("extractiontype", "Criterion of extracting next level terms:", choices = c("Median", "All")),

          sliderInput("num_samples", "Timeline Samples", min = 1, max = 40, value = 1),
          radioButtons("type", "Type", choices = c("Top Queries" = "topQueries", "Top Topics" = "topTopics","Rising Queries" = "risingQueries", "Rising Topics" = "risingTopics")),
          checkboxInput('preload', "Preload Data", FALSE),

          # Conditional panels that check the value of 'preload'
          conditionalPanel(
            condition = "input.preload == true",
            fileInput("fileline",
                      multiple = FALSE,
                      accept = c(".csv"),
                      label = 'Preload Line Plot File', buttonLabel = 'Import CSV'),
            fileInput("filerecursion",
                      multiple = FALSE,
                      accept = c(".csv"),
                      label = 'Preload Tree Plot File', buttonLabel = 'Import CSV'),
            fileInput("fileregion",
                      multiple = FALSE,
                      accept = c(".csv"),
                      label = 'Preload Map Plot File', buttonLabel = 'Import CSV')
          )
        )
    )

  ),

  div(class="logo",
      img(src="title.svg", alt="titlelogo")
  ),

  div(class="bar",
      tags$input(id="term", type="text", placeholder="Enter Search Term", class="searchbar"),
      actionButton("search_button", "", class = "custom-search-button")

  ),
  uiOutput("dynamic_buttons"),
  ),
  column(1,actionButton(inputId = "quit", label = "Quit"))
  ),

  # Buttons below search bar
  div(class="pattern-grid-md custom-grid",
      uiOutput("dynamic_ui"))
)


server <- function(input, output, session) {

  # Stop R if the user quits the app
  observe({
    if (input$quit == 1) stopApp()
  })
  # Check for the directory to save the downloaded data
  # When I put it here, it shows the folders, but closes the Shiny form as soon as I select a folder.
  # shinyDirChoose(
  #   input,
  #   'downloadPath',
  #   roots = c(home = '~')
  # )
  #
  # output$path <- renderText({
  #   DataPath <- parseDirPath(c(home = '~'), input$downloadPath)
  #   return(paste(DataPath))
  # })
  #
  # DataPath <- reactive({
  #   # Check if the directory path has been set
  #   if (is.null(input$downloadPath)) {
  #     # Default to the current working directory if no selection
  #     return(getwd())
  #   } else {
  #     # Use parseDirPath to get the actual file path from the selection
  #     return(parseDirPath(c(home = '~'), input$downloadPath))
  #   }
  # })
gV = getVolumes()
  shinyDirChoose(
    input,
    'downloadPath',
    roots = gV()
  )

  output$path <- renderText({
    DataPath <- parseDirPath(gV(), input$downloadPath)
    return(paste(DataPath()))
  })

  DataPath <- reactive({
    # Check if the directory path has been set
    if (is.null(input$downloadPath)) {
      # Default to the current working directory if no selection
      return(getwd())
    } else {
      # Use parseDirPath to get the actual file path from the selection
      return(parseDirPath(gV(), input$downloadPath))
    }
  })



  clean_filename <- function(filename) {
    tryCatch({
      getFilename(filename)
    }, error = function(e) {
      gsub("[^A-Za-z0-9_.-]", "_", filename)
    })
  }

  get_map_value <- function(api_key, startdate, enddate, country, terms) {

    sampled_terms <-  URLencode(terms, reserved = TRUE)

    terms_url <- paste0("term=", paste(sapply(sampled_terms, function(x) gsub("%20", "+", x)), collapse = "&terms="))


    if(country == '') {
      url <- paste0('https://www.googleapis.com/trends/v1beta/regions?',
                    terms_url,
                    '&restrictions.startDate=', startdate,
                    '&restrictions.endDate=', enddate,
                    '&key=', api_key,
                    '&alt=json')
    }
    else {
      url <- paste0('https://www.googleapis.com/trends/v1beta/regions?',
                    terms_url,
                    '&restrictions.startDate=', startdate,
                    '&restrictions.endDate=', enddate,
                    '&restrictions.geo=',country,
                    '&key=', api_key,
                    '&alt=json')
    }

    ## print(url)
    response <- GET(url)
    response_content <- httr::content(response, "parsed")
    # print(response_content)

    df <- jsonlite::fromJSON(toJSON(response_content$regions), flatten = TRUE)
    # print(df)
    df <- purrr::map_df(df, unlist)
    df$date = startdate
    # print(df)
    return(df)

  }

  get_all_map_values <- function(api_key, start_date, end_date, country, terms) {
    # Generate a sequence of the first day of each month between start_date and end_date
    dates_seq <- seq(as.Date(paste0(start_date, "-01")), as.Date(paste0(end_date, "-01")), by="months")

    # Initialize an empty list to store each month's results
    all_results <- list()

    for(date in dates_seq) {
      ## print(date)
      # Extract year and month in 'YYYY-MM' format
      current_date <- format(as.Date(date), "%Y-%m")

      # Get data for the current month
      df <- get_map_value(api_key, current_date, current_date, country, terms)

      # Append to the list
      all_results[[length(all_results) + 1]] <- df
    }

    result <- bind_rows(all_results)

    return(result)
  }

  wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI",
                           fontWeight = "bold", color = "random-dark", backgroundColor = "white",
                           minRotation = -pi/4, maxRotation = pi/4, shuffle = FALSE,
                           rotateRatio = 0, shape = "circle", ellipticity = 0.65,
                           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
  {
    if ("table" %in% class(data)) {
      dataOut = data.frame(name = names(data), freq = as.vector(data))
    }
    else {
      data = as.data.frame(data)
      dataOut = data[, 1:2]
      names(dataOut) = c("name", "freq")
    }
    if (!is.null(figPath)) {
      if (!file.exists(figPath)) {
        stop("cannot find fig in the figPath")
      }
      spPath = strsplit(figPath, "\\.")[[1]]
      len = length(spPath)
      figClass = spPath[len]
      if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
        stop("file should be a jpeg, jpg, png, bmp or gif file!")
      }
      base64 = base64enc::base64encode(figPath)
      base64 = paste0("data:image/", figClass, ";base64,",
                      base64)
    }
    else {
      base64 = NULL
    }
    weightFactor = size * 180/max(dataOut$freq)
    settings <- list(word = dataOut$name, freq = dataOut$freq,
                     fontFamily = fontFamily, fontWeight = fontWeight, color = color,
                     minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor,
                     gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation,
                     shuffle = shuffle, rotateRatio = rotateRatio, shape = shape,
                     ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
    chart = htmlwidgets::createWidget("wordcloud2", settings,
                                      width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0,
                                                                                                                              browser.padding = 0, browser.fill = TRUE))
    chart
  }

  # Create random strings for the multiple sampling of the timelines
  generate_random_strings <- function(n) {
    unique_strings <- character(0)

    while(length(unique_strings) < n) {
      new_string <- paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
      if (!(new_string %in% unique_strings)) {
        unique_strings <- c(unique_strings, new_string)
      }
    }

    return(unique_strings)
  }

  get_terms <- function(api_key, startdate, enddate, timelineresolution, country, terms, n_samples) {
    term_dfs <- list()
    for(j in 1:length(terms)){
      all_dfs <- list()

      unique_strings <- generate_random_strings(n_samples)
      i = 2

      for(i in 1:n_samples) {
        sampled_terms <- sapply(terms[j], function(term) {
          random_string <- unique_strings[i]
          if(i > 1){
            return(paste(term, random_string, sep = ' +'))}
          else{return(term)}
        })

        sampled_terms <- sapply(sampled_terms, function(x) URLencode(x, reserved = TRUE) )
        terms_url <- paste0("terms=", paste(sapply(sampled_terms, function(x) gsub("%20", "+", x)), collapse = "&terms="))


        if(country == '') {
          url <- paste0('https://www.googleapis.com/trends/v1beta/timelinesForHealth?',
                        terms_url,
                        '&time.startDate=', startdate,
                        '&time.endDate=', enddate,
                        '&timelineResolution=', timelineresolution,
                        '&key=', api_key,
                        '&alt=json')
        }
        else {
          url <- paste0('https://www.googleapis.com/trends/v1beta/timelinesForHealth?',
                        terms_url,
                        '&time.startDate=', startdate,
                        '&time.endDate=', enddate,
                        '&timelineResolution=', timelineresolution,
                        '&geoRestriction.regions=', country,
                        '&key=', api_key,
                        '&alt=json')
        }

        ## print(url)
        response <- GET(url)
        content <- httr::content(response, "parsed")
        df_list <- lapply(content$lines, function(x) {


          df_points <- do.call(rbind, lapply(x$points, function(y) data.frame(value = y$value, date = y$date, stringsAsFactors = FALSE)))
          df_points$api_call_term <-  x$term
          df_points$term <- gsub(" \\+.*$", "", x$term)  # remove the random string to get the original term
          df_points$date <- as.Date(df_points$date, format = "%b %d %Y")

          return(df_points)
        })

        df <- do.call(rbind, df_list)
        all_dfs[[i]] <- df
      }

      # Double-Assign to combined_df
      combined_df <<- do.call(rbind, all_dfs)

      # Calculate the median for each term and date
      ## print('pass1')
      aggregated_df <- combined_df %>%
        group_by(term, date) %>%
        summarise(value = median(value))
      ## print('pass2')
      ## print(aggregated_df)


      ## print('pass3')
      term_dfs[[j]] <- aggregated_df
      ## print('pass4')
    }

    ## print('pass5')

    return(do.call(rbind, term_dfs))


  }
  calculate_nodes <- function(n) {
    # Base case: if n is 1, return 1 (only one node)
    if (n == 1) {
      return(1)
    }

    # Recursive case: if n is even, divide by 2 and calculate nodes
    if (n %% 2 == 0) {
      return(1 + 2 * calculate_nodes(n / 2))
    }

    # Recursive case: if n is odd and greater than 1, go straight to 1 (for n nodes)
    if (n > 1) {
      return(1 + n)
    }
  }
  initialize_hierarchy_df <- function(term_og, startdate, enddate) {
    start_date <- ymd(paste0(startdate, "-01"))
    end_date <- ymd(paste0(enddate, "-01"))
    year_start <- start_date
    year_end <- min(year_start %m+% months(12) - days(1), end_date)

    if(input$splittype == 'All sub-levels'){
      hierarchy_df <- data.frame(
        title = character(),
        start_date = character(),
        end_date = character(),
        value = numeric(),
        mid = character(),
        isBreakout = character(),
        level = integer(),
        parent = character(),
        date_level = integer(),
        group_id = integer(),
        stringsAsFactors = FALSE
      )

      firstrow <- list(
        title = term_og,
        start_date = startdate,
        end_date = enddate,
        value = NA,
        mid = NA,
        isBreakout = NA,
        level = 1,
        parent = NA,
        date_level = 1,
        group_id = 1
      )
    }
    else{
      hierarchy_df <- data.frame(
        title = character(),
        start_date = character(),
        end_date = character(),
        value = numeric(),
        mid = character(),
        isBreakout = character(),
        level = integer(),
        parent = character(),
        date_level = character(),
        group_id = character(),
        stringsAsFactors = FALSE
      )

      firstrow <- list(
        title = term_og,
        start_date = startdate,
        end_date = paste0(format(year_end, "%Y-%m")),
        value = NA,
        mid = NA,
        isBreakout = NA,
        level = 1,
        parent = NA,
        date_level = '12 Months',
        group_id = 1
      )
    }


    hierarchy_df <- rbind(hierarchy_df, firstrow)
    return(hierarchy_df)
  }
  split_date_range <- function(start_date, end_date) {
    start_date <- ymd(paste0(start_date, "-01"))
    end_date <- ymd(paste0(end_date, "-01"))

    months_diff <- lubridate::interval(start_date, end_date) %/% months(1) + 1

    if(months_diff == 3) {
      mid_date1 <- start_date + months(1) - days(1)
      mid_date2 <- start_date + months(2)

      return(list(
        c(format(start_date, "%Y-%m"), format(mid_date1, "%Y-%m")),
        c(format(mid_date1 + days(1), "%Y-%m"), format(mid_date2 - days(1), "%Y-%m")),
        c(format(mid_date2, "%Y-%m"), format(end_date, "%Y-%m"))
      ))
    }

    mid_date <- start_date + months(months_diff %/% 2)

    list(
      c(format(start_date, "%Y-%m"), format(mid_date - days(1), "%Y-%m")),
      c(format(mid_date, "%Y-%m"), format(end_date, "%Y-%m"))
    )
  }
  split_by_YQM <- function(start_date, end_date, selected_subdivisions) {
    start_date <- ymd(paste0(start_date, "-01"))
    end_date <- ymd(paste0(end_date, "-01"))

    date_ranges <- list()


    if ("year" %in% selected_subdivisions) {
      # Split by Year (12 months from start date)
      year_start <- start_date
      subdivision = '12 Months'
      while (year_start < end_date) {
        year_end <- min(year_start %m+% months(12) - days(1), end_date)
        date_ranges <- c(date_ranges, list(c(paste0(year_start), paste0(year_end), subdivision, 'Y')))
        ## print(date_ranges)
        year_start <- year_end + days(1)
      }
    }

    if ("quarter" %in% selected_subdivisions) {
      # Split by Quarter (3 months from start date)
      subdivision = '3 Months'
      quarter_start <- start_date
      while (quarter_start < end_date) {
        quarter_end <- min(quarter_start %m+% months(3) - days(1), end_date)
        date_ranges <- c(date_ranges, list(c(paste0(quarter_start), paste0(quarter_end), subdivision, "Q")))
        ## print(date_ranges)
        quarter_start <- quarter_end + days(1)
      }
    }

    if ("month" %in% selected_subdivisions) {
      # Split by Month (1 month from start date)
      subdivision = '1 Month'
      month_start <- start_date
      while (month_start < end_date) {
        month_end <- min(month_start %m+% months(1) - days(1), end_date)
        date_ranges <- c(date_ranges, list(c(paste0(month_start), paste0(month_end), subdivision, "M")))
        ## print(date_ranges)
        month_start <- month_end + days(1)
      }
    }

    return(date_ranges)
  }

  recursive_split_yqm <- function(type, startdate, enddate, region, api_key, term_og, hierarchy_df, iterations, level, group_id, category, domain, date_parent) {

    date_splits <- split_by_YQM(startdate, enddate, input$yqmsubdivision)
    ## print('splitting...')
    ## print(date_splits)
    for (split in date_splits) {
      start_split <- as.Date(split[1], format="%Y-%m-%d")
      end_split <-  as.Date(split[2], format="%Y-%m-%d")
      date_level <- split[3]
      ## print(split)
      date_range <- paste(format(start_split, "%Y-%m-%d"), format(end_split, "%Y-%m-%d"), sep = " to ")

      hierarchy_df <- recursion_hierarchy(type, format(start_split, "%Y-%m"), format(end_split, "%Y-%m"), region, api_key, term_og, hierarchy_df, iterations, level, date_level, group_id, category, domain, date_parent)
      date_parent <- date_range
    }

    return(hierarchy_df)
  }
  recursive_split <- function(type, startdate, enddate, region, api_key, term_og, hierarchy_df, iterations, level, date_level, group_id, category, domain, date_parent) {

    hierarchy_df <- recursion_hierarchy(type, startdate, enddate, region, api_key, term_og, hierarchy_df, iterations, level, date_level, group_id, category, domain, date_parent)

    if(lubridate::interval(ymd(paste0(startdate, "-01")), ymd(paste0(enddate, "-01"))) %/% months(1) >= 1){
      date_range = paste(startdate, enddate, sep = " to ")
      splits <- split_date_range(startdate, enddate)
      for(split in splits){
        hierarchy_df <- recursive_split(type, split[1], split[2], region, api_key, term_og, hierarchy_df, iterations, level, date_level + 1, group_id + 1, category, domain , date_parent = date_range)
      }
    }
    return(hierarchy_df)
  }

  get_top_rising <- function(type, startdate, enddate,region, api_key, term, category, domain){
    start_date <- ymd(paste0(startdate, "-01"))
    end_date <- ymd(paste0(enddate, "-01"))
    months_diff <- lubridate::interval(start_date, end_date) %/% months(1) + 1
    incProgress(1/calculate_nodes(months_diff)/15)

    term <- URLencode(term, reserved = TRUE)
    term <- gsub("%20", "+", term)

    if(region == ''){
      url <- paste0('https://www.googleapis.com/trends/v1beta/',
                    type,
                    '?term=',
                    term,
                    '&restrictions.startDate=', startdate,
                    '&restrictions.endDate=', enddate,
                    '&restrictions.category=', category,
                    '&restrictions.property=', domain,
                    '&key=', api_key,
                    '&alt=json')

    }
    else{
      url <- paste0('https://www.googleapis.com/trends/v1beta/',
                    type,
                    '?term=',
                    term,
                    '&restrictions.startDate=', startdate,
                    '&restrictions.endDate=', enddate,
                    '&restrictions.geo=', region,
                    '&restrictions.category=', category,
                    '&restrictions.property=', domain,
                    '&key=', api_key,
                    '&alt=json')}

    ## print(url)



    url <- gsub(" ", "+", url)
    response <- GET(url)
    response_content <- httr::content(response, "parsed")
    # x = jsonlite::toJSON(response_content, auto_unbox = TRUE) %>% jsonlite::prettify()
    # filename = paste0(type, term, startdate, enddate, region)
    # write(x ,  paste0(filename, '.json'))
    df <- jsonlite::fromJSON(toJSON(response_content$item), flatten = TRUE)
    df <- purrr::map_df(df, unlist)
    return(df)
  }
  match_cols_and_bind <- function(df1, df2){
    missing_cols <- setdiff(names(df1), names(df2))

    for(col in missing_cols){
      df2[[col]] <- NA
    }

    df2 <- df2[names(df1)]

    rbind(df1, df2)
  }
  split_date_range_x_intervals <- function(start_date, end_date, x) {
    start_date <- ymd(paste0(start_date, "-01"))
    end_date <- ymd(paste0(end_date, "-01"))

    # Calculate the total duration in days
    total_days <- as.numeric(difftime(end_date, start_date, units = "days"))

    # Calculate the number of days for each interval
    days_per_interval <- total_days / x

    # Initialize an empty list to store the results
    date_ranges <- list()

    # Generate the date ranges
    for (i in 0:(x - 1)) {
      interval_start <- start_date + days(round(i * days_per_interval))
      interval_end <- start_date + days(round((i + 1) * days_per_interval)) - days(1)

      # Add the start and end dates of the current interval to the list
      date_ranges[[i + 1]] <- c(format(interval_start, "%Y-%m-%d"), format(interval_end, "%Y-%m-%d"))
    }

    return(date_ranges)
  }
  split_number_range <- function(x, n) {
    # Initialize an empty list to store the results
    num_ranges <- list()

    # Calculate the number of intervals
    num_intervals <- ceiling(x / n)

    # Generate the number ranges
    for (i in 1:num_intervals) {
      interval_start <- (i - 1) * n + 1
      interval_end <- min(i * n, x)  # The last interval can be shorter

      # Add the start and end numbers of the current interval to the list
      num_ranges[[i]] <- c(interval_start, interval_end)
    }

    return(num_ranges)
  }
  recursion_hierarchy = function(type, startdate, enddate, region, api_key, term, hierarchy_df, iterations, level, date_level, group_id, category, domain, date_parent){
    # Base case: if iterations are 0, return the current hierarchy dataframe
    if(iterations == 0){
      return(hierarchy_df)
    }

    df <- get_top_rising(type, startdate, enddate, region, api_key, term, category, domain)
    ## print(df)
    if(nrow(df) != 0){
      df <- dplyr::arrange(df, desc(value))


      if (input$extractiontype == 'ALL'){
        nrows = nrow(df)
        ## print('using all data')
      }
      else{
        ## print('using median data')
        if (nrow(df) <= 3) {
          nrows = nrow(df)
        } else {
          nrows = sum(df$value > median(df$value))
        }}


      df$level = level
      df$start_date = startdate
      df$end_date = enddate
      df$parent = term
      df$date_level = date_level
      ## print('date_level:')
      ## print(date_level)
      df$group_id = group_id
      df$date_parent = date_parent
      hierarchy_df <- match_cols_and_bind(hierarchy_df,df)


      if(type == 'topTopics' | type == 'risingTopics'){
        hierarchy_terms <- head(df, nrows)$mid
      } else {
        hierarchy_terms <- head(df, nrows)$title
      }

      ## print(hierarchy_terms)

      if(!is.null(hierarchy_terms) & length(hierarchy_terms) != 0){
        for(i in 1:length(hierarchy_terms)){
          df <- get_top_rising(type, startdate, enddate, region, api_key, hierarchy_terms[i], category, domain)
          hierarchy_df <- recursion_hierarchy(type, startdate, enddate, region, api_key, hierarchy_terms[i], hierarchy_df, iterations - 1, level + 1, date_level, group_id, category, domain, date_parent)
        }}}

    return(hierarchy_df)
  }


  fetched_df <- reactiveVal()
  line_df <- reactiveVal()
  start_date <- reactiveVal()
  end_date <- reactiveVal()
  api_key <- reactiveVal()
  selected_region <- reactiveVal()
  data_ready <- reactiveVal(FALSE)
  countries_data <- read.csv("countries.csv", stringsAsFactors = FALSE)
  regions_data <- read.csv("regions.csv", stringsAsFactors = FALSE)
  updateSelectInput(session, "country", choices = c('Worldwide' ,countries_data$country_name))

  output$dynamic_buttons <- renderUI({
    req(data_ready())
    div(class="buttons",
        tags$div(id = "arrow-icon")
    )})

  output$dynamic_ui <- renderUI({
      req(data_ready())
      dashboardBody(div(class = "container",

                        # First box: "Extracted Data"
                        div(class = "styled-box box1",
                            div(class = "box-title-section",
                                span(class = "box-title1", "Extracted Data"),
                                div(class = "additional-controls",
                                    downloadButton('downloadData', '', class = "download-btn")
                                )
                            ),
                            div(class = "box-content",
                                DT::dataTableOutput("data_table")
                            )
                        ),

                        # Second box: "tree of terms"
                        div(class = "styled-box box1",
                            div(class = "box-title-section",
                                span(class = "box-title1", "Tree of Terms"),
                                div(class = "multi-dropdown",
                                    uiOutput("date_range_ui"),
                                    selectInput("log", NULL, choices = c("Scaling: Log" = "Log","Scaling: None" = "None")),

                                ),
                                div(class = "additional-controls",
                                    downloadButton(outputId = "download_file_3", label = "", class = "download-btn")
                                )
                            ),
                            div(class = "box-content",
                                collapsibleTreeOutput("treePlot")
                            )
                            ),


                        # Third box: "Map"
                        div(class = "styled-box half box1",
                            div(class = "box-title-section",
                                span(class = "box-title1", "Map Over Time"),
                                div(class = "additional-controls",
                                    downloadButton(outputId = "download_file_map", label = "", class = "download-btn")
                                )
                            ),
                            div(class = "box-content",
                                plotly::plotlyOutput("mapplot")
                            )
                        ),


                        # Fourth box: "Terms Over Time"
                        div(class = "styled-box half box1",
                            div(class = "box-title-section",
                                span(class = "box-title1", "Terms Over Time"),
                                div(class = "additional-controls",
                                    downloadButton(outputId = "download_file_4", label = "", class = "download-btn")
                                )
                            ),
                            div(class = "box-content",
                                wordcloud2Output("wordcloud"),
                                uiOutput('dateslider_ui')
                            )
                        ),
                        # Fifth box: "Line Plot of Co-Related Terms"
                        div(class = "styled-box box1",
                            div(class = "box-title-section",
                                span(class = "box-title1", "Line Plot of Co-Related Terms"),
                                div(class = "multi-dropdown",
                                    selectInput("timelineresolution", NULL, choices = c("Timeline Resolution: Daily" = "Day","Timeline Resolution: Weekly" = "Week")),
                                    uiOutput("date_level_ui")
                                ),
                                div(class = "additional-controls",
                                    downloadButton('downloadData2', label = NULL, class = "download-btn")
                                )
                            ),
                            div(class = "box-content",
                                plotly::plotlyOutput("lineplot")
                            )
                        )
      )
      )
    })

  show_filter <- reactiveVal(FALSE)  # Initialize to FALSE

  observeEvent(input$filter_button, {
    show_filter(!show_filter())
  })
  output$show_filter <- reactive({
    show_filter()
  })
  outputOptions(output, "show_filter", suspendWhenHidden = FALSE)

output$date_range_ui <- renderUI({
  fetched_df_parsed <- req(fetched_df()) %>%
    mutate(date_range = paste(start_date, end_date, sep = " to ")) %>%
    arrange(date_level, start_date, end_date) %>%
    group_by(date_level) %>%
    distinct(date_range, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(id = row_number(),
           unique_id = paste(date_range, id, sep = "_"))
  unique_date_ranges = fetched_df_parsed$date_range %>% unique()

  # Create a named vector
  named_date_ranges <- setNames(fetched_df_parsed$date_range,
                                paste0(fetched_df_parsed$date_range,
                                      " (", fetched_df_parsed$date_level, ")"))

  # Use the named vector in the selectInput
  selectInput("date_ranges", NULL, choices = named_date_ranges)


})
output$date_level_ui <- renderUI({
  req(data_ready())
  unique_date_levels <- req(fetched_df())$date_level %>% unique()

  s_date_ranges <- setNames(unique_date_levels,
                                paste(
                                      "Date Level: ", unique_date_levels))



  selectInput("date_levels", NULL, choices = s_date_ranges)


})
output$region_ui <- renderUI({
  selected_country <- input$country
  country_code <- countries_data[countries_data$country_name == selected_country, "country_code"]
  available_regions <- regions_data[regions_data$parent_code == country_code, "region_name"]
  available_regions <- available_regions[available_regions!="(No regions)"]
  if(length(available_regions) != 0){available_regions = c("NA", available_regions)}
  if(length(available_regions) == 0) return(NULL) # If no regions for selected country
  selectInput("region", "Region", choices = available_regions, selected = 'NA')

})
output$dateslider_ui<- renderUI({
  req(fetched_df())
  fetched_df_bar <- fetched_df() %>% select(title, start_date, end_date,date_level, value)


  fetched_df_bar_select = fetched_df_bar %>% filter(date_level == max(fetched_df_bar$date_level)) %>%
    mutate(start_date = as.Date(paste0(start_date, "-01")),
           end_date = as.Date(paste0(end_date, "-01"))) %>%
    rowwise() %>%
    mutate(date_seq = list(seq(from = start_date, to = end_date, by = "1 month"))) %>%
    unnest(cols = c(date_seq)) %>%
    select(-start_date, -end_date) %>%
    rename(date = date_seq)



  fetched_df_bar_select <- fetched_df_bar_select %>%
    group_by(title, date) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>% arrange(date, value)



  fetched_df_bar_select_filtered <- fetched_df_bar_select %>%
    group_by(date) %>%
    ungroup()
  div(style = "display: flex; justify-content: center;",
      sliderInput("date",NULL, min = min(fetched_df_bar_select_filtered$date),max =max(fetched_df_bar_select_filtered$date),value=min(fetched_df_bar_select_filtered$date),timeFormat="%b %Y")
  )


})

  observeEvent(input$search_button, {

    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }

    api_key <- readLines(con = inFile$datapath[1], n = 1)
    api_key <- api_key[1]
    api_key(api_key)
    start_date <- format(input$date_range[1], "%Y-%m")
    start_date(start_date)
    end_date <- format(input$date_range[2], "%Y-%m")
    end_date(end_date)
    term <- input$term
    if(input$country == 'Worldwide')
    {selected_region <- ''}
    else if(input$region == 'NA')
    {country <- input$country
    selected_region <- countries_data[countries_data$country_name == country, "country_code"]
    }
    else if(input$region != 'NA'){
      region <- input$region
      selected_region <- regions_data[regions_data$region_name == region, "region_code"]
    }
    selected_region(selected_region)
    type <- input$type
    iterations <- input$iterations
    if(input$preload == TRUE){
    if (!is.null(input$filerecursion)) {
      inFile <- input$filerecursion$datapath
      df <- read.csv(inFile)

      fetched_df(df)
      data_ready(TRUE)
    }
    else{return(NULL)}
    }


    else{

    # Initialize the data frame
    hierarchy_df <- initialize_hierarchy_df(term, start_date(), end_date())
    ## print(hierarchy_df)

    observe({
      withProgress(message = 'Fetching API Results', value = 0, {

        # Run your recursive function
        if(input$splittype == 'All sub-levels'){
          df <- recursive_split(type, start_date(), end_date(), selected_region, api_key, term,hierarchy_df, iterations = iterations, level = 2, date_level = 1, group_id = 1, category = '0', domain = '', date_parent = NA)
          fetched_df(df)
          data_ready(TRUE)
        }
        else{

          df <- recursive_split_yqm(type, start_date(), end_date(), selected_region, api_key, term,hierarchy_df, iterations = iterations, level = 2, group_id = 1, category = '0', domain = '', date_parent = NA)
          fetched_df(df)
          data_ready(TRUE)
          }

      })
    })}

    observe({
    df <- req(fetched_df())
    filename <- clean_filename(paste0(format(Sys.time(), "%Y-%m-%d %H-%M-%S"),
                                      '_', input$type, '_', input$term, '_',
                                      input$selected_region, '_',
                                      input$start_date, '-', input$end_date,
                                      '_extracted_data.csv'))

    full_path <- paste0(req(DataPath()), "/", filename)
    write.csv(df, full_path)

    # Notify user
    showNotification(
      paste("Extracted data successfully saved in", DataPath()),
      type = "message",
      duration = NULL,
      closeButton = TRUE
    ) })



    # Output the data table
    output$data_table <- DT::renderDataTable({
      df <- req(fetched_df())
      DT::datatable(df, class = 'cell-border stripe')

    })


    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(DataPath(),"/",clean_filename(paste0(DataPath(),format(Sys.time(),format="%Y-%m-%d %H-%M-%S"), '_', type, '_', term, '_', start_date(), '_', end_date(), '_', selected_region, '_', 'iterations=',iterations, '.csv')))
      },
      content = function(file) {
        write.csv(req(fetched_df()), file)
      }
    )



    output$data_table2 <- DT::renderDataTable({
      df2 <- req(combined_df)
      DT::datatable(df2, class = 'cell-border stripe')

    })

    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste0(DataPath(), '/',clean_filename(paste0(DataPath(),format(Sys.time(),format="%Y-%m-%d %H-%M-%S"),'_', type,'_', term,'_', start_date(),'_', end_date(),'_', selected_region,'_','iterations=',iterations,'_','samples=',input$num_samples,'_','resolution=', input$timelineresolution,'_','datelevel=',input$date_levels, '.csv')))
      },
      content = function(file) {
        write.csv(req(line_df()), file)
      }
    )
  })

  output$treePlot <- renderCollapsibleTree({
    req(data_ready())
    fetched_df_levels <- req(fetched_df()) %>% select(title, parent, level, start_date, end_date, value)
    fetched_df_levels <- fetched_df_levels %>%
      mutate(date_range = paste(start_date, end_date, sep = " to "))
    fetched_df_levels = unique(rbind(fetched_df_levels[1,], fetched_df_levels %>%  filter(date_range == input$date_ranges )))
    fetched_df_levels$parent[is.na(fetched_df_levels$parent)] <- ""
    colnames(fetched_df_levels) <- c("Child", "Parent", "Title", "start_date", "end_date", "value", "date_range")
    fetched_df_levels = fetched_df_levels %>% select("Parent", "Child", "Title", "start_date", "end_date", "value", "date_range")
    fetched_df_levels$log_value = log(fetched_df_levels$value, base = exp(10))
    fetched_df_levels[1,'Parent'] = NA


    # Creating a 'Color' column
    fetched_df_levels$Color <- as.factor(fetched_df_levels$Title)
    levels(fetched_df_levels$Color) <- colorspace::rainbow_hcl(n = length(levels(fetched_df_levels$Color)))

    # fetched_df_levels = head(fetched_df_levels, n = 76)
    df_sorted <- fetched_df_levels %>% arrange(Title)

    # Iterate over the sorted dataframe to update node names
    for(i in 1:nrow(df_sorted)) {
      # Create a new unique node name
      new_name <- paste(df_sorted$Child[i], " (", df_sorted$Title[i], ")", sep="")
      new_name_Parent <- paste(df_sorted$Parent[i], " (", df_sorted$Title[i] - 1, ")", sep="")


      df_sorted$Parent[i] <- new_name_Parent


      # Update the current node's Child name
      df_sorted$Child[i] <- new_name
    }


    df_sorted[1,'Parent'] = NA

    if(input$log != 'Log'){
      df_sorted <- df_sorted %>% arrange(Title,desc(value) )

      # Creating the collapsible tree
      p = collapsibleTreeNetwork(
        df_sorted,
        attribute = "value",
        fill = "Color",
        nodeSize = "value",
        width = '100%',
        height = '100%'
      )
    }

    else{
      df_sorted <- df_sorted %>% arrange(Title,desc(log_value) )

      # Creating the collapsible tree
      p = collapsibleTreeNetwork(
        df_sorted,
        attribute = "value",
        fill = "Color",
        nodeSize = "log_value",
        width = '100%',
        height = '100%'
      )

    }

    # Create the dendrogram
    return(p)
  })


  x_scatter = reactive({
    req(fetched_df())

    terms <- req(fetched_df()) %>% filter(date_level == input$date_levels) %>% select(title) %>% unique()
    terms = terms$title
    if(input$preload == TRUE){
    if (!is.null(input$fileline)) {
      inFile <- input$fileline$datapath
      df <- read.csv(inFile)
      df$date <- as.Date(df$date)
      line_df(df)
    }
    else{
      return(NULL)}
      }

    else{
    df <- line_plot(api_key(), terms, selected_region(), start_date(), end_date(), input$timelineresolution)
    line_df(df)
    }
    p = ggplot(df, aes(x = date, y = value, col = term, group = term)) +
      geom_line() + theme_minimal() + labs(x = '', y = '')
    return(p)

  })

  observe({
    df <- req(line_df())
    write.csv(df, paste0(DataPath(),'/',clean_filename(paste0(format(Sys.time(),format="%Y-%m-%d %H-%M-%S"),'_', input$term,'_', selected_region(),'_', start_date(),'-', end_date(),'_','line','.csv'))))

    # Notify user
    showNotification(
      paste("Line plot data successfully saved in", DataPath()),
      type = "message",
      duration = NULL,
      closeButton = TRUE
    ) })


  output$lineplot = plotly::renderPlotly({
    req(fetched_df())

    if(input$preload == TRUE){
      if (is.null(input$fileline)) {
        return(NULL)
      } }

    p2 = plotly::ggplotly(x_scatter())

    return(p2)

  })

  line_plot <- function(api_key, terms, country, startdate, enddate, timelineresolution){
    my_interval <- lubridate::interval(ymd(paste0(startdate, "-01")), ymd(paste0(enddate, "-30")))
    if(timelineresolution == 'Day'){days_between <- as.duration(my_interval) / ddays(1)}
    else{days_between <- as.duration(my_interval) / dweeks(1)}
    if(days_between >= 2000){
      date_sequence <- split_date_range_x_intervals(startdate,enddate, ceiling(days_between/2000))
      df <- get_terms(api_key, startdate, enddate, timelineresolution, country, terms[1], input$num_samples)
      for(term in terms){
        for(range in date_sequence){
          df <- rbind(df,get_terms(api_key, range[1], range[2], timelineresolution, country, term, input$num_samples))
          }}
    }
    else{
      startdate = ymd(paste0(startdate, "-01"))
      enddate = ymd(paste0(enddate, "-30"))
      df <- get_terms(api_key, startdate, enddate, timelineresolution, country, terms[1], input$num_samples)
      n = floor(2000/days_between)

      split_term_intervals <- split_number_range(length(terms), n)
      ## print(split_term_intervals)
      for(interval in split_term_intervals){
        df <- rbind(df,get_terms(api_key, startdate, enddate, timelineresolution, country, terms[interval[1]:interval[2]], input$num_samples))

      }




    }

    return(df)
  }

  subset_data = reactiveVal()

  output$wordcloud <- renderWordcloud2({
    req(fetched_df())
    req(input$date)
    fetched_df_bar <- fetched_df() %>% select(title, start_date, end_date,date_level, value)

    fetched_df_bar_select = fetched_df_bar %>% filter(date_level == max(fetched_df_bar$date_level)) %>%
      mutate(start_date = as.Date(paste0(start_date, "-01")),
             end_date = as.Date(paste0(end_date, "-01"))) %>%
      rowwise() %>%
      mutate(date_seq = list(seq(from = start_date, to = end_date, by = "1 month"))) %>%
      unnest(cols = c(date_seq)) %>%
      select(-start_date, -end_date) %>%
      rename(date = date_seq)

    fetched_df_bar_select <- fetched_df_bar_select %>%
      group_by(title, date) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>% arrange(date, value)

    fetched_df_bar_select_filtered <- fetched_df_bar_select %>%
      group_by(date) %>%
      ungroup()

    ## print(input$date)
     date_string <- input$date
    formatted_date_string <- sub("(?<=-\\d{2}-).*", "01", date_string, perl = TRUE)
    subset_data <- fetched_df_bar_select_filtered[fetched_df_bar_select_filtered$date == as.Date(formatted_date_string, format = "%Y-%m-%d"), ]
    subset_data = subset_data %>% select(title, value)
    subset_data(subset_data)
    subset_data <- na.omit(subset_data)


    ## print('3977')
    wc = wordcloud2a(subset_data, size = 0.25)
    wc
  })

  result_df <- reactiveVal()

  output$mapplot <-  plotly::renderPlotly({
    selected_country <- input$country
  if(selected_country == 'Worldwide') {
    country_code <- ''
  } else {
    country_code <- countries_data[countries_data$country_name == selected_country, "country_code"]
  }


  if(input$preload == TRUE) {
    if (!is.null(input$filemap)) {
      inFile <- input$filemap$datapath
      result_df <- read.csv(inFile)
      result_df(result_df)
      return(result_df)
    } else {
      return(NULL)
    }
  } else {
    result_df <- get_all_map_values(api_key(), start_date(), end_date(), country_code, input$term)
    result_df(result_df)
  }



  observe({
    df <- req(result_df())
    write.csv(req(result_df()), paste0(DataPath(),'/',clean_filename(paste0(format(Sys.time(),format="%Y-%m-%d %H-%M-%S"),'_', input$term,'_', selected_country,'_', start_date(),'-', end_date(),'_','map','.csv'))))

    # Notify user
    showNotification(
      paste("Map data successfully saved in", DataPath()),
      type = "message",
      duration = NULL,
      closeButton = TRUE
    ) })

  # Decide on which map to use based on selection
  if(selected_country == 'Worldwide')
  { ## print('80')
    world_map <- ne_countries(returnclass = "sf")
    ## print('90')
    merged_data <- left_join(world_map, result_df, by = c("iso_a2" = "regionCode"))
    ## print('91')
    background_map <- ggplot() +
      geom_sf(data = world_map, fill = "grey", alpha = 0.3) +
      theme_minimal() +
      theme_void()}
  else{
    all_regional_map <- ne_states(returnclass = "sf")
    regional_map <- all_regional_map %>% filter(iso_a2 == country_code)
    merged_data <- left_join(regional_map, result_df, by = c("iso_3166_2" = "regionCode"))
    background_map <- ggplot() +
      geom_sf(data = regional_map, fill = "grey", alpha = 0.3) +
      theme_minimal() +
      theme_void()}
  background_grob <- ggplotGrob(background_map)
  map_plot <- ggplot() +
    annotation_custom(grob = background_grob) +  # Add static background
    geom_sf(data = merged_data, aes(fill = value, frame = date, color= value)) +
    labs(fill = "GT Value") +
    theme_minimal() + guides(color = "none") + theme(legend.position="top")
  ggplotly(map_plot) %>%
    animation_opts(frame = 1000, easing = "elastic", redraw = FALSE)


  })


  output$download_file_map <- downloadHandler(
    filename = function() {
      # paste0('sss', '.csv')
      paste0(DataPath(),'/',clean_filename(paste0(format(Sys.time(),format="%Y-%m-%d %H-%M-%S"),'_', input$type,'_', input$term,'_', start_date(),'_', end_date(),'_', selected_region(),'_','iterations=',input$iterations, '.csv')))
    },
    content = function(file) {
      write.csv(req(result_df()), file)
    }
  )


  output$download_file_4 <- downloadHandler(
    filename = function() {
      paste0(DataPath(),'/',clean_filename(paste0(format(Sys.time(),format="%Y-%m-%d %H-%M-%S"),'_', input$type,'_', input$term,'_', start_date(),'_', end_date(),'_', selected_region(),'_','iterations=',input$iterations, '.csv')))
    },
    content = function(file) {
      write.csv(req(subset_data()), file)
    }
  )


  output$download_file_3 <- downloadHandler(
    filename = function() {
      # paste0('sss', '.csv')
      paste0(DataPath(),'/',clean_filename(paste0(format(Sys.time(),format="%Y-%m-%d %H-%M-%S"),'_', input$type,'_', input$term,'_', start_date(),'_', end_date(),'_', selected_region(),'_','iterations=',input$iterations, '.csv')))
    },
    content = function(file) {
      write.csv(req(subset_data()), file)
    }
  )


}

# Run the application
shinyApp(ui = ui, server = server)