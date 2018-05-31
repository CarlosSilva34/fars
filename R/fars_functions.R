#' FARS Data Input.
#'
#' This functions reads a file in table format, and returns it as an object of the "tbl_df" class. If
#'      the file does not exist in the current directory, this function stops
#'      execution with an error message. While reading all messages are suppressed.
#'
#' @details This function uses the \code{read_csv()} function from the
#'          \code{readr} package, with \code{progress = FALSE} parameter.
#'          Then the \code{tbl_df()}, function from the \code{dplyr} package
#'          is called to convert the data into the tabl_df format before returing it.
#'
#' @param filename A character string giving the filename of the csv file to be
#'        read
#'
#' @return returns a data frame table if the file exists, throws an error if the file
#'   doesn't exist.
#'
#' @section Depends on:
#' \enumerate{
#'   \item \code{\link[readr]{read_csv}} in the \code{readr} package.
#'   \item \code{\link[dplyr]{tbl_df}} in the \code{dplyr} package.
#' }
#'
#' @importFrom readr
#' @importFrom dplyr
#'
#' @examples
#' \dontrun{
#'   fars_read("accident_2014.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make the Filename.
#'
#' This function generates the filename for a FARS datafile and takes a year as input and produces a valid FARS filename.
#'     It is an internal function used by other functions in this package.
#'
#' @param year An integer or string indicating the year that the data file generated.
#'
#' @return This function returns a character string representing the filename.
#'   Will return a filename with NA if the year parameter cannot be coerced to an integer.
#'
#' @examples
#' \dontrun{
#' make_filename(2014)
#' make_filename("2014")
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}
#'
#' Read FARS Data for Multiple Years.
#'
#' This function takes as its only parameter a vector of years, reads the FARS
#'    files for those years and returns the MONTH and year columns as a list.
#'
#' @param years a vector of integer or strings that indicated the year of the FARS data files.
#'
#' @return a list of FARS data for the years in the parameter \code{years}.
#'   Invalid years will have  a NULL entry in the returned list.
#'
#' @section Depends on:
#' \enumerate{
#'   \item \code{\link[dplyr]{mutate}} and \code{\link[dplyr]{select}} in the \code{dplyr} package.
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014, 2015))
#' fars_read_years(2013:2015)
#' fars_read_years(list(2013, 2014, 2015))
#' }
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}
#'
#' Summarize FARS data for multiple years
#'
#' This function can be used to get a data frame of FARS data for
#'   multiple years. Summarizes the number of fatal injuries suffered in motor vehicle traffic
#'   crashes by month with each year in its own column.
#'
#' @return A dataframe with the number of fatal injuries suffered in motor vehicle traffic
#'   crashes by month with each year in its own column. If no valid years are found, the function
#'   will give an error.
#'
#' @section Depends on:
#' \enumerate{
#'   \item \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{group_by}} and \code{\link[dplyr]{summarize}} in the
#'   \code{dplyr} package.
#'   \item \code{\link[tidyr]{spread}} in the \code{tidyr} package.
#' }
#'
#' @importFrom dplyr
#' @importFrom magrittr "%>%"
#' @importFrom tidyr
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' fars_summarize_years(c(2013:2015))
#' fars_summarize_years(list(2013, 2014, 2015))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}
#'
#' FARS map states.
#'
#' Draws the location of accidents in a US State for a given year
#'
#' @param state.num An integer that represents the state number.
#' @param year a vector of integer or strings that indicated the year of interest.
#'
#' @return This function draws a map of the State and shows one point for each fatality
#'   that occurred in that State. Will throw an error if an invalid
#'   state number is chosen or the chosen year's data does not exist.
#'
#' @section Depends on:
#' \enumerate{
#'   \item \code{\link[dplyr]{filter}} in the \code{dplyr} package.
#'   \item \code{\link[maps]{map}} in the \code{maps} package.
#'   \item \code{\link[graphics]{points}} in the \code{graphics} package.
#' }
#'
#' @importFrom dplyr
#' @importFrom maps
#' @importFrom graphics
#'
#' @examples
#' \dontrun{
#' fars_map_state(3, 2014)
#' fars_map_state("3", "2014")
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
