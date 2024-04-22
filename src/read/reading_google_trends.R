reading_google_trends <- function(data_file_path = NA) {
    #' @title Reading and cleaning google trends data
    #' 
    #' @description This function reads and cleans google trends data for
    #' different keywords.
    #' 
    #' @param data_file_path The path to the data files.
    #' 
    #' @return A data frame with the google trends data.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # read google trends data

    # list all data files
    files <- list.files(data_file_path, full.names = TRUE)

    # exlude the final prepared data file
    files <- files[!grepl("google_trends_", files)]

    # read all data files
    data_storage <- list()
    for (file in files) {
        dta <- data.table::fread(file)

        colnames(dta) <- c("date", "score")

        dta <- dta |>
            dplyr::mutate(
                date = as.character(date),
                score2 = score^2,
                keyword = gsub(".csv", "", basename(file)),
                keyword = stringr::str_split_fixed(
                    keyword,
                    "_",
                    2
                )[1]
            )

        data_storage[[file]] <- dta
    }
    
    # combine all
    google_data <- do.call(rbind, data_storage)

    #--------------------------------------------------
    # return

    return(google_data)
}