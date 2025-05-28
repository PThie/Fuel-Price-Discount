finding_string_in_code_files <- function(
    desired_string = NA
) {
    #' @title Find a string in all coding files
    #' 
    #' @description This function goes through all coding files and finds a string.
    #' This is helpful to find where a certain string is used in the code.
    #' 
    #' @param desired_string String you are looking for.
    #' 
    #' @return List of files where the string is found.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # list all coding files
    
    folders <- list.files(
        config_paths()[["src_path"]]
    )

    files <- c()
    for (folder in folders) {
        if (!folder %in% c("archive")) {
            file <- list.files(
                file.path(
                    config_paths()[["src_path"]],
                    folder
                ),
                full.names = TRUE,
                pattern = ".R$"
            )
            files <- c(files, file)
        }
    }

    #--------------------------------------------------
    # go through all coding files and find string

    output <- c()
    for (file in files) {
        content <- suppressWarnings(readLines(file))

        for (line in content) {
            if (grepl(desired_string, line)) {
                output <- c(output, file)
            }
        }
    }

    output <- unique(output)

    #--------------------------------------------------
    # return

    return(output)
}
