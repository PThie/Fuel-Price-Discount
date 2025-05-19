testing_robust_trends_dayspecific <- function(
    fuel_prices_april_august = NA
) {
    #' @title Testing robust trends with HonestDiD approach for specific days
    #' 
    #' @description This function tests for robust trends (parallel trends) by
    #' using the HonestDiD approach by Rambachan & Roth (2022) but only for
    #' individual days.
    #' 
    #' @param fuel_prices_april_august Fuel price data April to August 2022
    #' 
    #' @return List with honestDiD results
    #' @author Patrick Thiel
    #' 
    #----------------------------------------------
    # load library only needed here

    # install.packages("remotes")
    # Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
    # remotes::install_github("asheshrambachan/HonestDiD")
    library(HonestDiD)

    #----------------------------------------------
    # prepare data

    # add time to treatment in days
    avg_prices_prep <- fuel_prices_april_august |>
        dplyr::mutate(
            time_to_treatment = as.numeric(difftime(
                as.Date(date, "%Y-%m-%d"),
                config_globals()[["start_tr_de"]],
                units = "days"
            )),
            # set French stations to never-treated (in terms of time)
            time_to_treatment = dplyr::case_when(
                treat_region_de == "control" ~ 999,
                TRUE ~ time_to_treatment
            )
        )

    #----------------------------------------------
    # estimation function

    est_fun <- function(moddata, depvar = c("diesel", "e10"), regionFE) {
        fm <- formula(
            paste(
                depvar, "~",
                paste("relevel(as.factor(time_to_treatment), \"-1\") * relevel(as.factor(treat_region_de), \"control\")"),
                paste("| as.factor(date) + "),
                paste(regionFE)
            )
        )

        # estimation model
        est_mod <- fixest::feols(
            fml = fm,
            data = moddata,
            cluster = regionFE
        )
        return(est_mod)
    }

    #----------------------------------------------
    # estimation

    # dependent variables
    dep_cases <- c("diesel", "e10")

    # function to perform honestDiD
    performing_honestdid <- function(
        start_date = NA
    ) {
        #' @param start_date Start date of the post-period that should be tested
        
        #--------------------------------------------------
        # subset the data

        preperiod_data <- avg_prices_prep |>
            dplyr::filter(date < "2022-06-01")

        postperiod_data <- avg_prices_prep |>
            dplyr::filter(date == start_date)

        short_data <- rbind(
            preperiod_data,
            postperiod_data
        )
    
        #--------------------------------------------------
        # estimate model

        mod_list_event <- list()
        # loop through gasoline types
        for(dep_case in dep_cases){
            suppressMessages(mod <- est_fun(
                moddata = short_data,
                depvar = dep_case,
                regionFE = "station_id"
            ))
            mod_list_event[[dep_case]] <- mod
        }
    
        #--------------------------------------------------
        # test trends

        trend_test_list <- list()
        for(dep_case in dep_cases){
            betahat <- summary(mod_list_event[[dep_case]])$coefficients
            sigma <- summary(mod_list_event[[dep_case]])$cov.scaled

            # define the M
            ms <- seq(0.5, 2, by = 0.5)

            # define number of periods
            numpreprds <- 60
            numpostprds <- 1

            # period 1
            firstperiod <- HonestDiD::createSensitivityResults_relativeMagnitudes(
                betahat = betahat,
                sigma = sigma,
                numPrePeriods = numpreprds,
                numPostPeriods = numpostprds,
                Mbarvec = ms,
                grid.lb = -1,
                grid.ub = 1
            ) |>
            dplyr::mutate(
                period = 1,
                result = "sensitivity"
            ) |>
            as.data.frame()

            firstperiod_original <- HonestDiD::constructOriginalCS(
                betahat = betahat,
                sigma = sigma,
                numPrePeriods = numpreprds,
                numPostPeriods = numpostprds
            ) |>
            dplyr::mutate(
                period = 1,
                result = "original",
                Mbar = 0
            ) |>
            as.data.frame()

            # make one table
            trends <- rbind(
                firstperiod,
                firstperiod_original
            )
            
            # save
            trend_test_list[[dep_case]] <- trends
        }
        
        #----------------------------------------------
        # join in one table

        one_day_trends <- data.table::rbindlist(trend_test_list, idcol = TRUE) |>
            dplyr::rename(fuel_type = 1)

        # turn into factors
        one_day_trends <- one_day_trends |>
            dplyr::mutate(
                mbar_labels = dplyr::case_when(
                    Mbar == 0 ~ "Original",
                    TRUE ~ paste0(Mbar)
                ),
                mbar_labels = factor(
                    mbar_labels,
                    levels = c("Original", "0.5", "1", "1.5", "2")
                ),
                period = as.factor(period),
                period_label = start_date
            )

        # export
        openxlsx::write.xlsx(
            one_day_trends,
            file.path(
                config_paths()[["output_path"]],
                "estimation",
                paste0(
                    "honestdid_",
                    start_date,
                    ".xlsx"
                )
            ),
            rowNames = FALSE
        )

        # return
        return(one_day_trends)
    }

    #--------------------------------------------------
    # generate honestDiD results

    honestdid_results_June <- performing_honestdid(
        start_date = "2022-06-01"
    )

    honestdid_results_July <- performing_honestdid(
        start_date = "2022-07-01"
    )

    honestdid_results_August <- performing_honestdid(
        start_date = "2022-08-01"
    )

    # group into list
    honestdid_results_list <- list(
        honestdid_results_June,
        honestdid_results_July,
        honestdid_results_August
    )

    #--------------------------------------------------
    # return

    return(honestdid_results_list)
}