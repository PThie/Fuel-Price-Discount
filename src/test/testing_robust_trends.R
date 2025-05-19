testing_robust_trends <- function(
    fuel_prices_april_august = NA
) {
    #' @title Testing robust trends with HonestDiD approach
    #' 
    #' @description This function tests for robust trends (parallel trends) by
    #' using the HonestDiD approach by Rambachan & Roth (2022).
    #' 
    #' @param fuel_prices_april_august Fuel price data April to August 2022
    #' 
    #' @return List with honestDiD results
    #' @author Patrick Thiel
    
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
        start_date = NA,
        end_date = NA
    ) {
        #' @param start_date Start date of the post-period that should be tested
        #' @param end_date End date of the post-period that should be tested
        
        #--------------------------------------------------
        # subset the data

        preperiod_data <- avg_prices_prep |>
            dplyr::filter(date < "2022-06-01")

        postperiod_data <- avg_prices_prep |>
            dplyr::filter(date >= start_date) |>
            dplyr::filter(date <= end_date)

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
            numpostprds <- 7

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

            # period 2 to 7
            otherperiod_trends_list <- list()
            periods <- seq(2, 7, 1)
            for(prd in periods) {
                lvec <- HonestDiD::basisVector(prd, numpostprds)
                otherperiod <- HonestDiD::createSensitivityResults_relativeMagnitudes(
                    betahat = betahat,
                    sigma = sigma,
                    numPrePeriods = numpreprds,
                    numPostPeriods = numpostprds,
                    Mbarvec = ms,
                    grid.lb = -1,
                    grid.ub = 1,
                    l_vec = lvec
                ) |>
                dplyr::mutate(
                    period = prd,
                    result = "sensitivity"
                ) |>
                as.data.frame()

                otherperiod_original <- HonestDiD::constructOriginalCS(
                    betahat = betahat,
                    sigma = sigma,
                    numPrePeriods = numpreprds,
                    numPostPeriods = numpostprds,
                    l_vec = lvec
                ) |>
                dplyr::mutate(
                    period = prd,
                    result = "original",
                    Mbar = 0
                ) |>
                as.data.frame()

                results <- rbind(
                    otherperiod,
                    otherperiod_original
                )

                naming <- paste0("period_", prd)
                otherperiod_trends_list[[naming]] <- results
            }
        
            # make one data frame
            otherperiod_trends <- data.table::rbindlist(otherperiod_trends_list)
            trends <- rbind(firstperiod, firstperiod_original, otherperiod_trends)

            # save
            trend_test_list[[dep_case]] <- trends
        }

        #----------------------------------------------
        # join in one table

        one_week_trends <- data.table::rbindlist(trend_test_list, idcol = TRUE) |>
            dplyr::rename(fuel_type = 1)

        # generate sequence of dates
        max_date <- max(postperiod_data$date)
        min_date <- min(postperiod_data$date)
        seq_dates <- seq(min_date, max_date, by = "day")

        # test that are dates are in sequence
        tar_assert_true(all(seq_dates %in% postperiod_data$date))
        
        # turn into factors
        one_week_trends <- one_week_trends |>
            dplyr::mutate(
                mbar_labels = dplyr::case_when(
                    Mbar == 0 ~ "Original",
                    TRUE ~ paste0(Mbar)
                ),
                mbar_labels = factor(
                    mbar_labels,
                    levels = c("Original", "0.5", "1", "1.5", "2")
                ),
                period = factor(period, levels = seq(1, 7, 1)),
                period_label = dplyr::case_when(
                    period == 1 ~ seq_dates[1],
                    period == 2 ~ seq_dates[2],
                    period == 3 ~ seq_dates[3],
                    period == 4 ~ seq_dates[4],
                    period == 5 ~ seq_dates[5],
                    period == 6 ~ seq_dates[6],
                    period == 7 ~ seq_dates[7]
                )
            )

        # export
        openxlsx::write.xlsx(
            one_week_trends,
            file.path(
                config_paths()[["output_path"]],
                "estimation",
                paste0(
                    "honestdid_",
                    start_date,
                    "_",
                    end_date,
                    ".xlsx"
                )
            ),
            rowNames = FALSE
        )

        #--------------------------------------------------
        # return

        return(one_week_trends)
    }

    #--------------------------------------------------
    # generate honestDiD results

    honestdid_results_June <- performing_honestdid(
        start_date = "2022-06-01",
        end_date = "2022-06-07"
    )

    honestdid_results_July <- performing_honestdid(
        start_date = "2022-07-01",
        end_date = "2022-07-07"
    )

    honestdid_results_August <- performing_honestdid(
        start_date = "2022-08-01",
        end_date = "2022-08-07"
    )

    honestdid_results_August_end <- performing_honestdid(
        start_date = "2022-08-25",
        end_date = "2022-08-31"
    )

    # group into list
    honestdid_results_list <- list(
        honestdid_results_June,
        honestdid_results_July,
        honestdid_results_August,
        honestdid_results_August_end
    )

    #--------------------------------------------------
    # return

    return(honestdid_results_list)
}