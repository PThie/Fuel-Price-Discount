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
    #' @return NULL, Estimation plot for HonestDiD
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
        est_mod <- feols(
            fml = fm,
            data = moddata,
            cluster = regionFE
        )
        return(est_mod)
    }

    #----------------------------------------------
    # estimation

    mod_list_event <- list()

    # dependent variables
    dep_cases <- c("diesel", "e10")

    short_data <- avg_prices_prep |>
        dplyr::filter(date <= "2022-06-07")
    
    # loop through gasoline types
    for(dep_case in dep_cases){
        suppressMessages(mod <- est_fun(
            moddata = short_data,
            depvar = dep_case,
            regionFE = "station_id"
        ))
        mod_list_event[[dep_case]] <- mod
    }

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

    # join in one table
    one_week_trends <- data.table::rbindlist(trend_test_list, idcol = TRUE) |>
        dplyr::rename(fuel_type = 1)
    
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
            period = factor(period, levels = seq(1, 7, 1))
        )

    # export
    openxlsx::write.xlsx(
        one_week_trends,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            "honestdid_one_week.xlsx"
        ),
        rowNames = FALSE
    )

    #----------------------------------------------
    # plot for one-week trend

    # define colors
    pal <- MetBrewer::met.brewer(name = "Austria", n = 7)

    # loop through fuel types
    for(dep_case in dep_cases) {
        # subset data
        moddata <- one_week_trends |>
            dplyr::filter(fuel_type == dep_case & Mbar != 0)

        # generate plot
        vio_plot <- ggplot(
            data = moddata,
            aes(
                x = mbar_labels,
                y = ub,
                col = period
            )
        )+
            geom_errorbar(
                mapping = aes(
                    ymin = lb,
                    ymax = ub
                ),
                width = 0.15,
                position = "dodge",
                size = 1
            )+
            geom_hline(
                yintercept = 0
            )+
            scale_color_manual(
                name = "Days after FTD",
                values = pal
            )+
            labs(
                x = "M",
                y = ""
            )+
            theme_classic()+
            theme(
                panel.border = element_rect(linewidth = 1, fill = NA),
                axis.text = element_text(size = 19),
                axis.title = element_text(size = 20),
                legend.key.size = unit(1, "cm"),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 20),
                legend.position = "bottom"
            )
        
        # export
        filename <- paste0("honestdid_plot_one_week_", dep_case, ".png")
        suppressMessages(ggsave(
            plot = vio_plot,
            file.path(
                config_paths()[["output_path"]],
                "graphs",
                filename
            ),
            height = 8,
            width = 10
        ))
    }

    #--------------------------------------------------
    # return

    return(NULL)
}