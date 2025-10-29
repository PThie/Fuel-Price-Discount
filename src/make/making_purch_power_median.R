making_purch_power_median <- function(
    price_data = NA,
    german_stations = NA,
    microm_data_cleaned = NA
) {
    #' @title Analysis purchasing power based on median classification
    #' 
    #' @desciption This function analysis the passthrough given different
    #' purchasing power groups (i.e. income groups).
    #' 
    #' @param price_data Price data for Germany and France for
    #' April to August 2022
    #' @param german_stations German station information
    #' @param microm_data_cleaned Microm data cleaned (RWI-GEO-GRID)
    #' 
    #' @author Patrick Thiel

    #----------------------------------------------
    # summarise purchasing power on municipality level

    purchpower_municipality <- microm_data_cleaned |>
        dplyr::group_by(AGS) |>
        dplyr::summarise(
            total_purch_power = sum(purch_power, na.rm = TRUE),
            total_people = sum(people_total, na.rm = TRUE),
            purch_power_pp = total_purch_power / total_people
        ) |>
        as.data.frame()

    #----------------------------------------------
    # add purchasing power groups

    median_pp <- median(purchpower_municipality$purch_power_pp, na.rm = TRUE)

    # add purchasing power categories
    purchpower_municipality <- purchpower_municipality |>
        dplyr::mutate(
            purch_power_pp_cat = dplyr::case_when(
                purch_power_pp < median_pp ~ "below_median",
                purch_power_pp >= median_pp ~ "above_median"
            )
        )
    
    #----------------------------------------------
    # merge join stations and geo info
    # add also purchasing power

    german_stations_munic <- merge(
        german_stations |>
            sf::st_drop_geometry(),
        purchpower_municipality,
        by = "AGS"
    )

    #----------------------------------------------
    # merge to station prices

    avg_prices_pp <- merge(
        price_data,
        german_stations_munic,
        by = "station_id",
        all.x = TRUE
    )
    
    # replace NA for France
    # set purchasing power category to "outside_DE"
    avg_prices_pp$purch_power_pp_cat[avg_prices_pp$country == "FR"] <- "outside_DE"

    #----------------------------------------------
    # estimation

    # define purchasing power categories
    # define dependent variables
    cats <- c("above_median", "below_median")
    vars <- c("diesel", "e10")

    # list for storage
    est_results_munic <- list()

    # estimation function
    est_fun <- function(moddata, depvar = c("diesel", "e10"), pp_cat) {
        # subset for purchasing categories
        var_aux <- rlang::sym("purch_power_pp_cat")
        moddata_short <- moddata |>
            filter(!!var_aux == "outside_DE" | !!var_aux == pp_cat)

        # define formula
        fm <- formula(
            paste(
                depvar, "~",
                paste("+ relevel(as.factor(treat_tankrabatt_de), \"control\") * relevel(as.factor(treat_region_de), \"control\")"),
                paste("| as.factor(date) + station_id")
            )
        )

        # estimate
        est_mod <- feols(
            fml = fm,
            data = moddata_short,
            cluster = "station_id",
            notes = FALSE
        )

        # define model run
        mod <- paste(depvar, pp_cat, sep = "_")

        # extract estimate
        interaction <- "relevel(as.factor(treat_tankrabatt_de), \"control\")treated:relevel(as.factor(treat_region_de), \"control\")treated"
        est <- as.numeric(
            est_mod$coefficients[interaction]
        )

        # calculate confidence intervall
        con <- confint(est_mod, level = 0.95)[interaction, ]

        # put components together
        results_table <- data.frame(matrix(ncol = 4, nrow = 1))
        colnames(results_table) <- c("mod_name", "estimate", "lower_ci", "upper_ci")
        results_table <- results_table |>
            mutate(
                mod_name = mod,
                estimate = est,
                lower_ci = con[1, 1],
                upper_ci = con[1, 2]
            )

        # return output
        return(results_table)
    }

    # loop through options
    for(var in vars) {
        for(cat in cats) {
            # list name
            list_name <- paste(var, "pp_cat", cat, sep = "_")
            # loop through options and store output
            est_results_munic[[list_name]] <- est_fun(
                moddata = avg_prices_pp,
                depvar = var,
                pp_cat = cat
            )
        }
    }    

    # row bind the results
    est_results_munic_df <- rbindlist(est_results_munic)
    
    # export results
    openxlsx::write.xlsx(
        est_results_munic_df,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            "hetero_purch_power_munic_median.xlsx"
        )
    )

    #--------------------------------------------------
    # return

    return(NULL)
}