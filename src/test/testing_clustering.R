testing_clustering <- function(
    price_data = NA
) {
    #' @title Testing different clustering levels in the estimation
    #' 
    #' @description This function estimates the baseline model with different
    #' clustering levels (state, district, municipality, two-way clustering)
    #' and exports the results as LaTeX tables.
    #' 
    #' @param price_data Data frame with price data prepared for estimation.
    #' 
    #' @return List of estimation models with different clustering levels.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # estimation function
    
    est_fun_clustering <- function(
        moddata = NA,
        depvar = c("diesel", "e10"),
        cluster_name = NA
    ) {
        fm <- formula(
            paste(
                depvar, "~",
                paste("relevel(as.factor(treat_tankrabatt_de), \"control\") * relevel(as.factor(treat_region_de), \"control\")"),
                paste("| as.factor(date) + station_id")
            )
        )

        est_mod <- fixest::feols(
            fml = fm,
            data = moddata,
            cluster = cluster_name
        )

        return(est_mod)
    }

    #--------------------------------------------------
    # estimation

    vars <- c("diesel", "e10")
    mod_list <- list() 
    suppressMessages(for (var in vars) {
        # two-way clustering
        mod_twoway <- est_fun_clustering(
            moddata = price_data,
            depvar = var,
            cluster_name = c("station_id", "date")
        )

        # state-level clustering
        mod_state <- est_fun_clustering(
            moddata = price_data,
            depvar = var,
            cluster_name = "state_id"
        )

        # district-level clustering
        mod_district <- est_fun_clustering(
            moddata = price_data,
            depvar = var,
            cluster_name = "district_id"
        )

        # municipality-level clustering
        mod_municipality <- est_fun_clustering(
            moddata = price_data,
            depvar = var,
            cluster_name = "munic_id"
        )

        # store
        mod_list[[paste0(var, "_twoway")]] <- mod_twoway
        mod_list[[paste0(var, "_state")]] <- mod_state
        mod_list[[paste0(var, "_district")]] <- mod_district
        mod_list[[paste0(var, "_municipality")]] <- mod_municipality

        # export
        filename <- paste0(
            "testing_clustering_",
            var,
            ".tex"
        )
        fixest::esttex(
            mod_list[[paste0(var, "_state")]],
            mod_list[[paste0(var, "_district")]],
            mod_list[[paste0(var, "_municipality")]],
            mod_list[[paste0(var, "_twoway")]],
            headers = c("State", "District", "Municipality", "Two-way"),
            file = file.path(
                config_paths()[["output_path"]],
                "estimation",
                filename
            ),
            digits = "r3",
            dict = config_globals()[["coefnames"]],
            replace = TRUE,
            signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
        )
    })

    #--------------------------------------------------
    # return

    return(mod_list)
}