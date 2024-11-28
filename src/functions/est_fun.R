est_fun <- function(
    moddata,
    depvar = c("diesel", "e10"),
    fixef = c("time", "region", "time_region", "none"),
    event = FALSE,
    twoway_clustering = FALSE,
    temperature = FALSE
) {
    # Time FE
    if(fixef == "time"){
        fm <- formula(
            paste(
                depvar, "~",
                paste("relevel(as.factor(treat_tankrabatt_de), \"control\") * relevel(as.factor(treat_region_de), \"control\")"),
                paste("| as.factor(date)")
            )
        )
    } else if(fixef == "region"){
        fm <- formula(
            paste(
                depvar, "~",
                paste("relevel(as.factor(treat_tankrabatt_de), \"control\") * relevel(as.factor(treat_region_de), \"control\")"),
                paste("| station_id")
            )
        )
    } else if(fixef == "time_region") {
        fm <- formula(
            paste(
                depvar, "~",
                paste("relevel(as.factor(treat_tankrabatt_de), \"control\") * relevel(as.factor(treat_region_de), \"control\")"),
                paste("| as.factor(date) + station_id")
            )
        )
    } else {
        fm <- formula(
            paste(
                depvar, "~",
                paste("relevel(as.factor(treat_tankrabatt_de), \"control\") * relevel(as.factor(treat_region_de), \"control\")")
            )
        )
    } 
    
    # formula for event study
    if(event == TRUE) {
        fm <- formula(
            paste(
                depvar, "~",
                paste("relevel(as.factor(time_to_treatment), \"-1\") * relevel(as.factor(treat_region_de), \"control\")"),
                paste("| as.factor(date) + station_id")
            )
        )
    }

    # clustering along two dimensions
    if (twoway_clustering == FALSE) {
        # estimation model
        est_mod <- fixest::feols(
            fml = fm,
            data = moddata,
            cluster = "station_id"
        )
    } else {
        est_mod <- fixest::feols(
            fml = fm,
            data = moddata,
            cluster = c("station_id", "date")
        )
    }

    # for estimating the impact of temperature
    if (temperature == TRUE) {
        fm <- formula(
            paste(
                depvar,
                "~",
                paste("temperature +"),
                paste("relevel(as.factor(treat_tankrabatt_de), \"control\") * relevel(as.factor(treat_region_de), \"control\")"),
                paste("| as.factor(date) + station_id")
            )
        )
    }

    # return model
    return(est_mod)
}

