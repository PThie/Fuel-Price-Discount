calculating_fuel_consumption_purch_power <- function(microm_data_cleaned = NA) {
    #' @title Calculate fuel consumption for different income groups
    #' 
    #' @description This function calculates the fuel consumption for different
    #' income groups (below and above median).
    #' 
    #' @param microm_data_cleaned Cleaned microm data (RWI-GEO-GRID data).
    #' 
    #' @return Data frame with fuel consumption for different income groups.
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # fuel consumption over the period of FTD

    # share of diesel cars in 2022 (taken from Verkehr in Zahlen 2022/2023
    # Federal Ministry for Digital and Transport)
    share_diesel <- 0.305
    share_petrol <- 1 - share_diesel

    # total average fuel consumption (from MOP estimation) by income group
    # per year in liters (over three months; time frame of FTD)
    income_group_fuel_consumption <- data.frame(
        cbind(
            income_groups = c("below_median", "above_median"),
            fuel_cons_diesel = c(89.40528, 109.9434),
            fuel_cons_petrol = c(62.26438, 73.90228)
        )
    )

    # calculate the fuel consumption for the FTD period of three months
    # (therefore multiply by 3)
    income_group_fuel_consumption <- income_group_fuel_consumption |>
        dplyr::mutate(
            # set average fuel consumption as numeric
            dplyr::across(
                .cols = dplyr::contains("fuel_cons"),
                ~ as.numeric(.x)
            ),
            # multiply by 3 to get the fuel consumption over FTD period
            dplyr::across(
                .cols = dplyr::contains("fuel_cons"),
                ~ .x * 3
            )
        )

    #----------------------------------------------
    # clean griddata

    # add number of cars (by fuel type) and per person income
    microm_data_cleaned_prep <- microm_data_cleaned |>
        dplyr::mutate(
            # calculate the number of cars
            cars = car_density * num_households,
            # calculate number of diesel cars
            diesel_cars = cars * share_diesel,
            petrol_cars = cars * share_petrol,
            # calculate income per person
            income_pp = purch_power / people_total
        )

    # add income groups
    # income thresholds (based on median in grid data)
    income_threshold <- median(microm_data_cleaned_prep$income_pp, na.rm = TRUE)
    microm_data_cleaned_prep <- microm_data_cleaned_prep |>
        dplyr::mutate(
            income_groups = dplyr::case_when(
                income_pp < income_threshold ~ "below_median",
                income_pp >= income_threshold ~ "above_median"
            )
        )

    # merge fuel consumption by income group
    microm_data_cleaned_prep <- merge(
        microm_data_cleaned_prep,
        income_group_fuel_consumption,
        by = "income_groups",
        all.x = TRUE
    )

    #----------------------------------------------
    # analysis based on income groups

    # calculate total fuel consumption
    fuel_cons_total <- microm_data_cleaned_prep |>
        dplyr::group_by(income_groups) |>
        dplyr::summarise(
            n = n(),
            total_diesel_cars = sum(diesel_cars, na.rm = TRUE),
            total_petrol_cars = sum(petrol_cars, na.rm = TRUE),
            total_fuel_cons_diesel = total_diesel_cars * fuel_cons_diesel,
            total_fuel_cons_petrol = total_petrol_cars * fuel_cons_petrol
        ) |>
        dplyr::distinct() |>
        dplyr::filter(!is.na(income_groups))

    # export
    openxlsx::write.xlsx(
        fuel_cons_total,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "fuel_consumption_income_groups.xlsx"
        ),
        rowNames = FALSE
    )

    #--------------------------------------------------
    # return

    return(fuel_cons_total)
}