plotting_google_trends <- function(google_trends_data = NA) {
    #' @title Plotting Google trends
    #' 
    #' @description This function plots the Google trends data for the
    #' keywords "tankrabatt", "dieselpreis" and "benzinpreis".
    #' 
    #' @param google_trends_data Data frame with Google trends data
    #' 
    #' @return NULL, Plots for Google trends
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # plotting function for the different keywords

    plot_function <- function(plotname, kw) {
            #' @param kw Keyword of Google trend
            plot <- ggplot(
                    data = google_trends_data |>
                        dplyr::filter(keyword == kw)
                )+
                geom_rect(
                    aes(
                        xmin = config_globals()[["start_tr_de"]],
                        xmax = config_globals()[["end_tr_de"]],
                        ymin = 0,
                        ymax = 100
                    ),
                    fill = "grey80",
                    alpha = 0.4
                )+
                geom_text(
                    x = as.Date("2022-07-25"),
                    y = 95,
                    label = "Fuel tax discount",
                    size = 8.5
                )+
                geom_text(
                    x = as.Date("2022-04-25"),
                    y = 95,
                    label = paste0("Keyword: ", stringr::str_to_title(kw)),
                    size = 8.5
                )+
                scale_x_date(
                    breaks = "1 month",
                    date_labels = "%b"
                )+
                labs(
                    y = "Google score",
                    x = ""
                )+
                geom_line(
                    aes(x = as.Date(date, "%Y-%m-%d"), y = score),
                    linewidth = 1
                )+
                theme_classic()+
                theme(
                    axis.text = element_text(size = 23),
                    axis.title = element_text(size = 23)
                )

            filename <- paste0(plotname, ".png")
            suppressMessages(ggsave(
                plot = plot,
                file.path(
                    config_paths()[["output_path"]],
                    "graphs",
                    filename
                ),
                dpi = config_globals()[["owndpi"]],
                width = 13,
                height = 8
            ))
        }

    plot_function(plotname = "tankrabatt_trend", kw = "tankrabatt")
    plot_function(plotname = "dieselpreis_trend", kw = "dieselpreis")
    plot_function(plotname = "benzinpreis_trend", kw = "benzinpreis")

    #--------------------------------------------------
    # return
    
    return(NULL)
}