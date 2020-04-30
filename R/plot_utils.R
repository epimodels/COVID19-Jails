### INDIVIDUAL PLOTS - FOR A SINGLE PLOT ---------------

plot_individual_detailed <- function(.data_detailed, run_name){
  ggp <- .data_detailed %>%
    filter(!is.na(risk_group)) %>%
    ggplot(aes(x=time_days, y=state, color = risk_group)) +
    geom_line() +
    facet_grid(location ~ disease_state, scales=  "free") +
    xlab("Days") +
    ylab("Count") +
    ggtitle(paste0(run_name," - Detailed Metrics"))

  return(ggp)
}

plot_individual_metrics <- function(.data_metrics, run_name){

  ggp <- .data_metrics %>%
    ggplot(aes(x=time_days, y=value, color = Location)) +
    geom_line() +
    facet_col(~Outcome, scales=  "free") +
    xlab("Days") +
    ylab("Count") +
    ggtitle(paste0(run_name," - Topline Metrics"))

  return(ggp)
}

plot_individual_percents <- function(.data_detailed, run_name){

  ggp <- .data_detailed %>%
    filter(!is.na(location)) %>%
    group_by(disease_state,location_top,time,time_days) %>%
    summarise(percent = sum(location_percent)) %>%
    ggplot(aes(x = time_days, y = percent, fill = disease_state)) +
    geom_col(position = "stack") +
    facet_col(~location_top, scales = "free") +
    scale_y_continuous(labels = scales::percent, expand = expansion(0)) +
    ggtitle(paste0(run_name," - Percent of Locations in Disease States")) +
    ylab("Percent of Population") +
    xlab("Days")

  return(ggp)
}

plot_individual_poptype_percents <- function(.data_detailed, run_name){

  ggp <- .data_detailed %>%
    group_by(disease_state,people_type_top,time,time_days,run_name) %>%
    summarise(percent = sum(people_type_percent)) %>%
    ungroup() %>%
    ggplot(aes(x = time_days, y = percent, fill = disease_state)) +
    geom_col(position = "stack") +
    facet_col(~ people_type_top, scales = "free") +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Percent of Population Types in Disease States")

  return(ggp)
}

plot_individual_totalpops <- function(.data_detailed, run_name){

  ggp <- .data_detailed %>%
    ungroup() %>%
    select(people_type_top,time,time_days,people_type_population_total) %>%
    distinct() %>%
    ggplot(aes(x = time_days, y = people_type_population_total, color = people_type_top)) +
    geom_line() +
    facet_col(~ people_type_top, scales = "free") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Total People Type Populations (Denominator Check)")

  return(ggp)
}

plot_individual_location_pops <- function(.data_detailed, run_name){

  ggp <- .data_detailed %>%
    ungroup() %>%
    select(location_top,time,time_days,location_population_total) %>%
    distinct() %>%
    ggplot(aes(x = time_days, y = location_population_total, color = location_top)) +
    geom_line() +
    facet_col(~ location_top, scales = "free") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Total Location Populations (Denominator Check)")

  return(ggp)
}

plot_individual_diagnostics <- function(.data, run_name){

 detailed_df <- .data %>% pivot_long_lsoda_df_detailed()
 metrics_df <- .data %>% pivot_long_lsoda_df_metrics()

 p1 <- plot_individual_detailed(detailed_df, run_name)
 p2 <- plot_individual_metrics(metrics_df, run_name)
 p3 <- plot_individual_percents(detailed_df,run_name)
 p4 <- plot_individual_poptype_percents(detailed_df, run_name)
 p5 <- plot_individual_totalpops(detailed_df, run_name)
 p6 <- plot_individual_location_pops(detailed_df, run_name)

  plot(p1)
  plot(p2)
  plot(p3)
  plot(p4)
  plot(p5)
  plot(p6)
  return(invisible(list(p1,p2,p3,p4,p5,p6)))
}

### COMPOSITE PLOTS -- FOR MULTIPLE RUN COMPARISON ---------------------------
plot_composite_metrics <- function(.data_metrics, base_run_name = "Business As Usual", x_type = c("number","percent")){

  x_type <- match.arg(x_type)
  dl_tbl <- .data_metrics

  if(x_type == "number"){
    dl_tbl <- mutate(dl_tbl, out = value)
    .scale_y <- scale_y_continuous(labels = scales::comma,  expand = expansion(mult = c(0,.1)))
  } else {
    dl_tbl <- mutate(dl_tbl, out = share)
    .scale_y <- scale_y_continuous(labels = scales::percent,  expand = expansion(mult = c(0,.1)))
  }

  ggp <- dl_tbl %>%
    ggplot(aes(x = time_days,y = out, color = run_name)) +
    facet_wrap(Outcome ~ Location, scales = "free",nrow = 3, ncol =3) +
    geom_line() +
    scale_x_continuous(breaks = c(0,30,60,90,120), expand = expansion(mult = c(0,.1))) +
    .scale_y +
    theme(legend.position="bottom") +
    xlab("Days") +
    ylab("Cumulative Counts") +
    ggtitle("Cumulative Counts of People in Different Infections States by Location and Population")

  # i need sophie help here!

  return(ggp)
}

plot_composite_finish <- function(.data_metrics, base_run_name = "Business As Usual", x_type = c("number","percent")){

  x_type <- match.arg(x_type)

  dl_tbl <- .data_metrics %>%
    arrange(time) %>%
    filter(time == last(time))

  if(x_type == "number"){
    dl_tbl <- mutate(dl_tbl, out = value)
    .scale_x <- scale_x_continuous(labels = scales::comma)
  } else {
    dl_tbl <- mutate(dl_tbl, out = share)
    .scale_x <- scale_x_continuous(labels = scales::percent)
  }

  ggp <- ggplot(dl_tbl, aes(y = forcats::fct_reorder(run_name,out), x = out, fill = run_name)) +
    geom_col() +
    facet_wrap(Location ~ Outcome, scales = "free", ncol = 3, nrow = 3) +
    theme(legend.position = "bottom") +
    theme(axis.text.y = element_blank()) +
    .scale_x


  return(ggp)
}

plot_composite_diseaste_states <- function(.data_detailed, base_run_name = "Business As Usual"){

  ggp <- .data_detailed %>%
    filter(!is.na(location)) %>%
    group_by(disease_state,location_top,time,time_days,run_name) %>%
    summarise(percent = sum(location_percent)) %>%
    ggplot(aes(x = time_days, y = percent, color = run_name)) +
    geom_line() +
    facet_grid(disease_state ~ location_top, scales = "free") +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Percent of Locations in Disease States")

  return(ggp)
}

plot_composite_location_population <- function(.data_detailed,base_run_name = "Business As Usual"){

  ggp <- .data_detailed %>%
    select(location_top,time,time_days,run_name,location_population_total) %>%
    distinct() %>%
    ggplot(aes(x = time_days, y = location_population_total, color = run_name)) +
    geom_line() +
    facet_col(~ location_top, scales = "free") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Total Location Populations (Denominator Check)")

  return(ggp)
}

plot_composite_population_type <- function(.data_detailed, base_run_name = "Business As Usual"){

  ggp <- .data_detailed %>%
    select(people_type_top,time,time_days,run_name,people_type_population_total) %>%
    distinct() %>%
    ggplot(aes(x = time_days, y = people_type_population_total, color = run_name)) +
    geom_line() +
    facet_col(~ people_type_top, scales = "free") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Total People Type Populations (Denominator Check)")

  return(ggp)
}

plot_composite_diagnostics <- function(.data, base_run_name = "Business As Usual", x_type = c("number","percent")){

  detailed_df <- .data %>% pivot_long_lsoda_df_detailed()
  metrics_df <- .data %>% pivot_long_lsoda_df_metrics()

  p1 <- plot_composite_metrics(metrics_df, base_run_name, x_type)
  p2 <- plot_composite_finish(metrics_df,base_run_name, x_type)
  p3 <- plot_composite_diseaste_states(detailed_df,run_name)
  p4 <- plot_composite_location_population(detailed_df, run_name)
  p5 <- plot_composite_population_type(detailed_df, run_name)

  plot(p1)
  plot(p2)
  plot(p3)
  plot(p4)
  plot(p5)

  return(invisible(list(p1,p2,p3,p4,p5)))
}

### PLOT UTILS BASIC ---------
theme_basic <- function(
  color_base = c('grey','white','dark'),
  base_size = 11,
  base_line_size = base_size/22,
  base_rect_size = base_size/22,
  x_axis_label_padding = -.8, #for some plots, best to set this to 0. For bar, set negative until nice spacing
  flipped = FALSE #FOR FLIPPED CHARTS, NNED TO SWITCH AXES
){

  color_base <- match.arg(color_base)
  if(color_base == "grey"){
    major_line_color = "#B2B3B6"
    background_color = "#EAEBEB"
  } else if(color_base == "white"){
    major_line_color = "#EFECEA"
    background_color = "white"
  } else if(color_base == "dark"){
    major_line_color = "#EFECEA"
    background_color = "#231F20"
  }

  half_line <- base_size/2

  base_theme <- ggplot2::theme_grey(base_size = base_size,
                                    base_line_size = base_line_size,
                                    base_rect_size = base_rect_size)

  major_line <- ggplot2::element_line(
    colour = major_line_color,
    size = rel(1.2)
  )

  axis_line <- major_line

  background_rect <- ggplot2::element_rect(
    fill = background_color,
    colour = NA)

  bottom_axis_line_val <- x_axis_label_padding * half_line/2
  standard_axis_line_val <- .8 * half_line/2
  bottom_element_text_func <- function(marg_type = c("t","l","r","b"),text_color = "black"){
    marg_type <- match.arg(marg_type)
    marg_type <- marg_type[1]

    #TODO: Determine how to do this with tidyeval
    if(marg_type == "t"){
      axis_x <- ggplot2::element_text(margin = ggplot2::margin(t = bottom_axis_line_val), vjust = 0, colour = text_color)
      axis_y <- ggplot2::element_text(margin = ggplot2::margin(l = standard_axis_line_val), hjust = 1, colour = text_color)
    } else if(marg_type == "b"){
      axis_x <- ggplot2::element_text(margin = ggplot2::margin(b = bottom_axis_line_val), vjust = 0, colour = text_color)
      axis_y <- ggplot2::element_text(margin = ggplot2::margin(l = standard_axis_line_val), hjust = 1, colour = text_color)
    } else if (marg_type == "l") {
      axis_x <- ggplot2::element_text(margin = ggplot2::margin(b = standard_axis_line_val), vjust = 1, colour = text_color)
      axis_y <- ggplot2::element_text(margin = ggplot2::margin(l = bottom_axis_line_val), hjust = 0, colour = text_color)
    } else {
      axis_x <- ggplot2::element_text(margin = ggplot2::margin(b = standard_axis_line_val), vjust = 1, colour = text_color)
      axis_y <- ggplot2::element_text(margin = ggplot2::margin(r = bottom_axis_line_val), hjust = 0, colour = text_color)
    }

    return(list(x=axis_x,y=axis_y))
  }

  if(flipped){
    panel_grid_y <- ggplot2::element_blank()
    panel_grid_x <- major_line
    axis_line_x <- ggplot2::element_blank()
    axis_line_y <- axis_line
    axis_text_out <- bottom_element_text_func("t")
    axis_text_x <- axis_text_out[["x"]]
    axis_text_y <- axis_text_out[["y"]]
  } else {
    panel_grid_y <- major_line
    panel_grid_x <- ggplot2::element_blank()
    axis_line_x <-  axis_line
    axis_line_y <- ggplot2::element_blank()
    axis_text_out <- bottom_element_text_func("l")
    axis_text_x <- axis_text_out[["x"]]
    axis_text_y <- axis_text_out[["y"]]
  }

  base_theme + ggplot2::theme(
    #PANEL CONTROL --------
    panel.background = background_rect,
    plot.background = background_rect,
    panel.grid.major = NULL,
    panel.grid.major.y = panel_grid_y,
    panel.grid.major.x = panel_grid_x,
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    #TICK/AXIS CONTROL --------
    axis.ticks = ggplot2::element_blank(),
    axis.line = NULL,
    axis.line.x = ggplot2::element_blank(), #axis_line_x,
    axis.line.y = ggplot2::element_blank(), #axis_line_y,
    axis.text.x = axis_text_x,
    axis.text.y = axis_text_y,
    #LEGEND
    legend.background = element_rect(
      fill = background_color,
      colour = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key = ggplot2::element_blank(),
    #TITLE AND SUBTITLE CONTROLE ------------
    plot.title = element_text(size=base_size+ 2,
                              face = "bold",
                              colour = "black"
                              ),
    plot.subtitle = element_text(size = base_size, colour = "grey")
  )

}

theme_basic_white <- function(...){
  theme_basic(color_base = "white", base_size = 14)
}

## in ggplot2 use : scale_x_continuous(labels = suffix_formatter)
suffix_formatter <- function(x, digits = 0) {
  intl <- c(1e3, 1e6, 1e9, 1e12)
  suffixes <- c("K", "M", "B", "T")

  i <- findInterval(abs(x), intl)

  result <- character(length(abs(x)))

  # Note: for ggplot2 the last label element of x is NA, so we need to handle it
  ind_format <- !is.na(x) & i > 0

  # Format only the elements that need to be formatted
  # with suffixes and possible rounding
  result[ind_format] <- paste0(
    formatC(x[ind_format] / intl[i[ind_format]], format = "f", digits = digits),
    suffixes[i[ind_format]]
  )
  # And leave the rest with no changes
  result[!ind_format] <- as.character(x[!ind_format])

  return(invisible(result))
}