tidy_covid_run <- function(xstart,times,params,run_name = NULL){

  if(is.null(run_name))
    run_name = Sys.time() %>% as.character()

  res_df <- as.data.frame(lsoda(xstart,times,covid.model,params)) %>%
    mutate(run_name = run_name) %>%
    date_correctoins_lsoda_df() %>%
    name_cleanup_lsoda_df()

  return(res_df)
}

date_correctoins_lsoda_df <- function(.data){
  res_df <- .data %>%
    mutate(time_days = time / 24,
           time_weeks = time_days / 7,
           time_months = time_weeks / 4 #approximate obviously
           )

  return(res_df)
}

name_cleanup_lsoda_df <- function(.data){

  res_df <- .data %>%
    rename_at(vars(ends_with("Infections")), ~ stringr::str_replace(., "Infections","_infections") %>% paste0("metric_",.) %>% tolower()) %>%
    rename_at(vars(ends_with("Deaths")), ~ stringr::str_replace(., "Deaths","_deaths") %>% paste0("metric_",.) %>% tolower()) %>%
    rename_at(vars(ends_with("Hosp")), ~ stringr::str_replace(., "Hosp","_hosp") %>% paste0("metric_",.) %>% tolower()) %>%
    rename_all(~stringr::str_replace_all(.,"inmate","incarceratedpeople"))

  return(res_df)
}

pivot_long_lsoda_df_metrics <- function(.data){

  res_df <- .data %>%
    select(run_name,starts_with("time"),starts_with("metric_")) %>%
    tidyr::pivot_longer(starts_with("metric_"), names_to = "metric_variable", values_to = "value") %>%
    mutate(metric_variable = stringr::str_remove_all(metric_variable,"metric_") %>%
             stringr::str_replace_all("_"," ") %>% stringr::str_to_title()) %>%
    separate(metric_variable, into = c("Location","Outcome"), sep = " ") %>%
    mutate(Location = stringr::str_replace_all(Location,"Incarceratedpeople","Incarcerated People"),
           Outcome = stringr::str_replace_all(Outcome,"Hosp","Hospitalizations")
           )

  tbl_pop <- .data %>%
    pivot_long_lsoda_df_detailed() %>%
    ungroup() %>%
    arrange(time) %>%
    select(composite_top,composite_population_total,run_name,time) %>%
    distinct() %>%
    rename(Location = composite_top) %>%
    filter(Location != "Incarcerated People")

  tbl_arrests <- .data %>%
    select(time,run_name, composite_population_total = TotalArrests) %>%
    mutate(Location = "Incarcerated People")

  res_df <- left_join(res_df,
                      bind_rows(tbl_pop,tbl_arrests),
                      by = c("run_name", "Location","time")) %>%
    #releveling
    mutate(Location = forcats::fct_relevel(Location,
                                           "Community",
                                           "Incarcerated People",
                                           "Staff"
                                           ),
           Outcome = forcats::fct_relevel(Outcome,
                                          "Infections",
                                          "Hospitalizations",
                                          "Deaths"
                                          )
           ) %>%
    mutate(share = value / composite_population_total)

  return(res_df)
}

pivot_long_lsoda_df_detailed <- function(.data){

  res_df <- .data %>%
    ungroup() %>%
    select(run_name,starts_with("time"),matches("^[A-Z]{1,3}$")) %>%
    tidyr::pivot_longer(matches("^[A-Z]{1,3}$"), names_to = "model_variable", values_to = "state") %>%
    arrange(time) %>%
    group_by(run_name,model_variable) %>%
    mutate(cumulative_state = cumsum(state)) %>%
    ungroup() %>%
    mutate(
      first_letter = stringr::str_sub(model_variable,1,1),
      disease_state = case_when(
        first_letter == "S" ~ "Susceptible",
        first_letter == "E" ~ "Exposed \n(Active, No Symptoms)",
        first_letter == "I" ~ "Infected \n(Active, Symptoms)",
        first_letter == "H" ~ "Hospitalized",
        first_letter == "R" ~ "Recovered \n (or Removed -Dead)"
      ),
    second_letter = stringr::str_sub(model_variable,2,2),
    risk_group = case_when(
      second_letter == "l" ~ "Low Risk",
      second_letter == "h" ~ "High Risk \n(Immuno)",
      second_letter == "e" ~ "Elderly \n(65+)",
      second_letter == "o" ~ "Staff",
      second_letter == "k" ~ "Under 18"
    ),
    people_type_top = case_when(
      risk_group %in% c("Low Risk","High Risk \n(Immuno)","Elderly \n(65+)") ~ "Person who Could be Incarcerated",
      risk_group == "Staff" ~ "Staff",
      TRUE ~ "Ineligible for Incarceration \n (Children)"
    ),
    third_letter = stringr::str_sub(model_variable, 3,3),
    location = case_when(
      third_letter == "c" ~ "Community",
      third_letter == "p" ~ "Processing - Release Eligible",
      third_letter == "j" ~ "Jail",
      third_letter == "t" ~ "Trial - Release Ineligible"
      ),
    location_top = case_when(
      location %in% c("Jail","Processing - Release Eligible","Trial - Release Ineligible") ~ "Jail",
      TRUE ~ "Community" #includes kids
      ),
    # staff can be in both locations
    # jail is not a good composite since it doesnt match the others. I don't like this but best I have
    # TODO: Counting populations deserves its own tidy function yall - gotta guarantee the denomintor
    composite_top = case_when(
      people_type_top == "Staff" ~ "Staff",
      location_top == "Jail" ~ "Incarcerated People",
      TRUE ~ location_top
    )) %>%
    select(-ends_with("_letter")) %>%
    group_by(run_name,time,location_top) %>%
    mutate(
      location_population_total = sum(state)
    ) %>%
    ungroup() %>%
    group_by(run_name,time,people_type_top) %>%
    mutate(
      people_type_population_total = sum(state)
    ) %>%
    ungroup() %>%
    mutate(
      location_percent = state / location_population_total,
      people_type_percent = state / people_type_population_total
    ) %>%
    mutate(
           composite_population_total = case_when(
             people_type_top == "Staff" ~ people_type_population_total,
             TRUE ~ location_population_total
           )
    ) %>%
    #relevelling
    mutate(
    location_top = forcats::fct_relevel(
        location_top,
        "Jail",
        "Community"
    ),
    location = forcats::fct_relevel(
      location,
      "Jail",
      "Trial - Release Ineligible",
      "Processing - Release Eligible",
      "Community"
    ),
    people_type_top = forcats::fct_relevel(
      people_type_top,
      "Ineligible for Incarceration \n (Children)",
      "Person who Could be Incarcerated",
      "Staff"
    ),
    risk_group = forcats::fct_relevel(
      risk_group,
      "Elderly \n(65+)",
      "High Risk \n(Immuno)",
      "Low Risk",
      "Staff",
      "Under 18"
    ),
    disease_state = forcats::fct_relevel(
      disease_state,
      "Recovered \n (or Removed -Dead)",
      "Susceptible",
      "Hospitalized",
      "Infected \n(Active, Symptoms)",
      "Exposed \n(Active, No Symptoms)"
      )
    )

  return(res_df)
}

get_population_totals <- function(.data,pop_type = c("composite","population_type","location")){

  pop_type <- match.arg(pop_type)

  res <- .data %>%
    pivot_long_lsoda_df_detailed() %>%
    ungroup() %>%
    select(run_name,starts_with("time"),starts_with(pop_type)) %>%
    distinct()

}