library(ggsave) #publication quality graphics hopefully
graph_dir <- here::here("outputs")
rmarkdown::render("covid_scenario_report.Rmd")

#Figure figBaselineCurves
base_red <- ggsci::pal_npg(alpha = 1)(6)[1]

# a copy of the plot individual percents function but more readable and without some things people would be unfamilar with
ggp <- resdf_bau %>%
  pivot_long_lsoda_df_detailed() %>%
  filter(!is.na(location)) %>%
  filter(!stringr::str_detect(disease_state,"Recovered")) %>%
  filter(!stringr::str_detect(disease_state,"Susceptible")) %>%
  group_by(disease_state,location_top,time,time_days) %>%
  summarise(percent = sum(location_percent)) %>%
  ungroup() %>%
  ggplot(aes(x = time_days, y = percent, fill = disease_state)) +
  geom_area(position = "stack") +
  facet_row(~location_top) +
  scale_y_continuous(labels = scales::percent, expand = expansion(0)) +
  scale_x_continuous(breaks = c(0,30,60,90,120,150,180), expand = expansion(mult = c(0,.1))) +
  ggtitle(paste0("Baseline Simulated COVID-19 Epidemic")) +
  ylab("Percent of Population") +
  xlab("Days") +
  scale_fill_manual(values = c("Exposed \n(Active, No Symptoms)" = lighten(base_red,.6),
                               "Infected \n(Active, Symptoms)" = lighten(base_red,.3),
                               "Hospitalized" =  base_red
  ),
  name="Disease\nState"
  ) +
  theme(legend.position = c(0.8, 0.8),
        legend.direction = "vertical",
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "white",colour = "black",size = .1),
        legend.box.background = element_rect(color="black", fill = "white", size=.1))

ggp
ggsave(filename = "figBaselineCurve.png",
       plot = ggp,
       device = "png",
       path = graph_dir,
       width = 8, height = 5,
       dpi = "retina")
ggsave(filename = "figBaselineCurve.pdf",
       plot = ggp,
       device = "pdf",
       path = graph_dir,
       width = 8, height = 5,
       dpi = "retina")

#figDistancing
bau_red <-ggsci::pal_npg(alpha = 1)(6)[5]
sip_green <- ggsci::pal_npg(alpha = 1)(6)[3]

ggp <- comparison_df_base %>%
  pivot_long_lsoda_df_metrics() %>%
  ggplot(aes(x = time_days,y = value, color = run_name)) +
  facet_wrap(Outcome ~ Location, scales = "free",ncol = 3, nrow = 3) +
  geom_line() +
  scale_y_continuous(labels = suffix_formatter,  expand = expansion(mult = c(0,.1))) +
  scale_x_continuous(breaks = c(0,30,60,90,120,150,180), expand = expansion(mult = c(0,.1))) +
  xlab("Days") +
  ylab("Cumulative Counts") +
  ggtitle("Cumulative Outcomes by Location and Population",
          "End of Scenario at 6 Months; Count in Different Infection States"
  ) +
  scale_color_manual(values = c("Shelter in Place" = sip_green,
                                "Business As Usual" = bau_red
  ),
  name="Scenario Name"
  ) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        legend.background = element_rect(fill = "white",colour = NA,size = .1),
        legend.box.background = element_rect(color="white", fill = NA, size=.1))

ggp
ggsave(filename = "figDistancing.png",
       plot = ggp,
       device = "png",
       path = graph_dir,
       width = 8, height = 8,
       dpi = "retina")
ggsave(filename = "figDistancing.pdf",
       plot = ggp,
       device = "pdf",
       path = graph_dir,
       width = 8, height = 8,
       dpi = "retina")

#figReducedArrests
arrest_base_color <- ggsci::pal_npg(alpha = 1)(6)[4]
arrest_be_color <- lighten(arrest_base_color,.25)
arrest_ll_color <- lighten(arrest_base_color,.83)
arrest_fewer_color <- lighten(arrest_base_color,.9)
arrest_vp_color <- hex(mixcolor(.5,hex2RGB(lighten(arrest_base_color,.9)),hex2RGB(base_red)))

ggp <- comparison_df_with_arrests %>%
  pivot_long_lsoda_df_metrics() %>%
  arrange(time) %>%
  filter(time == last(time)) %>%
  ggplot(aes(y = forcats::fct_reorder(run_name,value), x = value,
             fill = forcats::fct_relevel(run_name,"Shelter in Place","Bail Eligible","Vulnerable Only","Low Level","Arrest Fewer People"))) +
  geom_col() +
  facet_wrap(Location ~ Outcome, scales = "free", ncol = 3, nrow = 3) +
  theme(legend.position = "bottom") +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  scale_x_continuous(labels = suffix_formatter) +
  scale_fill_manual(values = c("Shelter in Place" = arrest_base_color,
                               "Bail Eligible" = arrest_be_color,
                               "Arrest Fewer People" = arrest_fewer_color,
                               "Low Level"= arrest_ll_color,
                               "Vulnerable Only"= arrest_vp_color
  ),
  name="Scenario\nName"
  ) +
  guides(fill = guide_legend(nrow = 2)) +
  xlab("Population Count") +
  ggtitle("Outcomes by Arrest Reduction Scenarios",
          "End of Scenario at 6 Months, Baseline of Shelter in Place in Community"
  )

ggp
ggsave(filename = "figReducedArrests.png",
       plot = ggp,
       device = "png",
       path = graph_dir,
       width = 8, height = 8,
       dpi = "retina")
ggsave(filename = "figReducedArrests.pdf",
       plot = ggp,
       device = "pdf",
       path = graph_dir,
       width = 8, height = 8,
       dpi = "retina")

#figFastRelease
tmp_df <- comparisons_df_arrest_plus_release %>%
  pivot_long_lsoda_df_metrics() %>%
  arrange(time) %>%
  filter(time == last(time)) %>%
  mutate(change_text = stringr::str_extract_all(run_name,"[0-9|\\.|,|%]{1,4}"),
         change_numeric = stringr::str_remove_all(change_text,",") %>%
           stringr::str_remove_all("%") %>%
           as.numeric()) %>%
  separate(run_name,
           c("run_base","post_indicator"),
           sep = "\\+", remove = FALSE) %>%
  mutate_if(is.character,stringr::str_trim)

sip_df <- filter(tmp_df, run_name == "Shelter in Place")
tmp_df <- filter(tmp_df, run_name != "Shelter in Place")

ggp <- tmp_df %>%
  ggplot(aes(x = change_numeric, y = value, color = forcats::fct_relevel(run_base,"Bail Eligible", "Vulnerable Only","Low Level"))) +
  geom_point() +
  geom_hline(data = sip_df, aes(yintercept = value),linetype = "dashed", color = sip_green) +
  facet_wrap(Location ~ Outcome, scales = "free", ncol = 3, nrow = 3) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = suffix_formatter, expand = expansion(mult = c(0,.2))) +
  scale_x_continuous(breaks = c(1,1.5,2),labels = c("1X","1.5X","2X")) +
  xlab("Rate of Release Speed Compared to Baseline (1x)") +
  ylab("Count") +
  ggtitle("Outcomes by Arrest Reduction with Increased Release Rates",
          "End of Scenario at 6 Months, Baseline of Shelter in Place in Community"
  ) +
  scale_color_manual(values = c(
    "Bail Eligible" = arrest_be_color,
    "Low Level"= arrest_ll_color,
    "Vulnerable Only"= arrest_vp_color
  ),
  name = "Scenario Name"
  )

ggp
ggsave(filename = "figFastRelease.png",
       plot = ggp,
       device = "png",
       path = graph_dir,
       width = 8, height = 8,
       dpi = "retina")
ggsave(filename = "figFastRelease.pdf",
       plot = ggp,
       device = "pdf",
       path = graph_dir,
       width = 8, height = 8,
       dpi = "retina")

#figReducedMixing
tmp_df <- resdf_be_mix %>%
  pivot_long_lsoda_df_detailed() %>%
  filter(!stringr::str_detect(disease_state,"Recovered")) %>%
  filter(!stringr::str_detect(disease_state,"Susceptible")) %>%
  group_by(time,time_days,run_name,composite_top) %>%
  summarise(n = sum(state)) %>%
  ungroup() %>%
  mutate(change_text = stringr::str_extract_all(run_name,"[0-9|\\.|,|%]{1,4}"),
         change = stringr::str_remove_all(change_text,",") %>%
           stringr::str_remove_all("%") %>%
           as.numeric()) %>%
  separate(run_name,
           c("run_base","post_indicator"),
           sep = "\\+", remove = FALSE) %>%
  mutate_if(is.character,stringr::str_trim) %>%
  mutate(change_special = case_when(
    change == 100 ~ "Shelter in Place Equivalent",
    change == 10 ~ "Baseline Mixing",
    TRUE ~ "Between"
  ))

ggp <- ggplot(tmp_df,aes(x = time_days, y = n, fill = factor(change), colour = change_special)) +
  geom_line() +
  facet_col(~composite_top, scales = "free") +
  #geom_point(data = sip_df, aes(x = time_days, y = n,fill = "green")) +
  scale_y_continuous(labels = suffix_formatter, expand = expansion(0)) +
  scale_x_continuous(breaks = c(0,30,60,90,120,150,180),
                     expand = expansion(mult = c(0,.1))) +
  # scale_fill_manual(values = c("green" = sip_green),
  #                  name = "Community only Shelter in Place") +
  scale_color_manual(values = c("Shelter in Place Equivalent" = sip_green,
                                "Between" = lighten(arrest_be_color,.8),
                                "Baseline Mixing" = arrest_be_color),
                     name = "% of Community Mixing \n (Shelter in Place)") +
  ggtitle(
    paste0("Outcomes with Reducing Mixing"),
    "Bail Eligible Scneario with Mixing Reduction from Baseline to General Population Level Mixing"
  ) +
  ylab("Infections (incl. Hospitalized)") +
  xlab("Days")

ggp
ggsave(filename = "figReducedMixing.png",
       plot = ggp,
       device = "png",
       path = graph_dir,
       width = 8, height = 8,
       dpi = "retina")
ggsave(filename = "figReducedMixing.pdf",
       plot = ggp,
       device = "pdf",
       path = graph_dir,
       width = 8, height = 8,
       dpi = "retina")

#figImprovedDetection
tmp_df <- resdf_sip_detection %>%
  pivot_long_lsoda_df_metrics() %>%
  arrange(time) %>%
  filter(time == last(time)) %>%
  mutate(change_text = stringr::str_extract_all(run_name,"[0-9|\\.|,|%]{1,4}"),
         change_numeric = stringr::str_remove_all(change_text,",") %>%
           stringr::str_remove_all("%") %>%
           as.numeric()) %>%
  separate(run_name,
           c("run_base","post_indicator"),
           sep = "\\+", remove = FALSE) %>%
  mutate_if(is.character,stringr::str_trim) %>%
  filter(Location == "Incarcerated People")

ggp <- ggplot(tmp_df,aes(x = Outcome, y = value, fill = factor(change_numeric))) +
  geom_col(position = "dodge") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = suffix_formatter) +
  scale_fill_manual(values = c(
    "95" = darken(sip_green,.5),
    "99" = darken(sip_green,.25),
    "100"= sip_green
  ),
  name="Healthcare Access As Compared to Community (%)"
  ) +
  ylab("Population Count") +
  xlab("") +
  ggtitle("Jail Outcomes Depending on Healthcare Access in Jail",
          "End of Scenario at 6 Months, Assuming Shelter in Place in Community Only"
  )

ggp
ggsave(filename = "figImprovedDetection.png",
       plot = ggp,
       device = "png",
       path = graph_dir,
       width = 8, height = 4,
       dpi = "retina")
ggsave(filename = "figImprovedDetection.pdf",
       plot = ggp,
       device = "pdf",
       path = graph_dir,
       width = 8, height = 4,
       dpi = "retina")