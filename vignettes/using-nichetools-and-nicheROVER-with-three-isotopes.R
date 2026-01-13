## ----include = FALSE----------------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )

## ----message = FALSE----------------------------------------------------------
  {
  library(dplyr)
  library(ggplot2)
  library(ggtext)
  library(ggh4x)
  library(nicheROVER) 
  library(nichetools)
  library(patchwork)
  library(purrr)
  library(stringr)
  library(tidyr)
  }

## ----message = FALSE----------------------------------------------------------
  df <- fish %>% 
  janitor::clean_names()

## ----message = FALSE----------------------------------------------------------
  nsample <- 1000

## ----message = FALSE----------------------------------------------------------
fish_par <- df %>% 
  split(.$species) %>% 
  map(~ select(., d13c, d15n, d34s)) %>% 
  map(~ niw.post(nsample = nsample, X = .))

## ----message = FALSE----------------------------------------------------------
df_mu <- extract_mu(fish_par)

## -----------------------------------------------------------------------------
df_mu <- df_mu %>%
  mutate(
    element = case_when(
      isotope == "d15n" ~ "N",
      isotope == "d13c" ~ "C",
      isotope == "d34s" ~ "S",
    ),
    neutron = case_when(
      isotope == "d15n" ~ 15,
      isotope == "d13c" ~ 13,
      isotope == "d34s" ~ 34,
    )
  )

## ----message = FALSE----------------------------------------------------------
df_sigma <- extract_sigma(fish_par, isotope_n = 3)

## ----message = FALSE----------------------------------------------------------
df_sigma_cn <- extract_sigma(fish_par, 
                             data_format = "long", 
                             isotope_n = 3
) %>%
  filter(id != isotope)

## ----warning = FALSE----------------------------------------------------------
posterior_plots <- df_mu %>%
  split(.$isotope) %>%
  imap(
    ~ ggplot(data = ., aes(x = mu_est)) +
      geom_density(aes(fill = sample_name), alpha = 0.5) +
      scale_fill_viridis_d(begin = 0.25, end = 0.75,
                           option = "D", name = "Species") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.title.x =  element_markdown(),
            axis.title.y =  element_markdown(),
            legend.position = "none",
            legend.background = element_blank()
      ) +
      labs(
        x = paste("\u00b5<sub>\U03B4</sub>", "<sub><sup>",
                  unique(.$neutron), "</sup></sub>",
                  "<sub>",unique(.$element), "</sub>", sep = ""),
        y = paste0("p(\u00b5 <sub>\U03B4</sub>","<sub><sup>",
                   unique(.$neutron), "</sub></sup>",
                   "<sub>",unique(.$element),"</sub>",
                   " | X)"), sep = "")
  )

posterior_plots$d15n +
  theme(legend.position = c(0.18, 0.82)) + 
  posterior_plots$d13c + 
  posterior_plots$d34s

## ----message = FALSE----------------------------------------------------------
df_sigma_cn <- df_sigma_cn %>%
  mutate(
    element_id = case_when(
      id == "d15n" ~ "N",
      id == "d13c" ~ "C",
      id == "d34s" ~ "S",
    ),
    neutron_id = case_when(
      id == "d15n" ~ 15,
      id == "d13c" ~ 13,
      id == "d34s" ~ 34,
    ),
    element_iso = case_when(
      isotope == "d15n" ~ "N",
      isotope == "d13c" ~ "C",
      isotope == "d34s" ~ "S",
    ),
    neutron_iso = case_when(
      isotope == "d15n" ~ 15,
      isotope == "d13c" ~ 13,
      isotope == "d34s" ~ 34,
    )
  )

## ----message = FALSE----------------------------------------------------------
sigma_plots <- df_sigma_cn %>%
  group_split(id, isotope) %>%
  imap(
    ~ ggplot(data = ., aes(x = post_sample)) +
      geom_density(aes(fill = sample_name), alpha = 0.5) +
      scale_fill_viridis_d(begin = 0.25, end = 0.75,
                           option = "D", name = "Species") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.title.x =  element_markdown(),
            axis.title.y =  element_markdown(),
            legend.position = "none"
      ) +
      labs(
        x = paste("\U03A3","<sub>\U03B4</sub>",
                  "<sub><sup>", unique(.$neutron_id), "</sub></sup>",
                  "<sub>",unique(.$element_id),"</sub>"," ",
                  "<sub>\U03B4</sub>",
                  "<sub><sup>", unique(.$neutron_iso), "</sub></sup>",
                  "<sub>",unique(.$element_iso),"</sub>", sep = ""),
        y = paste("p(", "\U03A3","<sub>\U03B4</sub>",
                  "<sub><sup>", unique(.$neutron_id), "</sub></sup>",
                  "<sub>",unique(.$element_id),"</sub>"," ",
                  "<sub>\U03B4</sub>",
                  "<sub><sup>", unique(.$neutron_iso), "</sub></sup>",
                  "<sub>",unique(.$element_iso),"</sub>", " | X)", sep = ""),
      )
  )

sigma_plots[[1]] + 
  theme(legend.position = c(0.1, 0.82)) + 
  sigma_plots[[2]] + 
  sigma_plots[[4]]


