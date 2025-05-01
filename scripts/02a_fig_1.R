

# libraries --------------------------------------------------------------------
library(tidyverse)


# predicted values -------------------------------------------------------------

critval <- 1.96 # critical value

# No
newdata_N <- data.frame(
  
  residency = "No",
  years = mean(hgns$years),
  posttraining = mean(hgns$posttraining),
  frequency = mean(hgns$frequency)
  
)


preds <- predict(
  mod_1, 
  newdata = newdata_N, 
  type_pred = "response",
  se.fit = TRUE
)


fit_link <- preds$fit

fit_response <- mod_1$family$linkinv(fit_link)

upr_link <- preds$fit + (critval * preds$se.fit)

lwr_link <- preds$fit - (critval * preds$se.fit)

upr_response <- mod_1$family$linkinv(upr_link)

lwr_response <- mod_1$family$linkinv(lwr_link)

newdata_N <- as.data.frame(
  
  cbind(
    newdata_N,
    fit_response,
    upr_response,
    lwr_response
  )
  
)


# Yes
newdata_Y <- data.frame(
  
  residency = "Yes",
  years = mean(hgns$years),
  posttraining = mean(hgns$posttraining),
  frequency = mean(hgns$frequency)
  
)


preds <- predict(
  mod_1, 
  newdata = newdata_Y, 
  type_pred = "response",
  se.fit = TRUE
)


fit_link <- preds$fit

fit_response <- mod_1$family$linkinv(fit_link)

upr_link <- preds$fit + (critval * preds$se.fit)

lwr_link <- preds$fit - (critval * preds$se.fit)

upr_response <- mod_1$family$linkinv(upr_link)

lwr_response <- mod_1$family$linkinv(lwr_link)

newdata_Y <- as.data.frame(
  
  cbind(
    newdata_Y,
    fit_response,
    upr_response,
    lwr_response
  )
  
)


# rbind
newdata <- rbind(newdata_N, newdata_Y)


# plot -------------------------------------------------------------------------
newdata %>%
  
  ggplot(
    
    aes(
      x = residency, 
      y = fit_response,
      fill = residency
    )
    
  ) +

  geom_errorbar(
    aes(
      ymin = lwr_response,
      ymax = upr_response
    ),
    width = 0.025,
    show.legend = FALSE
  ) +
  
  geom_point(
    pch = 21,
    color = "black",
    size = 3
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  ggtitle("") +
  
  xlab("Performed HGNS \nsurgery during residency?") +
  
  ylab("Probability of\ndissection as 1st step") +
  
  labs(fill = "Significant \nResult?") +
  
  theme_bw() +
  
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"), 
    legend.text = element_text(size = 11),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  
  guides(
    color = guide_legend(
      reverse = TRUE
    )
  )

ggsave(
  "out/fig_1.png",
  width = 4,
  height = 3
)
