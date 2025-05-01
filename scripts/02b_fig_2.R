
# libraries --------------------------------------------------------------------
library(tidyverse)
library(ggsignif)
library(ggbeeswarm)


# predicted values -------------------------------------------------------------

critval <- 1.96 # critical value

# No
newdata_N <- data.frame(
  
  dissection = "0",
  resolved = "Yes",
  residency = "No",
  years = mean(hgns$years),
  posttraining = seq(1, 5, 0.1),
  frequency = mean(hgns$frequency)
  
)


preds <- predict(
  mod_2, 
  newdata = newdata_N, 
  type_pred = "response",
  se.fit = TRUE
)


fit_link <- preds$fit

fit_response <- mod_2$family$linkinv(fit_link)

upr_link <- preds$fit + (critval * preds$se.fit)

lwr_link <- preds$fit - (critval * preds$se.fit)

upr_response <- mod_2$family$linkinv(upr_link)

lwr_response <- mod_2$family$linkinv(lwr_link)

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
  
  dissection = "1",
  resolved = "Yes",
  residency = "Yes",
  years = mean(hgns$years),
  posttraining = seq(1, 5, 0.1),
  frequency = mean(hgns$frequency)
  
)


preds <- predict(
  mod_2, 
  newdata = newdata_Y, 
  type_pred = "response",
  se.fit = TRUE
)


fit_link <- preds$fit

fit_response <- mod_2$family$linkinv(fit_link)

upr_link <- preds$fit + (critval * preds$se.fit)

lwr_link <- preds$fit - (critval * preds$se.fit)

upr_response <- mod_2$family$linkinv(upr_link)

lwr_response <- mod_2$family$linkinv(lwr_link)

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
      x = posttraining, 
      y = fit_response,
      color = dissection,
      linetype = dissection,
      fill = dissection
    )
    
  ) +
  
  geom_ribbon(
    aes(
      ymin = lwr_response,
      ymax = upr_response
    ),
    alpha = 0.075,
    color = NA,
    show.legend = FALSE
  ) +
  
  geom_line(
    size = 1
  ) +
  
  scale_y_continuous(
    limits = c(0, 1.075),
    breaks = seq(0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  ggtitle("") +
  
  xlab("Number of HGNS surgeries \nperformed after residency") +
  
  ylab("Probability of needing >30 min \nto revise cuff placement") +
  
  labs(
    color = "Dissection\nas 1st step?",
    linetype = "Dissection\nas 1st step?"
    ) +
  
  theme_bw() +
  
  theme(
    plot.title = element_text(size = 17),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text = element_text(size = 17),
    axis.text.x = element_text(angle = 25, hjust = 1),
    axis.title = element_text(size = 17, face = "bold"),
    legend.title = element_text(size = 17, face = "bold"), 
    legend.text = element_text(size = 17),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  
 guides(
   color = "none",
   linetype = guide_legend(
     reverse = TRUE, 
     override.aes = list(color = c("#00BFC4", "#F8766D"))
   )
 ) +
  
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c(
      "<20", 
      "20-50",
      "50-100",
      "100-200",
      ">200"
      ) 
    ) +
  
 scale_linetype_manual(
   values = c("1" = "solid", "0" = "dotdash"),
   labels = c("1" = "Yes", "0" = "No")
 )


ggsave(
  "out/fig_2.png",
  height = 4.5,
  width = 6.5
)
