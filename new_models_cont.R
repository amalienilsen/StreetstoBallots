#models continious HK variable
library(nnet)
library(ggplot2)
library(ggeffects)

#previous abstainers 
test1<- multinom(categorical_outcome ~protestcount*QI1, data = novoterslive1)
test2<- multinom(categorical_outcome ~protestcount*QI1+QA2+education+QB4+female+higher_service+lower_service+working_class+telegram+QL22_6, data = novoterslive1)

stargazer(test1, test2, type="html", out="abstainers")
#plotting

predictions <- ggeffects::ggemmeans(test2, terms = c("protestcount", "QI1"))

predictions %>% ggplot(aes(x = x, y = predicted, colour = group)) +   
  geom_line(aes(colour = group)) +
  xlab("Protest Frequency") +
  ylab("predicted probability of vote choices")+
  facet_wrap(response.level ~ ., scales = "free") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2) 


plot1<-predictions %>%
  ggplot(aes(x = x, y = predicted, colour = group, fill = group, linetype = group)) +  # Map linetype to group (QI1 levels)
  geom_line(size = 1.2) +  # Increase line size for better visibility
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  xlab("Protest Frequency") +
  ylab("Predicted Probability of Vote Choices") +
  ggtitle("") +
  ylim(0,1) +
  theme_minimal(base_size = 15) +  # Use a minimal theme with larger base font size
  theme(
    legend.position = "top",  # Move legend to the top
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold title
    strip.text = element_text(face = "bold"),  # Bold facet labels
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove grid lines
    axis.line = element_line(colour = "black")  # Optional: Add black axis lines for better visibility
  ) +
  facet_wrap(~response.level, scales = "free") +
  scale_linetype_manual(values = c("longdash", "dashed", "dotdash", "solid")) +  # Set different line types for each level of QI1
  labs(
    colour = NULL,  # Remove the legend title for colour
    fill = NULL,    # Remove the legend title for fill
    linetype = NULL # Remove the legend title for linetype
  )

plot1
ggsave("hk_abs.png", plot = plot1, width = 10, height = 8, dpi = 300)


#defections 


defectmod3 <- glm(defection ~ protestcount * QI1,
                  data = combineregimelive1, family=binomial)


defectmod4 <- glm(defection ~ protestcount * QI1 + QA2 + education + QB4 + female + lower_service + higher_service + working_class+telegram+QL22_6, 
    data = combineregimelive1, family="binomial")


stargazer(defectmod3,defectmod4, type="html", out="defect")

library(sjPlot)
plot_1 <- plot_model(defectmod4, type = "pred", 
                     terms = c("protestcount", "QI1"),
                     title = "", axis.title = c("Protest count in district",
                                                "Predicted probabilities vote Opposition"),  ci.lvl = .90)
plot_1+geom_line(size = 0.8) +  # Increase line size for better visibility
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  xlab("Protest Frequency") +
  ylab("Predicted Probability of Defection") +
  ggtitle("") +
  ylim(0,1) +
  theme_minimal(base_size = 15) +  # Use a minimal theme with larger base font size
  theme(
    legend.position = "top",  # Move legend to the top
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold title
    strip.text = element_text(face = "bold"),  # Bold facet labels
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove grid lines
    axis.line = element_line(colour = "black")  # Optional: Add black axis lines for better visibility
  ) +
  scale_linetype_manual(values = c("longdash", "dashed", "dotdash", "solid")) +  # Set different line types for each level of QI1
  labs(
    colour = NULL,  # Remove the legend title for colour
    fill = NULL,    # Remove the legend title for fill
    linetype = NULL # Remove the legend title for linetype
  )





