comparisonScores <- masterScores %>% 
  filter(Group == "Economically Disadvantaged" | Group == "Not Economically Disadvantaged") %>%
  select(Group, DistName, Year, Math)

comparisonScores <- tidyr::drop_na(comparisonScores)

comparisonScores$Group <- ifelse(comparisonScores$Group == "Economically Disadvantaged", 0, 1)

split <- caTools::sample.split(comparisonScores$Group, SplitRatio = 0.75)
final.train <- subset(comparisonScores, split == T)
final.test <- subset(comparisonScores, split == F)
final.logistic.model <- glm(Group ~ Math + Year, family = binomial(link = 'logit'), data = final.train)
fitted.probabilites <- predict(final.logistic.model, final.test, type = 'response')
fitted.results <- ifelse(fitted.probabilites>0.5,1,0)
misClassError <- mean(fitted.results != final.test$Group)
print(1 - misClassError)
table(final.test$Group, fitted.probabilites>0.5)