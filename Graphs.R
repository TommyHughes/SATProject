pl <- ggplot2::ggplot(filter(masterScores, Group == "Economically Disadvantaged"), aes(x = Math)) +
  geom_freqpoly() +
  facet_wrap(~ Year)
print(pl)

pll <- ggplot2::ggplot(filter(masterScores, Group == "Not Economically Disadvantaged"), aes(x = Math)) +
  geom_freqpoly() +
  facet_wrap(~ Year)
print(pll)

plll <- ggplot2::ggplot(filter(masterScores, 
                               Group == "Economically Disadvantaged" | Group == "Not Economically Disadvantaged"),
                        aes(x = Group, y = Math, group = Group)) +
  geom_boxplot() +
  facet_wrap(~ Year) +
  coord_flip()
print(plll)