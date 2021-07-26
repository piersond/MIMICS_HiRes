### plot
my_theme <- theme_bw() + 
  theme(panel.spacing.x=unit(0.5, "lines"),panel.spacing.y=unit(1, "lines")) +
  theme(legend.position="bottom") +
  theme(strip.text = element_text(size = 12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=12)) +
  theme(legend.text=element_text(color='grey20',size=12)) +
  theme(legend.title=element_text(color='grey20',size=12)) +
  theme(panel.background = element_blank()) + 
  theme(strip.background = element_blank()) +
  theme(strip.placement = "outside") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())