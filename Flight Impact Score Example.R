library(ggplot2)

df <- data.frame("Time Bin" = c("T", "T-1", "T-2", "T-3", "T-4", "T-5", "T-6", 
                                "T-7", "T-8", "T-9", "T-10", "T-11", "T-12"),
                 "Flight Impact Score" = c(1,1,0.91, 0.82, 0.73, 0.64,0.55,
                                            0.46, 0.37, 0.28, 0.19, 0.1, 0.1))
TB = factor(df$Time.Bin, levels = c("T", "T-1", "T-2", "T-3", "T-4", "T-5", "T-6", 
                                    "T-7", "T-8", "T-9", "T-10", "T-11", "T-12"))
df1 <- data.frame(TB, 
                 "Flight Impact Score" = c(1,1,0.91, 0.82, 0.73, 0.64,0.55,
                                           0.46, 0.37, 0.28, 0.19, 0.1, 0.1))

ggplot(data = df1, aes(x = TB, y = Flight.Impact.Score)) + 
  geom_point(size = 4) +
  labs( y = "Flight Impact Score", 
        x = "Time Bin") + 
  theme_classic(base_size = 18)  
