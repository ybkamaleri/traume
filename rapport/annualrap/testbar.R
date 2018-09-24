
(gg <- ggplot(accMix) +
   geom_col(aes(var, i.n, fill = "Norge"), width = 0.65, position = position_dodge(width = 0.9)) +
   geom_col(aes(var, n, fill = "RHF"),
            width = 0.45, position = position_nudge(x = 0.25)) +
   coord_flip()

)

ymax1 <- transPlot[, max(pros, na.rm = TRUE)]
ymax <- ymax1 + ymax1 * 0.15

(gg1 <- ggplot(transPlot) +
   geom_segment(aes(ref, y = 0, xend = ref, yend = ymax1), linetype = 2) +
   geom_segment(data = data[ref == as.character(7),],
                aes(ref, 0, xend = ref, yend = ymax1), size = 1, color = "white") +
   geom_col(aes(ref, i.pros, fill = "Norge")) +
   geom_col(aes(ref, pros, fill = "HF"), width = 0.35) +
   coord_flip() +
   labs(title = "Helse Vest", x = "") +
   scale_fill_manual(values = c("Norge" = "lightblue", "HF" = "blue")) +
   ## geom_text(aes(ref, ymax, label = gsub(";", "\n", text1)), hjust = 1) +
   ## geom_text(aes(ref, ymax + 8, label = gsub(";", "\n", text2)), hjust = 1) +
   scale_y_continuous(expand = c(0,0), breaks = seq(0, ymax1, 5)) +
   scale_x_discrete(breaks = factor(transPlot$ref), labels = NULL) +
   geom_segment(aes(y = 0, yend = ymax1, x = -Inf, xend = -Inf)) +
   theme(
     legend.position = "bottom",
     axis.line = element_blank()
   )
)



(gg2 <- ggplot(transPlot) +
   geom_segment(aes(ref, y = 0, xend = ref, yend = ymax1), linetype = 2) +
   geom_segment(data = data[ref == as.character(7),],
                aes(ref, 0, xend = ref, yend = ymax1), size = 1, color = "white") +
   geom_col(aes(ref, i.pros, fill = "Norge")) +
   geom_col(aes(ref, pros, fill = "HF"), width = 0.35) +
   coord_flip() +
   labs(title = "Helse Nord", x = "") +
   scale_fill_manual(values = c("Norge" = "lightblue", "HF" = "blue")) +
   ## geom_text(aes(ref, ymax, label = gsub(";", "\n", text1)), hjust = 1) +
   ## geom_text(aes(ref, ymax + 8, label = gsub(";", "\n", text2)), hjust = 1) +
   scale_y_continuous(expand = c(0,0), breaks = seq(0, ymax1, 5)) +
   scale_x_discrete(breaks = factor(transPlot$ref), labels = transPlot$navn) +
   geom_segment(aes(y = 0, yend = ymax1, x = -Inf, xend = -Inf)) +
   theme(
     legend.position = "none",
     axis.line = element_blank()
   )
)

(gg3 <- ggplot(transPlot) +
   ## geom_segment(aes(ref, y = 0, xend = ref, yend = ymax1), linetype = 2) +
   ## geom_segment(data = data[ref == as.character(7),],
   ##              aes(ref, 0, xend = ref, yend = ymax1), size = 1, color = "white") +
   ## geom_col(aes(ref, i.pros, fill = "Norge")) +
   ## geom_col(aes(ref, pros, fill = "HF"), width = 0.35) +
   coord_flip() +
   ## labs(title = "Helse Nord", x = "") +
   ## scale_fill_manual(values = c("Norge" = "lightblue", "HF" = "blue")) +
   geom_text(aes(ref, 0, label = gsub(";", "\n", text1)), hjust = 1) +
   geom_text(aes(ref, 1, label = gsub(";", "\n", text2)), hjust = 1) +
   scale_y_discrete(breaks = factor(1:2), labels = NULL) +
   scale_x_discrete(breaks = factor(transPlot$ref), labels = NULL) +
   ## geom_segment(aes(y = 0, yend = ymax1, x = -Inf, xend = -Inf)) +
   theme(
     legend.position = "none",
     axis.line = element_blank()
   )
)


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(gg1)

p3 <- grid.arrange(arrangeGrob(gg2 + theme(legend.position="none"),
                               gg1 + theme(legend.position="none"),
                               gg3,
                               nrow=1),
                   mylegend, nrow=2,heights=c(20, 2))

cowplot::plot_grid(gg2, gg1, gg3, align = "h", nrow = 1, rel_widths = c(1, 1, 1/2))

fig1a <- p3
title <- "Test_figure"
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL
