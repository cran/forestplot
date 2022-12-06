## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, 
                      fig.height = 3)

## -----------------------------------------------------------------------------
library(forestplot)
library(dplyr)

## ---- fig.height=4, fig.width=8, message=FALSE--------------------------------
# Cochrane data from the 'rmeta'-package
base_data <- tibble::tibble(mean  = c(0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017),
                            lower = c(0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365),
                            upper = c(0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831),
                            study = c("Auckland", "Block", "Doran", "Gamsu",
                                      "Morrison", "Papageorgiou", "Tauesch"),
                            deaths_steroid = c("36", "1", "4", "14", "3", "1", "8"),
                            deaths_placebo = c("60", "5", "11", "20", "7", "7", "10"),
                            OR = c("0.58", "0.16", "0.25", "0.70", "0.35", "0.14", "1.02"))

base_data |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             clip = c(0.1, 2.5),
             xlog = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue") |> 
  fp_add_header(study = c("", "Study"),
                deaths_steroid = c("Deaths", "(steroid)"),
                deaths_placebo = c("Deaths", "(placebo)"),
                OR = c("", "OR")) |>
  fp_append_row(mean  = 0.531,
                lower = 0.386,
                upper = 0.731,
                study = "Summary",
                OR = "0.53",
                is.summary = TRUE) |> 
  fp_set_zebra_style("#EFEFEF")

## ---- fig.height=4, fig.width=8, message=FALSE--------------------------------
base_data |> 
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR), 
             clip = c(0.1, 2.5), 
             xlog = TRUE) |> 
  fp_add_lines() |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue",
               align = "lrrr",
               hrz_lines = "#999999") |> 
  fp_add_header(study = c("", "Study"),
                deaths_steroid = c("Deaths", "(steroid)") |> 
                  fp_align_center(),
                deaths_placebo = c("Deaths", "(placebo)") |> 
                  fp_align_center(),
                OR = c("", fp_align_center("OR"))) |>
  fp_append_row(mean  = 0.531,
                lower = 0.386,
                upper = 0.731,
                study = "Summary",
                OR = "0.53",
                is.summary = TRUE)

## ---- fig.height=4, fig.width=8, message=FALSE--------------------------------
base_data |> 
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR), 
             clip = c(0.1, 2.5), 
             xlog = TRUE) |> 
  fp_add_lines(h_3 = gpar(lty = 2), 
               h_11 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue",
               align = "lrrr",
               hrz_lines = "#999999") |> 
  fp_add_header(study = c("", "Study"),
                deaths_steroid = c("Deaths", "(steroid)") |> 
                  fp_align_center(),
                deaths_placebo = c("Deaths", "(placebo)") |> 
                  fp_align_center(),
                OR = c("", fp_align_center("OR"))) |>
  fp_append_row(mean  = 0.531,
                lower = 0.386,
                upper = 0.731,
                study = "Summary",
                OR = "0.53",
                is.summary = TRUE)

## ---- fig.height=4, fig.width=8, message=FALSE--------------------------------
base_data |> 
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR), 
             clip = c(0.1, 2.5), 
             vertices = TRUE,
             xlog = TRUE) |> 
  fp_add_lines(h_3 = gpar(lty = 2), 
               h_11 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue",
               align = "lrrr",
               hrz_lines = "#999999")  |> 
  fp_add_header(study = c("", "Study"),
                deaths_steroid = c("Deaths", "(steroid)") |> 
                  fp_align_center(),
                deaths_placebo = c("Deaths", "(placebo)") |> 
                  fp_align_center(),
                OR = c("", fp_align_center("OR"))) |>
  fp_append_row(mean  = 0.531,
                lower = 0.386,
                upper = 0.731,
                study = "Summary",
                OR = "0.53",
                is.summary = TRUE)

## -----------------------------------------------------------------------------
base_data |> 
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR), 
             clip = c(0.1, 2.5), 
             vertices = TRUE,
             xlog = TRUE) |> 
  fp_add_lines(h_3 = gpar(lty = 2), 
               h_11 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue",
               hrz_lines = "#999999") |> 
  fp_add_header(study = c("", "Study"),
                deaths_steroid = c("Deaths", "(steroid)"),
                deaths_placebo = c("Deaths", "(placebo)"),
                OR = c("", "OR")) |>
  fp_append_row(mean  = 0.531,
                lower = 0.386,
                upper = 0.731,
                study = "Summary",
                OR = "0.53",
                is.summary = TRUE) |> 
  fp_decorate_graph(box = gpar(lty = 2, col = "lightgray"),
                    graph.pos = 4) |> 
  fp_set_zebra_style("#f9f9f9")

## -----------------------------------------------------------------------------
data(dfHRQoL)

dfHRQoL |> 
  filter(group == "Sweden") |> 
  mutate(est = sprintf("%.2f", mean), .after = labeltext) |> 
  forestplot(labeltext = c(labeltext, est), 
             xlab = "EQ-5D index") |> 
  fp_add_header(est = expression(beta)) |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue")

## -----------------------------------------------------------------------------
# You can set directly the font to desired value, the next three lines are just for handling MacOs on CRAN
font <- "mono"
if (grepl("Ubuntu", Sys.info()["version"])) {
  font <- "HersheyGothicEnglish"
}
dfHRQoL |> 
  filter(group == "Sweden") |> 
  mutate(est = sprintf("%.2f", mean), .after = labeltext) |> 
  forestplot(labeltext = c(labeltext, est), 
             xlab = "EQ-5D index") |> 
  fp_add_header(est = "Est.") |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue",
               txt_gp = fpTxtGp(label = gpar(fontfamily = font)))

## -----------------------------------------------------------------------------
dfHRQoL |> 
  filter(group == "Sweden") |> 
  mutate(est = sprintf("%.2f", mean), .after = labeltext) |> 
  forestplot(labeltext = c(labeltext, est), 
             xlab = "EQ-5D index") |> 
  fp_add_header(est = "Est.") |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue",
               txt_gp = fpTxtGp(label = list(gpar(fontfamily = font),
                                             gpar(fontfamily = "",
                                                  col = "#660000")),
                                ticks = gpar(fontfamily = "", cex = 1),
                                xlab  = gpar(fontfamily = font, cex = 1.5)))

## -----------------------------------------------------------------------------
dfHRQoL |> 
  filter(group == "Sweden") |> 
  mutate(est = sprintf("%.2f", mean), .after = labeltext) |> 
  forestplot(labeltext = c(labeltext, est), 
             clip = c(-.1, Inf),
             xlab = "EQ-5D index") |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue")

## -----------------------------------------------------------------------------
dfHRQoL |> 
  filter(group == "Sweden") |> 
  mutate(est = sprintf("%.2f", mean), .after = labeltext) |> 
  forestplot(labeltext = c(labeltext, est), 
             boxsize = 0.2,
             clip = c(-.1, Inf),
             xlab = "EQ-5D index") |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue")

## ----fig.width=10, fig.height=4-----------------------------------------------
fp_sweden <- dfHRQoL |> 
  filter(group == "Sweden") |> 
  mutate(est = sprintf("%.2f", mean), .after = labeltext) |> 
  forestplot(labeltext = c(labeltext, est), 
             title = "Sweden",
             clip = c(-.1, Inf),
             xlab = "EQ-5D index",
             new_page = FALSE)

fp_denmark <- dfHRQoL |> 
  filter(group == "Denmark") |> 
  mutate(est = sprintf("%.2f", mean), .after = labeltext) |> 
  forestplot(labeltext = c(labeltext, est), 
             title = "Denmark",
             clip = c(-.1, Inf),
             xlab = "EQ-5D index",
             new_page = FALSE)

library(grid)
grid.newpage()
borderWidth <- unit(4, "pt")
width <- unit(convertX(unit(1, "npc") - borderWidth, unitTo = "npc", valueOnly = TRUE)/2, "npc")
pushViewport(viewport(layout = grid.layout(nrow = 1, 
                                           ncol = 3, 
                                           widths = unit.c(width,
                                                           borderWidth,
                                                           width))
                      )
             )
pushViewport(viewport(layout.pos.row = 1,
                      layout.pos.col = 1))
fp_sweden |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue")
upViewport()
pushViewport(viewport(layout.pos.row = 1,
                      layout.pos.col = 2))
grid.rect(gp = gpar(fill = "#dddddd", col = "#eeeeee"))
upViewport()
pushViewport(viewport(layout.pos.row = 1,
                      layout.pos.col = 3))
fp_denmark |> 
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue")
upViewport(2)

## -----------------------------------------------------------------------------
dfHRQoL |>
  group_by(group) |>
  forestplot(clip = c(-.1, 0.075),
             ci.vertices = TRUE,
             ci.vertices.height = 0.05,
             boxsize = .1,
             xlab = "EQ-5D index") |> 
  fp_add_lines("steelblue") |> 
  fp_add_header("Variable") |> 
  fp_set_style(box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555")),
               default = gpar(vertices = TRUE))

## -----------------------------------------------------------------------------
dfHRQoL |>
  group_by(group) |>
  forestplot(fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .25, # We set the box size to better visualize the type
             line.margin = .1, # We need to add this to avoid crowding
             clip = c(-.125, 0.075),
             xlab = "EQ-5D index") |> 
  fp_set_style(box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555")),
               default = gpar(vertices = TRUE)) |> 
  fp_set_zebra_style("#F5F9F9")

## -----------------------------------------------------------------------------
dfHRQoL |>
  group_by(group) |>
  forestplot(fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .25, # We set the box size to better visualize the type
             line.margin = .1, # We need to add this to avoid crowding
             clip = c(-.125, 0.075),
             lty.ci = c(1, 2),
             xlab = "EQ-5D index") |> 
  fp_set_style(box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555")),
               default = gpar(vertices = TRUE))

## -----------------------------------------------------------------------------
dfHRQoL |>
  group_by(group) |>
  forestplot(legend = c("Swedes", "Danes"),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .25, # We set the box size to better visualize the type
             line.margin = .1, # We need to add this to avoid crowding
             clip = c(-.125, 0.075),
             xlab = "EQ-5D index") |> 
  fp_set_style(box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555")))

## -----------------------------------------------------------------------------
dfHRQoL |>
  group_by(group) |>
  forestplot(legend = c("Swedes", "Danes"),
             legend_args = fpLegend(pos = list(x = .85, y = 0.25), 
                                    gp = gpar(col = "#CCCCCC", fill = "#F9F9F9")),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .25, # We set the box size to better visualize the type
             line.margin = .1, # We need to add this to avoid crowding
             clip = c(-.125, 0.075),
             xlab = "EQ-5D index") |> 
  fp_set_style(box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555")))

## -----------------------------------------------------------------------------
dfHRQoL |>
  group_by(group) |>
  forestplot(fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .25, # We set the box size to better visualize the type
             line.margin = .1, # We need to add this to avoid crowding
             clip = c(-.125, 0.075),
             xticks = c(-.1, -0.05, 0, .05),
             xlab = "EQ-5D index") |> 
  fp_set_style(box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555")))

## -----------------------------------------------------------------------------
xticks <- seq(from = -.1, to = .05, by = 0.025)
xtlab <- rep(c(TRUE, FALSE), length.out = length(xticks))
attr(xticks, "labels") <- xtlab

dfHRQoL |>
  group_by(group) |>
  forestplot(fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .25, # We set the box size to better visualize the type
             line.margin = .1, # We need to add this to avoid crowding
             clip = c(-.125, 0.075),
             xticks = xticks,
             xlab = "EQ-5D index") |> 
  fp_set_style(box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555")))

## -----------------------------------------------------------------------------
dfHRQoL |>
  group_by(group) |>
  forestplot(fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .25, # We set the box size to better visualize the type
             line.margin = .1, # We need to add this to avoid crowding
             clip = c(-.125, 0.075),
             xticks = c(-.1, -0.05, 0, .05),
             zero = 0,
             xlab = "EQ-5D index") |> 
  fp_set_style(box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555"))) |> 
  fp_decorate_graph(grid = structure(c(-.1, -.05, .05), 
                              gp = gpar(lty = 2, col = "#CCCCFF")))

## -----------------------------------------------------------------------------
dfHRQoL |>
  group_by(group) |>
  forestplot(fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .25, # We set the box size to better visualize the type
             line.margin = .1, # We need to add this to avoid crowding
             clip = c(-.125, 0.075),
             xlab = "EQ-5D index") |> 
  fp_set_style(box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555"))) |> 
  fp_decorate_graph(grid = structure(c(-.1, -.05, .05), 
                                     gp = gpar(lty = 2, col = "#CCCCFF")))

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  grid_arg <- c(-.1, -.05, .05)
#  attr(grid_arg, "gp") <- gpar(lty = 2, col = "#CCCCFF")
#  
#  identical(grid_arg,
#            structure(c(-.1, -.05, .05),
#                      gp = gpar(lty = 2, col = "#CCCCFF")))
#  # Returns TRUE

