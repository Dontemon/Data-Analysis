# Очистка рабочего пространства ----
while (dev.cur() != 1) {
  dev.off()
}
rm(list=ls())
cat("\014")

library(shiny)
runExample("01_hello")
