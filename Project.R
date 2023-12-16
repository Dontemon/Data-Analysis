# Очистка рабочего пространства ----
while (dev.cur() != 1) {
  dev.off()
}
rm(list=ls())
cat("\014")