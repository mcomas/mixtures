library(WDI)

ind.gdp = list(
  "NY.GDP.MKTP.CD" = "GDP (current US$)")

ll = WDIsearch(string="\\(% of GDP\\)")
ind.gdp = c(ind.gdp, ll[,2])
names(ind.gdp) = c("NY.GDP.MKTP.CD", ll[,1])

ind.co2.emission = list(
  "EN.ATM.CO2E.KT" = "CO2 emissions (kt)",
  "EN.ATM.CO2E.CP.KT" = "CO2 emissions from cement production (thousand metric tons)",
  "EN.ATM.CO2E.EG.ZS" = "CO2 intensity (kg per kg of oil equivalent energy use)",
  "EN.ATM.CO2E.FF.KT" = "CO2 emissions from fossil-fuels, total (thousand metric tons)",
  "EN.ATM.CO2E.GF.KT" = "CO2 emissions from gaseous fuel consumption (kt) ",
  "EN.ATM.CO2E.GL.KT" = "CO2 emissions from gas flaring (thousand metric tons)",
  "EN.ATM.CO2E.LF.KT" = "CO2 emissions from liquid fuel consumption (kt) ",
  "EN.ATM.CO2E.SF.KT" = "CO2 emissions from solid fuel consumption (kt) ",
  "EN.ATM.CO2E.PC" = "CO2 emissions (metric tons per capita)" )

ind.energy.use = list(
  "EG.USE.COMM.KT.OE" = "Energy use (kt of oil equivalent)",
  "EG.USE.PCAP.KG.OE" = "Energy use (kg of oil equivalent per capita)" )

ind.merchandise.imports = list(
  "TM.VAL.MRCH.CD.WT" = "Merchandise imports (current US$)",
  "TM.VAL.AGRI.ZS.UN" = "Agricultural raw materials imports (% of merchandise imports)",
  "TM.VAL.FOOD.ZS.UN" = "Food imports (% of merchandise imports)",
  "TM.VAL.FUEL.ZS.UN" = "Fuel imports (% of merchandise imports)",
  "TM.VAL.MANF.ZS.UN" = "Manufactures imports (% of merchandise imports)",
  "TM.VAL.MMTL.ZS.UN" = "Ores and metals imports (% of merchandise imports)",
  "TM.VAL.MRCH.R1.ZS" = "Merchandise imports from developing economies in East Asia & Pacific (% of total merchandise imports)",
  "TM.VAL.MRCH.R2.ZS" = "Merchandise imports from developing economies in Europe & Central Asia (% of total merchandise imports)",
  "TM.VAL.MRCH.R3.ZS" = "Merchandise imports from developing economies in Latin America & the Caribbean (% of total merchandise imports)",
  "TM.VAL.MRCH.R4.ZS" = "Merchandise imports from developing economies in Middle East & North Africa (% of total merchandise imports)",
  "TM.VAL.MRCH.R5.ZS" = "Merchandise imports from developing economies in South Asia (% of total merchandise imports)",
  "TM.VAL.MRCH.R6.ZS" = "Merchandise imports from developing economies in Sub-Saharan Africa (% of total merchandise imports)",
  "TM.VAL.MRCH.RS.ZS" = "Merchandise imports by the reporting economy, residual (% of total merchandise imports)")

ind.merchandise.exports = list(
  "TX.VAL.MRCH.CD.WT" = "Merchandise exports (current US$)",
  "TX.VAL.AGRI.ZS.UN" = "Agricultural raw materials exports (% of merchandise exports)",
  "TX.VAL.FOOD.ZS.UN" = "Food exports (% of merchandise exports)",
  "TX.VAL.FUEL.ZS.UN" = "Fuel exports (% of merchandise exports)",
  "TX.VAL.MANF.ZS.UN" = "Manufactures exports (% of merchandise exports)",
  "TX.VAL.MMTL.ZS.UN" = "Ores and metals exports (% of merchandise exports)",
  "TX.VAL.MRCH.R1.ZS" = "Merchandise exports from developing economies in East Asia & Pacific (% of total merchandise exports)",
  "TX.VAL.MRCH.R2.ZS" = "Merchandise exports from developing economies in Europe & Central Asia (% of total merchandise exports)",
  "TX.VAL.MRCH.R3.ZS" = "Merchandise exports from developing economies in Latin America & the Caribbean (% of total merchandise exports)",
  "TX.VAL.MRCH.R4.ZS" = "Merchandise exports from developing economies in Middle East & North Africa (% of total merchandise exports)",
  "TX.VAL.MRCH.R5.ZS" = "Merchandise exports from developing economies in South Asia (% of total merchandise exports)",
  "TX.VAL.MRCH.R6.ZS" = "Merchandise exports from developing economies in Sub-Saharan Africa (% of total merchandise exports)",
  "TX.VAL.MRCH.RS.ZS" = "Merchandise exports by the reporting economy, residual (% of total merchandise exports)")

indicators = c(ind.gdp, ind.co2.emission, ind.energy.use, ind.merchandise.imports, ind.merchandise.exports)
dat = WDI(indicator=names(indicators), country='all', start=1960, end=2012)

save(dat, indicators, file='data/wdi_data.RData')
