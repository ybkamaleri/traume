Sys.sleep(5)
app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(sidebarMain = "tab_rapport")
app$setInputs(sidebarItemExpanded = character(0))
app$setInputs(tidsrom_in = c("201-08-30", "2018-08-25"))
app$setInputs(tidsrom_in = c("2014-08-30", "2018-08-25"))
# Input 'tabAT_rows_current' was set, but doesn't have an input binding.
# Input 'tabAT_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(analyse_level_in = "2")
# Input 'tabAT_rows_current' was set, but doesn't have an input binding.
# Input 'tabAT_rows_all' was set, but doesn't have an input binding.
app$setInputs(health_level_in = "Helse Sør-Øst")
# Input 'tabAT_rows_current' was set, but doesn't have an input binding.
# Input 'tabAT_rows_all' was set, but doesn't have an input binding.
app$setInputs(tidsrom_in = c("201-08-30", "2018-08-25"))
app$setInputs(tidsrom_in = c("2010-08-30", "2018-08-25"))
app$setInputs(sidebarItemExpanded = "Filterforalder:")
app$setInputs(alder_in = c(20, 100))
app$snapshot()
