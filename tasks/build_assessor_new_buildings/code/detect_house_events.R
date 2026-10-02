# setwd("tasks/build_assessor_new_buildings/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

lead_years <- 5     # a new building's year built may precede the year its record is first assessed by up to five years
rebuild_jump <- 10  # a year built rising by at least ten years on one record card is a rebuilt house

# Residential record cards (classes 2xx, 1-6 units): a card first assessed in 2000 or later with a recent year built,
# or a card whose year built jumps to a recent year.
cards <- as.data.table(read_parquet("../input/residential_assessor_history.parquet",
  col_select = c("pin", "tax_year", "card_num", "class", "year_built", "num_apartments")))
setorder(cards, pin, card_num, tax_year)
cards[, first_year := min(tax_year), by = .(pin, card_num)]
cards[, previous_year_built := shift(year_built), by = .(pin, card_num)]
cards[, recent := !is.na(year_built) & year_built >= tax_year - lead_years & year_built <= tax_year + 1]
new_card <- cards[tax_year == first_year & first_year >= 2000 & recent, .(pin, card_num, tax_year, class, year_built, num_apartments, rule = "new card")]
rebuild <- cards[!is.na(previous_year_built) & year_built >= previous_year_built + rebuild_jump & recent,
  .(pin, card_num, tax_year, class, year_built, num_apartments, rule = "rebuild")]
rebuild <- unique(rebuild, by = c("pin", "card_num", "year_built"))
houses <- rbind(new_card, rebuild)[, units := fifelse(class %in% c("211", "212"), fcoalesce(as.numeric(num_apartments), 2), 1)][order(pin, card_num, tax_year)]
SaveData(houses, c("pin", "card_num", "tax_year"), "../output/house_events.csv")
