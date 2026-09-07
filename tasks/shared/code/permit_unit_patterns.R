# A dwelling count may precede a separate height, e.g. "(7) 3-STORY ROWHOMES".
# Counts of townhouse buildings are also not dwelling counts.
# A height without a dwelling count ("3-STORY ROWHOMES") is not a unit count.
attached_house_count_pattern <- paste0(
  "(?:\\b[0-9]{1,4}|\\([0-9]{1,4}\\))\\s*",
  "(?:(?:[0-9]+(?:\\.[0-9]+)?|ONE|TWO|THREE|FOUR|FIVE|SIX)",
  "[- ]*STOR(?:Y|IES)\\s+)?",
  "(?:ROW[- ]?HOMES?|ROW[- ]?HOUSES?|TOWN[- ]?HOMES?|TOWN[- ]?HOUSES?)\\b(?!\\s+BUILDINGS?\\b)"
)
