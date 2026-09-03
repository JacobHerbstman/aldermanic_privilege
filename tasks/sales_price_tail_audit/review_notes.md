# Reading notes for 24 sampled transactions

These are evidence-based judgments from the local county sales and property
histories, not deed verification or proposed individual overrides. Prices
below are nominal. IDs identify transaction rows in `output/tail_review.csv`.
The six strata each contribute four randomly selected transactions, with
seed 42. One PIN appears in two sampled sale years, so there are 23 properties.
Histories end in 2022. A later building is not automatically the building sold.

## Excluded by both rules: inside the main RD sample

- **96611396, 2006, PIN 17343110800000:** $3,183,500 for 1,717 sqft.
  Recorded prices are $267,000 in May, $3,183,500 in June, and $290,000
  in July, followed by $430,000 in January 2007. Area remains 1,717 sqft.
  The middle transfer is between a mortgage insurer and an acquisition
  company. Severe unresolved price/transaction anomaly; not grounds to
  invent a corrected consideration. Dates in 2006 are month-precision.
- **97120892, 2015, PIN 14311030400000:** $1,327,132 for 680 sqft.
  Area falls from 1,020 to 680 in the sale year, then rises to 1,598 in
  2021. Earlier recorded sale is $250,000 in 2012. Source descriptions
  deserve investigation; the later area cannot simply be applied backward.
- **97151404, 2018, PIN 17042180090000:** $4.3m for 3,024 sqft, after
  $3.5m in 2015. Recorded area is 4,220 in 2009–2014, 3,024 in 2015–2020,
  and 5,040 in 2021–2022. The repeat price is not an isolated spike;
  fluctuating recorded area is a material concern.
- **7288972, 2021, PIN 14314270280000:** $1.45m for 1,008 sqft, after
  $1.365m in 2017. The area falls from 1,749 to 1,008 in 2018. A renovation
  company bought for $535,000 in 2016 before the 2017 resale. Plausible
  high-value repeat transaction with unresolved building-description changes.

## Excluded by both rules: outside the main RD sample

- **96607325, 2009, PIN 14291140090000:** $1.315m for 1,035 sqft built
  in 1888. The next tax year reports 2,713 sqft built in 2008. Later sales
  are $1.395m in 2013 and $1.455m in 2019. Strong evidence consistent with
  a delayed new-building description, rather than an isolated price typo.
- **98233806, 2014, PIN 14331230470000:** $5.1m for 3,916 sqft. The next
  year reports 5,874 sqft, exactly 50% larger, with the same 1933 build
  year. This case does not satisfy the automated **more than** 50% flag.
  It illustrates why the flag is only an aid to reading, not an error verdict.
- **96553718, 2019, PIN 13364090390000:** $1.059m for 900 sqft built
  in 1895. The next year reports 2,337 sqft built in 2019. The prior
  recorded price was $390,000 in 2018. Pattern consistent with redevelopment
  and a delayed update; transaction-date completion remains unverified.
- **7437232, 2022, PIN 12011150040000:** $1.079m for 696 sqft built
  in 1950. A builder bought for $265,000 through an executor deed in 2021,
  then resold in 2022. No post-2022 property history is available in the
  frozen source. Possible redevelopment/resale; unresolved characteristics.

## Excluded only annually: inside the main RD sample

- **98423706, 2011, PIN 14193040370000:** $685,000 for 720 sqft, versus
  $695,000 in 2006. Area rises to 990 in 2013 and 2,184 in 2018, keeping
  a 1904 construction year. Repeat prices are consistent but the area is
  uncertain. The same PIN's 2006 sale is removed only by the pooled rule.
- **96482817, 2018, PIN 17042170530000:** $1.9m for 1,760 sqft, versus
  $1.78m in 2010, with area unchanged throughout the available history.
  A trustee sale, not automatically non-market. No sharp price contradiction
  found; plausible high-value repeat sale, not verified as error-free.
- **97076039, 2018, PIN 17062210310000:** $705,000 for 680 sqft sold
  to a development company. The source later describes a 5,468-sqft building
  constructed in 2021, class 297. That later record is outside the compact
  single-card/non-prorated area comparison. Consistent with a genuine purchase
  for subsequent redevelopment, not proof that the 2018 area was wrong.
- **98300795, 2018, PIN 13134070340000:** $880,000 for 864 sqft, after
  $437,499 in 2017. Area drops from 1,646 to 864 in the sale year, along
  with rooms and bedrooms. Unresolved source-description change; price
  increase alone does not distinguish renovation from miscoding.

## Excluded only annually: outside the main RD sample

- **97812819, 2017, PIN 14072200170000:** $824,900 for 785 sqft.
  Area is 1,219 through 2016, 785 in 2017–2020, and 1,874 from 2021.
  Subsequent sale is $926,500 in 2022. Price appears less anomalous than
  the changing description, but the chronology is not independently verified.
- **97070259, 2017, PIN 14064040120000:** $1.245m for 1,222 sqft, after
  $532,500 in 2014. Area rises to 3,211 in 2021, with nearly unchanged
  construction year. Possible expansion or delayed measurement update;
  four-year lag is not enough to date the change to the sale.
- **7147749, 2021, PIN 14322260220000:** $3.649m for 3,234 sqft, after
  $3.5m in 2017, when the same area was already recorded. The 2017 deed
  is categorized Other and is excluded from production, but remains useful
  as raw historical evidence. Plausible high-value repeat sale; no decisive
  price contradiction, and deed consideration remains unverified.
- **7218031, 2021, PIN 13232270360000:** $1.15m for 1,044 sqft built
  in 1921. The sole 2022 card reports 3,000 sqft built in 2021. A remodeling
  company bought for $260,000 in 2020 and sold in 2021. Pattern consistent
  with a delayed update following redevelopment.

## Excluded only by the pooled rule: inside the main RD sample

- **96487911, 2006, PIN 14074080160000:** $857,000 for 792 sqft built
  in 1893. The next year reports 2,367 sqft built in 2006. Later sales
  range from $780,000 to $977,500. A likely description mismatch that annual
  trimming would retain; completion at the sale date is not verified.
- **96277622, 2006, PIN 14193040370000:** $695,000 for 720 sqft. See
  the 2011 sale above: nearly equal prices, later increases in recorded
  area, and opposite classifications under the two percentile rules.
- **98316815, 2006, PIN 17072160020000:** $660,000 for 721 sqft. The
  2009 card increases area to 1,313 with the same construction year. A
  2009 Other-deed sale is $344,900, followed by $455,500 in 2010 and
  $645,600 in 2018. Neither the price path nor the area change establishes
  a simple correction to the 2006 sale.
- **97525415, 2008, PIN 17032010250000:** $3.9m for 4,027 sqft, with
  17 rooms, five bedrooms and five full baths. Area rises modestly to
  4,411 in 2016; the 2022 sale is $3.018m. Plausible high-value property,
  though price and deed details are not independently authenticated.

## Excluded only by the pooled rule: outside the main RD sample

- **97984222, 2006, PIN 13241220440000:** $784,000 for 752 sqft built
  in 1901. The next year reports 2,469 sqft built in 2006. Later sale is
  $740,000 in 2014. Another apparent delayed new-building description
  that the annual rule would retain.
- **97203902, 2006, PIN 14333000770000:** $2.9m for 3,008 sqft built
  in 1997, with four bedrooms and three full baths. Those characteristics
  remain unchanged through 2022. No repeat price is available. Expensive
  but without an internal contradiction demonstrated by this review.
- **97650341, 2006, PIN 14324080430000:** $2.8625m for 3,015 sqft,
  with four bedrooms and four full baths. Area is stable; the construction
  year varies slightly in later assessments. No repeat sale. This is not
  enough evidence to label the transaction invalid.
- **97266099, 2008, PIN 14183030260000:** $1.3499m for 1,332 sqft,
  between sales of $1.334m in 2007 and $1.36m in 2017. Area rises to
  4,200 in 2018 with the same construction year. Consistent high prices
  and a late area update point toward description uncertainty, but the
  timing of any actual expansion remains unknown.
