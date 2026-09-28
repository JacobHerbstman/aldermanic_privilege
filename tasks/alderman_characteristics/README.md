# Alderman characteristics and stated positions (hand research)

Three committed source tables, researched from public web sources in September 2026 (Wikipedia, Ballotpedia, the
Chicago Sun-Times and Tribune, WBEZ, WTTW, Block Club Chicago, the Chicago Reader, ward and City sites, obituaries,
Department of Justice releases). Each row lists the URLs it rests on, a confidence rating and notes; a field is NA
when no source documents it. Research was done by AI research agents under written coding rules; no field was
inferred from a surname, a photograph or ward demographics. Consumers link to these files directly.

- `sources/alderman_characteristics.csv`: one row for each of the 113 aldermen in the 2006–2022 stringency index:
  gender, race or ethnicity, birth year, true first year on the council, entry route (elected or appointed), pre-council
  occupation and category, lawyer, property-tax or real-estate work before or during council service, prior elected
  office, family political ties, Progressive Reform Caucus membership (the caucus formed in 2013), indictment or
  conviction for conduct related to office, and how they left office (still serving at the end of 2022 counts as
  still serving). Race is documented for 85 aldermen, from self-description, news descriptions, council Black or
  Latino caucus membership or documented ancestry; the other 28 are NA. Caucus membership was checked against the
  caucus's published roster.
- `sources/prerogative_positions.csv`: 57 public statements by 55 of those aldermen on aldermanic prerogative, each
  with its date, source type (candidate questionnaire, news statement, vote or sponsorship), the question asked, a
  direct quote and a coded position (keep, reform or limit, abolish, mixed, unclear). Nearly all are from the 2019
  Sun-Times candidate questionnaire and 2019–2023 news coverage; the 2023 WTTW voters' guide could be read only
  through search summaries, so those rows are medium confidence.
- `sources/community_zoning_processes.csv`: 16 aldermen's documented community-driven zoning processes (zoning
  advisory councils, published community-driven zoning and development processes, required community meetings or
  votes), with type, name, start and end year, a description and a quote. Coverage is partial: 13 wards were not yet
  searched, and nothing before 2011 was found. Three rows are aldermen who took office in 2023, after the index ends;
  two names were harmonized to the spellings used elsewhere in the project (Rossana Rodriguez-Sanchez, Walter
  Burnett, Jr.).
