# Clean residential sales

This task produces the canonical transaction-clean Chicago residential-sales file for 2006--2022. It retains classes 202--211, 234, 278, and 295; prices above $10,000; deeds that do not explicitly identify a land-only sale; single-PIN sales; and Warranty or Trustee deeds. Cook County's three published sale-quality flags must all be false. Buyer and seller names are not required, but records with the same usable party name on both sides after punctuation-insensitive normalization are excluded.

The output remains a transaction file. Property-structure restrictions, including the single-building and non-prorated-PIN requirements, are imposed when the sales are joined to sale-year improvement records.
