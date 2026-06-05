R := Rscript

all: ../output/isbe_campaign_disclosure_dictionary.txt ../output/isbe_committees_raw.tsv ../output/isbe_receipts_full_cycles_raw.tsv

../output/isbe_campaign_disclosure_dictionary.txt: | ../output
	curl -L https://www.elections.il.gov/campaigndisclosuredatafiles/CampaignDisclosureDataDictionary.txt -o "$@"

../output/isbe_committees_raw.tsv: | ../output
	curl -L https://www.elections.il.gov/campaigndisclosuredatafiles/Committees.txt -o "$@"

../output/isbe_receipts_full_cycles_raw.tsv: extract_receipts_full_cycles.R | ../output ../temp
	curl -L --fail --retry 10 --retry-all-errors -C - -o ../temp/Receipts.txt https://www.elections.il.gov/campaigndisclosuredatafiles/Receipts.txt
	$(R) $<

link-inputs: | ../input

include ../../generic.make
