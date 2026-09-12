# Construction density

Combine the selected residential and commercial measurements. Calculate FAR as building area divided by land area, and DUPAC as dwelling units times 43,560 divided by land area. Apply the recorded building-type decisions here.

Run `make` from `tasks/construction_density/code/`. Its Makefile lists each input and its producer.

- [calculate_construction_density.R](code/calculate_construction_density.R): `new_construction_measurements.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
