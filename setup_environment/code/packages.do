clear

//warning: at one point, gtools had to be compiled manually for M1 Macs
//see https://github.com/mcaceresb/stata-gtools/issues/73#issuecomment-803444445

// install from ssc
 
local PACKAGES distinct rd gtools reghdfe ftools ppmlhdfe parmest estout binscatter geodist ///
listtex egenmore matchit freqindex blindschemes outreg2 maptile spmap coefplot statastates ///
regsave palettes

foreach package in `PACKAGES' {
	capture which `package'
	if _rc==111 ssc install `package'
}

  
file open myfile using "../output/stata_packages.txt", write replace

file write myfile "Installed: `PACKAGES' `LOCAL_PACKAGES'"
file close myfile

maptile_install using "http://files.michaelstepner.com/geo_cbsa2013.zip"

// Install regsave
net install regsave, from(http://fmwww.bc.edu/RePEc/bocode/r) replace

// install modes
net install sg113_2, from(http://www.stata-journal.com/software/sj9-4)

