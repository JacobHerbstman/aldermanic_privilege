STAMP = ../output/.upstream_spatial_red_team_complete

all: $(STAMP)

$(STAMP): run_upstream_spatial_red_team_audit.R
	Rscript run_upstream_spatial_red_team_audit.R
	touch $(STAMP)

	@true
