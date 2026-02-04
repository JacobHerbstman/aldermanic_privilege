#!/bin/bash
# Test multiple uncertainty index specifications and compare key aldermen
# This script runs several variants and extracts scores for high-leverage aldermen

cd /Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_alderman_uncertainty_index/code

KEY_ALDERMEN="Brian Hopkins|Desmon Yancy|Ed Smith|Michael Chandler|Isaac Carothers|Emma Mitts|Walter Burnett|Jeanette Taylor|Howard Brookins|Latasha Thomas|Patrick Daley|Byron Sigcho"

echo "=== BASELINE: OLD STRICTNESS SCORES (Month FEs) ==="
grep -E "$KEY_ALDERMEN" ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | awk -F',' '{printf "%-25s %8.3f\n", $1, $2}' | sort -k2 -n
echo ""

# Spec 1: Current default (CA FE, two-stage, all covariates)
echo "=== SPEC 1: CA_FE + 2stage + all covariates (current) ==="
grep -E "$KEY_ALDERMEN" ../output/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeTRUE_2stage.csv | awk -F',' '{printf "%-25s %8.3f\n", $1, $7}' | sort -k2 -n
echo ""

# Spec 2: No CA FE (just month FE like the old method)
echo "Running Spec 2: NO CA_FE + 2stage..."
Rscript create_uncertainty_index.R --permit_type_fe=FALSE --review_type_fe=TRUE --include_porch=TRUE --ca_fe=FALSE --two_stage=TRUE 2>&1 | tail -5

echo "=== SPEC 2: NO CA_FE + 2stage ==="
grep -E "$KEY_ALDERMEN" ../output/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage.csv | awk -F',' '{printf "%-25s %8.3f\n", $1, $7}' | sort -k2 -n
echo ""

# Spec 3: No CA FE, no two-stage (simple mean residual)
echo "Running Spec 3: NO CA_FE + no 2stage..."
Rscript create_uncertainty_index.R --permit_type_fe=FALSE --review_type_fe=TRUE --include_porch=TRUE --ca_fe=FALSE --two_stage=FALSE 2>&1 | tail -5

echo "=== SPEC 3: NO CA_FE + NO 2stage ==="
grep -E "$KEY_ALDERMEN" ../output/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE.csv | awk -F',' '{printf "%-25s %8.3f\n", $1, $4}' | sort -k2 -n
echo ""

# Spec 4: With review type FE but no permit type
echo "Running Spec 4: With permit_type FE..."
Rscript create_uncertainty_index.R --permit_type_fe=TRUE --review_type_fe=TRUE --include_porch=TRUE --ca_fe=FALSE --two_stage=TRUE 2>&1 | tail -5

echo "=== SPEC 4: permit_type_fe=TRUE + NO CA_FE + 2stage ==="
grep -E "$KEY_ALDERMEN" ../output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage.csv | awk -F',' '{printf "%-25s %8.3f\n", $1, $7}' | sort -k2 -n
echo ""

echo "=== COMPARISON COMPLETE ==="
