SHELL := bash
include ../../shell_functions.make

.PHONY: all clean runorder list-tasks check-cycles

all: ../output/task_flow.png

# Generate the DOT graph from Makefile symlink rules
../output/graph.dot: graphmaker.sh | ../output
	bash graphmaker.sh

# Render the graph as a PNG image using graphviz
../output/task_flow.png: ../output/graph.dot | ../output
	dot -Grankdir=LR -Tpng ../output/graph.dot -o ../output/task_flow.png
	@echo "Generated ../output/task_flow.png"

# Also generate SVG for better quality
../output/task_flow.svg: ../output/graph.dot | ../output
	dot -Grankdir=LR -Tsvg ../output/graph.dot -o ../output/task_flow.svg
	@echo "Generated ../output/task_flow.svg"

# Print topologically-sorted make commands for running the entire pipeline
runorder: ../output/graph.dot
	@echo "# Topologically sorted build order:"
	@echo "# Run these commands in order to build the entire pipeline"
	@echo ""
	@grep '\->' ../output/graph.dot | \
		sed 's/^[[:space:]]*//' | \
		sed 's/ *-> */ /' | \
		grep -v 'data_raw' | \
		tsort 2>/dev/null | \
		sed 's/.*/make -C ..\/..\/&\/code/'

# List all tasks discovered in the dependency graph
list-tasks: ../output/graph.dot
	@echo "# Tasks discovered in dependency graph:"
	@grep ' -> ' ../output/graph.dot | \
		sed 's/^[[:space:]]*//' | \
		sed 's/ -> /\n/' | \
		sed 's/;$$//' | \
		sort | uniq | \
		grep -v 'data_raw'

# Check for circular dependencies in the graph
check-cycles: ../output/graph.dot
	@echo "Checking for circular dependencies..."
	@if sed '1,/^$$/d' ../output/graph.dot | \
		grep ' -> ' | \
		sed 's/^[[:space:]]*//' | \
		sed 's/ -> / /' | \
		sed 's/;$$//' | \
		tsort > /dev/null 2>&1; then \
		echo "✓ No circular dependencies found"; \
	else \
		echo "✗ ERROR: Circular dependencies detected!"; \
		sed '1,/^$$/d' ../output/graph.dot | \
			grep ' -> ' | \
			sed 's/^[[:space:]]*//' | \
			sed 's/ -> / /' | \
			sed 's/;$$//' | \
			tsort 2>&1 || true; \
		exit 1; \
	fi

clean:
	rm -f ../output/graph.dot ../output/task_flow.png ../output/task_flow.svg

include ../../generic.make
