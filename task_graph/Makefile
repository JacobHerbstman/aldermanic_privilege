all: paper_task_flow.svg

paper_task_flow.svg: paper_task_flow.dot
	dot -Tsvg $< -o $@

# The graph discovers reachable tasks; every task Makefile can change its edges.
paper_task_flow.dot: build_task_graph.py ../paper/Makefile $(wildcard ../tasks/*/code/Makefile)
	python3 $<

all: construction_tasks.svg construction_scripts.svg construction_steps.md

construction_tasks.svg: construction_tasks.dot
	dot -Tsvg $< -o $@

construction_scripts.svg: construction_scripts.dot
	dot -Tsvg $< -o $@

construction_tasks.dot: build_construction_graph.py ../tasks/prepare_new_construction/code/Makefile ../tasks/new_construction_analysis_data/code/Makefile ../tasks/new_construction_cleaning/code/Makefile
	python3 $< tasks

construction_scripts.dot: build_construction_graph.py ../tasks/prepare_new_construction/code/Makefile ../tasks/new_construction_analysis_data/code/Makefile ../tasks/new_construction_cleaning/code/Makefile
	python3 $< scripts

construction_steps.md: build_construction_graph.py ../tasks/prepare_new_construction/code/Makefile ../tasks/new_construction_analysis_data/code/Makefile ../tasks/new_construction_cleaning/code/Makefile
	python3 $< steps
