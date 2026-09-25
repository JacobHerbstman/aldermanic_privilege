all: paper_task_flow.svg

paper_task_flow.svg: paper_task_flow.dot
	dot -Tsvg $< -o $@

# The graph discovers reachable tasks; every task Makefile can change its edges.
paper_task_flow.dot: build_task_graph.py ../paper/Makefile $(wildcard ../tasks/*/code/Makefile)
	python3 $<
