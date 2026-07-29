# Production task graph

Builds the production task graph from input-output dependencies declared in top-level task Makefiles. Tasks under `tasks/audits/` are excluded.
Graph generation fails if the declared task dependencies contain a cycle.

Produces:
- `output/graph.txt`
- `output/task_flow.png`

Approx. runtime: less than one minute.
