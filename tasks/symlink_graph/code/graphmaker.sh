#!/bin/bash
# =============================================================================
# graphmaker.sh - Generate a DOT graph of task dependencies from Makefiles
# =============================================================================
# This script parses all Makefiles in tasks/*/code/ to extract symlink rules
# and generates a directed graph showing task dependencies.
#
# Symlink rules follow these patterns:
#   ../input/X: ../../{upstream_task}/output/X | ../input
#   ../input/X: ../../../data_raw/X | ../input
#   ../input/%.parquet: ../../{upstream_task}/output/%.parquet | ../input
#
# Output: ../output/graph.dot (DOT format for graphviz)
# =============================================================================

set -e

OUTPUT_FILE="../output/graph.dot"

# Start the DOT file with graph settings
cat > "$OUTPUT_FILE" << 'EOF'
digraph G {
    rankdir=LR;
    node [shape=box, fontname="Helvetica"];
    edge [color=gray40];
    
    // Style the data_raw node specially
    data_raw [style=filled, fillcolor=lightgray, label="data_raw"];
EOF

# -----------------------------------------------------------------------------
# Step 1: Find all Makefiles and extract symlink rules
# -----------------------------------------------------------------------------
# We look for lines matching:
#   ../input/something: ../../task_name/output/something
#   ../input/something: ../../../data_raw/something
#   ../input/%.ext: ../../task_name/output/%.ext (pattern rules)
#
# The grep pattern captures lines with ../input on left side and either
# /output/ or /data_raw/ on right side of the colon

find ../../*/code -maxdepth 1 -name "Makefile" 2>/dev/null | while read makefile; do
    # Extract the current task name from the path
    # ../../task_name/code/Makefile -> task_name
    current_task=$(echo "$makefile" | sed 's|^\.\./\.\./||' | sed 's|/code/Makefile$||')
    
    # Skip symlink_graph itself to avoid self-reference
    if [ "$current_task" = "symlink_graph" ]; then
        continue
    fi
    
    # Extract all symlink rules from this Makefile
    # Pattern 1: ../input/X: ../../upstream_task/output/X
    # Pattern 2: ../input/X: ../../../data_raw/X
    grep -E '^\.\./input/.*:.*\.\./\.\.' "$makefile" 2>/dev/null | while read line; do
        # Check if this is a data_raw dependency
        if echo "$line" | grep -q 'data_raw'; then
            # This task depends on data_raw
            echo "    data_raw -> $current_task"
        else
            # Extract the upstream task name from ../../upstream_task/output/
            upstream=$(echo "$line" | grep -oE '\.\./\.\./[^/]+/output' | sed 's|\.\./\.\./||' | sed 's|/output||')
            if [ -n "$upstream" ] && [ "$upstream" != "$current_task" ]; then
                echo "    $upstream -> $current_task"
            fi
        fi
    done
done | sort | uniq >> "$OUTPUT_FILE"

# Close the graph
echo "}" >> "$OUTPUT_FILE"

echo "Generated $OUTPUT_FILE"
echo "Tasks found: $(grep -c ' -> ' "$OUTPUT_FILE" 2>/dev/null || echo 0) edges"
