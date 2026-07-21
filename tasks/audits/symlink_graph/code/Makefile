all: ../output/task_flow.png


../output/graph.txt: graphmaker.sh Makefile $(wildcard ../../../*/code/Makefile) | ../output
	bash graphmaker.sh
	sed '1d; $$d' $@ | tr -d '"' | sed 's/->/ /' | tsort >/dev/null

../output/task_flow.png: ../output/graph.txt | ../output
	dot -Grankdir=LR -Tpng ../output/graph.txt -o ../output/task_flow.png

runorder: ../output/graph.txt | ../output
	sed '1d; $$d' ../output/graph.txt | sed 's/->/ /' | tsort | sed 's/\(.*\)/make -C ..\/..\/\1\/code/'

include ../../../generic.make
