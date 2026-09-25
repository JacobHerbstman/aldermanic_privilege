all:
	$(MAKE) -C tasks/setup_environment/code
	$(MAKE) -C replication
	$(MAKE) -C paper
