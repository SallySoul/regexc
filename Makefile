.PHONY: test
test:
	swipl -f src/load.pl -t 'show_coverage(run_tests)'

.PHONY: doc_server
doc_server:
	swipl -f src/documentation_server.pl

