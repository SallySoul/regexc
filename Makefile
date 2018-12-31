TEMPDIRS = build build/

Distributeable_Path="build/regexc"

all: .dirFile $(Distributeable_Path)

$(Distributeable_Path):
	swipl -o $(Distributeable_Path) -c src/interface.pl --stand_alone=true

.dirFile:
	for dir in $(TEMPDIRS); do \
		mkdir -p $$dir ; \
	done
	@touch .dirFile

.PHONY: clean
clean:
	rm -f -r $(TEMPDIRS) .dirFile

.PHONY: test
test:
	swipl -f src/load.pl -t 'show_coverage(run_tests)'

.PHONY: doc_server
doc_server:
	swipl -f src/documentation_server.pl

