TEMPDIRS = build build/distributable

MacOS_Distributeable_Path="build/distributable/re2b"

all: .dirFile $(MacOS_Distributeable_Path)

$(MacOS_Distributeable_Path):
	swipl -o $(MacOS_Distributeable_Path) -g main -c src/interface.pl --stand_alone=true

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

