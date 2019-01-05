TEMPDIRS = build build/

Distributeable_Path=./build/distributeable/regexc
Static_Documentation_Dir=./build/static_documentation
Source_Dir=./src

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
	cd src; swipl -f documentation_server.pl

.PHONY: clean_static_docs
clean_static_docs:
	rm -f -r $(Static_Documentation_Server)

.PHONY: static_docs
static_docs: $(Static_Documentation_Dir)

$(Static_Documentation_Dir): clean_static_docs
	swipl -f $(Source_Dir)/load.pl -g 'doc_save("$(Source_Dir)", [doc_root("$(Static_Documentation_Dir)"), recursive(true)]), halt(0)'
