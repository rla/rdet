version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile=rdet-$(version).tgz
remote=www-data@packs.rlaanemets.com:/sites/packs.rlaanemets.com/public/rdet

package: test
	tar cvzf $(packfile) prolog tests pack.pl README.md LICENSE

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

upload: package
	scp $(packfile) $(remote)/$(packfile)

.PHONY: test package upload
