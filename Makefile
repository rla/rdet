version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile=rdet-$(version).tgz
remote=www-data@packs.rlaanemets.com:/sites/packs.rlaanemets.com/public/rdet

package: test
	tar cvzf $(packfile) prolog tests pack.pl README.md LICENSE

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

benchmark-rdet:
	swipl -s benchmark/benchmark-rdet.pl -g benchmark,halt -t 'halt(1)'

benchmark-rdet-opt:
	swipl -O -s benchmark/benchmark-rdet.pl -g benchmark,halt -t 'halt(1)'

benchmark-vanilla:
	swipl -s benchmark/benchmark-vanilla.pl -g benchmark,halt -t 'halt(1)'

benchmark-vanilla-opt:
	swipl -O -s benchmark/benchmark-vanilla.pl -g benchmark,halt -t 'halt(1)'

benchmark: benchmark-rdet benchmark-vanilla benchmark-rdet-opt benchmark-vanilla-opt

upload: package
	scp $(packfile) $(remote)/$(packfile)

.PHONY: test package upload benchmark-rdet benchmark-rdet-opt benchmark-vanilla benchmark-vanilla-opt benchmark
