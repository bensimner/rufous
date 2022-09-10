all: benchmarks

# this is super ugly, but we use a separate .stack-work/ directory to keep all the versions compiled with --profile
benchmarks:
	stack build --work-dir .stack-work-profiling --profile
	stack bench --work-dir .stack-work-profiling --profile --benchmark-arguments="+RTS -hc -p -RTS" -- rufous:bench:queue
.PHONY: benchmarks

clean:
	stack clean --full --work-dir .stack-work-profiling