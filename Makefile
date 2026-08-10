all: build

build:
	swift build -c release

test:
	swift test
	emacs -Q --batch --load test.el -f ert-run-tests-batch-and-exit
