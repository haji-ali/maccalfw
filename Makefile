
all: build

build:
	swiftc -Xcc -fmodule-map-file=src/module.modulemap \
		-I/opt/homebrew/include/ \
		src/EmacsUtil.swift \
		src/MacCalfw.swift \
		-O -emit-library -o libmaccalfw.dylib

test:
	emacs -Q --batch --load test.el
