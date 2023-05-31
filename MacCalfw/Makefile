all: build

build: Sources/
	swift build -Xswiftc -I/opt/homebrew/include/
	#swiftc example.swift  -I . -L . -Xlinker -sectcreate -Xlinker __TEXT -Xlinker __info_plist -Xlinker ./Info.plist

test:
	emacs -Q --batch --load test.el
