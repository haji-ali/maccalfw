all: exec

exec:
	cd MacCalfw
	swift run -Xswiftc -I/opt/homebrew/include/
	#swiftc example.swift  -I . -L . -Xlinker -sectcreate -Xlinker __TEXT -Xlinker __info_plist -Xlinker ./Info.plist
