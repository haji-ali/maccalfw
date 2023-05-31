(module-load (locate-library (expand-file-name
                              "./.build/debug/libmaccalfw.dylib")
                             t))

(message "%S" (maccalfw-get-calendars))

(message "%S" (maccalfw-fetch-events "Hello" 0 0))
