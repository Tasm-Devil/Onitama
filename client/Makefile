# Default to debug for development
all: debug

# Debug build (with time-travel debugger)
debug:
	@echo "Building Elm in DEBUG mode..."
	elm make --debug --output=../assets/elm.js src/Main.elm

# Production/Release build (optimized, no debugger)
release:
	@echo "Building Elm in RELEASE mode (optimized)..."
	elm make --optimize --output=../assets/elm.js src/Main.elm
	# Optional: further minify with uglifyjs (requires npm install uglify-js)
	# uglifyjs ../assets/elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output "../assets/elm.min.js"

clean:
	rm -f ../assets/elm.js ../assets/elm.min.js
	rm -rf elm-stuff

.PHONY: all debug release clean