../assets/elm.min.js: src/Main.elm
	elm make --optimize --output=elm.js src/Main.elm
	mv elm.js ../assets/
	uglifyjs ../assets/elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output "../assets/elm.min.js"

clean:
	rm ../assets/*.js
	rm -r elm-stuff

#--optimize