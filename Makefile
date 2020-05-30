app.js: src/*
	elm make src/App.elm --output app.js

clean:
	rm -f app.js

.PHONY: clean
