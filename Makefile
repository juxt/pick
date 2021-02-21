.PHONY: all test lint lint2 watch clean

all: 	lint2 test

test:
	echo "pick:<span foreground='#ff3'>TEST</span>" > /tmp/pick-test-status
	clojure -Mtest && echo "pick:PASS" > /tmp/pick-test-status || echo "<span foreground='red'>pick:FAIL</span>" > /tmp/pick-test-status

lint:
	clojure -Alint

lint2:
	clj-kondo --lint src/juxt --lint test/juxt

watch:
	find . -name "*.clj" | entr make test

clean:
	rm -f /tmp/pick-test-status
