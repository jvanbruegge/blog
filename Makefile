.PHONY: all watch

all:
	cabal run blog

watch:
	make all
	(cd build && browser-sync start --server -w) &
	while inotifywait -e modify -e create -e move --exclude '\.git|\.shake' -r .; do \
		make all; \
	done

