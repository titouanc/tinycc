%.s: fixture/%.tiny target/debug/tinycc
	./tinycc $< > $@

%.prog: %.s
	gcc -m32 -g -o $@ $< stubs.c

clean:
	rm -f *.s *.prog
