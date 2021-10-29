
CFLAGS=-Wall -Wcolumn-overflow -Wtruncate -Wunreachable
LDFLAGS=-lm

./%: ./%.cbl
	echo `date` $< >>build.log
	cobc $(CFLAGS) -t $@.lst -x -o $@ $<

all: ./b64demo1 ./b64demo2

.PHONY: all

getfavicon:
	curl http://rosettacode.org/favicon.ico > favicon.ico

testdemo1:
	./b64demo1 file > b64demo1.out
	./b64demo2 < b64demo1.out > b64demo2.out
	# remove trailing x'0a' added by the DISPLAY of the output
	head -c -1 b64demo2.out > b64demo2.out1
	od -A x -t x1z -v b64demo2.out1 > b64demo2.out1.od
	od -A x -t x1z -v favicon.ico > favicon.ico.od
	diff b64demo2.out1.od favicon.ico.od

testdemo2:
	echo 'bGlnaHQgd29yay4=' | ./b64demo2
	echo 'bGlnaHQgd29yay4' | ./b64demo2
	echo 'bGlnaHQgd29yaw==' | ./b64demo2
	echo 'bGlnaHQgd29yaw' | ./b64demo2
	echo 'bGlnaHQgd29y' | ./b64demo2
	echo 'bGlnaHQgd28=' | ./b64demo2
	echo 'bGlnaHQgd28' | ./b64demo2
	echo 'bGlnaHQgdw==' | ./b64demo2
	echo 'bGlnaHQgdw' | ./b64demo2
	echo 'VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g=' | ./b64demo2
	echo 'VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g' | ./b64demo2

