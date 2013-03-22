BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
ETCDIR ?= /etc

.PHONY: test install uninstall clean

dist/build/xcp-networkd/xcp-networkd:
	obuild configure
	obuild build

test:
	./dist/build/network_test/network_test -verbose

install:
	install -D dist/build/xcp-networkd/xcp-networkd $(DESTDIR)$(SBINDIR)/xcp-networkd
	install -D dist/build/networkd_db/networkd_db $(DESTDIR)$(BINDIR)/networkd_db

uninstall:
	rm -f $(DESTDIR)$(SBINDIR)/xcp-networkd
	rm -f $(DESTDIR)$(SBINDIR)/networkd_db

clean:
	rm -rf dist
