dist_lisp_LISP = org-ref-multi-index.el
EXTRA_DIST = README.org
CLEANFILES = .pkg-tmp/ org-ref-multi-index-$(PACKAGE_VERSION).tar
SUBDIRS = test
AM_ELCFLAGS = --eval '(require (quote bytecomp))'

package: org-ref-multi-index-$(PACKAGE_VERSION).tar $(srcdir)/README.org

srclisp=$(dist_lisp_LISP:%.el=$(srcdir)/%.el)

org-ref-multi-index-$(PACKAGE_VERSION).tar: $(srclisp) $(srcdir)/README.org
	mkdir -p .pkg-tmp/org-ref-multi-index-$(PACKAGE_VERSION)/ && \
	cp $(srclisp) .pkg-tmp/org-ref-multi-index-$(PACKAGE_VERSION)/ && \
	cp $(srcdir)/README.org .pkg-tmp/org-ref-multi-index-$(PACKAGE_VERSION)/ && \
	cd .pkg-tmp && tar cf $@ org-ref-multi-index-$(PACKAGE_VERSION)/ && \
	cd .. && mv -v .pkg-tmp/org-ref-multi-index-$(PACKAGE_VERSION).tar . && \
	rm -rf .pkg-tmp

dist-hook: package
