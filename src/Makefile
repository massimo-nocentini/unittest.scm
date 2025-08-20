
test:
	cd test && csi -s bootstrap.scm
	cd test && csc learning.scm && ./learning

install:
	chicken-install -sudo


format:
	scheme-indent -T 2 < unittest.scm > unittest.scm.tmp && mv unittest.scm.tmp unittest.scm
	scheme-indent -T 2 < test/bootstrap.scm > test/bootstrap.scm.tmp && mv test/bootstrap.scm.tmp test/bootstrap.scm
	scheme-indent -T 2 < test/learning.scm > test/learning.scm.tmp && mv test/learning.scm.tmp test/learning.scm

