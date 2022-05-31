MAIN=main.fnl
run: $(MAIN)
	tic80 --skip --fs=. $(MAIN)
edit: $(MAIN)
	tic80 --skip --fs=. --cmd='load main.fnl & edit'
