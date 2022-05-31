MAIN=main.fnl
run: $(MAIN)
	tic80 --skip --fs=. $(MAIN)
edit: $(MAIN)
	tic80 --skip --fs=. --cmd='load main.fnl & edit'
buildweb: $(MAIN)
	tic80 --skip --fs=. --cmd='load main.fnl & export html bit-path & exit' --cli
buildlinux: $(MAIN)
	tic80 --skip --fs=. --cmd='load main.fnl & export linux bit-path-linux & exit' --cli
buildwin: $(MAIN)
	tic80 --skip --fs=. --cmd='load main.fnl & export win bit-path-win & exit' --cli
buildmac: $(MAIN)
	tic80 --skip --fs=. --cmd='load main.fnl & export mac bit-path-mac & exit' --cli
buildcart: $(MAIN)
	tic80 --skip --fs=. --cmd='load main.fnl & export binary bit-path & exit' --cli
