dispatcher: dispatcher.c crt.h kleenex.c thr_pool.c thr_pool.h
	gcc -pthread -o dispatcher dispatcher.c kleenex.c thr_pool.c -I.

opt: dispatcher.c crt.h kleenex.c thr_pool.c thr_pool.h
	gcc -O3 -pthread -o dispatcher_opt dispatcher.c kleenex.c thr_pool.c -I.
