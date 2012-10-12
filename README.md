kona.hs
=======

Description
-----------
A command-line utility to download images from http://konachan.com.

Dependencies
------------
	attoparsec, aeson, http-conduit, stm, stm-chan, url

Usage
-----
	Usage: runhaskell kona.hs [OPTS] tags...
	  -p[PAGE]     --page[=PAGE]        set page number
	  -a[AMOUNT]  --amount[=AMOUNT]     set amount of results
	  -t[THREADS]  --threads[=THREADS]  set amount of threads
	  -i[TYPE]     --image-type[=TYPE]  set image type ([p]review, [s]ample, [f]ull)
	  -h           --help               view this help page
