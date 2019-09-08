;;; -*- Gerbil -*-
;;; (C) vyzo at hackzen.org
;;; belmarca
;;; Simple web server
(import :std/net/httpd
        :std/net/address
        :std/text/json
        :std/sugar
        :std/iter
        :std/getopt
        :gerbil/gambit/threads
        (only-in :gerbil/gambit/random random-integer)
        :belmarca/fwd)

(export main)

(include "handlers.ss")

(def (run address)
  (let (httpd (start-http-server! address mux: (make-default-http-mux default-handler)))
    (route /getpost)
    (route /json)
    (thread-join! httpd)))

(def (main . args)
  (def gopt
    (getopt (option 'address "-a" "--address"
                    help: "server address"
                    default: "127.0.0.1:8080")))

  (try
   (let (opt (getopt-parse gopt args))
     (run (hash-get opt 'address)))
   (catch (getopt-error? exn)
     (getopt-display-help exn "hellod" (current-error-port))
     (exit 1))))
