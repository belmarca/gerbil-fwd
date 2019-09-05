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
    ;; register route as in Gerbil docs
    ;; (http-register-handler httpd "/" root-rh)
    ;; register route using fwd macros
    ;; in this particular case, httpd is captured in the lexical scope
    ;; and the route handler is assumed to be a-rh
    (route /a)
    ;; (route /b b-rh)
    ;; (route /d in /v1)
    ;; (route /d d-rh in /v3)
    ;; (route /random)
    ;; (route /echo)
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
