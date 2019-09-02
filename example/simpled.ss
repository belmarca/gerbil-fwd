;;; -*- Gerbil -*-
;;; (C) vyzo at hackzen.org
;;; belmarca
;;; Simple web server
(import :std/net/httpd
        :std/net/address
        :std/text/json
        (only-in :std/sugar try catch)
        :std/iter
        :std/getopt
        :gerbil/gambit/threads
        :belmarca/fwd)
(export main)

(def (run address)
  (let (httpd (start-http-server! address mux: (make-default-http-mux default-rh)))
    ;; register route as in Gerbil docs
    (http-register-handler httpd "/" root-handler)
    ;; register route using fwd macros
    ;; in this particular case, httpd is captured in the lexical scope
    ;; and the route handler is assumed to be hello-rh
    (route /a)
    (route /b b-rh)
    (route /c using httpd)
    (route /d d-rh using httpd)
    (route /d in /v1)
    (route /d in /v2 using httpd)
    (route /d d-rh in /v3)
    (route /d d-rh in /v4 using httpd)
    (thread-join! httpd)))

;; Standard handler as shown in Gerbil docs.
(def (root-handler req res)
  (http-response-write res 200 '(("Content-Type" . "text/plain"))
    (string-append "hello, " (inet-address->string (http-request-client req)) "\n")))

;; defines (a-rh (req res) ...)
(defhandler a
  (http-response-write res 200 '(("Content-Type" . "text/plain")) "a\n"))

(defhandler b
  (http-response-write res 200 '(("Content-Type" . "text/plain")) "b\n"))

(defhandler c
  (http-response-write res 200 '(("Content-Type" . "text/plain")) "c\n"))

(defhandler d
  (http-response-write res 200 '(("Content-Type" . "text/plain")) "d\n"))

;; default-handler
(defhandler default
  (http-response-write res 404 '(("Content-Type" . "text/plain"))
                       "these aren't the droids you are looking for.\n"))

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
