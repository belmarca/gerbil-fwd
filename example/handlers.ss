;;; -*- Gerbil -*-
;;; (C) vyzo at hackzen.org
;;; belmarca
;;; Route handlers

;; Standard handler as shown in Gerbil docs.
;; (defhandler (root 200 [ct-text/plain])
;;   (string-append "hello, " (inet-address->string (http-request-client req)) "\n"))

;; default
(def (default-handler req res)
  (http-response-write res 404 '(("Content-Type" . "text/plain"))
    "these aren't the droids you are looking for.\n"))

(defhandler a
  GET: (http-response-write res 200 [ct-text/plain] "a\n"))

;; (defhandler json
;;   GET: (http-response-write res 200 '(("Content-Type" . "text/plain"))
;;                             (let ((headers (http-request-headers req)))
;;                                (
