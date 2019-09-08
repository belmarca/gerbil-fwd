;;; -*- Gerbil -*-
;;; (C) vyzo at hackzen.org
;;; belmarca
;;; Route handlers

(defhandler default
  GET: (http-response-write res 200 [ct-text/plain] "default-handler\n"))

(defhandler getpost
  GET: (http-response-write res 200 [ct-text/plain] "get\n")
  POST: (http-response-write res 200 [ct-text/plain] "post\n"))

(defhandler json
  POST: (http-response-write res 200 [ct-app/json]
                             ;; just print the raw data don't parse into json
                             (list->string
                              (map integer->char (u8vector->list body)))))
