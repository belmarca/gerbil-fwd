;;; -*- Gerbil -*-
;;; (C) vyzo at hackzen.org
;;; belmarca
;;; Route handlers

(defhandler default
  GET: (response 200 "default-handler\n"))

(defhandler getpost
  GET: (response 200 "get\n")
  POST: (response 200 "post\n"))

(defhandler json
  POST: (response 200 [ct-app/json]
                  (list->string
                   (map integer->char (u8vector->list body)))))

(defhandler whatever
  GET: (response 200 "whatever\n")
  ELSE: (response 404 "not found\n"))
