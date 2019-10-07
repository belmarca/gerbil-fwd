;;; -*- Gerbil -*-
;;; (C) vyzo at hackzen.org
;;; belmarca
;;; Route handlers

;; (def (http-501)
;;   (response 501 [ct-text/plain] "501\n"))

(defhandler default
  GET: (response 200 "default-handler\n"))

(defhandler getpost
  GET: (response 200 "get\n")
  POST: (response 200 "post\n"))

(defhandler json
  POST: (let (json (call-with-input-u8vector body read-json))
          (if (hash-key? json 'name)
            (let (name (hash-ref json 'name))
              (response 200 [ct-text/plain]
                        (quasistring "hello, #{name}!")))
            (response 422 "422\n"))))

(defhandler json2
  POST: (let (json (call-with-input-u8vector body read-json))
          (let-hash json
            (let (name .?name)
              (if name
                (response 200 (quasistring "hello, #{name}!"))
                (response 422 "422\n"))))))

(defhandler whatever
  GET: (response 200 "whatever\n")
  ELSE: (response 404 "not found\n"))
