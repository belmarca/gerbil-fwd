;; Copyright (c) 2019 Marc-André Bélanger <marc-andre.belanger@asciimail.com>
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(import :std/test
        :std/net/httpd
        :std/net/request
        :std/text/json
        :std/sugar
        :gerbil/gambit/random
        :belmarca/fwd)

(export fwd-test)

;;; handlers
(defhandler default
  GET: (response 200 "default-handler\n"))

(defhandler getpost
  GET:  (response 200 "get\n")
  POST: (response 200 "post\n"))

(defhandler json
  POST: (let (json (call-with-input-u8vector body read-json))
          (let-hash json
            (let (name .?name)
              (if name
                (response 200 (quasistring "hello, #{name}!\n"))
                (response 422 "422\n"))))))

(defhandler else
  ELSE: (response 404 "404\n")
  GET:  (response 200 "else-handler\n"))

(def fwd-test
  (test-suite "test :belmarca/fwd"

    (def server-address
      "127.0.0.1:9999")

    (def server-url
      (string-append "http://" server-address))

    (def httpd
      (start-http-server! server-address mux: (make-recursive-http-mux default-handler)))

    (route /getpost)
    (route /json)
    (route /else)

    (test-case "test default handler"
      (let* ((url (string-append server-url "/"))
             (get (http-get url))
             (put (http-put url)))
        (check (request-status get) => 200)
        (check (request-text get) => "default-handler\n")
        (request-close get)
        ;; Other methods aren't implemented
        (check (request-status put) => 501)
        (check (request-text put) => "501\n")
        (request-close put)))


    (test-case "test /getpost handler"
      (let* ((url (string-append server-url "/getpost"))
             (get (http-get url))
             (post (http-post url data: "")))
        ;; GET
        (check (request-status get) => 200)
        (check (request-text get) => "get\n")
        (request-close get)
        ;; POST
        (check (request-status post) => 200)
        (check (request-text post) => "post\n")
        (request-close post)))

    (test-case "test /json handler"
      (let* ((url (string-append server-url "/json"))
             (data "{\"name\": \"visitor\"}")
             (bad-data "{\"x\": \"visitor\"}")
             (post (http-post url data: data))
             (bad-post (http-post url data: bad-data)))
        ;; good
        (check (request-status post) => 200)
        (check (request-text post) => "hello, visitor!\n")
        (request-close post)
        ;; bad
        (check (request-status bad-post) => 422)
        (check (request-text bad-post) => "422\n")
        (request-close bad-post)))

    (test-case "test /else handler"
      (let* ((url (string-append server-url "/else"))
             (get (http-get url))
             (put (http-put url)))
        ;; GET
        (check (request-status get) => 200)
        (check (request-text get) => "else-handler\n")
        (request-close get)
        ;; PUT - ELSE should match as we bypass default-handler
        (check (request-status put) => 404)
        (check (request-text put) => "404\n")
        (request-close put)))

    (stop-http-server! httpd)))
