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


(import (only-in :std/net/httpd http-register-handler)
        (for-syntax
         (only-in :std/srfi/13 string-drop)))

(export defhandler route)

;; Binds a function of (req res) in the scope of the caller.
;; This is not immediately useful but will be the basis
;; for the HTTP verb macros.
(defsyntax (defhandler stx)
  (syntax-case stx ()
    ((macro id body ...)
     (identifier? #'id)
     (with-syntax ((req (datum->syntax #'macro 'req))
                   (res (datum->syntax #'macro 'res))
                   (hid (datum->syntax #'macro
                          (string->symbol
                           (string-append
                            (symbol->string (stx-e #'id))
                            "-rh"))))) ;; rh = route handler
       #'(define-values (hid)
           (lambda (req res) body ...))))))

(begin-syntax
  (def (path->handler stx)
    (string->symbol
     (string-drop
      (string-append
       (symbol->string (stx-e stx))
       "-rh")
      1))))

;; TODO: refactor this monster
(defsyntax (route stx)
  (syntax-case stx (in using)

    ((macro path)
     (identifier? #'path)
     (with-syntax ((server (datum->syntax #'macro 'httpd))
                   (spath (symbol->string (stx-e #'path)))
                   (handler (datum->syntax #'macro (path->handler #'path))))
       #'(http-register-handler server spath handler)))

    ((macro path handler)
     (identifier? #'path)
     (with-syntax ((server (datum->syntax #'macro 'httpd))
                   (spath (symbol->string (stx-e #'path))))
       #'(http-register-handler server spath handler)))

    ((macro path using server)
     (identifier? #'path)
     (with-syntax ((spath (symbol->string (stx-e #'path)))
                   (handler (datum->syntax #'macro (path->handler #'path))))
       #'(http-register-handler server spath handler)))

    ((macro path handler using server)
     (identifier? #'path)
     (with-syntax ((spath (symbol->string (stx-e #'path))))
       #'(http-register-handler server spath handler)))

    ((macro path in parent)
     (andmap identifier? [#'path #'parent])
     (with-syntax ((server (datum->syntax #'macro 'httpd))
                   (spath (string-append (symbol->string (stx-e #'parent))
                                         (symbol->string (stx-e #'path))))
                   (handler (datum->syntax #'macro (path->handler #'path))))
       #'(http-register-handler server spath handler)))

    ((macro path in parent using server)
     (andmap identifier? [#'path #'parent])
     (with-syntax ((spath (string-append (symbol->string (stx-e #'parent))
                                         (symbol->string (stx-e #'path))))
                   (handler (datum->syntax #'macro (path->handler #'path))))
       #'(http-register-handler server spath handler)))

    ((macro path handler in parent)
     (andmap identifier? [#'path #'parent])
     (with-syntax ((server (datum->syntax #'macro 'httpd))
                   (spath (string-append (symbol->string (stx-e #'parent))
                                         (symbol->string (stx-e #'path)))))
       #'(http-register-handler server spath handler)))

    ((macro path handler in parent using server)
     (andmap identifier? [#'path #'parent])
     (with-syntax ((spath (string-append (symbol->string (stx-e #'parent))
                                         (symbol->string (stx-e #'path)))))
       #'(http-register-handler server spath handler)))))

