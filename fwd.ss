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


(import (only-in :std/net/httpd
                 http-register-handler
                 http-response-write
                 http-request-method)
        (for-syntax
         (only-in :std/net/httpd
                  http-register-handler
                  http-response-write
                  http-request-method)
         (only-in :std/iter for in-range)
         (only-in :std/srfi/13
                  string-drop)))

(export defhandler route
        ct-text/plain ct-text/html ct-app/json)

(begin-syntax
  (def +handler-suffix+ "-handler")

  (def (path->handler stx)
    (string->symbol
     (string-drop
      (string-append
       (symbol->string (stx-e stx))
       +handler-suffix+)
      1)))

  (def (identifier->handler stx)
    (string->symbol
      (string-append
       (symbol->string (stx-e stx))
       +handler-suffix+))))

;; Content-Types
(define-values (ct-text/plain ct-text/html ct-app/json)
  (values
   '("Content-Type" . "text/plain")
   '("Content-Type" . "text/plain")
   '("Content-Type" . "application/json")))

(defsyntax (defhandler stx)
  ;; parse the kw: method from the arguments
  ;; and return a list to be used by case
  (def (parse-methods stx)
    (let lp ((rest stx)
             (methods []))
      (syntax-case rest ()
        ((GET: response . rest)
         (lp #'rest (cons [['GET] #'response] methods)))
        ((HEAD: response . rest)
         (lp #'rest (cons [['HEAD] #'response] methods)))
        ((POST: response . rest)
         (lp #'rest (cons [['POST] #'response] methods)))
        ((PUT: response . rest)
         (lp #'rest (cons [['PUT] #'response] methods)))
        ((DELETE: response . rest)
         (lp #'rest (cons [['DELETE] #'response] methods)))
        ((CONNECT: response . rest)
         (lp #'rest (cons [['CONNECT] #'response] methods)))
        ((OPTIONS: response . rest)
         (lp #'rest (cons [['OPTIONS] #'response] methods)))
        ((TRACE: response . rest)
         (lp #'rest (cons [['TRACE] #'response] methods)))
        ((PATCH: response . rest)
         (lp #'rest (cons [['PATCH] #'response] methods)))
        ;; must deal with default responses, can't just hang!
        (() methods))))

  (syntax-case stx ()
    ((macro id kw ...)
     (with-syntax* ((methods (parse-methods #'(kw ...)))
                    (verb (datum->syntax #'macro 'verb))
                    (req (datum->syntax #'macro 'req))
                    (res (datum->syntax #'macro 'res))
                    (hid (datum->syntax #'macro (identifier->handler #'id)))
                    ;; must deal with default responses, can't just hang!
                    (mcase (datum->syntax #'macro
                             (cons #'case
                                   (cons #'verb #'methods)))))
       #'(define-values (hid)
           (lambda (req res)
             (let ((verb (http-request-method req)))
               mcase)))))))

;; TODO: refactor this monster
(defsyntax (route stx)
  (syntax-case stx (in)

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

    ((macro path in parent)
     (andmap identifier? [#'path #'parent])
     (with-syntax ((server (datum->syntax #'macro 'httpd))
                   (spath (string-append (symbol->string (stx-e #'parent))
                                         (symbol->string (stx-e #'path))))
                   (handler (datum->syntax #'macro (path->handler #'path))))
       #'(http-register-handler server spath handler)))

    ((macro path handler in parent)
     (andmap identifier? [#'path #'parent])
     (with-syntax ((server (datum->syntax #'macro 'httpd))
                   (spath (string-append (symbol->string (stx-e #'parent))
                                         (symbol->string (stx-e #'path)))))
       #'(http-register-handler server spath handler)))))
