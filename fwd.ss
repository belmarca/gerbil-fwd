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


(import :std/net/httpd
        :std/stxparam
        (for-syntax
         :std/net/httpd
         :std/stxparam
         (only-in :std/iter for in-range)
         (only-in :std/srfi/13
                  string-drop)))

(export defhandler route response
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

  ;; TODO: use new format-id in stdlib
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

;; see std/actor/message.ss
(defrules defparam ()
  ((_ macro param)
   (begin
     (defsyntax-parameter param #f)
     (defsyntax (macro stx)
       (if (identifier? stx)
         (cond
          ((syntax-parameter-value (quote-syntax param))
           => values)
          (else
           (raise-syntax-error #f "Bad syntax; not in reaction context" stx)))
         (raise-syntax-error #f "Bad syntax" stx))))))

(defparam @request @@request)
(defparam @response @@response)

(defrules response ()
  ((_ status-code body)
   (stx-number? #'status-code)
   (http-response-write @response status-code [ct-text/plain] body))
  ((_ status-code headers body)
   (stx-number? #'status-code)
   (http-response-write @response status-code headers body)))

(defsyntax (defhandler stx)
  ;; parse the kw: method from the arguments
  ;; and return a list to be used by case
  (def (parse-methods stx)
    (let lp ((rest stx)
             (methods [])
             (e #f))
      (syntax-case rest ()
        ((GET: response . rest)
         (lp #'rest (cons [['GET] #'response] methods) #f))
        ((HEAD: response . rest)
         (lp #'rest (cons [['HEAD] #'response] methods) #f))
        ((POST: response . rest)
         (lp #'rest (cons [['POST] #'response] methods) #f))
        ((PUT: response . rest)
         (lp #'rest (cons [['PUT] #'response] methods) #f))
        ((DELETE: response . rest)
         (lp #'rest (cons [['DELETE] #'response] methods) #f))
        ((CONNECT: response . rest)
         (lp #'rest (cons [['CONNECT] #'response] methods) #f))
        ((OPTIONS: response . rest)
         (lp #'rest (cons [['OPTIONS] #'response] methods) #f))
        ((TRACE: response . rest)
         (lp #'rest (cons [['TRACE] #'response] methods) #f))
        ((PATCH: response . rest)
         (lp #'rest (cons [['PATCH] #'response] methods) #f))
        ((ELSE: response . rest)
         (lp () (append methods [['else #'response]]) #t))
        (()
         (if e
           methods
           (append methods [['else '(response 501 "501\n")]]))))))
  
  (syntax-case stx ()
    ((macro id kw ...)
     (with-syntax* ((verb (datum->syntax #'macro 'verb))
                    (body (datum->syntax #'macro 'body))
                    (url (datum->syntax #'macro 'url))
                    (hid (datum->syntax #'macro (identifier->handler #'id)))
                    (methods (parse-methods #'(kw ...)))
                    (mcase (datum->syntax #'macro
                             (cons #'case (cons #'verb #'methods)))))
       #'(define-values (hid)
           (lambda (req res)
             (syntax-parameterize ((@@request (quote-syntax req))
                                   (@@response (quote-syntax res)))
               ;; TODO: add other vars to let
               ;; see src/std/net/httpd/handler.ss
               (let ((verb (http-request-method @request))
                     (body (http-request-body @request)) ;; u8vector
                     (url (http-request-url @request)))
                 mcase))))))))

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
