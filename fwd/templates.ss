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

(import :std/format
        :std/misc/ports
        :std/misc/process
        :std/srfi/13
        (for-syntax :std/misc/ports
                    :std/generic))

(export #t)

;; lexical interpolation
;; we could just as well use an alist or a map
(defsyntax (interpolate-lexical stx)
  ;; turn things into lists of characters
  (defgeneric ->char-list
    (lambda args #f))
  (defmethod (->char-list (x <string>))
    ;; everything has to be reversed, see algo in parse
    (reverse (string->list x)))
  (defmethod (->char-list (x <number>))
    (reverse (string->list (number->string x))))

  (def (stx->char-list stx)
    (with-syntax* ((ev (eval stx))
                   ((chars ...) (syntax->datum (->char-list #'ev))))
      #'(chars ...)))

  ;; parse the string for template variables
  (def (parse stx ctx)
    (let (port (open-input-string (stx-e stx)))
      (let lp ((c (read-char port))
               (sexps [])
               (str []))
        (cond
         ((eq? c #\#) ;; template variable start?
          (let ((c+1 (read-char port)))
            (cond
             ((eq? c+1 #\{) ;; yes, read sexp
              (let (sexp (read port))
                (if (eq? (read-char port) #\}) ;; closed template variable?
                  (if (symbol? sexp)
                    (lp (read-char port) (cons sexp sexps) (cons #\a (cons #\~ str)))
                    ;; need generic ->string function
                    (lp (read-char port) sexps (append (stx->char-list sexp) str))) ;; eval the sexp, then to-string
                  (error "Bad quasistring formatting."))))
             (else ;; no, start again
              (lp (read-char port) sexps str)))))
         ((eq? c #!eof) ;; eof so return syntax
          (close-input-port port)
          (with-syntax (((vars ...) (datum->syntax ctx (reverse sexps)))
                        (str (list->string (reverse str))))
            #'(format str vars ...)))
         (else ;; no, start again
          (lp (read-char port) sexps (cons c str)))))))

  (syntax-case stx ()
    ((macro s)
     (stx-string? #'s)
     (with-syntax ((ps (parse #'s #'macro)))
       #'ps))
    ((macro s ctx)
     (stx-string? #'s)
     (with-syntax ((ps (parse #'s #'ctx)))
       #'ps))))

(defsyntax (include-template stx)
  (syntax-case stx ()
    ((macro file)
     (stx-string? #'file)
     (with-syntax ((template (call-with-input-file (stx-e #'file) read-all-as-string)))
       #'(interpolate-lexical template macro)))))
