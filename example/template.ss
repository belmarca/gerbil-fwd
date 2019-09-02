(import :std/misc/text
        :std/misc/list
        :std/pregexp
        :std/iter
        :std/srfi/13
        :std/format
        :std/sugar)

;; TODO: Add module level definition for the default delimiter
;; to avoid repitition and facilitate maintenance.
(def (remove-bad-matches vars omit)
  (let ((goodies []))
    (for (var vars)
      (unless (string-contains var omit)
        (set! goodies (flatten (cons var goodies)))))
    goodies))

(def (match-regexp pat str . opt-args)
  "Like pregexp-match but for all matches til end of str"
  (let ((n (string-length str))
        (ix-prs []))
    (let lp ((start 0))
      (let* ((pp (pregexp-match-positions pat str start n))
             (ix-pr (pregexp-match pat str start n)))
        (if ix-pr
          (let ((pos (+ 1 (cdar pp))))
            (set! ix-prs (flatten (cons ix-pr ix-prs)))
            (if (< pos n)
              (lp pos)
              ix-prs))
          (reverse ix-prs))))))

(def (interpolate-sequential tmplt . vars)
  (if (string? tmplt)
    (let* ((hb (pregexp "\\#\\{([a-zA-Z0-9]*)\\}"))
           (ivars (remove-bad-matches (match-regexp hb tmplt) "#"))
           (newstr (pregexp-replace* hb tmplt "~a")))
      (apply format newstr vars))))

;; > (interpolate-sequential "{{x}} {{y}} {{z}}" "EXE" "WHY" "ZEE")
;; "EXE WHY ZEE"

;; TODO: Sanitization
;; TODO: Refactor repeated with-syntax*
;; TODO: Allow for safe ("sandboxed") and unsafe sexp interpolation
(defsyntax (interpolate-lexical stx)
  (syntax-case stx ()
    ((macro tmplt delim)
     (stx-andmap stx-string? [#'tmplt #'delim])
     (with-syntax* ((str (syntax-e #'tmplt))
                    (re (pregexp (syntax-e #'delim)))
                    (tvars (remove-bad-matches (match-regexp #'re #'str) "#"))
                    (newstr (pregexp-replace* #'re #'str "~a"))
                    (vals (datum->syntax #'macro (map string->symbol #'tvars))))
       (syntax-case #'vals ()
         ((val . rest)
          (stx-andmap identifier? #'[val . rest])
          #'(apply format newstr [val . rest])))))
    ((macro tmplt delim ctx)
     (stx-andmap stx-string? [#'tmplt #'delim])
     (with-syntax* ((str (syntax-e #'tmplt))
                    (re (pregexp (syntax-e #'delim)))
                    (tvars (remove-bad-matches (match-regexp #'re #'str) "#"))
                    (newstr (pregexp-replace* #'re #'str "~a"))
                    (vals (datum->syntax #'ctx (map string->symbol #'tvars))))
       (syntax-case #'vals ()
         ((val . rest)
          (stx-andmap identifier? #'[val . rest])
          #'(apply format newstr [val . rest])))))))

;; > (displayln (let ((y 10) (x 0)) (interpolate-lexical "x is #{x}\ny is #{y}" "\\#\\{([a-zA-Z0-9]*)\\}")))
;; x is 0
;; y is 10

;; Input is not sanitized. You can easily break this by
;; passing a template with format parameters already present.
;; E.g.:
;; > (let ((a 0)) (quasistring "a: #{a} ~a"))
;; *** ERROR IN std/format#format -- Missing format argument "a: ~a ~a" (0)
(defsyntax (quasistring stx)
  (syntax-case stx ()
    ((macro tmplt)
     (stx-string? #'tmplt)
     #'(interpolate-lexical tmplt "\\#\\{([a-zA-Z0-9]*)\\}" macro))
    ((macro tmplt delim)
     (stx-andmap stx-string? [#'tmplt #'delim])
     #'(interpolate-lexical tmplt delim macro))))

;; > (displayln (let ((x "World")) (quasistring "Hello,\n #{x}!")))
;; Hello,
;;  World!

;; > (displayln (let ((a 0) (b 1) (c 2)) (quasistring "a: #{a}\nb: #{b}\nc: #{c}"))) 
;; a: 0
;; b: 1
;; c: 2

(defsyntax (lambda-interpolate-hash-table stx)
  (syntax-case stx ()
    ((macro tmplt)
     (stx-string? #'tmplt)
     (with-syntax ((str (stx-e #'tmplt)))
       (let* ((hb (pregexp "\\#\\{([a-zA-Z0-9]*)\\}"))
              (tvars (remove-bad-matches (match-regexp hb str) "#"))
              (newstr (pregexp-replace* hb str "~a"))
              (keys (map string->symbol tvars)))
         #'(lambda (ht)
             (let ((vals (map (cut hash-ref ht <>) (quote keys))))
               (apply format newstr vals))))))))
  
;; > (def ht (hash (a "World!")))                                 
;; > (displayln ((lambda-interpolate-hash-table "Hello #{a}") ht))
;; Hello World!

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
                            "-handler")))))
       #'(define-values (hid)
           (lambda (req res) body ...))))))
