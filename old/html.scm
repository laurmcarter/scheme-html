; core macros {{{

(define-syntax define-tag
  (syntax-rules ()
    ((_ id ((f* (v* ...)) ...))
     (define-syntax id
       (syntax-rules ()
         ((_ ((f v) (f^ v^) (... ...)) b* (... ...))
          (let ((t (symbol->string 'id))
                (f* (symbol->string 'f*))
                ...)
            (let ((fs `((,f* . (,v* ...)) ...))
                  (f (symbol->string 'f))
                  (f^ (symbol->string 'f^))
                  (... ...))
              (and (acceptable? t f v fs) (acceptable? t f^ v^ fs) (... ...)
                   (wrap (open-tag t `((,f . ,v) (,f^ . ,v^) (... ...)) #f)
                         (close-tag t)
                         b*
                         (... ...))))))
         ((_ b* (... ...))
          (let ((t (symbol->string 'id)))
            (wrap (open-tag t '() #f) (close-tag t) b* (... ...)))))))))

(define-syntax define-tag-stub
  (syntax-rules ()
    ((_ id ((f* (v* ...)) ...))
     (define-syntax id
       (syntax-rules ()
         ((_ ((f v) (f^ v^) (... ...)))
          (let ((t (symbol->string 'id))
                (f* (symbol->string 'f*))
                ...)
            (let ((fs `((,f* . (,v* ...)) ...))
                  (f (symbol->string 'f))
                  (f^ (symbol->string 'f^))
                  (... ...))
              (and (acceptable? t f v fs) (acceptable? t f^ v^ fs) (... ...)
                   (open-tag t `((,f . ,v) (,f^ . ,v^) (... ...)) #t))))))))
     ((_)
      (let ((t (symbol->string 'id)))
        (open-tag t '() #t)))))

(define-syntax export-doc
  (syntax-rules ()
    ((_ s s* ...)
     (begin
       (display (comment "Translated from scheme by scm2html"))
       (display s)
       (display s*)
       ...
       (exit)))))

(define-syntax comment
  (syntax-rules ()
    ((_ s s* ...)
     (string-join " " "<!--" s s* ... "-->"))))

(define author
  (lambda (auth)
    (comment "Author:" auth)))

(define-syntax wrap
  (syntax-rules ()
    ((_ open close s* ...)
     (string-append open s* ... close))))

(define-syntax string-join
  (syntax-rules ()
    ((_ sep s) s)
    ((_ sep s s^ s* ...)
     (string-append
       s
       (str-maybe sep s^)
       (str-maybe sep s*)
       ...))))

; }}}

; helpers {{{

(define str-maybe
  (lambda (sep s)
    (if (eq? s "")
      ""
      (string-append sep s))))

(define open-tag
  (lambda (tag fs stub?)
    (angle-bracket
      (string-join " " tag (attributes fs))
      (if stub? "/" ""))))

(define attributes
  (lambda (fs)
    (cond
      ((null? fs) "")
      (else (let ((f (caar fs))
                  (v (cdar fs)))
              (string-join " "
                (string-append f "=" (dquote v))
                (attributes (cdr fs))))))))

(define close-tag
  (lambda (t)
    (angle-bracket (string-append "/" t))))

(define acceptable?
  (lambda (source f v fs)
    (cond
      ((assq f fs)
       => (lambda (p)
            (let ((vs (cdr p)))
              (or (null? vs)
                  (str-mem? v vs)))))
      (else (attr-error source f v)))))

(define str-mem?
  (lambda (s ls)
    (cond
      ((null? ls) #f)
      (else (or (string=? (car ls) s)
                (str-mem? s (cdr ls)))))))

(define-syntax squote
  (syntax-rules ()
    ((_ s s* ...)
     (wrap "'" "'" s s* ...))))

(define-syntax dquote
  (syntax-rules ()
    ((_ s s* ...)
     (wrap "\"" "\"" s s* ...))))

(define-syntax angle-bracket
  (syntax-rules ()
    ((_ s s* ...)
     (wrap "<" ">" s s* ...))))

(define-syntax paren
  (syntax-rules ()
    ((_ s s* ...)
     (wrap "(" ")" s s* ...))))

(define-syntax bracket
  (syntax-rules ()
    ((_ s s* ...)
     (wrap "[" "]" s s* ...))))

(define-syntax brace
  (syntax-rules ()
    ((_ s s* ...)
     (wrap "{" "}" s s* ...))))

(define attr-error
  (lambda (source f v)
    (errorf source "Unacceptable value for attribute \'~a\': ~a" f v)))

; }}}

(define-syntax style
  (syntax-rules (single)
    ((_ single (tag (f v) (f* v*) ...))
     (let ((tag (symbol->string 'tag))
           (f (symbol->string 'f))
           (f* (symbol->string 'f*))
           ...)
       (string-append tag (brace (string-append
                                   (cmd (string-join ":" f v))
                                   (cmd (string-join ":" f* v*))
                                   ...)))))
    ((_ (tag fv0 fv0* ...) (tag* fv* fv** ...) ...)
     (let ((t "style"))
       (wrap (open-tag t '() #f) (close-tag t) 
         (style single (tag fv0 fv0* ...))
         (style single (tag fv* fv** ...))
         ...)))))

(define-syntax style-attr
  (syntax-rules ()
    ((_ (f* v*) ...)
     (let ((f* (symbol->string 'f*))
           ...)
       (string-join ";"
         (string-append f* ":" v*)
         ...)))))

; vim: lispwords+=string-join
