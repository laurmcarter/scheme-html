(load "pmatch.scm")

(define php-out-file (make-parameter 'dummy))
(define css-out-file (make-parameter 'dummy))

; macros {{{

(define-syntax define-type
  (syntax-rules ()
    ((_ (id f* ...))
     (define id
       (lambda (f* ...)
         `(id ,f* ...))))))

(define-syntax doc
  (syntax-rules ()
    ((_ b b* ...)
     (let ((php-out (php-out-file))
           (css-out (css-out-file)))
       (eval-branch 'b css-out php-out)
       (eval-branch 'b* css-out php-out)
       ...))))

(define string-join
  (lambda (sep)
    (lambda s*
      (cond
        ((null? s*) "")
        (else (apply string-append
                     (cons (car s*)
                           (map (lambda (s)
                                  (str-maybe sep s))
                                (cdr s*)))))))))

;(define-syntax string-join
;  (syntax-rules ()
;    ((_ sep s) s)
;    ((_ sep s s^ s* ...)
;     (string-append
;       s
;       (str-maybe sep s^)
;       (str-maybe sep s*)
;       ...))))

(define-syntax extend-env
  (syntax-rules ()
    ((_ (x a) (x* a*) ...)
     (env `((,x ,a) (,x* ,a*) ... . ,(env))))))

(define-syntax define-tags
  (syntax-rules ()
    ((_ (id s f a) (id* s* f* a*) ...)
     (begin
       (define-tag id s f a)
       (define-tag id* s* f* a*)
       ...))))

(define-syntax define-tag
  (syntax-rules (stub? format)
    ((_ id (stub? s) (format f))
     (extend-env ('id
       (let ((fmt (pmatch f
                    (block 0)
                    (line 1)
                    (none 2))))
         (make-tag (symbol->string 'id) s fmt)))))))

(define-syntax mk-tag
  (syntax-rules ()
    ((_ id code)
     (define-tag id
       (stub? (parse-code 'stub 'code))
       (format (parse-code 'fmt 'code))))))

(define-syntax mk-tags
  (syntax-rules ()
    ((_ (id code) (id* code*) ...)
     (begin
       (mk-tag id code)
       (mk-tag id* code*)
       ...))))

; wrapping defs {{{

(define-syntax wrap
  (syntax-rules ()
    ((_ open close s* ...)
     (string-append open s* ... close))))

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

; }}}

;(define-syntax define-tag
;  (syntax-rules (stub? format attrs)
;    ((_ id (stub? s) (format f) (attrs e e* ...))
;     (define id
;       (let ((fmt (pmatch f
;                    (block 0)
;                    (line 1)
;                    (none 2)))
;             (as (attributes e e* ...)))
;         (make-tag (symbol->string 'id) s fmt as))))))

(define-syntax attributes
  (syntax-rules (std)
    ((_ std e* ...)
     (attributes (class) (id) (title) (style) (dir "ltr" "rtl") (lang) e* ...))
    ((_ (a v* ...) ...)
     (let ((a (symbol->string 'a))
           ...)
       `((,a ,v* ...) ...)))))

; }}}

; types/env {{{

(define-type (empty-env))

(define-type (closure x body env))

(define env (make-parameter (empty-env)))

;(define-record tag (name stub? formatting attrs))

(define-record tag (name stub? format))

; }}}

(define eval-branch
  (lambda (e css php)
    (pmatch e
      [(css . ,e*)
       (with-output-to-file css
         (lambda ()
           (display (eval-css e)))
         'replace)]
      [,else
       (with-output-to-file php
         (lambda ()
           (display (eval-html e)))
         'replace)])))

(define eval-html
  (lambda (e)
    (pmatch e
      [,l (guard (literal? l)) l]
      [,b (guard (boolean? b)) (if b "True" "False")]
      [,x (guard (symbol? x)) (lookup x)]
      [(php . ,b*) (eval-php b*)]
      [(,t (attrs . ,fs) . ,b*)
       (let ((t (eval-html t))
             (fs (eval-attrs fs))
             (b* (eval/join b*)))
         (apply-tag t fs b*))]
      [(,t . ,b*)
       (let ((t (eval-html t))
             (fs (eval-attrs '()))
             (b* (eval/join b*)))
         (apply-tag t fs b*))])))

(define eval/join
  (lambda (b*)
    (cond
      ((null? b*) "")
      (else (let ((b (car b*))
                  (b* (cdr b*)))
              (string-append
                (eval-html b)
                (eval/join b*)))))))

(define eval-attrs
  (lambda (fs)
    (cond
      ((null? fs) '())
      (else (let ((f (symbol->string (caar fs)))
                  (v (cadar fs))
                  (fs (cdr fs)))
              (cons `(,f . ,v) (eval-attrs fs)))))))

(define eval-php
  (lambda (b*)
    (apply (string-join " ") (cons "php" (map symbol->string b*)))))

(define eval-css
  (lambda (e)
    (pmatch e
      [(css . ,e*)
       (apply (string-join "\n")
              (map eval-css e*))]
      [(,t . ,fs)
       (let ((t (symbol->string t))
             (fs (css-fmt (eval-attrs fs))))
         ((string-join "\n")
          t
          "{"
          fs
          "}"))])))

(define css-fmt
  (lambda (fs)
    (cond
      ((null? fs) "")
      (else (let ((f (caar fs))
                  (v (cdar fs)))
              (string-append
                (string-append f ": " v ";")
                (str-maybe "\n" (css-fmt (cdr fs)))))))))

; helpers {{{

(define literal?
  (lambda (x)
    (or (string? x)
        (number? x))))

(define lookup
  (lambda (y)
    (apply-env (env) y)))

(define apply-env
  (lambda (env y)
    (pmatch env
      [(empty-env) (empty-env-error y)]
      [((,x ,a) . ,env)
       (if (eq? x y)
         a
         (apply-env env y))])))

(define parse-code
  (lambda (type code)
    (pmatch code
      [stub
        (pmatch type
          [stub #t]
          [fmt 'none])]
      [stub/br
        (pmatch type
          [stub #t]
          [fmt 'line])]
      [struct
        (pmatch type
          [stub #f]
          [fmt 'block])]
      [line
        (pmatch type
          [stub #f]
          [fmt 'line])]
      [std
        (pmatch type
          [stub #f]
          [fmt 'none])])))

(define apply-tag
  (lambda (t fs b*)
    (let ((open (open-tag t fs))
          (close (close-tag t)))
      (cond
        ((tag-stub? t)
         (if (not (string=? "" b*))
           (nonempty-stub-error (tag-name t))
           open))
        (else ((string-join " ") open b* close))))))

(define open-tag
  (lambda (t fs)
    (let ((attrs (fmt-attrs fs))
          (f (tag-format t)))
      (string-append
        (if (= f 0) "\n" "")
        (angle-bracket
          ((string-join " ")
            (tag-name t)
            attrs
            (if (tag-stub? t) "/" "")))
        (if (= f 0) "\n" "")))))

(define close-tag
  (lambda (t)
    (let ((f (tag-format t)))
      (if (tag-stub? t)
        ""
        (string-append
          (if (= f 0) "\n" "")
          (angle-bracket
            (string-append "/" (tag-name t)))
          (if (< f 2) "\n" ""))))))

(define fmt-attrs
  (lambda (fs)
    (cond
      ((null? fs) "")
      (else (let ((f (caar fs))
                  (v (cdar fs)))
              ((string-join " ")
                (string-append f "=" (dquote v))
                (fmt-attrs (cdr fs))))))))

(define str-maybe
  (lambda (sep s)
    (if (eq? s "")
      ""
      (string-append sep s))))

; }}}

; errors {{{

(define nonempty-stub-error
  (lambda (name)
    (errorf 'apply-tag "Stub tag '~a' treated as nonstub" name)))

(define empty-env-error
  (lambda (y)
    (errorf 'apply-env "Unbound variable: ~a" y)))

; }}}

; vim: lispwords+=define-type,string-join
