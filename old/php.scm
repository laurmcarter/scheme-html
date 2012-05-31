(load "html.scm")

(define-syntax php
  (syntax-rules ()
    ((_ b* ...)
     (let ((open "<?php")
           (close "?>"))
       (string-join " "
         open
         b*
         ...
         close)))))

(define-syntax php-if
  (syntax-rules ()
    ((_ (t t* ...) (c c* ...) (a a* ...))
     (php "if (" t t* ... ") {"
                 c c* ...
                 "} else {"
                 a a* ...
                 "}"))
    ((_ (t t* ...) (c c* ...))
     (php "if (" t t* ... ") {"
                 c c* ...
                 "}"))))

(define-syntax php-break
  (syntax-rules ()
    ((_ s s* ...)
     (string-append "?>" s s* ... "<?php"))))

(define-syntax cmd
  (syntax-rules ()
    ((_ b* ...)
     (string-append (string-join " " b* ...) ";"))))

(define-syntax define-cmd
  (syntax-rules ()
    ((_ (c cstr))
     (define-syntax c
       (syntax-rules ()
         ((_ b* (... ...))
          (cmd cstr b* (... ...))))))
    ((_ (c cstr) (c* cstr*) ...)
     (begin
       (define-cmd c cstr)
       (define-cmd c* cstr*)
       ...))))

(define lookup
  (lambda (a i)
    (let ((i (if (number? i) (number->string i) (squote i))))
      (string-append a (bracket i)))))

(define-syntax call
  (syntax-rules ()
    ((_ f a a* ...)
     (string-append f "(" (string-join ", " a a* ...) ")"))))
