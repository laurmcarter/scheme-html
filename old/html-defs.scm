(load "html.scm")
(load "php.scm")

(define-syntax define-tag-std
  (syntax-rules ()
    ((_ t (e* ...))
     (define-tag t
       ((class ())
        (id ())
        (title ())
        (style ())
        (dir ("ltr" "rtl"))
        (lang ())
        e*
        ...)))))

(define-tag html
  ((content ())
   (scheme ())
   (http-equiv ())))

(define-tag title
  ((lang ())
   (dir ())))

(define-tag head
  ((profile ())
   (lang ())
   (dir ())))

; macros {{{

(define-syntax define-formats
  (syntax-rules ()
    ((_ t)
     (define-tag-std t ()))
    ((_ t t* ...)
     (begin
       (define-formats t)
       (define-formats t* ...)))))

(define-syntax define-headers
  (syntax-rules ()
    ((_ h)
     (define-tag-std h ((align ("left" "right" "center" "justified")))))
    ((_ h h* ...)
     (begin
       (define-headers h)
       (define-headers h* ...)))))

(define-syntax define-quotations
  (syntax-rules ()
    ((_ q)
     (define-tag-std q ((cite ()))))
    ((_ q q* ...)
     (begin
       (define-quotations q)
       (define-quotations q* ...)))))

; }}}

; tags1 {{{

(define hr
  (lambda () "<hr />"))

(define br
  (lambda () "<br />"))

(define-tag-std body ())

(define-tag-std p ((align ("left" "right" "center" "justified"))))

(define-tag-std a
  ((name ())
   (href ())
   (hreflang ())
   (type ())
   (rel ("alternate" "appendix" "bookmark" "chapter" "contents" "copyright" "glossary" "help" "home" "index" "next" "prev" "section" "start" "stylesheet" "subsection"))
   (rev ("alternate" "appendix" "bookmark" "chapter" "contents" "copyright" "glossary" "help" "home" "index" "next" "prev" "section" "start" "stylesheet" "subsection"))
   (charset ())
   (shape ())
   (coords ())
   (target ("_blank" "_self" "_top" "_parent"))
   (accesskey ())
   (tabindex ())))

(define-tag-std img
  ((name ())
   (longdesc ())
   (src ())
   (alt ())
   (ismap ())
   (usemap ())
   (width ())
   (height())
   (align ("left" "center" "right" "justify"))
   (border ())))

; }}}

(define-headers h1 h2 h3 h4 h5 h6)

(define-formats b i big em small strong sub sup ins del)
(define-formats code kbd samp tt var pre)
(define-formats abbr acronym address bdo cite dfn)

(define-quotations blockquote q)

; <form> {{{
(define-tag-std form
  ((action ())
   (method ())
   (enctype ("application/x-www-form-urlencoded"
             "multipart/form-data"))
   (accept-charset ())
   (accept ())
   (name ())
   (target ())))
; }}}

; <input> {{{
(define-tag-std input
  ((type ("text" "password" "checkbox" "radio" "submit" "reset" "file" "hidden" "image" "button"))
   (name ())
   (value ())
   (size ())
   (maxlength ())
   (checked ())
   (src ())
   (accept ())
   (readonly ())
   (disabled ())
   (tabindex ())
   (accesskey ())
   (ismap ())
   (usemap ())))
; }}}

; <div> {{{
; }}}

; <li> {{{
; }}}

; <ol> {{{
; }}}

; <ul> {{{
; }}}


; <applet> {{{
; }}}

; <area> {{{
; }}}

; <base> {{{
; }}}

; <basefont> {{{
; }}}

; <button> {{{
; }}}

; <caption> {{{
; }}}

; <center> {{{
; }}}

; <col> {{{
; }}}

; <colgroup> {{{
; }}}

; <dd> {{{
; }}}

; <dir> {{{
; }}}

; <dl> {{{
; }}}

; <dt> {{{
; }}}

; <fieldset> {{{
; }}}

; <font> {{{
; }}}

; <frame> {{{
; }}}

; <frameset> {{{
; }}}

; <iframe> {{{
; }}}

; <isindex> {{{
; }}}

; <label> {{{
; }}}

; <legend> {{{
; }}}

; <link> {{{
; }}}

; <map> {{{
; }}}

; <menu> {{{
; }}}

; <meta> {{{
; }}}

; <noframes> {{{
; }}}

; <noscript> {{{
; }}}

; <object> {{{
; }}}

; <optgroup> {{{
; }}}

; <option> {{{
; }}}

; <param> {{{
; }}}

; <script> {{{
; }}}

; <select> {{{
; }}}

; <span> {{{
; }}}

; <table> {{{
; }}}

; <tbody> {{{
; }}}

; <td> {{{
; }}}

; <textarea> {{{
; }}}

; <tfoot> {{{
; }}}

; <th> {{{
; }}}

; <thead> {{{
; }}}

; <tr> {{{
; }}}


; vim: lispwords+=define-tag,define-tag-std,define-tag-std
