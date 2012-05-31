(load "html2.scm")

; boilerplate {{{

(define-syntax define-formats
  (syntax-rules ()
    ((_ t)
     (define-tag t
       (stub? #f)
       (format 'none)
       (attrs std)))
    ((_ t t* ...)
     (begin
       (define-formats t)
       (define-formats t* ...)))))

(define-syntax define-headers
  (syntax-rules ()
    ((_ h)
     (define-tag h
       (stub? #f)
       (format 'line)
       (attrs std (align "left" "right" "center" "justified"))))
    ((_ h h* ...)
     (begin
       (define-headers h)
       (define-headers h* ...)))))

(define-syntax define-quotations
  (syntax-rules ()
    ((_ q)
     (define-tag q
       (stub? #f)
       (format 'none)
       (attrs std (cite))))
    ((_ q q* ...)
     (begin
       (define-quotations q)
       (define-quotations q* ...)))))

; }}}

; custom {{{

(define-syntax define-structs
  (syntax-rules (attrs)
    ((_ id (attrs e e* ...))
     (define-tag id
       (stub? #f)
       (format 'block)
       (attrs e e* ...)))
    ((_ (id as) (id* as*) ...)
     (begin
       (define-structs id as)
       (define-structs id* as*)
       ...))))

(define-syntax define-stubs
  (syntax-rules (attrs)
    ((_ id (attrs e e* ...))
     (define-tag id
       (stub? #t)
       (format 'none)
       (attrs e e* ...)))
    ((_ (id as) (id* as*) ...)
     (begin
       (define-stubs id as)
       (define-stubs id* as*)
       ...))))

(define-syntax define-std-tags
  (syntax-rules (attrs)
    ((_ t (attrs e e* ...))
     (define-tag id
       (stub? #f)
       (format 'none)
       (attrs e e* ...)))
    ((_ t t* ...)
     (begin
       (define-std-tags t)
       (define-std-tags t*)
       ...))))

; }}}

; main tags {{{

(define-headers h1 h2 h3 h4 h5 h6)

(define-formats b i big em small strong sub sup)
(define-formats ins del code kbd samp tt var pre)
(define-formats abbr acronym address bdo cite dfn)

(define-quotations blockquote q)

(define-stubs hr br)

(define-structs
  (html (attrs (content) (scheme) (http-equiv)))
  (head (attrs (profile) (lang) (dir "ltr" "rtl")))
  (body (attrs std)))

(define-tag title
  (stub? #f)
  (format 'line)
  (attrs (lang) (dir "ltr" "rtl")))

(define-tag p
  (stub? #f)
  (format 'line)
  (attrs std (align "left" "right" "center" "justified")))

(define-tag a
  (stub? #f)
  (format 'line)
  (attrs std
         (name)
         (href)
         (hreflang)
         (type)
         (rel ("alternate" "appendix" "bookmark" "chapter" "contents" "copyright" "glossary" "help" "home" "index" "next" "prev" "section" "start" "stylesheet" "subsection"))
         (rev ("alternate" "appendix" "bookmark" "chapter" "contents" "copyright" "glossary" "help" "home" "index" "next" "prev" "section" "start" "stylesheet" "subsection"))
         (charset)
         (shape)
         (coords)
         (target ("_blank" "_self" "_top" "_parent"))
         (accesskey)
         (tabindex)))

(define-tag img
  (stub? #f)
  (format 'line)
  (attrs std
         (name)
         (longdesc)
         (src)
         (alt)
         (ismap)
         (usemap)
         (width)
         (height())
         (align "left" "center" "right" "justify")
         (border)))

; }}}

; forms {{{

(define-tag form
  (stub? #f)
  (format 'block)
  (attrs std
         (action)
         (method)
         (enctype "application/x-www-form-urlencoded" "multipart/form-data")
         (accept-charset)
         (accept)
         (name)
         (target)))

(define-tag input
  (stub? #t)
  (format 'line)
  (attrs std
         (type "text" "password" "checkbox" "radio" "submit" "reset" "file" "hidden" "image" "button")
         (name)
         (value)
         (size)
         (maxlength)
         (checked)
         (src)
         (accept)
         (readonly)
         (disabled)
         (tabindex)
         (accesskey)
         (ismap)
         (usemap)))

(define-tag textarea
  (stub? #f)
  (format 'block)
  (attrs std
         (name)
         (rows)
         (cols)
         (readonly)
         (disabled)
         (tabindex)))

(define-tag button
  (stub? #f)
  (format 'line)
  (attrs std
         (name)
         (value)
         (type)
         (disabled)
         (accesskey)
         (tabindex)))

(define-tag select
  (stub? #f)
  (format 'block)
  (attrs std
         (name)
         (size)
         (multiple)
         (tabindex)))

(define-tag option
  (stub #f)
  (format 'line)
  (attrs std
         (selected)
         (value)
         (label)
         (tabindex)
         (disabled)))

(define-tag optgroup
  (stub #f)
  (format 'block)
  (attrs std
         (label)
         (disabled)))

(define-tag fieldset
  (stub? #f)
  (format 'block)
  (attrs std
         (accesskey)))

(define-tag label
  (stub? #f)
  (format 'line)
  (attrs std
         (for)
         (accesskey)))

; }}}

; lists {{{

(define-tag div
  (stub? #f)
  (format 'block)
  (attrs std
         (align "left" "right" "center" "justified")))

(define-tag li
  (stub? #f)
  (format 'line)
  (attrs std
         (value)))

(define-tag ol
  (stub? #f)
  (format 'block)
  (attrs std))

(define-tag ul
  (stub? #f)
  (format 'block)
  (attrs std))

; }}}

#!eof

; <applet> {{{
; }}}

; <area> {{{
; }}}

; <base> {{{
; }}}

; <basefont> {{{
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

; <param> {{{
; }}}

; <script> {{{
; }}}

; <span> {{{
; }}}

; <table> {{{
; }}}

; <tbody> {{{
; }}}

; <td> {{{
; }}}

; <tfoot> {{{
; }}}

; <th> {{{
; }}}

; <thead> {{{
; }}}

; <tr> {{{
; }}}


; vim: lispwords+=define-tag,define-tags
