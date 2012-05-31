(load "html.scm")

; macros {{{

(define-syntax struct-tags
  (syntax-rules ()
    ((_ id id* ...)
     (mk-tags (id struct)
           (id* struct)
           ...))))

(define-syntax line-tags
  (syntax-rules ()
    ((_ id id* ...)
     (mk-tags (id line)
           (id* line)
           ...))))

(define-syntax std-tags
  (syntax-rules ()
    ((_ id id* ...)
     (mk-tags (id std)
           (id* std)
           ...))))

(define-syntax stub-tags
  (syntax-rules ()
    ((_ id id* ...)
     (mk-tags (id stub)
           (id* stub)
           ...))))

(define-syntax stub/br-tags
  (syntax-rules ()
    ((_ id id* ...)
     (mk-tags (id stub/br)
           (id* stub/br)
           ...))))

; }}}

; main
(struct-tags html head body)
(line-tags title p img)

(line-tags h1 h2 h3 h4 h5 h6)

(stub-tags hr br)

; formats
(std-tags b i big em small strong sub sup)
(std-tags ins del code kbd samp tt var pre)
(std-tags abbr acronym address bdo cite dfn)
(std-tags blockquote q)

; forms
(struct-tags form textarea select optgroup fieldset object)
(line-tags button option label)
(stub/br-tags input)

(struct-tags div ol ul applet frameset iframe map menu)

(stub/br-tags base area frame link meta param)
(line-tags caption legend li col colgroup td dd dt span th)

(struct-tags noframes noscript)

(struct-tags script table tbody tr tfoot thead)

; vim: lispwords+=define-tag,define-tags
