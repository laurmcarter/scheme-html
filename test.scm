(php-out-file "test.php")
(css-out-file "test.css")

(doc
  (html
    (head
      (title "PHP Test")
      (link (attrs
              (rel "stylesheet")
              (type "text/css")
              (href "test.css"))))
    (body
      (p (h1 "Testing!"))
      (p (b "Hello World!"))))
  (css
    (body
      (background-color "green")
      (padding "0.5em")
      (margin-top "15%")
      (margin-left "15%")
      (margin-right "15%"))
    (h1
      (font-family
        "Garamond, \"Times New Roman\", serif")
      (font-size "200%"))))
