(define-cmd (echo "echo"))

(export-doc
  (author "Kyle Carter")
  (html
    (head
      (title "PHP Test")
      (style
        (body
          (background-color "green"))))
    (body
      (php
        (echo (squote (p (b "Hello World")))))
      (form ((action "action.php")
             (method "post"))
            (p "Your name: " (input ((type "text")
                                     (name "name")
                                     (
      (p 
        (php (echo (lookup "$_SERVER" "HTTP_USER_AGENT"))))
      (php-if ((call "strpos" (lookup "$_SERVER" "HTTP_USER_AGENT") (squote "MSIE"))
               "!== FALSE")
              ((echo (squote "You are using Internet Explorer." (br))))
              ((echo (squote "You are not using Internet Explorer." (br)))))
      (a ((href "http://www.w3schools.com")) "This is a link"))))
