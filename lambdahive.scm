(use-modules (commonmark))

(define sample-doc
  "# A CommonMark document
==============

1. here is a list
2. with another item

    this is some code

A regular paragraph")

;; Parse the CommonMark
(define doc-sxml (commonmark->sxml sample-doc))

(equal? "hello" "hello")

(define any
  (lambda (ls)
    (cond [(null? ls) #fc]
	  [(car ls) (any (cdr ls))]
	  [(not (car ls)) #t])))

(define in-list?
  (lambda (memb xs)
    (if (eq? (memq memb xs) #f)
	#f
	#t)))

(define html-header?
  (lambda (tag)
    (in-list? tag '(h1 h2 h3 h4 h5 h6))))

(define html-list?
  (lambda (tag)
    (in-list? tag '(ol ul li))))

(define html-paragraph?
  (lambda (tag)
    (eq? tag 'p)))

(define enclosable-tag?
  (lambda (tag)
    (map (lambda (p?) (p? tag))
	 (list
	  html-header?
	  html-list?
	  html-paragraph?))))

(define get-html
  (lambda (sxml)
    (cond
      [(null? sxml) ""]
      ;; Root of the document
      [(not (symbol? (car sxml)))
       (string-append (get-html (car sxml)) (get-html (cdr sxml)))] 
      ;; Basically, any HTML tag such as <h1> and <h2>
      ;; are enclosable.
      [(any (enclosable-tag? (car sxml)))
       (html-enclose sxml)])))

(define enclose-aux
  (lambda (tag str)
    (let ((tag-str (symbol->string tag)))
      (string-append "<" tag-str ">" str "</" tag-str ">"))))

(define html-enclose
  (lambda (sxml)
    (cond
      ;; We have arrived at the root node. We can enclose.
      [(string? (cadr sxml))
       (enclose-aux (car sxml) (cadr sxml))]
      [(list? (cadr sxml))
       (enclose-aux (car sxml) (get-html (cadr sxml)))])))
	
;; Example
(display (get-html doc-sxml))
