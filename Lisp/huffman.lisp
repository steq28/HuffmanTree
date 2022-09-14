;(he-generate-huffman-tree '((a . 5) (b . 4) (c . 1)))
(defstruct node value sinistra destra)

(defun he-generate-huffman-tree (symbols-n-weights)
    (cond ((null symbols-n-weights) (null))
        (T (crea_albero symbols-n-weights '()))
    )
)

(defun crea_albero (lista albero)
    (write (cdr lista))
    (cond
        ((numberp (cdr lista))
            albero)
        (T 
            ;(sort lista 'ordina)
            ;(write lista)
            ;(terpri)
            ;(write (unisci lista))
            ;(terpri)
            ;;(write (cdr (cdr lista)))
            ;(terpri)
            ;(write (push (unisci lista) (cdr (cdr lista))))
            (terpri)
            (terpri)
            (crea_albero (if (null (cdr (cdr lista))) (unisci lista) (push (unisci lista) (cdr (cdr lista))) ) (list (unisci lista) albero))
        )
    )
)

(defun ordina (a b)

  (< (cdr a) (cdr b)))

(defun unisci (lista)
    (cons (list (car (car lista)) (car (second lista))) (+ (cdr (car lista)) (cdr (second lista))))
)
