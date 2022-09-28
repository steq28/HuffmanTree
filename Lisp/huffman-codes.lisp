;;Stefano Quaggio 866504
;;Glauco Rinaldi 851624

;;-*-Mode:Lisp-*-

(defun he-generate-huffman-tree (symbols-n-weights)
  (generate-tree (couples symbols-n-weights)))

(defun generate-tree (symbols-n-weights)
  (if (null (cdr symbols-n-weights))
      (car symbols-n-weights)
      (let ((a  (stable-sort symbols-n-weights 'maggiore)))
        (generate-tree
         (append (list (cons
                        (app (list (first (first a))
                                   (first (first (rest a)))))
                        (cons (+ (first (rest (first a)))
                                 (first (rest (first (rest a)))))
                              (cons (first a)
                                    (list (first (rest a)))))
                        ))
                 (rest (rest a))
                 )))))


; tramuta una lista di coppie "cons cells" in lista di coppie "list"
(defun couples (l)
  (mapcar #'cons-list l))


(defun cons-list (l)
  (list (car l) (cdr l)))

; appiattisce lista
(defun app (x)
  (cond ((null x) x)
        ((atom x) (list x))
        (T (append (app (first x)) (app (rest x))))))

(defun maggiore (L1 L2)
  (if (<  (car (cdr L1)) (car (cdr L2)))
      t
      nil))

; i nodi sono della forma: (K V L R)
(defun he-encode (message huffman-tree)
  (labels ((encode-1 (message current-node)
             (unless (null message)
               (cond ((leaf-p current-node) (encode-1 (rest message)
                                                      huffman-tree))

                     ((contains-left (first message) current-node)
                      (cons 0 (encode-1 message (left-tree current-node))))

                     ((contains-right (first message) current-node)
                      (cons 1 (encode-1 message (right-tree current-node))))

                     (t (error "unknown character"))))))
    (encode-1 message huffman-tree)))



(defun he-decode (bits huffman-tree)
  (labels ((decode-1 (bits current-branch)
             (unless (null bits)
               (let ((next-branch (choose-branch (first bits)
                                                 current-branch)))
                 (if (leaf-p next-branch)
                     (cons (leaf-symbol next-branch) (decode-1 (rest bits)
                                                               huffman-tree))
                     (decode-1 (rest bits) next-branch))))))
    (decode-1 bits huffman-tree)))


(defun choose-branch (bit branch)
  (cond ((= 0 bit) (left-tree branch))
        ((= 1 bit) (right-tree branch))
        (t (error "Found bad bit ~D. only 0 or 1 allowed" bit))))


; ritorna t se 'char' è nel nodo SINISTRO
(defun contains-left (char node)
  (let ((left-key (node-key (left-tree node))))
    (or (eql (if (atom left-key) left-key (car left-key))  char)
        (if (not (atom left-key)) (find char left-key)))))


; ritorna t se 'char' è nel nodo DESTRO
(defun contains-right (char node)
  (let ((right-key (node-key (right-tree node))))
    (or (eql (if (atom right-key) right-key (car right-key))  char)
        (if (not (atom right-key)) (find char right-key)))))


; ritorna il simbolo del nodo foglia
(defun leaf-symbol (node)
  (when (leaf-p node)
    (first node)))

; sotto albero sinistro
(defun left-tree (node)
  (unless (leaf-p node)
    (third node)))

; sotto albero destro
(defun right-tree (node)
  (unless (leaf-p node)
    (fourth node)))

; chiave nodo
(defun node-key (node)
  (first node))

; valore nodo
(defun node-value (node)
  (second node))

; T quando 'node' è una foglia
(defun leaf-p (node)
  (and (eq nil (third node))
       (eq nil (fourth node))))

; lista di coppie symbol-bits
(defun he-generate-symbol-bits-table (huffman-tree)
  (labels ((sym-bits (symbols)
	     (unless (null symbols)
	       (cons (list (car symbols)
			   (he-encode (list (car symbols)) huffman-tree))
		     (sym-bits (rest symbols))))))
    (sym-bits (node-key huffman-tree))))


; legge il contenuto di filename e lo codifica
(defun he-encode-file (file-name huffman-tree)
  (he-encode (file-to-list file-name) huffman-tree))


; ritorna una lista del contenuto del file
(defun file-to-list (file-name)
  (with-open-file (in file-name :direction :input
                      :if-does-not-exist :error)
    (read-list-from in)))


(defun read-list-from (input)
  (let ((e (read input nil 'end)))
    (unless (eq e 'end)
      (cons e (read-list-from input)))))


(defun he-print-huffman-tree (huffman-tree &optional (indent-level 0))
  (unless (null huffman-tree)
    (format t "~S ==> ~D ~% ~S ~% ~S ~%" (first huffman-tree)
            (second huffman-tree)
            (he-print-huffman-tree (third huffman-tree)
                                   (+ 1 indent-level))
            (he-print-huffman-tree (fourth huffman-tree)
                                   (+ 1 indent-level)))))
