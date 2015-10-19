;;;; Bertoldi David 735213
;;;; Garasi Gabriele Francesco 735964

;;;; ----- LISP URI PARSER -----

;;; A Common Lisp Library that builds structures
;;; that rappresents URI from strings
;;; The library accepts 6 types of URI
;;; if any part of the syntax results wrong 
;;; it will be generated an error message 

;; structure definition
;; :scheme :userinfo :host :port :path :query :fragment
(defstruct uri scheme userinfo host port path query fragment)

;; The main function
;; parse-uri:  string --> list
(defun parse-uri (stringa)
 (let ((lista (coerce stringa 'list)))
       (if (null lista) (error "Empty is not a URI")
       (check-scheme lista))))

;; An helper function: it makes results more comprehensible
(defun magitrasforma (lista)
 (if (null lista) nil
     (coerce lista 'string)))


;;; The code below defines the atoms of an URI
;;; <identifier, host-identifier, digit, query, fragment>
(defun is-query (lista)
(if (null lista) nil
 (eval (cons 'and (mapcar (lambda (char) (if (eq char #\#) nil t)) lista)))))

(defun is-fragment (lista)
(if (null lista) nil
 (eval (cons 'and (mapcar 'characterp lista)))))

(defun is-char-id (char)
(if (null char) nil
 (if (or (eq char #\/) 
          (eq char #\?)
          (eq char #\#)
          (eq char #\@)
          (eq char #\:)) nil t)))

(defun is-char-host (char)
 (if (or (eq char #\.)
         (eq char #\/)
         (eq char #\?)
         (eq char #\#)
         (eq char #\@)
         (eq char #\:)) nil t))

(defun is-identificatore (lista)
 (eval (cons 'and (mapcar (lambda (char) (if (or (eq char #\/)
                                                 (eq char #\?)
                                                 (eq char #\#)
                                                 (eq char #\@)
                                                 (eq char #\:)) nil t)) 
                                                  lista))))

(defun is-identificatore-host (lista)
 (eval (cons 'and (mapcar (lambda (char) (if (or (eq char #\.)
                                                 (eq char #\/)
                                                 (eq char #\?)
                                                 (eq char #\#)
                                                 (eq char #\@)
                                                 (eq char #\:)) nil t)) 
                                                  lista))))

(defun is-digit (lista)
 (eval (cons 'and (mapcar (lambda (num) (if (null (to-number num)) nil 
                                             t)) lista))))

;; this function (with its helper) checks the propriety of an IP address
;; basically it's useless
;; {IP} c {HOST}
(defun is-ip (ip)
 (if (is-ip-h (ipip (coerce (ipify ip) 'string))) 
     (if (and (integerp (to-number (car ip))) 
              (integerp (to-number (car (last ip))))) 
         t nil) 
 nil))


(defun ipify (ip)
 (if (integerp (to-number (car ip))) (cons (car ip) (ipify (cdr ip)))
     (if (eq (car ip) #\.) (if (eq (car (cdr ip)) #\.) nil
                           (cons #\Space (ipify (cdr ip)))) 
         nil)))

(defun ipip (ip)
 (with-input-from-string (s ip)
  (cons (read s nil 'eof) 
        (cons (read s nil 'eof) 
              (cons (read s nil 'eof) 
                    (cons (read s nil 'eof) 
                          (cons (read s nil 'eof) nil)))))))
                           
(defun is-ip-h (ip)
 (if (and (integerp (in-position ip 1)) (<= (in-position ip 1) 255)
          (integerp (in-position ip 2)) (<= (in-position ip 2) 255)
          (integerp (in-position ip 3)) (<= (in-position ip 3) 255)
          (integerp (in-position ip 4)) (<= (in-position ip 4) 255)
          (eq (in-position ip 5) 'eof)) t nil))

;; A little function that transforms #\numbers in numbers
(defun to-number (n)
 (cond ((eq n #\0) 0)
       ((eq n #\1) 1)
       ((eq n #\2) 2)
       ((eq n #\3) 3)
       ((eq n #\4) 4)
       ((eq n #\5) 5)
       ((eq n #\6) 6)
       ((eq n #\7) 7)
       ((eq n #\8) 8)
       ((eq n #\9) 9)
       (t nil)))

;;; Below the syntactic definition of scheme, userinfo, path, host
(defun is-scheme (lista)
 (and (is-identificatore lista) (not (null lista))))

(defun is-userinfo (lista)
(if (null lista) nil
 (and (is-identificatore lista) (not (null lista)))))

(defun is-path (lista)
(if (null lista) nil
 (if (is-char-id (car lista)) (is-path2 lista) nil)))

(defun is-path2 (lista)
 (if (null lista) t
     (if (is-char-id (car lista)) (is-path2 (cdr lista))
         (if (eq (car lista) #\/) 
             (cond ((is-char-id (car (cdr lista))) (is-path2 (cdr lista)))
                                                   ((null (cdr lista)) t)
                                                   (t nil)) 
         nil))))

(defun is-host (lista)
(if (null lista) nil
(cond ((is-ip lista) t)
     ((is-char-host (car lista)) (is-host2 lista))
     (t nil))))

(defun is-host2 (lista)
 (if (null lista) t
     (if (is-char-host (car lista)) (is-host2 (cdr lista))
         (if (eq (car lista) #\.) (if (is-char-host (car (cdr lista))) 
                                      (is-host2 (cdr lista))
                                      nil) nil))))

(defun is-path-after-auth (lista)
(if (eq (car lista) #\/) t
 (if (eq (car lista) #\/) (if (is-path (cdr lista)) t nil) nil)))

(defun is-port (lista)
(if (null lista) nil
 (is-digit lista)))

;;; The core: definition of the automata that
;;; recognizes the syntax of a URI

(defun in-position (lista n)
 (if (= n 1) (car lista)
             (in-position (cdr lista) (- n 1))))

;; list-to-sym and sym-to-list "read" the string
(defun list-to-sym (lista sym)
(if (eval (cons 'or (mapcar (lambda (x) (eq x sym)) lista))) 
 (if (null lista) nil
  (if (eq (car lista) sym) nil
    (append (list (car lista)) (list-to-sym (cdr lista) sym)))) nil))

(defun sym-to-list (lista sym)
 (if (null lista) nil
  (if (eq (car lista) sym) (cdr lista)
      (sym-to-list (cdr lista) sym))))

;; check-scheme begin the parsing and
;; recognize the type of URIs
(defun check-scheme (lista)
 (let ((l (list-to-sym lista #\:))
       (s (sym-to-list lista #\:)))
  (if (is-scheme l) 
    (cond ((equal l '(#\m #\a #\i #\l #\t #\o)) 
                     (mailto-ss s (list l)))           
          ((equal l '(#\n #\e #\w #\s)) 
                         (news-ss s (list l)))
          ((or (equal l '(#\t #\e #\l))
               (equal l '(#\f #\a #\x))) 
                            (telfax-ss s (list l)))
           (t (if (and (eq (car s) #\/)
                       (eq (car (cdr s)) #\/))
                            (first-uri-type s (list l)) 
                            (second-uri-type s (list l)))))
  (error "Not valid SCHEME: SCHEME IS OBLIGATORY
          definition of SCHEME:
          scheme := <identifier>
          identifier := chars without {/ ? # @ :}"))))

;; the 1) type of URI
(defun first-uri-type (lista cumulo)
 (userinfo-opt (cdr (cdr lista)) cumulo))

;; the 2) type of URI
(defun second-uri-type (lista cumulo)
 (if (eq (car lista) #\/) 
     (path-opt (cdr lista) (append cumulo '(nil) 
                                          '(nil) 
                                          '(nil)))
     (path-opt lista (append cumulo '(nil) 
                                    '(nil) 
                                    '(nil)))))

;; the 3) type of URI
(defun mailto-ss (lista cumulo)
 (if (is-userinfo lista) (the-end (append cumulo (list lista)))
     (if (is-userinfo (list-to-sym lista #\@)) 
         (host-opt (sym-to-list lista #\@) 
                   (append cumulo (list (list-to-sym lista #\@))))
  (error "NOT VALID USERINFO: USERINFO IS OBLIGATORY
          definition of USERINFO:
          userinfo := <identifier>
          identifier := chars without {/ ? # @ :}"))))

;; the 4) type of URI
(defun news-ss (lista cumulo)
 (if (is-host lista) (the-end (append cumulo (list nil lista)))
  (error "NOT VALID HOST: HOST IS OBLIGATORY
          definition of HOST:
          host := <host-identifier> [ . <host-identifier>]*
          host-identifier := chars without {. / ? # @ :}")))

;;the 5) and the 6) type of URI
(defun telfax-ss (lista cumulo)
 (if (is-userinfo lista) (the-end (append cumulo (list lista)))
  (error "NOT VALID USERINFO: USERINFO IS OBLIGATORY
          definition of USERINFO:
          userinfo := <identifier>
          identifier := chars without {/ ? # @ :}")))

;; optional presence of ['@' userinfo]
(defun userinfo-opt (lista cumulo) 
 (let ((l (list-to-sym lista #\@))
       (s (sym-to-list lista #\@)))
 (if (is-userinfo l) (host-obb s (append cumulo (list l)))
 (if (and (null l) (not (eq (car lista) #\@)))
                     (if (null s)
                         (host-obb lista (append cumulo '(nil)))
                         (host-obb s (append cumulo '(nil) 
                                                     (list l))))
                      
  (error "NOT VALID USERINFO
          definition of USERINFO:
          userinfo := <identifier>
          identifier := chars without {/ ? # @ :}")))))

;; obligatory presence of host
(defun host-obb (lista cumulo)
 (if (is-host lista) (the-end (append cumulo 
                                     (list lista)))
  (cond ((is-host (list-to-sym lista #\:)) 
                (port-opt (sym-to-list lista #\:) 
                          (append cumulo 
                                  (list (list-to-sym lista #\:)))))
          ((is-host (list-to-sym lista #\/)) 
                (path-opt (sym-to-list lista #\/) 
                          (append cumulo 
                                  (list (list-to-sym lista #\/)) 
                                 '(nil)))) 
          ((is-host (list-to-sym lista #\?)) 
                (query-opt (sym-to-list lista #\?) 
                           (append cumulo 
                                   (list (list-to-sym lista #\?)) 
                                  '(nil) 
                                  '(nil))))
          ((is-host (list-to-sym lista #\#)) 
               (fragment-opt (sym-to-list lista #\#) 
                             (append cumulo 
                                     (list (list-to-sym lista #\#)) 
                                    '(nil) 
                                    '(nil) 
                                    '(nil))))
(t (error "NOT VALID HOST: HOST IS OBLIGATORY
          definition of HOST:
          host := <host-identifier> [ . <host-identifier>]*
          host-identifier := chars without {. / ? # @ :}")))))

;; optional presence of [':' port]
(defun port-opt (lista cumulo)
 (if (is-port lista) (the-end (append cumulo 
                                     (list lista)))
  (cond ((is-port (list-to-sym lista #\/)) 
                (path-opt (sym-to-list lista #\/) 
                          (append cumulo 
                                 (list (list-to-sym lista #\/)))))  
        ((is-port (list-to-sym lista #\?)) 
                (query-opt (sym-to-list lista #\?) 
                           (append cumulo 
                                   (list (list-to-sym lista #\?)) 
                                  '(nil))))
        ((is-port (list-to-sym lista #\#)) 
                (fragment-opt (sym-to-list lista #\#) 
                              (append cumulo 
                                      (list (list-to-sym lista #\#)) 
                                     '(nil) 
                                     '(nil))))
(t (error "NOT VALID PORT
          definition of PORT:
          port := <digit>
          digit := chars form 0 to 9")))))
 
;; optional presence of [path]
(defun path-opt (lista cumulo)
 (cond ((is-path lista) (the-end (append cumulo 
                                        (list lista))))
       ((eq (car lista) #\?) (query-opt (cdr lista) (append cumulo 
                                                          '(nil))))
       ((eq (car lista) #\#) (fragment-opt (cdr lista) 
                                           (append cumulo 
                                           '(nil) 
                                           '(nil))))
       ((null lista) (the-end cumulo)) 
       ((is-path (list-to-sym lista #\?)) 
                 (query-opt (sym-to-list lista #\?) 
                            (append cumulo 
                            (list (list-to-sym lista #\?)))))
       ((is-path (list-to-sym lista #\#)) 
                (fragment-opt (sym-to-list lista #\#)
                              (append cumulo 
                                      (list (sym-to-list lista #\#)) 
                                     '(nil))))
(t (error "NOT VALID PATH 
          definition of PATH: 
          path :=  <identifier> [ / <identifier>]* '/'
          identifier := chars without {/ ? # @ :}"))))

;; optional presence of ['?' query]
(defun query-opt (lista cumulo)
 (cond ((is-query lista) (the-end (append cumulo (list lista))))
       ((is-query (list-to-sym lista #\#)) 
                  (fragment-opt (sym-to-list lista #\#)
                                (append cumulo 
                                (list (list-to-sym lista #\#)))))
(t (error "NOT VALID QUERY
          definition of QUERY:
          query := chars without {#}"))))

;; optional presence of ['#' fragment]
(defun fragment-opt (lista cumulo)
 (cond ((is-fragment lista) (the-end (append cumulo (list lista))))
(t (error "NOT VALID FRAGMENT
          definition of FRAGMENT:
          fragment := chars"))))

;; optional presence of ['@' host]
(defun host-opt (lista cumulo)
 (if (is-host lista) (the-end (append cumulo (list lista)))
  (error "NOT VALID HOST
          definition of HOST:
          host := <host-identifier> [ . <host-identifier>]*
          host-identifier := chars without {. / ? # @ :}")))

;; The final function (TFF)
(defun the-end (cumulo)
 (make-uri :scheme (magitrasforma (in-position cumulo 1))
           :userinfo (magitrasforma (in-position cumulo 2))
           :host (magitrasforma (in-position cumulo 3))
           :port (magitrasforma (in-position cumulo 4))
           :path (magitrasforma (in-position cumulo 5))
           :query (magitrasforma (in-position cumulo 6))
           :fragment (magitrasforma (in-position cumulo 7))))
