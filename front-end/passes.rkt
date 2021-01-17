#lang nanopass

#|
Compiladores 2021-1

Autores:
313245312 - González Alvarado Raúl
316255709 - Hernández Cano Alejandro
xxxxxxxxx - Miguel
xxxxxxxxx - Miren

Lenguajes y passes utilizados en el front-end del compilador
|#

(provide (all-defined-out))

;; Definición del lenguaje fuente
(define-language LF
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (list (l))
   (string (s))
   (type (t)))
  (Expr (e body)
    x
    pr
    c
    l
    s
    t
    (primapp pr e* ...)
    (define x e)
    (while [e0] e1)
    (for [x e0] e1)
    (begin e* ... e)
    (if e0 e1)
    (if e0 e1 e2)
    (lambda ([x* t*] ...) body* ... body)
    (let ([x* t* e*] ...) body* ... body)
    (letrec ([x* t* e*] ...) body* ... body)
    (list e* ...)
    (e0 e1 ...)))

;; Predicados (para list y string se usan los de racket)
(define (variable? x) (and (symbol? x) (not (primitive? x)) (not (constant? x))))

(define (primitive? x)
  (memq x '(+ - * / car cdr length and not or < > equal? iszero? ++ --)))

(define (constant? x)
  (or (integer? x)
      (char? x)
      (boolean? x)))

;; SISTEMA DE TIPOS
;; Int | Char | Bool | Lambda | List | (List of T) | (T → T)
(define (type? x) (or (b-type? x) (c-type? x)))
(define (b-type? x) (memq x '(Bool Char Int List Lambda)))
(define (c-type? x) (if (list? x) 
	(let* (
		[f (car x)]
		[s (cadr x)]
		[t (caddr x)])
	(or (and (equal? f 'List) (equal? s 'of) (type? t)) 
		(and (type? f) (equal? s '→) (type? t))))
	#f))

(define-parser parser-LF LF)

;; Lenguaje sin ifs de una sola rama
(define-language L1
  (extends LF)
  (Expr (e body)
        (- (if e0 e1))))
 
(define-parser parser-L1 L1)

;; Proceso 1: remove-one-armed-if
;; Fue un ejercicio de la práctica 3
(define-pass remove-armed-if : LF (ir) -> L1 ()
  (Expr : Expr (ir) -> Expr ()
    [(if ,[e0] ,[e1])
     `(if ,e0 ,e1 (void))]))

;; Lenguaje con strings como listas de caracteres
(define-language L2
  (extends L1)
  (terminals
   (- (string (s))))
  (Expr (e body)
        (- s)))

(define-parser parser-L2 L2)

;; Elimina strings de L1 y las convierte en listas de char
;; Fue un ejercicio de la práctica 3
;; TODO: Pendiente hasta que se confirme el manejo del constructor lista
(define-pass remove-string : L1 (ir) -> L2 ()
  (Expr : Expr (ir) -> Expr ()
        [,s (string->list s)]))

;; Definimos L3 (extiende de L2) que elimina los let y letrec de múltiples parámetros y
;; los deja con un solo parámetro.
(define-language L3
  (extends L2)
  (Expr (e body)
        (- (let ([x* t* e*] ...) body* ... body)
           (letrec ([x* t* e*] ...) body* ... body))
        (+ (let ([x* t* e*]) body* ... body)
           (letrec ([x* t* e*]) body* ... body))))


;; Currifica las expresiones let y letrec del lenguaje L2 y las convierte en let y letrec del lenguaje L3
;; Ejercicio de práctica 4
;;
;; curry-let: L2 -> L3
(define-pass curry-let : L2 (ir) -> L3 ()
  (Expr : Expr (ir) -> Expr ()
         [(let ([,x* ,t* ,[e*]] ...) ,[body*] ... ,[body])
          (let f ([x* x*] [t* t*] [e* e*] [body body] [body* body*])
            (if (<= (length x*) 1)
                `(let ([,(car x*) ,(car t*) ,(car e*)]) ,body* ... ,body)
                `(let ([,(car x*) ,(car t*) ,(car e*)]) ,(f (cdr x*) (cdr t*) (cdr e*) body body*))))]
         [(letrec ([,x* ,t* ,[e*]] ...) ,[body*] ... ,[body])
          (let f ([x* x*] [t* t*] [e* e*] [body body] [body* body*])
            (if (<= (length x*) 1)
                `(letrec ([,(car x*) ,(car t*) ,(car e*)]) ,body* ... ,body)
                `(letrec ([,(car x*) ,(car t*) ,(car e*)]) ,(f (cdr x*) (cdr t*) (cdr e*) body body*))))]))

;; Cambia los let que tiene como asignaciones una lambda y las convierte en letrec 
;; Ejercicio de práctica 4
;;
;; identify-assignments: L3 -> L3
(define-pass identify-assignments : L3 (ir) -> L3 ()
  (Expr : Expr (ir) -> Expr ()
        [(let ([,x ,t ,[e]]) ,[body*] ... ,[body])
         (if (equal? t 'Lambda)
             `(letrec ([,x ,t ,e]) ,body* ... ,body)
             `(let ([,x ,t ,e]) ,body* ... ,body))]))

;; Lenguaje en el cual se agregan los `letfun`
(define-language L4
  (extends L3)
  (Expr (e body)
        (+ (letfun ([x* t* body*] e*)))))

;; aux. variables de expresiones
(define (vars e)
  (nanopass-case
   (L3 Expr) e
   [,x (cons x null)]
   [,c null]
   [,l (append-map vars l)]
   [,pr null]
   [(primapp ,pr ,e* ...) (append-map vars e*)]
   [(define ,x ,e) (cons x null)]
   [(while [,e0] ,e1) (append (vars e0) (vars e1))]
   [(for [,x ,e0] ,e1) (append (list x) (vars e0) (vars e1))]
   [(begin ,e* ... ,e) (append (vars e) (append-map vars e*))]
   [(if ,e0 ,e1 ,e2) (append (vars e0) (vars e1) (vars e2))]
   [(lambda ([,x* ,t*] ...) ,body* ..., body)
    (append x* (append-map vars body*) (vars body))]
   [(let ([,x ,t ,e]) ,body* ... ,body)
    (append (list x) (append-map vars body*) (vars body))]
   [(letrec ([,x ,t ,e]) ,body* ... ,body)
    (append (list x) (append-map vars body*) (vars body))]
   [(list ,e* ...) (append-map vars e*)]
   [(,e0 ,e1 ...) (append (vars e) (append-map vars e1))]))

;; aux. dada una lista de variables y un posible identificador, regresa ese identificador
;; si no esta presente dentro de las variables dadas, u otro fresco en otro caso
(define (newid2 vrs x)
  (if (memq x vrs)
      (newid2 vrs (string->symbol (string-append (symbol->string x) "0")))
      x))

;; aux. dada una lista de variables, regresa identificador fresco
(define (newid vrs)
  (newid2 vrs (string->symbol "f")))

;; Agrega nombres a las funciones lambda
;; Ejercicio de la práctica 4
(define-pass un-anonymous : L3 (ir) -> L4 ()
  (Expr : Expr (ir) -> Expr ()
        [(lambda ([,x* ,t*] ...) ,[body*] ... ,[body])
         `(letfun ([,(newid (vars ir)) Lambda (lambda ([,x* ,t*] ...) ,body* ... ,body)]) ,(newid (vars ir)))]))


;; aux. regresa si la aridad del operador es compatible con el numero
;; de argumentos (dada en numero).
(define (prc-ar pr actual)
  (match pr
    ["+" #t]
    ["-" (> actual 0)]
    ["*" #t]
    ["/" (> actual 0)]
    ["length" (eq? 1 actual)]
    ["car" (eq? 1 actual)]
    ["cdr" (eq? 1 actual)]
    ["and" #t]
    ["not" (eq? 1 actual)]
    ["or" #t]
    ["<" (> actual 0)]
    [">" (> actual 0)]
    ["equal?" (eq? actual 2)]
    ["iszero?" (eq? actual 2)]
    ["++" (eq? 1 actual)]
    ["--" (eq? 1 actual)]))

;; verifica si las primitivas estan siendo aplicadas a la cantidad de elementos adecuada
;; Ejercicio de la práctica 4
(define-pass verify-arity : L4 (ir) -> L4 ()
  (Expr : Expr (ir) -> Expr ()
        [(primapp ,pr ,[e*] ...)
         (if (prc-ar (symbol->string pr) (length e*))
             `(primapp ,pr ,e* ...)
             (error (string-append "Wrong arity for " (symbol->string pr)
                                   ", got <" (~v (length e*)) ">")))]))

;; Verifica que la expresión no tenga variables libres, en caso de tenerlas manda un error,
;; si no tiene variables libres devuelve la misma expresion.
;; Ejercicio de la práctica 4
;;
;; verfify-vars: L4 -> L4 || error
(define-pass verify-vars : L4 (ir) -> L4 ()
  (Expr : Expr (ir [env null]) -> Expr ()
        [,x
         (if (memq x env)
             x
             (error (string-append "Free variable: " (symbol->string x))))]
        [(let ([,x ,t ,[e]]) ,[Expr : body* (cons x env) -> body*] ... ,[Expr : body (cons x env) -> body])
         `(let ([,x ,t ,e]) ,body* ... ,body)]
        [(letrec ([,x ,t ,[Expr : e (cons x env) -> e]]) ,[Expr : body* (cons x env) -> body*] ... ,[Expr : body (cons x env) -> body])
         `(letrec ([,x ,t ,e]) ,body* ... ,body)]
        [(lambda ([,x* ,t*] ...) ,[Expr : body* (append x* env) -> body*] ... ,[Expr : body (append x* env) -> body])
         `(lambda ([,x* ,t*] ...) ,body* ... ,body)]))


