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
    (primapp pr e* ... e)
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
