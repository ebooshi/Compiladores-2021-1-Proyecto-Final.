#lang nanopass


(require racket/pretty)

#|
Compiladores 2021-1

Autores:
313245312 - González Alvarado Raúl
316255709 - Hernández Cano Alejandro
315116663 - Muñoz Barón Luis Miguel 
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

; =================================================================== Front End ===================================================================================
; ================================================================= (Passes 1 - 7) ================================================================================

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
(define-pass remove-string : L1 (ir) -> L2 ()
  (Expr : Expr (ir) -> Expr ()
        [,s `(list ,(string->list s) ...)]))

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
        (+ (letfun ([x* t* body*]) e*))))

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
   [(,e0 ,e1 ...) (append (vars e0) (append-map vars e1))]))

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
        [(letfun ([,x ,t ,[Expr : e (cons x env) -> e]]) ,[Expr : body* (cons x env) -> body*])
         `(letfun ([,x ,t ,e]) ,body*)]
        [(lambda ([,x* ,t*] ...) ,[Expr : body* (append x* env) -> body*] ... ,[Expr : body (append x* env) -> body])
         `(lambda ([,x* ,t*] ...) ,body* ... ,body)]))



; =================================================================== Middle End ===================================================================================
; ================================================================= (Passes 8 - 11) ================================================================================

; Lambdas de una sola asignacion.
(define-language L5
  (extends L4)
  (Expr (e body)
        (- (lambda ([x* t*] ...) body* ... body)
           (e0 e1 ...))
        (+ (lambda ([x t])  body* ... body)
           (e0 e1))))

(define-parser parse-L5 L5)

; Transforma lambda expresiones y aplicaciones por su equivalente currificada.
(define-pass curry : L4 (ir) -> L5 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ([,x* ,t*] ...) ,[body])
         (let f ([bindingx* x*]
                 [bindingt* t*])
           (if (equal? (length bindingx*) 1)
               `(lambda ([,(car bindingx*) ,(car bindingt*)]) ,body)
               `(lambda ([,(car bindingx*) ,(car bindingt*)]) ,(f (cdr bindingx*) (cdr bindingt*)))))]
        [(,[e0] ,[e1] ...)
         (let f ([be0 e0]
                 [be1 e1])
           (if (equal? (length be1) 0)
               `,be0
               (f `(,be0 ,(car be1)) (cdr be1))))]))

; Constantes quot
(define-language L6
  (extends L5)
  (Expr (e body)
        (- c)
        (+ (quot c))))

(define-parser parse-L6 L6)

; Transforma las constantes en constantes quot
(define-pass quote-const : L5 (ir) -> L6 ()
  (Expr : Expr (ir) -> Expr ()
        [,c `(quot ,c)]))

; Lenguaje para constantes tipadas.
(define-language L7
  (extends L6)
  (Expr (e body)
        (- (quot c))
        (+ (const t c))))

(define-parser parse-L7 L7)

; Tipado de cada constante.
(define-pass type-const : L6 (ir) -> L7 ()
  (Expr : Expr (e) -> Expr ()
        [(quot ,c)
         (cond
           [(boolean? c) `(const Bool ,c)]
           [(number? c) `(const Int ,c)]
           [(char? c) `(const Char ,c)])]))

; Auxiliar get var type from context
(define (get x ctx)
  (   if (empty? ctx)
         (error "No existe la variable en el contexto locote")       ; La variable no fue encontrada en el contexto, lanzamos un error jejajsajas
         (if (equal? x (caar ctx))  ; Si la variable es la misma que la que esta almacenada en la cabeza de la lista
             (cdar ctx)            ; Regresamos el tipo que tiene su pareja.
             (get x (cdr ctx)))))          ; Si no es, se llama la funcion recursivamente con el resto de la lista.

; Unificacion de tipos
(define (unify t1 t2)
	(if (and (type? t1) (type? t2))
		(cond
                  [(equal? t1 t2) #t]
                  [(and (equal? 'List t1) (list? t2)) (equal? (car t2) 'List)]
                  [(and (equal? 'List t2) (list? t1)) (equal? (car t1) 'List)]
                  [(and (list? t1) (list? t2)) (and (unify (car t1) (car t2)) (unify (caddr t1) (caddr t2)))]
                  [else #f])
		(error "Se esperaban 2 tipos")))

;----------------------------------------------------- Algoritmo de inferencia J -------------------------------------
(define (J expr ctx)
  (nanopass-case (L7 Expr) expr
                 ; Regla Var.
                 [,x (get x ctx)]
                 ; Regla Const.
                 [(const ,t ,c) t]
                 ; Regla Begin.
                 [(begin ,e* ... ,e)
                  (let f ([e* e*]
                          [e e])
                    (if (empty? e*)
                        (J e ctx)
                        (f (cdr e*) (e))))]
                 ; Regla arit, car, cdr. 
                 [(primapp ,pr ,e* ...)
                  (cond
                    [(or (equal? pr 'length) (equal? pr 'car) (equal? pr 'cdr))
                     (let ([t (J (car e*) ctx)])
                       (if (equal? (car t) 'List)
                           (cond
                             [(equal? pr 'car) (third t)]
                             [(equal? pr 'cdr) t]
                             [(equal? pr 'length) 'Int])
                           (error "Los operandos no son de tipo List.")))]
                    [(or (equal? pr '+) (equal? pr '-) (equal? pr '*) (equal? pr '/))
                     (if (and (unify 'Int (J (first e*) ctx)) (equal? (J (car e*) ctx) (J (second e*) ctx)) )
                         'Int
                         (error "Los operadores no son de tipo Int."))])]

                 ; Regla If.
                 [(if ,e0 ,e1 ,e2)
                  (let ([t0 (J e0 ctx)]
                        [t1 (J e1 ctx)]
                        [t2 (J e2 ctx)])
                    (if (and (unify t0 'Bool) (unify t1 t2))
                        t1
                        (error "F: Las ramas del if no tienen el mismo tipo y/o la guarda no es booleana.")))]

                 ;Regla While.
                   [(while [,e0] ,e1) (if (equal? (J e0 ctx) 'Bool) 
			     		  (J e1 ctx) 
			     		  (error "El tipo no corresponde con el valor."))]
                 
                 ;Regla Lambda.
                 [(lambda ([,x ,t]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [type (J body ctxN)])
                    `(,t → ,type))]
                 ; Regla let.
                 [(let ([,x ,t ,e]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [t0 (J e ctx)]
                         [t1 (J body ctxN)])
                    (if (unify t t0)
                        t1
                        (error "Argumentos no compatibles.")))]
                 ; Regla letrec.
                 [(letrec ([,x ,t ,e]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                        [t0 (J e ctxN)]
                        [t1 (J body ctxN)])
                    (if (unify t t0)
                        t1
                        (error "Tipos no unificables.")))]
                 ; Regla letfun. que onda con los tipos ->?
                 [(letfun ([,x ,t ,e]) ,body)
                 (let* ([ctxN (set-add ctx (cons x t))]
                       [t0 (J e ctx)]
                       [t1 (J body ctxN)])
                   (if (and (list? t0) (equal? (second t0) '→) (unify t t0))
                       t1
                       (error "Tipos no unificables.")) )]
                 ; Regla empty.
                 ; Regla List.
                 ; Regla List (Esta la puse yo y la verdad no se si va)
                 [(list ,e* ...)
                  (if (empty? e*)
                      'List
                      (let* ([t (map (lambda (x) (J x ctx)) e*)]
                             [b (foldr (lambda (x y) (equal? x y)) (car t) t)])
                        (list 'List 'of (car t))))]
                 ; Regla App.
                 [(,e0 ,e1)
                  (let* ([t0 (J e0 ctx)]
                         [t1 (J e1 ctx)])
                    (if (list? t0)
                        (if (unify (car t0) t1)
                            (third t0)
                            (error "F: El dominio y la entrada de la función son distintas. :'v"))
                        (error "F: El primer parámetro no es una función. :c")))]))

; inferencia de tipos
(define-pass type-infer : L7 (ir) -> L7 ()
  (Expr : Expr (e) -> Expr ()
        [(let ([,x ,t ,e]) ,body)
         (if (equal? t 'List)
             `(let ([,x ,(J e '()) ,e]) ,body) ir)]
        [(letrec ([,x ,t ,e]) ,body)
         (if (or (equal? t 'Lambda) (equal? t 'List))
             `(letrec ([,x ,(J e '()) ,e]) ,body) ir)]
        [(letfun ([,x ,t ,e]) ,body)
         (if (equal? t 'Lambda)
             `(letfun ([,x ,(J e '()) ,e]) ,body) ir)]))

; Lenguaje para descurrificar Lambda expresiones.
(define-language L8
  (extends L7)
  (Expr (e body)
    (- (lambda ([x t]) body* ... body))
    (+ (lambda ([x* t*] ...) body* ... body))))

(define-parser parse-L8 L8)

(define-pass uncurry : L7 (ir) -> L8 ()
  (definitions
    (define (des-curry espl lst)
      (nanopass-case (L7 Expr) espl
      [(lambda ([,x ,t]) ,body) (des-curry body (append lst (list (cons x t))))]
      [else (values espl lst)])))
  (Expr : Expr (ir) -> Expr ()
        [(lambda ([,x ,t]) ,[body]) (let-values ([(e elem) (des-curry ir '())])
                                      (let ([x* (map car elem)]
                                            [e* (map cdr elem)])
                                        `(lambda ([,x* ,e*] ...) ,(parse-L8 (unparse-L7 e)))))]))


; =================================================================== Back End =====================================================================================
; ================================================================ (Passes 12 - 13) ================================================================================

(define-language L9
  (extends L8)
  (terminals
   (- (list (l)))
   (+ (array (arr)))
   (+ (length (len))))
  (Expr (e body)
    (- l)
    (- (list e* ...))
    (+ (array len t [e* ...]))))

(define-parser parse-L9 L9)

(define (length? x) (and (integer? x) (>= x 0)))
; TODO xdxxdxdxddxddx
(define (array? x)
  (if (and (list? x) (eq? (length x) 4))
      (let* ([a (car x)] ; array
             [l (cadr x)] ; 2
             [t (caddr x)] ; Int
             [e* (cadddr x)]) ; (1 2)
        (and (eq? a 'array) (length? l) (type? t) (list? e*) (eq? (length e*) l) (andmap constant? e*)))
      #f))

(define (typeof e)
  (nanopass-case (L9 Expr) e
                 [(const ,t ,c) t]))

(define-pass list-to-array : L8 (ir) -> L9 ()
  (Expr : Expr (e) -> Expr ()
        [(list ,[e*] ...)
         (if (empty? e*)
             `(array ,0 Int [,null])
             (let* ([e0 (first e*)]
                    [t (typeof e0)]
                    [n (length e*)])
               `(array ,n ,t [,e* ...])))]))