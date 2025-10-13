#lang eopl


;; ------------------------------------------
; Taller 3 - Interpretador
; Jhoan Sebastian Fernandez - 2222772
; Luis Carlos Lucero Anaguano - 2027560
; Heidy Lizbeth Gelpud Acosta - 2242550
; Repositorio: https://github.com/18luisc/Taller3.git
;; ------------------------------------------

;;*******************************************************
;; Gramática:
;;
;; Valores denotados: Texto + Número + Booleano + ProcVal
;; Valores expresado: Texto + Número + Booleano + ProcVal
;; 
;; <programa>  := <expresion>
;;                un-programa (exp) 
;; 
;; <expresion> := <numero>
;;                numero-lit (num)
;; 
;;             := "\"" <texto> "\""
;;                texto-lit (txt)
;; 
;;             := <identificador>
;;                var-exp (id)
;;
;;             := (<expresion> <primitiva-binaria> <expresion>)
;;                primapp-bin-exp (exp1 prim-binaria exp2)
;; 
;;             := <primitiva-unaria> (<expresion>)
;;                primapp-un-exp (prim-unaria exp)
;;
;;             := declarar (<identificador> = <expresion> (;)) { <expresion> }
;;                variableLocal-exp (ids exps cuerpo)
;;
;;             := procedimiento (<identificador>*',') haga <expresion> finProc
;;                procedimiento-exp (ids cuerpo)
;;
;;             := "evaluar" <expresion> (expresion ",")*  finEval
;;                app-exp(exp exps) 
;;
;;             := letrec <identificador> (<identificador>) = <expresion> in <expresion>
;;                letrec-exp (p-names b-vars p-bodies letrec-body)
;; 
;; <primitiva-binaria> :=  + (primitiva-suma)
;; 
;;                     :=  ~ (primitiva-resta)
;; 
;;                     :=  / (primitiva-div)
;; 
;;                     :=  * (primitiva-multi)
;; 
;;                     :=  concat (primitiva-concat)
;; 
;; <primitiva-unaria>  :=  longitud (primitiva-longitud)
;; 
;;                     :=  add1 (primitiva-add1)
;; 
;;                     :=  sub1 (primitiva-sub1)
;;******************************************************

;; 0. Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   ("@" letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
  (digit (arbno digit) "." digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (text
   ((or letter "_") (arbno (or letter digit "_" ":"))) string)
  ))

;; Especificación Sintáctica
(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion (text) texto-lit)
    (expresion (identifier) var-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)
    (expresion ("declarar" "(" (separated-list identifier "=" expresion ";") ")" "{" expresion "}")
                variableLocal-exp)
    (expresion ("procedimiento" "(" (separated-list identifier ",") ")" "haga" expresion "finProc")
                procedimiento-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval")
                app-exp)
    (expresion ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expresion) "in" expresion) 
                letrec-exp)

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)

    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)))

;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpreter
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-programa  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (eval-expresion body (ambiente-inicial))))))

;; ---------------------------------------------------
;; 1. Definición del ambiente
;; ---------------------------------------------------

(define-datatype ambiente ambiente?
  (vacio)
  (extendido (ids (list-of symbol?))
             (vals (list-of expresion?))
             (ambiente-padre ambiente?)))

;; ---------------------------------------------------
; 2. Ambiente inicial
;; ---------------------------------------------------

(define ambiente-inicial
  (extendido '(@a @b @c @d @e)
             (list (numero-lit 1) 
                   (numero-lit 2) 
                   (numero-lit 3) 
                   (texto-lit "hola") 
                   (texto-lit "FLP"))
             (vacio)))

;; ---------------------------------------------------
; 3. Función buscar-variable
;; ---------------------------------------------------

(define (buscar-variable id amb)
  (cases ambiente amb
    (vacio () 'not-found)
    (extendido (ids vals amb-padre)
      (let ([pos (member id ids)])
        (if pos
            (list-ref vals (- (length ids) (length pos)))
            (buscar-variable id amb-padre)))))) 

;; ---------------------------------------------------
;; 4. Booleanos: valor-verdad?
;; ---------------------------------------------------

(define (valor-verdad? v)
  (cond
    [(number? v) (not (= v 0))]
    [else #t]))

;; ---------------------------------------------------
;; 5. Función eval-expresion
;; ---------------------------------------------------
;; Evalúa cualquier expresión del lenguaje según el ambiente actual.
(define (eval-expresion exp amb)
  (cases expresion exp

    ;; Números literales
    (numero-lit (num)
      num)

    ;; Textos literales
    (texto-lit (txt)
      txt)

    ;; Variables: busca en el ambiente
    (var-exp (id)
      (let ([val (buscar-variable id amb)])
        (if (equal? val 'not-found)
            (eopl:error "Error: la variable no existe ~s" id)
            val)))

    ;; Primitivas binarias (+, ~, *, /, concat)
    (primapp-bin-exp (exp1 prim exp2)
      (let* ([v1 (eval-expresion exp1 amb)]
             [v2 (eval-expresion exp2 amb)])
        (eval-prim-bin prim v1 v2)))

    ;; Primitivas unarias (add1, sub1, longitud)
    (primapp-un-exp (prim exp1)
      (let ([v (eval-expresion exp1 amb)])
        (eval-prim-un prim v)))

    ;; Condicional: Si ... entonces ... sino ... finSI
    (condicional-exp (test-exp true-exp false-exp)
      (if (valor-verdad? (eval-expresion test-exp amb))
          (eval-expresion true-exp amb)
          (eval-expresion false-exp amb)))

    ;; Otros casos aún no implementados
    (else (eopl:error "Expresión no soportada todavía"))))

;; ---------------------------------------------------
;; 6. Evaluación de primitivas binarias
;; ---------------------------------------------------
(define (eval-prim-bin prim v1 v2)
  (cases primitiva-binaria prim
    (primitiva-suma () (+ v1 v2))
    (primitiva-resta () (- v1 v2))
    (primitiva-multi () (* v1 v2))
    (primitiva-div ()
      (if (zero? v2)
          (eopl:error "Error: división por cero")
          (/ v1 v2)))
    (primitiva-concat ()
      (if (and (string? v1) (string? v2))
          (string-append v1 v2)
          (eopl:error "concat solo funciona con textos"))))
  )

;; ---------------------------------------------------
;; 7. Evaluación de primitivas unarias
;; ---------------------------------------------------
(define (eval-prim-un prim v)
  (cases primitiva-unaria prim
    (primitiva-longitud ()
      (if (string? v)
          (string-length v)
          (eopl:error "longitud solo funciona con textos")))
    (primitiva-add1 ()
      (if (number? v) (+ v 1)
          (eopl:error "add1 solo funciona con números")))
    (primitiva-sub1 ()
      (if (number? v) (- v 1)
          (eopl:error "sub1 solo funciona con números")))))
