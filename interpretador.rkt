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
; Any? Auxiliar para recibir cualquier parametro
;; ---------------------------------------------------

(define-datatype ambiente ambiente?
  (vacio)
  (extendido (ids (list-of symbol?))
             (vals (list-of any?))
             (ambiente-padre ambiente?))
  (extendido-recursivo (proc-names (list-of symbol?))
                       (b-vars (list-of (list-of symbol?)))
                       (proc-bodies (list-of expresion?))
                       (ambiente-padre ambiente?)))

(define (any? x) #t)

;; ---------------------------------------------------
; 2. Ambiente inicial
;; ---------------------------------------------------

(define ambiente-inicial
  (lambda ()
    (extendido '(@a @b @c @d @e)
               '(1 2 3 "hola" "FLP")
             (vacio))))

;; ---------------------------------------------------
; Ambiente extendido
;; extend-amb <list-of symbols> <list-of SchemeVal> ambiente -> ambiente
;; usage: (extend-amb '(x ... x_n) '(v ... v_n) env)
;; -> (extendido '(x ... x_n) '(v ... v_n) env)
;; ---------------------------------------------------
(define extend-amb
  (lambda (syms vals env)
    (extendido syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (p-names b-vars p-bodies old-env)
    (extendido-recursivo
     p-names b-vars p-bodies old-env)))

;; ---------------------------------------------------
; 6. Defición de procVal (cerradura)
;; creacion de procedimientos
;; ---------------------------------------------------
(define-datatype procVal procVal?
  (cerradura
    (lista-ID (list-of symbol?))
    (exp expresion?)
    (amb ambiente?)))

;; ---------------------------------------------------
; Función buscar-variable
;; ---------------------------------------------------

(define (buscar-variable id amb)
  (cases ambiente amb
    (vacio ()
      (eopl:error 'buscar-variable "Variable no encontrada: ~s" id))
    (extendido (ids vals amb-padre)
      (let ([pos (member id ids)])
        (if pos
            ;; retorna directamente el valor almacenado (ya nativo)
            (list-ref vals (- (length ids) (length pos)))
            (buscar-variable id amb-padre))))
    (extendido-recursivo (proc-names b-vars proc-bodies amb-padre)
      (let ([pos (member id proc-names)])
        (if pos
            ;; construye la cerradura a partir de la información del letrec
            (let* ([index (- (length proc-names) (length pos))]
                   [proc-body (list-ref proc-bodies index)]
                   [proc-vars (list-ref b-vars index)])
              (cerradura proc-vars proc-body amb))  ; cerradura es un valor nativo (procVal)
            (buscar-variable id amb-padre))))))


;; ---------------------------------------------------
; 3. Función valor-verdad?
;; Definición de expresiones numericas que representan booleanos.
;; ---------------------------------------------------
(define valor-verdad?
  (lambda (x)
  (not (zero? x))))

;; -----------------------------------------------------
;; eval-expresion <expresion> <ambiente> -> numero
;; Uso: (eval-expresion exp env) -> numero
;; -----------------------------------------------------
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable id env))
      (primapp-bin-exp (rand1 prim rand2)
        (let ((args (eval-rands (list rand1 rand2) env)))
          (apply-primitiva-bin prim args)))
      (primapp-un-exp (prim rand)
        (let ((args (eval-rands (list rand) env)))
          (apply-primitiva-un prim args)))
      (condicional-exp (test-exp true-exp false-exp)
        (if (valor-verdad? (eval-expresion test-exp env))
            (eval-expresion true-exp env)
            (eval-expresion false-exp env)))
      (variableLocal-exp (ids exps cuerpo)
        (let ((args (eval-rands exps env)))
          (eval-expresion cuerpo
                          (extend-amb ids args env))))
      (procedimiento-exp (ids cuerpo)
        (cerradura ids cuerpo env))
      (app-exp (rator rands)
        (let ((proc (eval-expresion rator env))
              (args (eval-rands rands env)))
          (if (procVal? proc)
              (apply-procedimiento proc args)
              (eopl:error 'eval-expression
                          "Se intentó aplicar un no-procedimiento ~s" proc))))
      (letrec-exp (p-names b-vars p-bodies letrec-body)
        (eval-expresion letrec-body
                        (extend-env-recursively p-names b-vars p-bodies env)))
      )))

;; apply-procedimiento: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedimiento
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
                 (eval-expresion body (extend-amb ids args env))))))

;; Funciones auxiliares para eval-expresion
(define eval-rands
(lambda (rands env)
(map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
(lambda (rand env)
(eval-expresion rand env)))

;; apply-primitiva-bin: <primitiva> <list-of-expresion> -> SchemeVal
(define apply-primitiva-bin
  (lambda (prim args)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ (car args) (cadr args)))
      (primitiva-resta () (- (car args) (cadr args)))
      (primitiva-div () (/ (car args) (cadr args)))
      (primitiva-multi () (* (car args) (cadr args)))
      (primitiva-concat () (string-append (car args) (cadr args)))))) ; implementar apply-concat

;; apply-primitiva-un: <primitiva> <list-of-expresion> -> SchemeVal
(define apply-primitiva-un
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length (car args))) ; implementar como hallar la longitud apply-length
      (primitiva-add1 () (+ (car args) 1))
      (primitiva-sub1 () (- (car args) 1)))))


(show-the-datatypes)
(display (interpreter))


