#lang eopl


;; ------------------------------------------
; Taller 3 - Interpretador
; Jhoan Sebastian Fernandez - 2222772
; Luis Carlos Lucero Anaguano - 2027560
; Heidy Lizbeth Gelpud Acosta - 2242550
; Repositorio: https://github.com/18luisc/Taller3.git
;; ------------------------------------------


;; ---------------------------------------------------
; 1. Definición de valores y expresiones del lenguaje
;; ---------------------------------------------------

(define-datatype expresion expresion?
  (numero-lit (num number?))
  (texto-lit (txt string?))
  (var-exp (id symbol?))
  (primapp-bin-exp (exp1 expresion?) (prim primitiva-binaria?) (exp2 expresion?))
  (primapp-un-exp (prim primitiva-unaria?) (exp expresion?))
)

;; ---------------------------------------------------
; 2. Definición de primitivas unarias y binarias
;; ---------------------------------------------------

(define-datatype primitiva-binaria primitiva-binaria?
  (primitiva-suma)
  (primitiva-resta)
  (primitiva-multi)
  (primitiva-div)
  (primitiva-concat)
)

(define-datatype primitiva-unaria primitiva-unaria?
  (primitiva-longitud)
  (primitiva-add1)
  (primitiva-sub1)
)

;; ---------------------------------------------------
; 3. Definición del ambiente
;; ---------------------------------------------------

(define-datatype ambiente ambiente?
  (vacio)
  (extendido (ids (list-of symbol?))
             (vals (list-of expresion?))
             (ambiente-padre ambiente?)))



;; ---------------------------------------------------
; 4. Ambiente inicial
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
; 5. Función buscar-variable
;; ---------------------------------------------------

(define (buscar-variable id amb)
  (cases ambiente amb
    (vacio () 'not-found)
    (extendido (ids vals amb-padre)
      (let ([pos (member id ids)])
        (if pos
            (list-ref vals (- (length ids) (length pos)))
            (buscar-variable id amb-padre)))))) 
