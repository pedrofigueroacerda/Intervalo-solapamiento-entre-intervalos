
;Nombres:
;Pedro Figueroa Cerda
;Compilado con DrRacket
;SO: Windows 10 Home Single Lenguaje
;idea:
; 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
;_______  _______       __________    _____
;   ____       _______
;
;      ___________________________

; lista ejemplo para merge (merge-sort '((5 8) (1 4) (2 4) (7 10) (15 17) (11 14)))
;(cover '((0 2) (2 3) (4 6) (5 8) (7 9) (10 13) (11 12) (15 16)) '(1 14))
;((1 3) (4 9) (10 13))
;(free '((0 2) (2 3) (4 6) (5 8) (7 9) (10 13) (11 12) (15 16)) '(1 14))
;((3 4) (9 10) (13 14))

; funcion para incertar elementos en lista
(define (insert L x)
    (cond
         ((null? L) (list x))
         ((< x (car L)) (cons x L))
         (else (cons (car L) (insert (cdr L) x)))
    )
)

; funcion para ordenamiento, merge sort (ordenamiento por mezcla)

(define (merge A B)
    (cond
         ((null? A) B)
         ((null? B) A)
         ((< (caar A) (caar B)) ;hacemos el ordenamiento con respecto al primer elemento de la lista de lista
             (cons (car A) (merge (cdr A) B))
         )
         (else
             (cons (car B) (merge A (cdr B)))
         )))

;funcion que divide en dos la lista

(define (split L)
    (cond
        ((null? L)
            (list (list) (list)) ; (() ())
        )
        ((null? (cdr L))
            (list (list (car L)) (list))
        )
        (else
            (define AB (split (cddr L))) ; (cddr L) => (cdr (cdr L))
            (list (cons (car L) (car AB)) (cons (cadr L) (cadr AB)))
        )
    )
)
; ordenamiento por mezcla

(define (merge-sort L)
    (if (or (null? L) (null? (cdr L)))
        L
        (let*
            (
                (AB (split L))
                (A (merge-sort (car AB)))
                (B (merge-sort (cadr AB)))
            )
            (merge A B)
        )
        
    )
)

;La implementacion viene desde aca
;se parte por el generdor de rangos 

; funcion que concatena elementos en una nueva lista (()()()())
; esta funcion toma el rango ingresado (elemento B en funcion cover) y los tranforma en una lista de rangos sucesivos
;(de esta forma
;(n, n+1)
;  ( n+1, n+2)
;         ...
;           .
;           ....
;              .
;             (n+i, n+i+1)
;hasta el cdr de la lista que contiene dos elementos inicio y fin (rango)

(define (generador a b)
  (define (generadoraux a b L)
    (if (< a b)
        (cons (list a (+ a 1)) (generadoraux (+ a 1) b L))
        L
        )
    )
  (generadoraux a b (list))
  )

;*********************************************************************************************************************

;teste para el cond que hay en comparador

(define (comparar A B)
  (if (and (<= (car B) (car A)) (<= (car(cdr A)) (car(cdr B)))) 1 2)
  )

;comparador si esta contenido el elemento s en alguno d elos valores de la lista X

(define (comparador X s)
  (cond    ; aca debo poner un cond + un lista sea distinto de nulo, or not null y hago la recurrencia
     ((null? X) #f)
     ((= (comparar s (car X)) 1) #t)
     (else (comparador (cdr X) s)) ;pongo en else F, y asi se convierte en un bool 
     )
  )


; esta funcion realizara las comparaciones en la lista, dentro de ella definiremos
; algunas funciones de comparacion el la lista de rangos( variable Lista, que contiene A rangos ordenados con respecto al primer elemento)


(define (coveraux L R)
  (define (coveraux1 L R ini fin comienzo)
    (cond
      ((null? R) (list))
      ((and (comparador L (car R)) (equal? ini 0)) (coveraux1 L (cdr R) (= ini (caar R)) (+ (caar R) 1) (cons (caar R) comienzo))); si esta contenido el elemento R en alguna de las listas en L
      ((and (comparador L (car R)) (not (equal? ini 0))) (coveraux1 L (cdr R) ini (+ fin 1) comienzo))
      (else (cons (list (car comienzo) fin) (coveraux1 L (cdr R) 0 1 (list)) )
        )
      )
    )
  (coveraux1 L R 0 1 (list)) 
 )
 


; funcion cover

(define (cover A B)
  (let*
      (
         (Lista (merge-sort A))
         (rangos (generador (car B) (car (cdr B))))
       )
       (coveraux Lista rangos)
    )

 )

;**********************************************************************************************************************

;para dejar un poco mas ordenado, utilizamos un generador al igual que en Interval
(define (generadorf a b)
  (define (generadoraux1 a b L)
    (if (< a b)
        (cons (list a (+ a 1)) (generadoraux1 (+ a 1) b L))
        L
        )
    )
  (generadoraux1 a b (list))
  )

;usamos el complemento de la otra funcion para obtener la lista

(define (compararf rang B)
  (if (not (and (<= (car B) (car rang)) (<= (car(cdr rang)) (car(cdr B))))) 1 2)
  )

;comparador si esta contenido el elemento s en alguno d elos valores de la lista B

(define (comparadorf X r)
  (cond    ; aca debo poner un cond + un lista sea distinto de nulo, or not null y hago la recurrencia
     ((null? X) #t)     
     ((= (compararf r (car X)) 1) (comparadorf (cdr X) r))
     (else #f) ;pongo en else F, y asi se convierte en un bool 
     )
  )



;se define la funcion auxiliar de free, donde este entrega cada intervalo libre dentro de un rango dado

(define (freeaux L R)
  (define (freeaux1 free L R)
    (cond
      ((null? R) free)
      ((equal? (comparadorf L (car R)) #t) (freeaux1 (cons (list (caar R) (+ (caar R) 1)) free) L (cdr R)))
      (else (freeaux1 free L (cdr R)))
       )
    )
  (freeaux1 (list) L R)
    )
 
;(free '((0 2) (2 3) (4 6) (5 8) (7 9) (10 13) (11 12) (15 16)) '(1 14))
;((3 4) (9 10) (13 14))

;se define la funcion free
(define (free A B)
  (let*(
        (Lista (merge-sort A))
        (rangos (generadorf (car B) (car (cdr B))))
      )
    (reverse (freeaux Lista rangos))
    )
  )


;************************************************************************************************

;funcion max subset

;se define la funcion max_subset
;primera condicion en
;si S tiene solo un intervalo, da el mismo S [caso base]
;si no
;-si limite superior de primer intervalo es menor o igual al limite inferior del segundo intervalo, entonces no se solapan
;-si el primer intervalo cubre menos area que el segundo, se procede agregando el primer intervalo
;-si el segundo intervalo cubre menos area, se ignora el primer intervalo, y se opera el resto de la lista nuevamente



;calcula el amplio de un intervalo, mediante resta
(define (int_dif A)
    (- (car(cdr A)) (car A))
)


(define (max_subset1 S)
    (cond 
        ((= 1 (length S)) S)
        (else
            (cond
                ((<= (cadr(car S)) (caar(cdr S))) (cons(car S) (max_subset1 (cdr S))))
                ((< (int_dif(car S)) (int_dif(cadr S))) (cons (car S) (max_subset1 (cddr S))))
                ((> (int_dif(car S)) (int_dif(cadr S))) (max_subset1 (cdr S)))
            )
        )
    )
)

(define (max_subset S)
  (let (
        (lista (merge-sort S))
        )
    (max_subset1 lista)
    )
  )
        

