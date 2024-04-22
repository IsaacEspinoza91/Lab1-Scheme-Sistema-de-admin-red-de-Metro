#lang racket
(provide driver driver? get-id-driver get-name-driver get-maker-train-driver)

;TDA driver, abstraccion de un conductor de tren, habilita la conduccion del fabricante de tren (train-maker)
;Representacion: lista con elementos (id conductor - nombre del conductor - fabricante del tren que el conductor sabe maneja)



;CONTRUCTOR
#|DOM: id conductor (entero) X nombre conductor (string) X fabricante de los trenes que conduce (string)
REC: conductor (TDA driver)
Funcion que crea un elemento del TDA driver|#
;Nivel de implementacion: 1
(define driver (lambda (id name maker-train)
                 (if (and (id-driver? id) (name-driver? name) (maker-train-driver? maker-train))
                     (list id name maker-train)
                     null)
                 )
  )



;FUNCIONES DE PERTENENCIA

#|DOM: conductor (TDA driver)
REC: bool
Funcion que verifica si un elemento es del tipo TDA driver|#
(define (driver? conductor)
        (if (and (not (empty? conductor)) (id-driver? (car conductor)) (name-driver? (cadr conductor))
                 (maker-train-driver? (caddr conductor)))
            #t
            #f)
  )

#|DOM: id conductor (entero no negativo)
REC:bool
Funcion que determina si un elemento puede ser id de un TDA driver|#
(define (id-driver? id) (if (integer? id) #t #f))

#|DOM: nombre conductor (string)
REC: bool
Funcion que determina si un elemento puede ser nombre de un TDA driver|#
(define (name-driver? name) (if (string? name) #t #f))

#|DOM: fabricante de tren (string)
REC: bool
Funcion que determina si un elemento puede ser maker-train de un TDA driver|#
(define (maker-train-driver? maker) (if (string? maker) #t #f))




;SELECTORES

#|DOM: conductor (TDA driver)
REC: id de conductor (entero no negativo)
Funcion que obtiene el elemento id de un TDA driver|#
(define (get-id-driver conductor) (if (driver? conductor) (car conductor) null))

#|DOM: conductor (TDA driver)
REC: nombre del conductor (string)
Funcion que obtiene el elemento name de un TDA driver|#
(define (get-name-driver conductor) (if (driver? conductor) (cadr conductor) null))

#|DOM: conductor (TDA driver)
REC: fabricante del tren que conduce el conductor (string)
Funcion que obtiene el elemento maker-train de un TDA driver|#
(define (get-maker-train-driver conductor) (if (driver? conductor) (caddr conductor) null))


;MODIFICADORES

#|DOM: conductor (TDA driver) X id tren (entero no negativo)
REC: conductor (TDA driver)
Funcion que crea un TDA driver a partir del elemento driver del argumento, pero con un nuevo id |#
(define (set-id-driver conductor nueva-id)
        (if (and (driver? conductor) (id-driver? nueva-id))
            (driver nueva-id (get-name-driver conductor) (get-maker-train-driver conductor))
            null)
  )

#|DOM: conductor (TDA driver) X  nombre tren (string)
REC: conductor (TDA driver)
Funcion que crea un TDA driver a partir del elemento driver del argumento, pero con un nuevo nombre |#
(define (set-name-driver conductor nuevo-nombre)
        (if (and (driver? conductor) (name-driver? nuevo-nombre))
            (driver (get-id-driver conductor) nuevo-nombre (get-maker-train-driver conductor))
            null)
  )

#|DOM: conductor (TDA driver) X fabricante del tren que conduce el conductor (string)
REC: conductor (TDA driver)
Funcion que crea un TDA driver a partir del elemento driver del argumento, pero con un nuevo maker-train |#
(define (set-maker-train-driver conductor nuevo-fabricante)
        (if (and (driver? conductor) (maker-train-driver? nuevo-fabricante))
            (driver (get-id-driver conductor) (get-name-driver conductor) nuevo-fabricante)
            null)
  )
