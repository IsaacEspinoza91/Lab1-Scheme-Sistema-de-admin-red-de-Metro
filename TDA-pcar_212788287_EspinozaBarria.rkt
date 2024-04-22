#lang racket
(require "TDA-section_212788287_EspinozaBarria.rkt")
(require "TDA-station_212788287_EspinozaBarria.rkt")
(require "TDA-type-station_212788287_EspinozaBarria.rkt")
(require "TDA-line_212788287_EspinozaBarria.rkt")
(provide pcar pcar? tr ct get-type-pcar get-model-pcar get-capacity-pcar)


;TDA pcar, abstraccion de un carro/bagon de pasajeros que posteriormente conforman un tren
;pueden ser del tipo terminal (tr) o del tipo central (ct)
;Representacion: lista con elementos (id del carro - capacidad de pasajeros del carro - modelo del carro - tipo del carro)



;definiciones para el type-pcar
(define tr "tr")
(define ct "ct")


;CONSTRUCTOR
;DOM: id (entero) X capacidad de pasajeros del carro (entero positivo) X modelo de carro (string) X tipo de carro (type-station) 
;REC: station (lista de elementos), en caso de algun argumento invalido retorna lista vacia
;Declarativo
;funcion que crea un elemento del TDA pcar
;Nivel de implementacion: 1
(define pcar (lambda (id capacity model type)
               (if (and (number? id) (integer? capacity) (string? model))
                   (list id capacity model type)
                   null)
               )
  )




;FUNCIONES DE PERTENENCIA
#|DOM: estacion (station)
REC: bool
funcion que verifica si un elemento es del tipo station |#
(define pcar? (lambda (carro)
                   (if (and (id-pcar? (car carro)) (capacity-pcar? (cadr carro))
                            (model-pcar? (caddr carro)) (type-pcar? (cadddr carro))
                            )
                       #t
                       #f
                       )
                   )
  )

#|DOM: id (entero)
 REC: bool
funcion que verifica si un dato puede ser id de un TDA pcar|#
(define id-pcar? (lambda (id) (if (number? id) #t #f)))

#|DOM: capacidad de pasajeros de un carro pcar (entero positivo)
 REC: bool
funcion que verifica si un dato puede ser tiempo de parada en un TDA estation|#
(define capacity-pcar? (lambda (capacity) (if (integer? capacity) #t #f)))

#|DOM: modelo de carro (string)
 REC: bool
funcion que verifica si un dato puede ser modelo de un TDA pcar|#
(define model-pcar? (lambda (model) (if (string? model) #t #f)))

#|DOM: tipo de pcar (terminal o central) (string)                                                                 OJO ESTO, TODAVIA NO CREO EL TDA TYPE-PCAR
 REC: bool
funcion que verifica si un dato puede ser type-pcar en un TDA pcar|#
(define type-pcar? (lambda (tipo)
                        (if (or (equal? "tr" tipo) (equal? "ct" tipo))
                            #t
                            #f)
                        )
  )




;SELECTORES

#|DOM: carro de pasajeros (pcar)
REC: id (entero) U {null}
funcion que obtiene el elemento id de un TDA pcar |#
(define get-id-pcar (lambda (carro)
                        (if (pcar? carro)
                            (car carro)
                            null
                            )
                        )
  )

#|DOM: carro de pasajeros (pcar)
REC: capacidad (entero positivo) U {null}
funcion que obtiene el elemento capacity de un TDA pcar|#
(define get-capacity-pcar (lambda (carro)
                           (if (pcar? carro)
                               (cadr carro)
                               null)
                           )
  )

#|DOM: carro de pasajeros (pcar)
REC: modelo de carro (string) U {null}
funcion que obtiene el elemento modelo de carro de un TDA pcar|#
(define get-model-pcar (lambda (carro)
                                  (if (pcar? carro)
                                      (caddr carro)
                                      null)
                                   )
  )

#|DOM: carro de pasajeros (pcar)
REC: tipo de carro (string) U {null}                                                                          OJO VER ESTO TIPO DE CARRO
funcion que obtiene el elemento tipo de carro de un TDA pcar |#
(define get-type-pcar (lambda (carro)
                               (if (pcar? carro)
                                   (cadddr carro)
                                   null)
                               )
  )






;MODIFICADORES

#|DOM:carro de pasajeros (pcar) X id (entero)
REC: carro de pasajeros (pcar) U {null}
funcion que crea un elemento TDA pcar a partir del elemento pcar de argumento, pero con una nueva id |#
(define set-id-station (lambda (carro new-id)
                        (if (and (pcar? carro) (id-pcar? new-id))
                            (pcar new-id (get-capacity-pcar carro) (get-model-pcar carro) (get-type-pcar carro))
                            null
                            )
                        )
  )

#|DOM: carro de pasajeros (pcar)) X capacity (entero positivo)
REC: (pcar) U {null}
funcion que crea un elemento TDA pcar a partir del elemento pcar de argumento, pero con un nuevo capacity |#
(define set-capacity-station (lambda (carro new-capacity)
                          (if (and (pcar? carro)(capacity-pcar? new-capacity))
                              (pcar (get-id-pcar carro) new-capacity (get-model-pcar carro) (get-type-pcar carro))
                              null
                              )
                          )
  )

#|DOM: carro de pasajeros (pcar) X modelo de carro (string)
REC: (pcar) U {null}
funcion que crea un elemento TDA pcar a partir del elemento pcar de argumento, pero con un nuevo modelo de carro |#
(define set-model-station (lambda (carro new-model)
                                  (if (and (pcar? carro)(model-pcar? new-model))
                                      (pcar (get-id-pcar carro) (get-capacity-pcar carro) new-model (get-type-pcar carro))
                                      null
                                      )
                                  )
  )

#|DOM: carro de pasajeros (pcar) X tiempo de parada (entero positivo)
REC: (pcar) U {null}
funcion que crea un elemento TDA pcar a partir del elemento pcar de argumento, pero con un nuevo tipo de carro|#
(define set-type-pcar (lambda (carro new-type)
                               (if (and (pcar? carro)(type-pcar? new-type))
                                   (pcar (get-id-pcar carro) (get-capacity-pcar carro) (get-model-pcar carro) new-type)
                                   null
                                   )
                               )
  )