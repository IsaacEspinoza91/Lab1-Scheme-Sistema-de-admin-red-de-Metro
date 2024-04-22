#lang racket
(require "TDA-pcar_212788287_EspinozaBarria.rkt")
(require "TDA-section_212788287_EspinozaBarria.rkt")
(require "TDA-station_212788287_EspinozaBarria.rkt")
(require "TDA-type-station_212788287_EspinozaBarria.rkt")
(require "TDA-line_212788287_EspinozaBarria.rkt")
(provide train train? train-add-car train-remove-car train-capacity get-id-train get-maker-train get-rail-type-train get-speed-train
         get-station-stay-time-train)

;TDA train, abstraccion de un tren del metro conformado por vagones (TDAs pcar)
;Representacion: lista con elementos (id del tren - fabricante del tren - tipo de vias compatibles con el tren - rapidez del tren (km/h)
;              - tiempo de espera del tren por estacion - carros del tren/TDAs pcar* (sin aridad de secciones, pueden ser varias)



;CONSTRUCTOR TDA train

#|DOM: id (entero) X fabricante (string) X tipo de vias (strig) X rapidez del tren (real positivo)
       X tiempo del tren por estacion (real no negativo) X carros* (TDAs pcar*)
REC: tren del metro (train)
Funcion que crea un elemento del TDA tren, (puede tener o no carros)|#
;Nivel de implementacion: 1
(define train (lambda (id maker rail-type speed station-stay-time . carros)
               (if (and (id-train? id) (maker-train? maker) (rail-type-train? rail-type) (speed-train? speed) (station-stay-time-train? station-stay-time))
                        ;no se verifica carros porque puede que estos se agreges despues en otra funcion
                   (cond
                     [(empty? carros) (list id maker rail-type speed station-stay-time carros)]
                     [(pcars-train? carros) (list id maker rail-type speed station-stay-time carros)]
                     [else null])
                   null)
               )
  )



;FUNCIONES DE PERTENENCIA

#|DOM: tren (TDA train)
REC: bool
Recursion de cola, debido al trabajo de listas con una posible gran cantidad de elementos, las funciones auxiliares llamadas "compatible-model0pcars" y
      "compatible-type-pcars" utilizan recursividad de cola sin dejar estados pendientes
Funcion que verifica si un elemento pertence al TDA train, respetando la compatibilidad de carros|#
;Nivel de implementacion: 1
(define train? (lambda (tren)
                 (if (and (train-without-check-pcars? tren) (not (empty? (get-pcars-train tren)))
                          (compatible-type-pcars (get-pcars-train tren) 0) (compatible-model-pcars (get-pcars-train tren)))
                     #t
                     #f)
                 )
  )

#|DOM: tren (TDA train)
REC: bool
Declarativo
Funcion que determina si un elemento pertenece al TDA train pero sin considerar la velidez del parametro carros del tren (lista de TDAs pcar)|#
(define train-without-check-pcars? (lambda (tren)
                 (if (and (id-train? (car tren)) (maker-train? (cadr tren))
                          (rail-type-train? (caddr tren)) (speed-train? (cadddr tren))
                          (station-stay-time-train? (cadddr (cdr tren))))
                          ;los carros no se verifican aca porque pude ser un tren sin carros agregados
                     #t
                     #f
                     )
                 )
  )

#|DOM: numero de id (entero)
REC: bool
Declarativo
Funcion que determina si un elemento ID puede ser ID de un TDA train|#
(define (id-train? id) (if (integer? id) #t #f))

#|DOM: fabricante del tren (string)
REC: bool
Declarativo
FUncion que determina si un elemento maker puede ser maker de un TDA train|#
(define (maker-train? fabricante) (if (string? fabricante) #t #f))

#|DOM: tipo de vias compatibles con un tren (string)
REC: bool
Declarativo
Funcion que determina si un elemento rail-type puede ser rail-type de un TDA train|#
(define (rail-type-train? tipo-via) (if (string? tipo-via) #t #f))

#|DOM: rapidez de un tren (real positivo)
REC: bool
Declarativo
Funcion que determina si un elemento speed puede ser speed de un TDA train|#
(define (speed-train? rapidez) (if (positive? rapidez) #t #f))

#|DOM: tiempo de espera por estacion de un tren (real no negativo)
REC: bool
Declarativo
Funcion que determina si un elemento station-stay puede ser station-stay de un TDA train|#
(define (station-stay-time-train? tiempo-parado) (if (or (positive? tiempo-parado) (zero? tiempo-parado)) #t #f))

#|DOM: carros de un tren (lista de TDAs pcar)
REC: bool
Recursion de cola
Funcion que determina si un elemento carros puede ser la lista de carros de un TDA train, sin verificar todavia la compatibilidad de carros|#
(define pcars-train? (lambda (carros)
                       (if (empty? carros)
                           #f
                           (if (and (pcar? (car carros)))
                               (if (empty? (cdr carros))
                                   #t
                                   (pcars-train? (cdr carros)))
                               #f)
                           )
                       )
  )



;SELECTORES

#|DOM: tren (TDA train)
REC: id tren (entero positivo) U {null}
Funcion que obtiene el elemento id de un TDA train|#
(define get-id-train (lambda (tren)
                       (if (train-without-check-pcars? tren)
                           (car tren)
                           null)
                       )
  )

#|DOM: tren (TDA train)
REC: maker train (string) U {null}
Funcion que obtiene el elemento maker de un TDA train|#
(define get-maker-train (lambda (tren)
                          (if (train-without-check-pcars? tren)
                              (cadr tren)
                              null)
                          )
  )

#|DOM: tren (TDA train)
REC: rail-type (string) U {null}
Funcion que obtiene el elemento rail-type de un TDA train|#
(define get-rail-type-train (lambda (tren)
                              (if (train-without-check-pcars? tren)
                                  (caddr tren)
                                  null)
                              )
  )

#|DOM: tren (TDA train)
REC: speed (real positivo) U {null}
Funcion que obtiene el elemento speed de un TDA train|#
(define get-speed-train (lambda (tren)
                          (if (train-without-check-pcars? tren)
                              (cadddr tren)
                              null)
                          )
  )

#|DOM: tren (TDA train)
REC: station-stay-time (real no negativo) U {null}
Funcion que obtiene el elemento maker de un TDA train|#
(define get-station-stay-time-train (lambda (tren)
                                      (if (train-without-check-pcars? tren)
                                          (cadddr (cdr tren))
                                          null)
                                      )
  )

#|DOM: tren (TDA train)
REC: carros del tren (lista de TDAs pcar) U {null}
Funcion que obtiene el elemento caros de un TDA train|#
(define get-pcars-train (lambda (tren)
                          (if (train-without-check-pcars? tren)
                              (cadddr (cddr tren))
                              null)
                          )
  )




;MODIFICADORES

#|DOM: tren (TDA train) x nueva id de tren (entero positivo)
REC: tren (TDA train)
Funcion que crea un elemento TDA train a partir del elemento tren de los argumentos, pero con un nueva id |#
(define set-id-train (lambda (tren new-id)
                       (if (and (train-without-check-pcars? tren) (id-train? new-id))
                           (train new-id (get-maker-train tren) (get-rail-type-train tren) (get-speed-train tren)
                                  (get-station-stay-time-train tren) (get-pcars-train tren))
                           null)
                       )
  )

#|DOM: tren (TDA train) x nuevo fabricante del tren (string)
REC: tren (TDA train)
Funcion que crea un elemento TDA train a partir del elemento tren de los argumentos, pero con un nueva fabricante (maker) |#
(define set-maker-train (lambda (tren new-maker)
                       (if (and (train-without-check-pcars? tren) (maker-train? new-maker))
                           (train (get-id-train tren) new-maker (get-rail-type-train tren) (get-speed-train tren)
                                  (get-station-stay-time-train tren) (get-pcars-train tren))
                           null)
                       )
  )

#|DOM: tren (TDA train) x nuevo tipo de riel compatible con el tren (string)
REC: tren (TDA train)
Funcion que crea un elemento TDA train a partir del elemento tren de los argumentos, pero con un nueva tipo de riel compatible (rail-type) |#
(define set-rail-type-train (lambda (tren new-rail)
                       (if (and (train-without-check-pcars? tren) (rail-type-train? new-rail))
                           (train (get-id-train tren) (get-maker-train tren) new-rail (get-speed-train tren)
                                  (get-station-stay-time-train tren) (get-pcars-train tren))
                           null)
                       )
  )

#|DOM: tren (TDA train) x nuevo rapidez del tren (real positivo)
REC: tren (TDA train)
Funcion que crea un elemento TDA train a partir del elemento tren de los argumentos, pero con un nueva rapidez (speed) |#
(define set-speed-train (lambda (tren new-speed)
                       (if (and (train-without-check-pcars? tren) (speed-train? new-speed))
                           (train (get-id-train tren) (get-maker-train tren) (get-rail-type-train tren) new-speed
                                  (get-station-stay-time-train tren) (get-pcars-train tren))
                           null)
                       )
  )

#|DOM: tren (TDA train) x nuevo tiempo de parada en estacion (real no negativo)
REC: tren (TDA train)
Funcion que crea un elemento TDA train a partir del elemento tren de los argumentos, pero con un nuevo tiempo de tren (station-stay-time) |#
(define set-station-stay-time-train (lambda (tren new-time)
                       (if (and (train-without-check-pcars? tren) (station-stay-time-train? new-time))
                           (train (get-id-train tren) (get-maker-train tren) (get-rail-type-train tren) (get-speed-train tren)
                                  new-time (get-pcars-train tren))
                           null)
                       )
  )


#|DOM: tren (TDA train) x nueva lista de carros del tren (lista de TDAs pcar)
REC: tren (TDA train)
Funcion que crea un elemento TDA train a partir del elemento tren de los argumentos, pero con nuevos carros (lista de TDAs pcar)|#
(define set-pcars-train (lambda (tren new-pcars)
                          (if (train-without-check-pcars? tren)
                              (train (get-id-train tren) (get-maker-train tren) (get-rail-type-train tren) (get-speed-train tren)
                                  (get-station-stay-time-train tren) new-pcars)
                              null)
                          )
  )








;OTRAS FUNCIONES


#|DOM: lista de carros (lista de TDAs pcar) X numero 0 (real 0)
REC: bool
Recursion de cola
Funcion que determina si todos los carros de una lista de TDAs pcar son compatibles en su tipo, es decir, que solo los carros de los extremos sean
       del tipo tr y los carros centrales del tipo ct. Ademas de considerar la estructura minima de un tren como dos carros de tipo terminal (tr - tr)|#
(define (compatible-type-pcars carros n)    ;  y compatibilidad de modelos de carro
        (cond
          [(and (empty? carros) (> n 1)) #t];caso en que se recorrieron todos los carros en orden y solo son terminales el primero y ultimo carro
          [(empty? carros) #f] ; caso en que el tren solo tenga un carro, entonces no es un tren valido... La estructura minima del tren es tr-tr
          [(and (eqv? (get-type-pcar (car carros)) tr) (or (= n 0) (= (length carros) 1)))   (compatible-type-pcars (cdr carros) (+ n 1))]; extremos tr
          [(and (eqv? (get-type-pcar (car carros)) ct) (not (or (= n 0) (= (length carros) 1)))) (compatible-type-pcars (cdr carros) (+ n 1))] ;caso de que los centros sean tipo ct
          [else #f] ;caso en que hay algun carro central en un extremo o que haya algun carro terminal en el medio
          )
  )

#|DOM: carros del tren (lista de TDA pcar) X modelo de los carros a verificar (elemento model de TDA pcar)
REC: bool
Recursion de cola
Funcion que determina si todos los carros de una lista de TDAs pcar son del mismo modelo|#
(define (compatible-model-pcars pcars)
        (define (fn-aux carros modelo)
              (if (empty? carros)
                  #t     ;caso en que se recorrio toda la lista de carros y todos son del mismo modelo
                  (if (eqv? (get-model-pcar (car carros)) modelo)
                      (fn-aux (cdr carros) modelo);recursion de cola no deja estados pendientes
                #f
                      )
                  )
          )
        (fn-aux pcars (get-model-pcar (car pcars)))
  )


#| DOM: tren (TDA train)
REC: capacidad (real no negativo)
Recursion natural, debido al calculo numerico simple
Función que permite determinar la capacidad maxima de pasajeros en un tren valido|#
;Nivel de implementacion: 1
(define train-capacity (lambda (tren)
                    ;DOM: (lista de TDAs pcar)      REC: (real no negativo)        Recursion natural
                    ;Funcion que calcula la capacidad de cada carro de una lista de carros y la suma
                    (define fn-aux (lambda (carros)
                                     (if (empty? carros)
                                         0
                                         (+ (get-capacity-pcar (car carros)) (fn-aux (cdr carros))))
                                     )
                      )
                    (if (train? tren)
                        (fn-aux (get-pcars-train tren))
                        0); en el script sale 0 al ingresar un tren invalido
                    )
  )

#|DOM: tren sin validar pcars, con o sin carros  (TDA train) X carro del tren (TDA pcar) X posicion para ingresar carro en la lista de TDAs pcar(entero no negativo)
REC: tren sin validar pcars (TDA Train)
Recursion natural, debido a la facilidad de uso ya que solo es agregar un elemento en una posicion de una lista
Funcion que agrega un carro en una posicion especifica dentro de la lista de TDAs pcars de un tren (TDA train)|#
;Nivel de implementacion: 1
(define train-add-car (lambda (tren carro posicion)
                        ;DOM: (lista) X (elemento) X posicion (entero no negativo)        Rec: (lista)          Recursion natural
                        ;Funcion que agrega un elemeento a una lista en una posicion dada
                        (define (agregar-elemento-en-lista lista elemento indice)
                                (cond
                                  [(empty? lista) (cons elemento null)];caso lista vacia
                                  [(= indice 0) (cons elemento lista)]
                                  [else (cons (car lista) (agregar-elemento-en-lista (cdr lista) elemento (- indice 1)))]
                                  )
                          )
                        (if (compatible-model-pcars (cons carro (get-pcars-train tren)));condicion para no agregar carros incompatibles a un tren
                            (cond
                              [(not (pcar? carro)) tren];caso carro no es TDA pcar
                              [else (list (get-id-train tren) (get-maker-train tren) (get-rail-type-train tren)
                                          (get-speed-train tren) (get-station-stay-time-train tren)
                                          (agregar-elemento-en-lista (get-pcars-train tren) carro posicion)       )]
                              )
                            tren
                        )
                        )
  )

#|DOM: tren sin validar pcars (TDA train) x posicion de carro dentro del tren (entero no negativo)
REC: tren sin validar pcars (TDA train)
Recursion natural, debido a la facilidad del uso de listas en este caso, solo se elimina un elemento en una posicion
Funcion que elimina un carro de un TDA train, de modo que se elimina un carro en una posicion especifica de la lista carros del tren|#
;Nivel de implementacion: 1
(define train-remove-car (lambda (tren posicion)
                           ;DOM: (lista) X posicion (entero no negativo)        REC; (lista)          Recursion natural
                           ;Funcion que elimina un elemento de una lista en una posicion dada
                           (define (eliminar-elemento-en-lista lista position)
                                   (cond
                                     [(empty? lista) null];caso lista vacia se devuelve la misma
                                     [(= position 0) (cdr lista)];caso base llego a la posicion y retorno la cola o caso eliminar la primera posicon
                                     [else (cons (car lista) (eliminar-elemento-en-lista (cdr lista) (- position 1)))]
                                     )
                             )
                           (cond
                             [(> posicion (length (get-pcars-train tren))) tren];posicion invalida (menor al largo de la lista) se devulve el TDA train
                             [else (list (get-id-train tren) (get-maker-train tren) (get-rail-type-train tren)
                                         (get-speed-train tren) (get-station-stay-time-train tren)
                                         (eliminar-elemento-en-lista (get-pcars-train tren) posicion))]
                             )
                           )
  )




