#lang racket
(require "TDA-pcar.rkt")
(require "TDA-section.rkt")
(require "TDA-station.rkt")
(require "TDA-type-station.rkt")
(require "TDA-line.rkt")
(require "TDA-train.rkt")
(require "TDA-driver.rkt")


;TDA subway, abstraccion de sistema de metro
;Representacion: lista con elementos
;             (id metro - nombre metro - lista de trenes del metro - lista de lineas del metro -
;              lista de conductores - lista de elementos del tipo par donde el primer elemento
;              es la id de la linea y el segundo es una lista de trenes asociados a esa linea  -
;              lista de elementos con (id conductor - id tren - hora de partida - estacion de
;              partida - estacion de llegada)  para determinar el recorrido de trenes y conductores )




;CONSTRUCTOR
#|DOM: id (entero) X nombre metro (string)
REC: metro (subway)
Funcion que crea un elemento del TDA subway, el que es una lista de 7 elementos, en un inicio, el TDA
     solo tendra 2 elementos (id y nombre), los demas seran agregamos posteriormente en otras funciones
|#
(define subway (lambda (id nombre)(list id nombre null null null null null)))




;FUNCION DE PERTENECIA
(define subway-without-all-elements? (lambda (metro)
                                    (and (integer? (car metro)) (string? (cadr metro)) (list? (caddr metro))
                                         (list? (cadddr metro)) (list? (cadddr (cdr metro)))
                                         (list? (cadddr (cddr metro))) (list? (cadddr (cdddr metro))))
                                    )
  )





;SELECTORES
#|DOM: metro (subway)
REC: id (entero)
Funcion que retorna el elemento id de un TDA subway|#
(define get-id-subway (lambda (metro)(if (subway-without-all-elements? metro) (car metro) null)))

#|DOM:metro (subway)
REC: nombre metro (string)
Funcion que retorna el elemento nombre de un TDA subway|#
(define get-name-subway (lambda (metro)(if (subway-without-all-elements? metro) (cadr metro) null)))

#|DOM:metro (subway)
REC: lista de trenes (list TDAs train)
Funcion que retorna el elemento trenes de un TDA subway|#
(define get-trains-subway (lambda (metro)(if (subway-without-all-elements? metro) (caddr metro) null)))

#|DOM:metro (subway)
REC: lista de lineas (list TDAs line)
Funcion que retorna el elemento lineas de un TDA subway|#
(define get-lines-subway (lambda (metro)(if (subway-without-all-elements? metro) (cadddr metro) null)))

#|DOM:metro (subway)
REC: lista de conductores (list TDAs driver)
Funcion que retorna el elemento conductores de un TDA subway|#
(define get-drivers-subway (lambda (metro)(if (subway-without-all-elements? metro) (cadddr (cdr metro)) null)))

#|DOM:metro (subway)
REC: lista de pares id-line y lista trains
Funcion que retorna el elemento line-trains de un TDA subway|#
(define get-line-trains-subway (lambda (metro)(if (subway-without-all-elements? metro) (cadddr (cddr metro)) null)))

#|DOM:metro (subway)
REC:lista de elementos de recorrido (id conductor - id tren - hora de partida - estacion de partida
   - estacion de llegada)
Funcion que retorna el elemento rutas de un TDA subway|#
(define get-routes-subway (lambda (metro)(if (subway-without-all-elements? metro) (cadddr (cdddr metro)) null)))





;MODIFICADORES
#|DOM: metro (subway) X id (entero)
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento id|#
(define (set-id-subway metro new-id)
  (if (subway-without-all-elements? metro)
      (list new-id (get-name-subway metro) (get-trains-subway metro) (get-lines-subway metro)
           (get-drivers-subway metro) (get-line-trains-subway metro) (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X nombre (string)
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento nombre|#
(define (set-name-subway metro new-name)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) new-name (get-trains-subway metro) (get-lines-subway metro)
            (get-drivers-subway metro) (get-line-trains-subway metro) (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X trenes (list TDAs train) | null
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento trains|#
(define (set-trains-subway metro list-new-trains)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) (get-name-subway metro) list-new-trains (get-lines-subway metro)
            (get-drivers-subway metro) (get-line-trains-subway metro) (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X lineas (lista TDAs lines) | null
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento lines|#
(define (set-lines-subway metro list-new-lines)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro) list-new-lines 
            (get-drivers-subway metro) (get-line-trains-subway metro) (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X conductores (lista TDAs driver) | null
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento drivers|#
(define (set-drivers-subway metro list-new-drivers)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro) (get-lines-subway metro) 
            list-new-drivers (get-line-trains-subway metro) (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X line-trains (lista de pares (id linea - lista de TDAs train)) | null
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento line-trains|#
(define (set-line-trains-subway metro list-new-line-trains)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro) (get-lines-subway metro) 
            (get-drivers-subway metro) list-new-line-trains (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X rutas (lista de elementos de ruta) | null
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento routes|#
(define (set-routes-subway metro list-new-routes)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro) (get-lines-subway metro) 
            (get-drivers-subway metro) (get-line-trains-subway metro) list-new-routes)
      null)
  )











;OTRAS FUNCIONES

#|DOM: metro (subway) X trenes+ (TDAs train+ ) (puede ser 1 o mas)
REC: metro (subway)
Recursion natural
Funcion que agrega los trenes del argumento al parametro de trenes dentro del subway, usa la funcion
     fn-aux-recursion para cumplir el requisito de implementacion de emplear algun tipo de recursividad|#
(define subway-add-train (lambda (metro . trenes)
                           ;funcion que usa la recursion natural para retornar una lista con los elementos
                           ;de una lista de entrada. Requisito de implementacion 
                           (define (fn-aux-recursion lista)
                             (if (empty? lista)
                                 null
                                 (cons (car lista) (fn-aux-recursion (cdr lista))))
                             )
                           
                           (if (subway-without-all-elements? metro)
                               (list (get-id-subway metro) (get-name-subway metro)
                                     (fn-aux-recursion trenes) (get-lines-subway metro)
                                     (get-drivers-subway metro) (get-line-trains-subway metro)
                                     (get-routes-subway metro))
                               null)
                           )
  )


#|DOM: metro (subway) X lineas+ (TDAs line+) (puede ser 1 o mas)
REC: metro (subway)
NO RECURSION a diferencia de subway-add-train
Funcion que agrega las lineas del argumento al parametro de lineas del subway|#
(define subway-add-line (lambda (metro . lineas)
                           (if (subway-without-all-elements? metro)
                               (list (get-id-subway metro) (get-name-subway metro)
                                     (get-trains-subway metro) lineas
                                     (get-drivers-subway metro) (get-line-trains-subway metro)
                                     (get-routes-subway metro))
                               null)
                           )
  )



#|DOM: metro (subway) X conductores+ (TDAs driver+) (puede ser 1 o mas)
REC: metro (subway)
SIN RECURSION
Funcion que agrega los conductores del argumento al parametro de drivers del subway|#
(define subway-add-driver (lambda (metro . conductores)
                           (if (subway-without-all-elements? metro)
                               (list (get-id-subway metro) (get-name-subway metro)
                                     (get-trains-subway metro) (get-lines-subway metro)
                                     conductores (get-line-trains-subway metro)
                                     (get-routes-subway metro))
                               null)
                           )
  )


#|DOM: metro (subway)
REC: null
Funcion que imprime en panta todos los elementos de un metro TDA subway. Utiliza varias funciones
   auxiliares que estan encapsuladas dentro de la misma|#
(define (subway->string metro)

  ;DOM: conductores (lista de TDAs driver)      REC: no
  (define (printear-drivers drivers)
    ;DOM: conductor (TDA driver)     REC: no
    (define (printear-conductor conduc)
      (display "\n   Id conductor: ")
      (display (number->string (get-id-driver conduc)))
      (display "\n   Nombre: ")
      (display (get-name-driver conduc))
      (display "\n   Fabricante de trenes que conduce: ")
      (display (get-maker-train-driver conduc))
      (display "\n")
      )
    (map (lambda (s) (printear-conductor s)) drivers)
    )

  ;DOM: trenes (lista de TDAs train)    REC: no
  (define (printear-trenes list-trains)
    ;DOM: tren (TDA train)     REC: no
    (define (printear-tren tren)
      (display "\n   Id tren: ")
      (display (number->string (get-id-train tren)))
      (display "\n   Fabricante: ")
      (display (get-maker-train tren))
      (display "\n   Tipo de riel: ")
      (display (get-rail-type-train tren))
      (display "\n   Rapidez: ")
      (display (number->string (get-speed-train tren)))
      (display (string-append "\n   Tiempo de espera por estacion: "
                            (number->string (get-station-stay-time-train tren))
                            " minutos\n"))
      )
    (map (lambda (a) (printear-tren a)) list-trains)
    )

  ;DOM: lineas (lista de TDAs line)   REC: no
  (define (printear-lineas lineas)
    ;DOM: linea (TDA line)    REC: no
    (define (printear-linea linea)
      ;DOM: seccion (TDA section)     REC:no
      (define (printear-seccion seccion)
        (display "\n      Estacion 1: ")
        (display (string-append (get-name-station (get-station1-section seccion))
                         "  Tipo: " (get-type-station-station (get-station1-section seccion))))
        (display "\n      Estacion 2: ")
        (display (string-append (get-name-station (get-station2-section seccion))
                         "  Tipo: " (get-type-station-station (get-station2-section seccion))))
        (display "\n      Distancia entre estaciones: ")
        (display (number->string (get-distance-section seccion)))
        (display "km\n      Costo monetario: ")
        (display (number->string (get-cost-section seccion)))
        (display "\n")
        )

      (display "\n   Id linea: ")
      (display (number->string (get-id-line linea)))
      (display (string-append "\n   Nombre: " (get-name-line linea)))
      (display "\n   Tipo de riel: ")
      (display (get-rail-type-line linea))
      (display "\n   Secciones:")
      (map (lambda (a) (printear-seccion a)) (get-sections-line linea))
      (display "\n")
      )
    (map (lambda (a)(printear-linea a)) lineas)
    )
  
  (display "\n\nDatos del sistema de metro\n")
  (display (string-append "Nombre: " (get-name-subway metro)  "\n\n"))
  (display "Trenes:")
  (printear-trenes (get-trains-subway metro))
  (display "\nLineas:")
  (printear-lineas (get-lines-subway metro))
  (display "\nConductores:")
  (printear-drivers (get-drivers-subway metro))
  null; devolvemos un null al finalizar de imprimir los datos del subway
  )










(define (1printear-drivers drivers)
  (define (printear-conductor conduc)
    (display "\n   Id conductor: ")
    (display (number->string (get-id-driver conduc)))
    (display "\n   Nombre: ")
    (display (get-name-driver conduc))
    (display "\n   Fabricante de trenes que conduce: ")
    (display (get-maker-train-driver conduc))
    (display "\n")
    )
  (map (lambda (s) (printear-conductor s)) drivers)
  )

(define (1printear-trenes list-trains)
  (define (printear-tren tren)
    (display "\n   Id tren: ")
    (display (number->string (get-id-train tren)))
    (display "\n   Fabricante: ")
    (display (get-maker-train tren))
    (display "\n   Tipo de riel: ")
    (display (get-rail-type-train tren))
    (display "\n   Rapidez: ")
    (display (number->string (get-speed-train tren)))
    (display (string-append "\n   Tiempo de espera por estacion: "
                          (number->string (get-station-stay-time-train tren))
                          " minutos\n"))
    )
  (map (lambda (a) (printear-tren a)) list-trains)
  )







(define (1printear-lineas lineas)
  
  (define (printear-linea linea)
    
    (define (printear-seccion seccion)
      (display "\n      Estacion 1: ")
      (display (string-append (get-name-station (get-station1-section seccion))
                       "  Tipo: " (get-type-station-station (get-station1-section seccion))))
      (display "\n      Estacion 2: ")
      (display (string-append (get-name-station (get-station2-section seccion))
                       "  Tipo: " (get-type-station-station (get-station2-section seccion))))
      (display "\n      Distancia entre estaciones: ")
      (display (number->string (get-distance-section seccion)))
      (display "km\n      Costo monetario: ")
      (display (number->string (get-cost-section seccion)))
      (display "\n")
      )

    (display "\n   Id linea: ")
    (display (number->string (get-id-line linea)))
    (display (string-append "\n   Nombre: " (get-name-line linea)))
    (display "\n   Tipo de riel: ")
    (display (get-rail-type-line linea))
    (display "\n   Secciones:")
    (map (lambda (a) (printear-seccion a)) (get-sections-line linea))
    (display "\n")
    )

  (map (lambda (a)(printear-linea a)) lineas)
  )








