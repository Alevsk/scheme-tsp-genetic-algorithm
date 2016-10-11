; Informacion basica
; ejemplo de individuo: (distancia,(1,2,5,3,7,6,1))

(define (crearPaths population_size path_size matriz_adyacencia)
  (cond ((< population_size 0)
         '()
        )
        (else
           (define newPath (crearPath (length matriz_adyacencia) (length matriz_adyacencia) '()))
           (cons (reverse (cons (car newPath) (reverse newPath)))
             (crearPaths (- population_size 1) path_size matriz_adyacencia)
            )
         )
   )
 )

;funcion que regresa la poblacion con sus caminos y distancia del camino
(define (crearPoblacion population_size path_size matriz_adyacencia)
      (distances (crearPaths population_size path_size matriz_adyacencia) matriz_adyacencia)
   )

;Crear individuos validos

(define (crearPath path_size maxCity citiesList)
  (define randomCity 0)
  (cond ((< path_size 1)
         '()
        )
        (else
         
          (set! randomCity (+ (random maxCity) 1))
          
          (cond ((= 0 (bannedCities randomCity citiesList))
                  (set! citiesList (cons randomCity citiesList))
                  (cons randomCity (crearPath (- path_size 1) maxCity citiesList))
                 )
                (else
                  (crearPath path_size maxCity citiesList)
                 )
           )
        )
   )
 )

(define (bannedCities city citiesList)
   (if(null? citiesList)
      0
      (if(= city (car citiesList))
         1
         (bannedCities city (cdr citiesList))
       )
   )
)

;Seleccion

(define (seleccion population population_size)
  ;Elegimos a la primera mejor mitad y por ultimo agregamos al peor de todos para que haya diversidad
  (cons (seleccion_mejores population population_size population_size) (car (reverse population)))
 )

(define (seleccion_mejores population cont population_size)
  ;(cond ((< cont (/ population_size 2))
  (cond ((< cont 1)
         '()
         )
        (else
          (cons (car population) (seleccion_mejores (cdr population) (- cont 1) population_size))
         )
   )
 )

;Mutacion

(define (mutacion population matrizAdyacencia)
  ;(display population)
  (cond ((null? population) 
         '()
         )
        (else
       
          (define menor (random (+ (length (car (cdr (car population)))) 1)))
          (define mayor (random (- (length (car (cdr (car population)))) 1)))
          
          (cond ((< mayor menor) 
                  (define tmpMayor menor)
                  (set! menor mayor)
                  (set! mayor tmpMayor)
                 )
           )          
          
          (if(> mayor (-(length (car (cdr (car population)))) 2))
             (set! mayor (- (length (car (cdr (car population)))) 2))
                    (if(= mayor menor)
                    (set! menor (- menor 1))
                    )
           )
          
          (cond ((= menor 0)
                 (set! menor 1)
                 (if(= mayor menor)
                    (set! mayor 2)
                    )
                 )
           )
          
          (if(= mayor menor)
            (set! menor 1)
           )
          
          (define distanciaOriginal (car (car population)))
          
          ;(display    (car (cdr (car (cdr (car population))))))
          (display    (car (cdr (car population))))
         
          (define distanciaMutada (distancia matrizAdyacencia (swap (car (cdr (car population))) menor mayor)))
          
          (if (< distanciaMutada distanciaOriginal)
              (cons (cons distanciaMutada (list (swap (car (cdr (car population))) menor mayor))) (mutacion (cdr population) matrizAdyacencia))
              (cons (car population) (mutacion (cdr population) matrizAdyacencia))   
           )
         )
   )  
)

;;;;;;;;;;
(define (swap lst index1 index2)
    (define (find-elements count lst)
      (cond ((null? lst) '())
            ((= count index1)
             (cons (car lst) (find-elements (+ 1 count) (cdr lst))))
            ((= count index2) (car lst))
            (else 
              (find-elements (+ 1 count) (cdr lst)))))

    (define (build-list count elements lst)
      (cond ((null? lst) '()) 
            ((= count index1) 
             (cons (cdr elements) (build-list (+ 1 count) elements (cdr lst))))
            ((= count index2) 
             (cons 
              (car elements) (cdr lst)))
            (else  
              (cons (car lst) (build-list (+ 1 count) elements (cdr lst))))))
    (build-list 0 (find-elements 0 lst) lst))
;;;;;;;;;;

;Distancia

(define (distances paths matrizAdyacencia)
  (if(null? paths)
     '()
     (cons (cons (distancia matrizAdyacencia (car paths)) (list (car paths))) (distances (cdr paths) matrizAdyacencia))))

(define (findCities city counter matrizAdyacencia) ;regresa la lista de las distancias de city de la matriz de adyacencia
  (if(= city counter)
     (car matrizAdyacencia)
     (findCities city (+ counter 1) (cdr matrizAdyacencia))))

(define (findDistance toCity counter fromCityDistances) ;regresa la distancia entre la primera y la segunda ciudad
  (if(= toCity counter)
     (car fromCityDistances)
     (findCities toCity (+ counter 1) (cdr fromCityDistances))))

(define (distancia matrizAdyacencia path) ;funcion para obtener la distancia total de un camino
  ;(display path)
  (if (null? (cdr path))
      0
      (+ (distanceFromTo (car path) (car (cdr path)) matrizAdyacencia) (distancia matrizAdyacencia (cdr path)))))

;Ordenamiento

(define (distanceFromTo city1 city2 matrizAdyacencia) ; regresa la distancia de 2 ciudades
  (findDistance city2 1 (findCities city1 1 matrizAdyacencia)))

(define (sortPopulation population) ;este metodo ordena la lista
  (if(null? population)
     '()
     (cons (highestElement population 0 '()) (sortPopulation (eraseFromPopulation population (highestElement population 0 '()))))))


(define (eraseFromPopulation population element); este metodo borra un elemento de una lista
  (if (null? population)
      '()
      (if (equal? element (car population))
          (eraseFromPopulation (cdr population) '())
          (cons (car population) (eraseFromPopulation (cdr population) element)))))

(define (highestElement population max element);buscar el elemento con mayor valor
  (if (null? population)
      element
      (if (> (car (car population)) max)
          (highestElement (cdr population) (car (car population)) (car population))
          (highestElement (cdr population) max element))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tsp matriz_adyacencia); en inicio se recibe la matriz de adyacencia (lista de listas)
  (if(null? matriz_adyacencia)
     '() 
      (start matriz_adyacencia)
     )
  )

(define (start matriz_adyacencia)
	(mainLoop (sortPopulation (crearPoblacion (* (length matriz_adyacencia) 3) (length matriz_adyacencia) matriz_adyacencia)) matriz_adyacencia 10)
 )

(define (mainLoop poblacion matriz_adyacencia generaciones)
  (display poblacion)
  (display " / ")
  (cond ((< generaciones 0)
         (display poblacion)
         )
        (else
           ;Seleccion de individuos mas optimos
           (define IndividuosSeleccionados (seleccion poblacion (length poblacion)))
                      
           ;Cruza y mutacion
           (define IndividuosMutados (mutacion poblacion matriz_adyacencia))
           
           (mainLoop (sortPopulation IndividuosMutados) matriz_adyacencia (- generaciones 1))
           
           ;(mainLoop (sortPopulation (mutacion (seleccion poblacion (length poblacion)) matriz_adyacencia)) matriz_adyacencia (- generaciones 1))
         )
    )
)