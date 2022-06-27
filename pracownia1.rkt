#lang racket

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join)

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Poland"  517 #t)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define (empty-table columns) (table columns '()))



; Utility funkcje

(define (find-number l val n)
  (cond                                         ;znajduje numer podanej kolumny w tabeli (find dziala dla list, to istotna roznica w kwestii dzialania)
    [[null? l] -1]                              ;oczywiscie istnieje funkcja findf, ktora robi more or less to samo (w tej linijce by bylo #f IIRC)
    [[equal? (column-info-name (car l)) val] n] ;ale customowa implementacja wydaje sie czytelniejsza
    [else [find-number (cdr l) val (+ 1 n)]])) 



(define (reverse-parse l)                     ;odwraca listy-itemy w liscie list
  (if (null? l)
      null
      (cons (reverse (car l)) (reverse-parse (cdr l)))))



(define (<? a b)
  (cond                                       ;(niemalze) uniwersalna funkcja do porownywania               
    [[number? a]
     [< a b]]
  
    [[string? a]
     [string<? a b]]
  
    [[symbol? a]
     [symbol<? a b]]
  
    [[boolean? a]
     [and (not a) b]]))



(define (find l x i) (cond                    ;znajduje pozycje na liscie (lub informuje, ze nie ma elementu)
                     [[null? l]               ;nieco inna niz find-number, podtrzymuje opinie, ze tak czytelniej niz z findf i powtarzajacymi sie lambdami
                      -1]
                     
                     [[equal? (car l) x]
                      i]
                     
                     [else
                      [find (cdr l) x (+ i 1)]]))



; Wstawianie

(define (correct-type? pattern row)           ;sprawdza, czy wiersz pasuje do tabeli 
    (if
      [or (null? pattern) (null? row)]
      [and (null? pattern) (null? row)]       ;wiersz musi miec taka sama wielkosc jak schemat tabeli
      
      [and                                                                              
        [if (equal? (column-info-type (car pattern)) 'string)
            (string? (car row))
            #t]
        
        [if (equal? (column-info-type (car pattern)) 'number)
            (number? (car row))
            #t]
        
        [if (equal? (column-info-type (car pattern)) 'symbol)
            (symbol? (car row))
            #t]
        
        [if (equal? (column-info-type (car pattern)) 'boolean)
            (boolean? (car row))
            #t]
        
        [correct-type? (cdr pattern) (cdr row)]]))           ;oczywiscie musi smigac dla nie tylko pierwszego elementu list



(define (table-insert row tab)
 (if [correct-type? (table-schema tab) row]                  ;czy wiersz pasuje do tabeli?
     [table                                                  ;tak
      (table-schema tab)
      (cons row (table-rows tab))]
     
     [error "Błąd: nieprawidłowy typ wiersza"]))             ;nie



; Projekcja

(define (build-rows t tab n) ;dokleja n-ta kolumne tablicy tab do t
    (if (null? tab)
        t
        (cons (cons (list-ref (car tab) n) (car t))
              (build-rows (cdr t) (cdr tab) n))))

  
  (define (table-insert-xd t col tab) ;wstawia do t kolumne o podanej nazwie (na poczatek)
    (let ([n (find-number (table-schema tab) col 0)])
      (if (= n -1) 
      t
      
       (table
        [cons (list-ref (table-schema tab) n)
              (table-schema t)]
        [build-rows (table-rows t) (table-rows tab) n]))))

  
  (define (table-project-help t cols tab) ;konstruuje tabele (wspak, jak wyzej zaznaczylem)
    (if (null? cols) 
        t
        (table-project-help (table-insert-xd t (car cols) tab) (cdr cols) tab)))
  
    

(define (table-project-rev cols tab) ;sklada docelowa tabele (uwaga, na odwrot i trzeba ja odwrocic!)
  (table-project-help (table '()
                             (build-list (length (table-rows tab)) (lambda (x) '())))
                      cols tab))
  


(define (table-project cols tab)
  (let ([T (table-project-rev cols tab)]) ;poprawia tabele do odpowiedniej wartosci
    (table [reverse (table-schema T)]
           [reverse-parse (table-rows T)])))



; Sortowanie
(define (list<=? a b cmp l) ;porownuje listy (tej samej dlugosci, zakladam, ze dane sa poprawne) wg komparatora
  (if (null? cmp)
      #t
      (let ([C (find-number l (car cmp) 0)])
        (if (= C -1)
            #t
            (let ([A (list-ref a C)] [B (list-ref b C)])
              (cond                 
                [[<? A B] #t]
                [[equal? A B] (list<=? a b (cdr cmp) l)]
                [else #f]))))))



(define (split l)                               ;mergesort (rozbity na kilka intuicyjnie zrozumialych procedur)
  (define (split-help l l0 k n)                 ;wiem, ze wrecz zasugerowano, by uzyc sorta z biblioteki standardowej
    (if (>= k n)                                ;ale nie chce marnowac swojej roboty (+ chyba nie jest jakos tragicznie napisany?)
        (cons (reverse l0) l)
        (split-help
         (cdr l)
         (cons (car l) l0) (+ k 1) (- n 1))))
  
  (split-help l null 0 (length l)))



(define (merge a b cmp l)
  (cond
    [(null? a) b]
    [(null? b) a]
    [(if (list<=? (car a) (car b) cmp l)
         (cons (car a) (merge (cdr a) b cmp l))
         (cons (car b) (merge a (cdr b) cmp l)))]))



(define (merge-sort t cmp l)
  (let ([x (split t)])
  (if (> (length t) 1)
      (merge (merge-sort (car x) cmp l)
             (merge-sort (cdr x) cmp l) cmp l)
      t)))



(define (table-sort cols tab)
  (table
   (table-schema tab)
   (merge-sort (table-rows tab) cols (table-schema tab))))



; Selekcja

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (test-eq row f schema)
  (let ([n (find-number schema (eq-f-name f) 0)])
    (if (= n -1)
        #f
        (equal? (eq-f-val f) (list-ref row n)))))



(define (test-eq2 row f schema)
  (let ([n (find-number schema (eq2-f-name f) 0)]
        [m (find-number schema (eq2-f-name2 f) 0)])
    (cond
      [[= n -1] #f] 
      [[= m -1] #f]
      [else [equal?
             (list-ref row n)
             (list-ref row m)]])))



(define (test-lt row f schema)
  (let ([n (find-number schema (lt-f-name f) 0)])
    (if (= n -1)
        #f
        (<? (list-ref row n) (lt-f-val f)))))



(define (test-f row f schema)
  (cond
    [[boolean? f] f] ;szczegolny przypadek
    [[and-f? f] [and (test-f row (and-f-l f) schema) (test-f row (and-f-r f) schema)]]
    [[or-f? f] [or (test-f row (or-f-l f) schema) (test-f row (or-f-r f) schema)]]
    [[not-f? f] [not (test-f row (not-f-e f) schema)]]
    [[eq-f? f] [test-eq row f schema]]
    [[eq2-f? f] [test-eq2 row f schema]]
    [[lt-f? f] [test-lt row f schema]]))



(define (table-select form tab)
  (table
   (table-schema tab)
   (filter (lambda (x) (test-f x form (table-schema tab))) (table-rows tab))))



; Zmiana nazwy

(define (table-rename col ncol tab)
  (define (new-schema oldschema schema col ncol)
    (cond                                                                       ;tworzy nowy schemat
      [[null? schema] null]                                                     ;nie ma szukanej kolumny, zostawiamy as-is
      [[equal? (column-info-name (car schema)) col]                             ;czy znalezlismy szukana kolumne?
       [cons (column-info ncol (column-info-type (car schema))) (cdr schema)]]  ;tak, zmieniamy nazwe
      [else [cons (car schema) (new-schema oldschema (cdr schema) col ncol)]])) ;nie, szukamy szczescia dalej

  
 (table (new-schema
         (table-schema tab)
         (table-schema tab) col ncol) (table-rows tab)))  



; Złączenie kartezjańskie

(define (build-new-rows elem rows) ;spina jeden wiersz tabeli 1 z wierszami tabeli 2
  (if (null? rows) null
      (cons
       (append elem (car rows))
       (build-new-rows elem (cdr rows))))) 


   
(define (build-new-rows2 rows1 rows2) ;generuje wiersze nowej tabeli
  (if (null? rows1)
      null
      (append
       (build-new-rows (car rows1) rows2)
       (build-new-rows2 (cdr rows1) rows2)))) 



(define (table-cross-join tab1 tab2)
  (table (append (table-schema tab1) (table-schema tab2))  ;nowy schemat (sklejone oba stare schematy)
         (build-new-rows2 (table-rows tab1) (table-rows tab2)))) 



; Złączenie

(define (labels t1 t2 xs ys) ;segregacja na wystepujace w obu tabelach i wystepujace w tylko jednej tabeli 
  (cond 
    [[null? t2] [if (null? t1)
                    (cons xs ys)
                    (labels t2 t1 xs ys)]]
                                  
    [[null? t1] [if (= -1 (find xs (car t2) 0))
                    (labels t1 (cdr t2) xs (cons (car t2) ys))
                    (labels t1 (cdr t2) xs ys)]]
                                  
    [[not (= -1 (find t2 (car t1) 0))] [labels (cdr t1) t2 (cons (car t1) xs) ys]]
                                  
    [else [labels (cdr t1) t2 xs (cons (car t1) ys)]])) 



(define (labels2 l) ;bierze tylko nazwy kolumn, a nie cale ich definicje
  (if (null? l)
      null
      (cons (column-info-name (car l)) (labels2 (cdr l)))))



(define (formula row l schema) ;generuje formule "rowne na wyroznionych pozycjach pierwszemu elementowi tabeli"
  (if (null? l)
      #t ;pusty warunek jest zawsze prawdziwy
      (and-f (eq-f (car l) (list-ref row (find-number schema (car l) 0))) (formula row (cdr l) schema))))

(define (equal-to-first-help t f schema) ;znajduje elementy identyczne z pierwszym na kluczowych (wystepujacych w obu tabelach) pozycjach
  (cond 
    [[null? t] null]
    [[test-f (car t) f schema] [cons (car t) (equal-to-first-help (cdr t) f schema)]]
    [else null]))



(define (equal-to-first t l schema)
  (let* ([f (formula (car t) (reverse l) schema)]
         [T (equal-to-first-help t f schema)])
    (cons T (length T))))



(define (natural-merge-help row row2 l schema schema2) ;scala 2 wiersze (roznica wzgledem funkcji z cross-join jest taka, ze ta nie duplikuje)
  (cond
    [[null? l] null]
    [[= -1 (find schema (car l) 0)] [cons
                                     (list-ref row2 (find schema2 (car l) 0))
                                     (natural-merge-help row row2 (cdr l) schema schema2)]]
    [else [cons
           (list-ref row (find schema (car l) 0))
           (natural-merge-help row row2 (cdr l) schema schema2)]]))



(define (natural-merge t t2 t2b l schema schema2 n m nm mm)  ;merge'uje w zadany sposob tablice (ale paskudna funkcja)
  (cond
    [[= n nm] null] ;koniec merge'owania
    
    [[= m mm]
     [natural-merge (cdr t) t2b t2b l schema schema2 (+ n 1) 0 nm mm]] ;mdli mnie od tej funkcji, not gonna lie
    
    [else
     [cons
      (natural-merge-help (car t) (car t2) l schema schema2)
      (natural-merge t (cdr t2) t2b l schema schema2 n (+ m 1) nm mm)]]))



(define (compare a b l l2 cmp) ;porownuje 2 rozne listy (ostro)
  (if (null? cmp)
      #f
      (let* ([C (find-number l (car cmp) 0)]
             [D (find-number l2 (car cmp) 0)]
             [A (list-ref a C)]
             [B (list-ref b D)])
        (cond
          [[<? A B] #t]
          [[equal? A B] [if (null? (cdr cmp)) #f (compare a b l l2 (cdr cmp))]]
          [else #f]))))



(define (merge-tables t t2 l l2 schema schema2) ;scala tabele tak jak nalezy, dziala podobnie do mergesorta
  (cond 
    [[or (null? t) (null? t2)] null] ;iloczyn kartezjanski i tak bedzie wtedy pusty
                                              
    [[compare (car t) (car t2) schema schema2 l] ;pierwsze elementy nie sa rowne, wiec pobieramy nastepne
     [merge-tables (cdr t) t2 l l2 schema schema2]]
                                              
    [[compare (car t2) (car t) schema2 schema l]
     [merge-tables t (cdr t2) l l2 schema schema2]]
                                              
    [else [let* ([A (equal-to-first t l schema)]
                 [B (equal-to-first t2 l schema2)])
            (append (natural-merge (car A) (car B) (car B) l2 (labels2 schema) (labels2 schema2) 0 0 (cdr A) (cdr B)) ;natural-merge strikes back
                    (merge-tables [list-tail t (cdr A)] [list-tail t2 (cdr B)] l l2 schema schema2))]]))



(define (table-natural-join tab1 tab2)
  (let* ([L (labels (table-schema tab1) (table-schema tab2) null null)] 
         [l (labels2 (car L))] ;znajduje wspolne kolumny
         [T1 (table-sort l tab1)] ;wstepnie sortuje obie tabele wg wspolnych kolumn
         [T2 (table-sort l tab2)]
         [X (append (car L)(cdr L))])
    (table X                     
           (merge-tables (table-rows T1) (table-rows T2) l (labels2 X) (table-schema tab1) (table-schema tab2)))))