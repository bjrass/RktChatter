(module Map racket 
  (require (file "Common.rkt"))
  
  ;; \brief
  ;; Container for mapping keys to values
  ;;
  ;; \remark
  ;; Internally uses a list of pairs of keys/values
  ;;
  ;; \remark
  ;; Simple to use, but for large sets where the keys have the same type
  ;; a map with a binary tree structure should be considered.
  ;;
  ;; \todo
  ;; Should implement a container interface if more custom containers
  ;; are made.
  (define Map%
    (class object%
      (super-new)
      (init (compare equal-values?))
      
      (define m-internal-map '())
      (define comparator compare)
      
      ;; \todo
      ;; Should implement a iterator interface if more custom containers
      ;; are made.
      (define Iterator%
        (class object%
          (super-new)
          (init map)
          
          (define m-remaining map)
          
          (define/public (next!)
            (set! m-remaining (mcdr m-remaining)))
          (define/public (next)
            (new Iterator% [map (mcdr m-remaining)]))
          (define/public (finished?)
            (null? m-remaining))
          (define/public (key)
            (mcar (mcar m-remaining)))
          (define/public (value)
            (mcdr (mcar m-remaining)))))
      
      ;; \brief
      ;; Creates a new iterator for iterating the map
      (define/public (getIterator) (new Iterator% [map m-internal-map]))
      
      ;; \brief
      ;; Gives whether the map is empty
      (define/public (empty?) (null? m-internal-map))
      
      ;; \brief
      ;; Finds the value of 'key' in the map
      (define/public (find key)
        (define (iter remn)
          (cond
            ((null? remn) TRUE-NULL)
            
            ((comparator (mcar (mcar remn)) key)
             (mcdr (mcar remn)))
            
            (else (iter (mcdr remn)))))
        (iter m-internal-map))
      
      ;; \brief
      ;; Puts 'value' to be associated with 'key' in the map
      (define/public (put! key value)
        (define (iter remn)
          (cond
            
            ; If there already is a mapping of key, change it's value
            ((not (null? remn))
             (if (comparator (mcar (mcar remn)) key)
                 (set-mcdr! (mcar remn) value)
                 (iter (mcdr remn))))
            
            ; Otherwise, prepend a new mapping to the internal map
            (else (set! m-internal-map (mcons (mcons key value) m-internal-map)))))
        (iter m-internal-map))
      
      ;; \brief
      ;; Returns whether there is a mapping for 'key' in the map
      (define/public (has? key)
        (define (iter remn)
          (cond
            ((null? remn) #f)
            
            ((comparator (mcar (mcar remn)) key)
             #t)))
        (iter m-internal-map))
      
      ;; \brief
      ;; Erases the mapping of 'key' in the map, if there is one
      (define/public (erase! key)
        (define (iter remn prev)
          (unless (null? remn)
            (if (comparator (mcar (mcar remn)) key)
                (if (null? prev)
                    (set! m-internal-map (mcdr remn))
                    (set-mcdr! prev (mcdr remn)))
                (iter (mcdr remn) remn))))
        (iter m-internal-map '()))
      
      ))(provide Map%))