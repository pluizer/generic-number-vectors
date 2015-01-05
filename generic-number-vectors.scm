(module generic-number-vectors
    *
  (import chicken scheme)
  (use srfi-4 fast-generic)
  
(define-syntax %generate-generics
  (er-macro-transformer
   (lambda (exp rename cmp?)
     (apply
      (lambda (_ . <types>)

	`(begin
	  ,@(map (lambda (<type>)

		   `(,(rename 'begin)

		      (,(rename 'define-type) ,<type> ,(symbol-append <type> '?))

		      (,(rename 'define-generic) (number-subvector (,<type> v) from to)
			(,(symbol-append 'sub <type>) v from to))

		      (,(rename 'define-generic) (number-vector-type (,<type> v))
			',<type>)

		      (,(rename 'define-generic) (number-vector-length (,<type> v))
			(,(symbol-append <type> '-length) v))

		      (,(rename 'define-generic) (number-vector-ref (,<type> v) index)
			(,(symbol-append <type> '-ref) v index))

		      (,(rename 'define-generic) (number-vector-set! (,<type> v)
								     index value)
			(,(symbol-append <type> '-set!) v index value))

		      (,(rename 'define-generic) (number-vector->list (,<type> v))
			(,(symbol-append <type> '->list) v))))
		 
		 <types>)

	  (,(rename 'define) (number-vector type . values)
	    (,(rename 'apply) (,(rename 'case) type
		       ,@(map (lambda (<type>)
				`((,<type>) ,<type>))
			      <types>))
		   values))

	  (,(rename 'define) (make-number-vector type n #!optional
						 value nongc finalize)
	    ((,(rename 'case) type
	       ,@(map (lambda (<type>)
			`((,<type>) ,(symbol-append 'make- <type>)))
		      <types>))
	     n value nongc finalize))

	  (,(rename 'define) (list->number-vector type lst)
	    ((,(rename 'case) type
	       ,@(map (lambda (<type>)
			`((,<type>) ,(symbol-append 'list-> <type>)))
		      <types>))
	     lst))
	  )

	)
      exp))))

(%generate-generics u8vector
		   s8vector
		   u16vector
		   s16vector
		   u32vector
		   s32vector
		   f32vector
		   f64vector)

)
