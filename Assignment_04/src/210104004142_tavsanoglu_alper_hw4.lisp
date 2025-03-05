(defun prolog_prove (axioms query)
  (labels(
    
    
    (variable_cap (var)									;;For checking string to see valid and capital 
      (and (stringp var)
           (char<= #\A (char var 0) #\Z)))
    
    
    (query_get (q)									;;For getting the variable from a query
      (let ((varg nil))
        (dolist (predic q)
          (dolist (codit predic)
            (when (variable_cap codit)
              (push codit varg))))
 	(car varg)))
    
    
    (bind_get (codit subst)								;;For getting the binding for a term
      (if (variable_cap codit)
          (let ((bind_arg (assoc codit subst :test #'equal)))
            (if bind_arg
                (bind_get (cdr bind_arg) subst) codit))
          codit))
    
    
    (subst_cond (predic subst)								;;To Substitute terms in predicate based on subst list
      (mapcar (lambda (codit) (bind_get codit subst)) predic))
    
    
    (renm_args (axom dpth)								;;To Rename variables in axiom to avoid variable clashes
      (labels ((renm_codit (codit)
                 (if (variable_cap codit)
                     (concatenate 'string codit (write-to-string dpth))
			codit)))
        (if (= (length axom) 1)
            (list (cons (caar axom)
                       (mapcar #'renm_codit (cdar axom))))
            (cons (cons (caar axom)
                       (mapcar #'renm_codit (cdar axom)))
                  (cons "<" (mapcar (lambda (predic)
                                    		(cons (car predic)
                                          		(mapcar #'renm_codit (cdr predic))))
                                  (cddr axom)))))))
    
    
    (unif_pred (predf predl subst)							;;To unify two predicates and update the subst list
      (when (and (equal (length predf) (length predl))
                 (equal (car predf) (car predl)))
        (let ((n_subst subst))
          (loop for l1 in (cdr predf)
                for l2 in (cdr predl)
                do (let ((v1 (bind_get l1 n_subst))
                        (v2 (bind_get l2 n_subst)))
                     (cond ((equal v1 v2))
                           ((variable_cap v1)
                            (setf n_subst (acons v1 v2 n_subst)))
                           ((variable_cap v2)
                            (setf n_subst (acons v2 v1 n_subst)))
                           (t (return-from unif_pred nil)))))
         n_subst)))
    
    
    (prove_cond (targs axioms subst dpth pth)						;;To proof prolog
      (if (null targs)
          (list subst)
          (let* ((targ (car targs))
                 (rest_targs (cdr targs))
                 (results nil)
                 (cur_targ (subst_cond targ subst)))
            (unless (or (>= dpth 10)
                       (member cur_targ pth :test #'equal))
              (dolist (axiom axioms)
                (let* ((renm_axom (renm_args axiom dpth))
                       (head (car renm_axom))
                       (n_subst (unif_pred targ head subst)))
                  (when n_subst
                    (if (= (length renm_axom) 1)
                        (setf results (append results (prove_cond rest_targs axioms n_subst dpth (cons cur_targ pth))
                                      )
                        )
                        (setf results
                              (append results
                                      (prove_cond (append (cddr renm_axom) rest_targs) axioms n_subst (1+ dpth) (cons cur_targ pth))
                               ))))))
              results)))))


    (let* ((var (query_get query))					;; Get the variable from the query and start proving
           (results (prove_cond query axioms nil 0 nil)))
      (if var
          (remove-duplicates (mapcar (lambda (subst) (list var (bind_get var subst))) results) :test #'equal)
          (if results t nil)
          )
     )
 )
)


;; Test function with different examples
(defun test_prolog ()
  (format t "~%Simplified Version of Prolog Theorem Prover Tests~%")

  ;;Test 1: Homework Example
  (format t "~%Test 1: Homework Example~%~%")
	(let ((axioms '(  
                    (("father" "jim" "jill")) 
                    (("mother" "mary" "jill")) 
                    (("father" "samm" "jim")) 
                    (("ancestor" "X" "Y") "<" ("parent" "X" "Y")) 
                    (("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y")) 
                    (("parent" "X" "Y") "<" ("mother" "X" "Y")) 
                    (("parent" "X" "Y") "<" ("father" "X" "Y")))) 
        	(query1 '(("ancestor" "X" "jill")))  
        	(query2 '(("ancestor" "X" "jill")("mother" "X" "bob")))
        	(query3 '(("ancestor" "X" "jill")("father" "X" "jim"))))
   	(format t "Query-1 (ancestor X jill): ~A~%" (prolog_prove axioms query1))
   	(format t "Query-2 (ancestor X jill) (mother X bob): ~A~%" (prolog_prove axioms query2))
	(format t "Query-3 (ancestor X jill) (father X jim): ~A~%" (prolog_prove axioms query3)))
	
;;Test 2 u can remove comments to try 
#|
  ;; Test 2: City connections
  (format t "~%Test 2: City Connections~%")
  	(let ((axioms '(
                  (("direct_route" "nyc" "boston"))
                  (("direct_route" "boston" "montreal"))
                  (("can_reach" "X" "Y") "<" ("direct_route" "X" "Y"))
                  (("can_reach" "X" "Y") "<" ("can_reach" "X" "Z") ("can_reach" "Z" "Y"))))
     	       (query '(("can_reach" "X" "montreal"))))
    	(format t "Query (can_reach X montreal): ~A~%" (prolog_prove axioms query)))
|#


;;Test 3 u can remove comments to try 
#|
  ;; Test 3: Knowledge dependencies
  (format t "~%Test 3: Knowledge Dependencies~%")
  	(let ((axioms '(
                  (("requires" "algebra" "arithmetic"))
                  (("requires" "calculus" "algebra"))
                  (("prerequisite" "X" "Y") "<" ("requires" "X" "Y"))
                  (("prerequisite" "X" "Y") "<" ("prerequisite" "X" "Z") ("prerequisite" "Z" "Y"))))
               (query '(("prerequisite" "X" "arithmetic"))))
   	 (format t "Query (prerequisite X arithmetic): ~A~%" (prolog_prove axioms query)))
|#


;;Test 4 u can remove comments to try 
#|
  ;; Test 4: Dependencies
  (format t "~%Test 4: Dependencies~%")
  	(let ((axioms '(
                  (("depends" "app1" "lib1"))
                  (("depends" "app2" "app1"))
                  (("depends" "app3" "app2"))
                  (("requires" "X" "Y") "<" ("depends" "X" "Y"))
                  (("requires" "X" "Y") "<" ("depends" "X" "Z") ("requires" "Z" "Y"))))
              (query1 '(("requires" "X" "lib1")))
              (query2 '(("requires" "app3" "lib1"))))
    	(format t "Query1: ~A~%" (prolog_prove axioms query1))
   	(format t "Query2: ~A~%" (prolog_prove axioms query2)))
|#

(format t "~%Test Completed!~%")

)

(test_prolog)
