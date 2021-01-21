(let ([x Int 10][y Lambda (lambda ([z Int]) (+ z 1))])
	(if #t
		(if #f
			"hola"
			lambda ([w Int]) (+ w 10))))

