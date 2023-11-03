(defun fib(n)(if(< n 2)1(+(fib(- n 2))(fib(- n 1)))))(print(fib 10))(exit)
